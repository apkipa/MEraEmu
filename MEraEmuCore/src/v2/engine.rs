use std::{
    any,
    borrow::Cow,
    cell::RefCell,
    collections::{BTreeMap, VecDeque},
    num::{NonZero, NonZeroUsize},
    ops::{ControlFlow, DerefMut},
    sync::atomic::AtomicBool,
};

use anyhow::Context;
use bstr::ByteSlice;
use cstree::{
    build::NodeCache,
    interning::{InternKey, Interner, Resolver, TokenKey},
};
use either::Either;
use hashbrown::{HashMap, HashSet};
use indoc::indoc;
use interning::ThreadedTokenInterner;
use iter::{IndexedSequentialParallelBridge, ParallelIndexedSequentialBridge};
use itertools::Itertools;
use num_ordinal::{ordinal0, Ordinal, Osize};
use once_cell::sync::Lazy;
use rayon::iter::ParallelIterator;
use rclite::{Arc, Rc};
use regex::Regex;
use rustc_hash::FxBuildHasher;
use safer_ffi::derive_ReprC;
use serde::{Deserialize, Serialize};
use smallvec::smallvec;

#[cfg(feature = "legacy_cst")]
use crate::cst::interpret::{EraInterpretError, EraInterpreter};
use crate::types::*;

use super::{
    codegen::EraCodeGenerator,
    lexer::EraLexer,
    vm::{EraExecIp, EraFuncExecFrame, EraVirtualMachine, EraVirtualMachineState},
};

use crate::util::rcstr::{self, ArcStr, RcStr};
use crate::util::syntax::*;
use crate::util::*;

pub use crate::types::EraCompilerHostFile as MEraEngineHostFile;

/* Design notes:
 * * Dynamic evaluation from strings:
 * - The engine can evaluate expressions (Int/Str) / statements (Void) from strings.
 * When requested from the debugger eval interface, the engine will protect the
 * current execution state and evaluate the expression in a new context inherited
 * from the current one, which cannot be inspected (if something goes wrong, the
 * engine will restore the original state before giving control back to the user).
 * If evaluation happens in code because of stuff like PRINTFORMS, the engine will
 * do the same thing, but will instead allow the user to pause and inspect the new context.
 * The new context appears as a special "<VM>/VM{number}" source file in the source
 * map, and the user can inspect the variables and call stack as usual.
 * - Since the evaluation goes through the same code paths for compilation and execution,
 * it is possible that the interning pool gets polluted with temporary tokens, which
 * affects both memory usage and snapshot capabilities. To mitigate this, the engine
 * will use a separate interning pool for evaluation, which will be cleared after each
 * evaluation.
 */

#[derive(Debug)]
struct InitialVarDesc {
    pub is_string: bool,
    pub dims: EraVarDims,
    pub src_file: ArcStr,
    pub src_span: SrcSpan,
    pub is_const: bool,
    pub is_charadata: bool,
    pub is_global: bool,
    pub is_savedata: bool,
    pub initial_sval: Option<Vec<StrValue>>,
    pub initial_ival: Option<Vec<IntValue>>,
}

pub struct MEraEngine<Callback> {
    ctx: EraCompilerCtx<'static, Callback>,
    interner: Arc<ThreadedTokenInterner>,
    vm_state: EraVirtualMachineState,
    config: MEraEngineConfig,
}

// Charas variables grow dynamically. The initial capacity is 128, and it grows by 8 each time.
pub const INITIAL_CHARA_CAP: u32 = 128;
pub const CHARA_CAP_GROWTH_STEP: u32 = 8;

#[derive(thiserror::Error, Debug, Serialize, Deserialize)]
#[error("{msg}")]
pub struct MEraEngineError {
    pub msg: String,
}
impl MEraEngineError {
    pub fn new(msg: String) -> Self {
        MEraEngineError { msg }
    }
}
impl From<anyhow::Error> for MEraEngineError {
    fn from(value: anyhow::Error) -> Self {
        Self::new(value.to_string())
    }
}
impl From<String> for MEraEngineError {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}
impl From<MEraEngineError> for String {
    fn from(value: MEraEngineError) -> Self {
        value.msg
    }
}

#[derive_ReprC]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(u32)]
pub enum MEraEngineVmCacheStrategy {
    /// No cache.
    Disabled,
    /// Cache an array of pointers to subroutines. (unsupported)
    Simple,
    /// Generate a list of CALL instructions.
    FastJit,
}

#[derive_ReprC]
#[derive(Debug, Clone, Serialize, Deserialize)]
#[repr(C)]
pub struct MEraEngineConfig {
    /// Memory limit in bytes. 0 means no limit.
    pub memory_limit: u64,
    /// Number of threads to use for parallel compilation. 0 means auto. Should set to 1
    /// when debugging MEraEmuCore engine to make things easier.
    pub threads_cnt: u32,
    /// Whether to enable debug mode.
    pub enable_debug: bool,
    /// Whether to disable source map generation. This can reduce memory usage,
    /// but will make debugging harder.
    pub no_src_map: bool,
    /// Cache strategy for the virtual machine when executing scripts.
    pub vm_cache_strategy: MEraEngineVmCacheStrategy,
}
impl Default for MEraEngineConfig {
    fn default() -> Self {
        MEraEngineConfig {
            memory_limit: 0,
            threads_cnt: 0,
            enable_debug: false,
            no_src_map: false,
            vm_cache_strategy: MEraEngineVmCacheStrategy::Disabled,
        }
    }
}

pub trait MEraEngineSysCallback {
    /// Callback for script errors.
    fn on_error(&mut self, diag: &DiagnosticProvider) {}
    /// Callback for RAND statements. Note that you should always return a fully
    /// filled random u64; the engine will internally cache entropy to reduce
    /// the total amount of syscalls.
    fn on_get_rand(&mut self) -> u64 {
        0
    }
    /// Callback for PRINT family statements.
    fn on_print(&mut self, content: &str, flags: EraPrintExtendedFlags) {}
    //fn on_debugprint(&mut self, content: &str, flags: EraPrintExtendedFlags);
    /// Callback for HTML_PRINT statements.
    fn on_html_print(&mut self, content: &str, no_single: i64) {}
    fn on_wait(&mut self, any_key: bool, is_force: bool) {}
    fn on_twait(&mut self, duration: i64, is_force: bool) {}
    fn on_input_int(
        &mut self,
        default_value: Option<i64>,
        can_click: bool,
        allow_skip: bool,
    ) -> ControlFlow<(), Option<i64>> {
        ControlFlow::Continue(None)
    }
    fn on_input_str(
        &mut self,
        default_value: Option<&str>,
        can_click: bool,
        allow_skip: bool,
    ) -> ControlFlow<(), Option<String>> {
        ControlFlow::Continue(None)
    }
    fn on_tinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<i64>> {
        ControlFlow::Continue(None)
    }
    fn on_tinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<String>> {
        ControlFlow::Continue(None)
    }
    fn on_oneinput_int(&mut self, default_value: Option<i64>) -> ControlFlow<(), Option<i64>> {
        ControlFlow::Continue(None)
    }
    fn on_oneinput_str(&mut self, default_value: Option<&str>) -> ControlFlow<(), Option<String>> {
        ControlFlow::Continue(None)
    }
    fn on_toneinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<i64>> {
        ControlFlow::Continue(None)
    }
    fn on_toneinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<String>> {
        ControlFlow::Continue(None)
    }
    fn on_reuselastline(&mut self, content: &str) {}
    fn on_clearline(&mut self, count: i64) {}
    fn on_print_button(&mut self, content: &str, value: &str, flags: EraPrintExtendedFlags) {}
    /// Callbacks for variable getters & setters. May return a string to report as execution errors.
    fn on_var_get_int(&mut self, name: &str, idx: usize) -> Result<i64, anyhow::Error> {
        Ok(0)
    }
    fn on_var_get_str(&mut self, name: &str, idx: usize) -> Result<String, anyhow::Error> {
        Ok(String::new())
    }
    fn on_var_set_int(&mut self, name: &str, idx: usize, val: i64) -> Result<(), anyhow::Error> {
        Ok(())
    }
    fn on_var_set_str(&mut self, name: &str, idx: usize, val: &str) -> Result<(), anyhow::Error> {
        Ok(())
    }
    // Graphics subsystem
    fn on_gcreate(&mut self, gid: i64, width: i64, height: i64) -> i64 {
        0
    }
    fn on_gcreatefromfile(&mut self, gid: i64, path: &str) -> i64 {
        0
    }
    fn on_gdispose(&mut self, gid: i64) -> i64 {
        0
    }
    fn on_gcreated(&mut self, gid: i64) -> i64 {
        0
    }
    fn on_gdrawsprite(
        &mut self,
        gid: i64,
        sprite_name: &str,
        dest_x: i64,
        dest_y: i64,
        dest_width: i64,
        dest_height: i64,
        color_matrix: Option<&EraColorMatrix>,
    ) -> i64 {
        0
    }
    fn on_gclear(&mut self, gid: i64, color: i64) -> i64 {
        0
    }
    fn on_spritecreate(
        &mut self,
        name: &str,
        gid: i64,
        x: i64,
        y: i64,
        width: i64,
        height: i64,
    ) -> i64 {
        0
    }
    fn on_spritedispose(&mut self, name: &str) -> i64 {
        0
    }
    fn on_spritecreated(&mut self, name: &str) -> i64 {
        0
    }
    fn on_spriteanimecreate(&mut self, name: &str, width: i64, height: i64) -> i64 {
        0
    }
    fn on_spriteanimeaddframe(
        &mut self,
        name: &str,
        gid: i64,
        x: i64,
        y: i64,
        width: i64,
        height: i64,
        offset_x: i64,
        offset_y: i64,
        delay: i64,
    ) -> i64 {
        0
    }
    fn on_spritewidth(&mut self, name: &str) -> i64 {
        0
    }
    fn on_spriteheight(&mut self, name: &str) -> i64 {
        0
    }
    // Filesystem subsystem
    fn on_open_host_file(
        &mut self,
        path: &str,
        can_write: bool,
    ) -> anyhow::Result<Box<dyn MEraEngineHostFile>> {
        anyhow::bail!("no such file");
    }
    fn on_check_host_file_exists(&mut self, path: &str) -> anyhow::Result<bool> {
        Ok(false)
    }
    fn on_delete_host_file(&mut self, path: &str) -> anyhow::Result<()> {
        anyhow::bail!("no such file");
    }
    fn on_list_host_file(&mut self, path: &str) -> anyhow::Result<Vec<String>> {
        Ok(Vec::new())
    }
    // Others
    fn on_check_font(&mut self, font_name: &str) -> i64 {
        0
    }
    // NOTE: Returns UTC timestamp (in milliseconds).
    fn on_get_host_time(&mut self) -> u64 {
        0
    }
    fn on_get_config_int(&mut self, name: &str) -> anyhow::Result<i64> {
        anyhow::bail!("no such config entry");
    }
    fn on_get_config_str(&mut self, name: &str) -> anyhow::Result<String> {
        anyhow::bail!("no such config entry");
    }
    // NOTE: Returns { b15 = <key down>, b0 = <key triggered> }. For key codes, refer
    //       to https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes.
    fn on_get_key_state(&mut self, key_code: i64) -> i64 {
        0
    }
}

impl<T: MEraEngineSysCallback> EraCompilerCallback for T {
    fn emit_diag(&mut self, diag: crate::types::DiagnosticProvider) {
        self.on_error(&diag);
        diag.into_diag().cancel();
    }

    fn on_get_rand(&mut self) -> u64 {
        self.on_get_rand()
    }

    fn on_print(&mut self, content: &str, flags: EraPrintExtendedFlags) {
        self.on_print(content, flags)
    }

    fn on_debugprint(&mut self, content: &str, flags: EraPrintExtendedFlags) {
        self.on_print(content, flags)
    }

    fn on_html_print(&mut self, content: &str, no_single: i64) {
        self.on_html_print(content, no_single)
    }

    fn on_wait(&mut self, any_key: bool, is_force: bool) {
        self.on_wait(any_key, is_force)
    }

    fn on_twait(&mut self, duration: i64, is_force: bool) {
        self.on_twait(duration, is_force)
    }

    fn on_input_int(
        &mut self,
        default_value: Option<i64>,
        can_click: bool,
        allow_skip: bool,
    ) -> ControlFlow<(), Option<i64>> {
        self.on_input_int(default_value, can_click, allow_skip)
    }

    fn on_input_str(
        &mut self,
        default_value: Option<&str>,
        can_click: bool,
        allow_skip: bool,
    ) -> ControlFlow<(), Option<String>> {
        self.on_input_str(default_value, can_click, allow_skip)
    }

    fn on_tinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<i64>> {
        self.on_tinput_int(
            time_limit,
            default_value,
            show_prompt,
            expiry_msg,
            can_click,
        )
    }

    fn on_tinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<String>> {
        self.on_tinput_str(
            time_limit,
            default_value,
            show_prompt,
            expiry_msg,
            can_click,
        )
    }

    fn on_oneinput_int(&mut self, default_value: Option<i64>) -> ControlFlow<(), Option<i64>> {
        self.on_oneinput_int(default_value)
    }

    fn on_oneinput_str(&mut self, default_value: Option<&str>) -> ControlFlow<(), Option<String>> {
        self.on_oneinput_str(default_value)
    }

    fn on_toneinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<i64>> {
        self.on_toneinput_int(
            time_limit,
            default_value,
            show_prompt,
            expiry_msg,
            can_click,
        )
    }

    fn on_toneinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<String>> {
        self.on_toneinput_str(
            time_limit,
            default_value,
            show_prompt,
            expiry_msg,
            can_click,
        )
    }

    fn on_reuselastline(&mut self, content: &str) {
        self.on_reuselastline(content)
    }

    fn on_clearline(&mut self, count: i64) {
        self.on_clearline(count)
    }

    fn on_print_button(&mut self, content: &str, value: &str, flags: EraPrintExtendedFlags) {
        self.on_print_button(content, value, flags)
    }

    fn on_var_get_int(&mut self, name: &str, idx: usize) -> Result<i64, anyhow::Error> {
        self.on_var_get_int(name, idx)
    }

    fn on_var_get_str(&mut self, name: &str, idx: usize) -> Result<String, anyhow::Error> {
        self.on_var_get_str(name, idx)
    }

    fn on_var_set_int(&mut self, name: &str, idx: usize, val: i64) -> Result<(), anyhow::Error> {
        self.on_var_set_int(name, idx, val)
    }

    fn on_var_set_str(&mut self, name: &str, idx: usize, val: &str) -> Result<(), anyhow::Error> {
        self.on_var_set_str(name, idx, val)
    }

    fn on_gcreate(&mut self, gid: i64, width: i64, height: i64) -> i64 {
        self.on_gcreate(gid, width, height)
    }

    fn on_gcreatefromfile(&mut self, gid: i64, path: &str) -> i64 {
        self.on_gcreatefromfile(gid, path)
    }

    fn on_gdispose(&mut self, gid: i64) -> i64 {
        self.on_gdispose(gid)
    }

    fn on_gcreated(&mut self, gid: i64) -> i64 {
        self.on_gcreated(gid)
    }

    fn on_gdrawsprite(
        &mut self,
        gid: i64,
        sprite_name: &str,
        dest_x: i64,
        dest_y: i64,
        dest_width: i64,
        dest_height: i64,
        color_matrix: Option<&EraColorMatrix>,
    ) -> i64 {
        self.on_gdrawsprite(
            gid,
            sprite_name,
            dest_x,
            dest_y,
            dest_width,
            dest_height,
            color_matrix,
        )
    }

    fn on_gclear(&mut self, gid: i64, color: i64) -> i64 {
        self.on_gclear(gid, color)
    }

    fn on_spritecreate(
        &mut self,
        name: &str,
        gid: i64,
        x: i64,
        y: i64,
        width: i64,
        height: i64,
    ) -> i64 {
        self.on_spritecreate(name, gid, x, y, width, height)
    }

    fn on_spritedispose(&mut self, name: &str) -> i64 {
        self.on_spritedispose(name)
    }

    fn on_spritecreated(&mut self, name: &str) -> i64 {
        self.on_spritecreated(name)
    }

    fn on_spriteanimecreate(&mut self, name: &str, width: i64, height: i64) -> i64 {
        self.on_spriteanimecreate(name, width, height)
    }

    fn on_spriteanimeaddframe(
        &mut self,
        name: &str,
        gid: i64,
        x: i64,
        y: i64,
        width: i64,
        height: i64,
        offset_x: i64,
        offset_y: i64,
        delay: i64,
    ) -> i64 {
        self.on_spriteanimeaddframe(name, gid, x, y, width, height, offset_x, offset_y, delay)
    }

    fn on_spritewidth(&mut self, name: &str) -> i64 {
        self.on_spritewidth(name)
    }

    fn on_spriteheight(&mut self, name: &str) -> i64 {
        self.on_spriteheight(name)
    }

    fn on_open_host_file(
        &mut self,
        path: &str,
        can_write: bool,
    ) -> anyhow::Result<Box<dyn MEraEngineHostFile>> {
        self.on_open_host_file(path, can_write)
    }

    fn on_check_host_file_exists(&mut self, path: &str) -> anyhow::Result<bool> {
        self.on_check_host_file_exists(path)
    }

    fn on_delete_host_file(&mut self, path: &str) -> anyhow::Result<()> {
        self.on_delete_host_file(path)
    }

    fn on_list_host_file(&mut self, path: &str) -> anyhow::Result<Vec<String>> {
        self.on_list_host_file(path)
    }

    fn on_check_font(&mut self, font_name: &str) -> i64 {
        self.on_check_font(font_name)
    }

    fn on_get_host_time(&mut self) -> u64 {
        self.on_get_host_time()
    }

    fn on_get_config_int(&mut self, name: &str) -> anyhow::Result<i64> {
        self.on_get_config_int(name)
    }

    fn on_get_config_str(&mut self, name: &str) -> anyhow::Result<String> {
        self.on_get_config_str(name)
    }

    fn on_get_key_state(&mut self, key_code: i64) -> i64 {
        self.on_get_key_state(key_code)
    }
}

#[derive_ReprC]
#[repr(u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy)]
pub enum EraCsvLoadKind {
    Abl,
    Exp,
    Talent,
    Palam,
    Train,
    Mark,
    Item,
    Base,
    Source,
    Ex,
    Str,
    Equip,
    TEquip,
    Flag,
    TFlag,
    CFlag,
    TCVar,
    CStr,
    Stain,
    CDFlag1,
    CDFlag2,
    StrName,
    TStr,
    SaveStr,
    Global,
    Globals,
    Chara_,
    _Replace,
    _Rename,
    GameBase,
    VariableSize,
    ImageResources,
}

pub struct EmptyCallback;
impl MEraEngineSysCallback for EmptyCallback {}

#[derive(Debug)]
pub struct MEraEngineBuildProgress {
    pub current: usize,
    pub total: usize,
}

pub trait MEraEngineBuilderCallback {
    fn on_build_progress(&mut self, tag: &str, progress: &MEraEngineBuildProgress) {
        // Do nothing
    }
}

impl MEraEngineBuilderCallback for EmptyCallback {}

pub struct MEraEngineBuilder<SysCallback, BuilderCallback> {
    ctx: EraCompilerCtx<'static, SysCallback>,
    builder_callback: Option<BuilderCallback>,
    config: MEraEngineConfig,
    watching_vars: HashSet<Ascii<ArcStr>, FxBuildHasher>,
    initial_vars: Option<HashMap<Ascii<ArcStr>, InitialVarDesc, FxBuildHasher>>,
    interner: Arc<ThreadedTokenInterner>,
    /// Whether the headers have been compiled.
    is_header_finished: bool,
    erh_count: usize,
    erb_count: usize,
    rayon_threadpool: Option<rayon::ThreadPool>,
    erb_loader_rx: Option<std::sync::mpsc::Receiver<MEraEngineAsyncErbLoaderMsg>>,
    gid_file_cache: HashMap<String, i64, FxBuildHasher>,
}

impl<T: MEraEngineSysCallback> MEraEngineBuilder<T, EmptyCallback> {
    pub fn new(callback: T) -> MEraEngineBuilder<T, EmptyCallback> {
        let interner = Arc::new(ThreadedTokenInterner::new());
        let ctx = unsafe { EraCompilerCtx::new(callback, std::mem::transmute(&*interner)) };
        let mut this = MEraEngineBuilder {
            ctx,
            builder_callback: Default::default(),
            config: Default::default(),
            watching_vars: Default::default(),
            initial_vars: Some(Default::default()),
            interner,
            is_header_finished: false,
            erh_count: 0,
            erb_count: 0,
            rayon_threadpool: None,
            erb_loader_rx: None,
            gid_file_cache: Default::default(),
        };

        // Initialize default global variables
        this.add_initial_vars();

        this
    }
}

struct MEraEngineAsyncErbLoaderMsg {
    pub filename: ArcStr,
    pub content: arcstr::ArcStr,
}

#[derive_ReprC]
#[repr(opaque)]
pub struct MEraEngineAsyncErbLoader {
    tx: std::sync::mpsc::Sender<MEraEngineAsyncErbLoaderMsg>,
}

impl MEraEngineAsyncErbLoader {
    pub fn load_erb(&self, filename: &str, content: &[u8]) -> Result<(), MEraEngineError> {
        // Handle UTF-8 BOM
        let content = content
            .strip_prefix("\u{feff}".as_bytes())
            .unwrap_or(content);
        let content = simdutf8::basic::from_utf8(content)
            .map_or_else(|_| String::from_utf8_lossy(content), |s| Cow::Borrowed(s));
        let content: arcstr::ArcStr = content.into();
        let filename = ArcStr::from(filename);

        self.tx
            .send(MEraEngineAsyncErbLoaderMsg { filename, content })
            .map_err(|e| MEraEngineError::new(format!("failed to send message: {e}")))
    }
}

impl<T: MEraEngineSysCallback, U: MEraEngineBuilderCallback> MEraEngineBuilder<T, U> {
    pub fn with_builder_callback<U2: MEraEngineBuilderCallback>(
        self,
        new_callback: U2,
    ) -> MEraEngineBuilder<T, U2> {
        MEraEngineBuilder {
            ctx: self.ctx,
            builder_callback: Some(new_callback),
            config: self.config,
            watching_vars: self.watching_vars,
            initial_vars: self.initial_vars,
            interner: self.interner,
            is_header_finished: self.is_header_finished,
            erh_count: self.erh_count,
            erb_count: self.erb_count,
            rayon_threadpool: self.rayon_threadpool,
            erb_loader_rx: self.erb_loader_rx,
            gid_file_cache: self.gid_file_cache,
        }
    }
    pub fn with_config(mut self, new_config: MEraEngineConfig) -> Self {
        self.config = new_config;
        self
    }

    pub fn get_config(&self) -> MEraEngineConfig {
        self.config.clone()
    }

    pub fn set_config(&mut self, new_config: MEraEngineConfig) -> Result<(), MEraEngineError> {
        self.config = new_config;
        Ok(())
    }

    pub fn load_csv(
        &mut self,
        filename: &str,
        content: &[u8],
        kind: EraCsvLoadKind,
    ) -> Result<(), MEraEngineError> {
        use crate::util::SubsliceOffset;

        // Handle UTF-8 BOM
        let content = content
            .strip_prefix("\u{feff}".as_bytes())
            .unwrap_or(content);
        let content = &arcstr::ArcStr::from(content.to_str_lossy());
        let filename = ArcStr::from(filename);

        // Add to source map
        Arc::get_mut(&mut self.ctx.source_map).unwrap().insert(
            filename.clone(),
            EraSourceFile {
                filename: filename.clone(),
                text: Some(content.clone()),
                compressed_text: None,
                cst_root: None,
                ast_data: None,
                macro_map: Default::default(),
                defines: EraDefineScope::new(),
                kind: EraSourceFileKind::Csv,
                newline_pos: Vec::new(),
                is_transient: false,
            },
        );

        // Loads key-value pairs from CSV to variables (*NAME arrays). Optional reverse lookup indices
        // are also created if `kind` is provided.
        let mut load_kvs =
            |target: &str, kind: Option<EraCsvVarKind>| -> Result<(), MEraEngineError> {
                let rows = self.parse_csv::<2>(filename.clone(), content);

                let Some(initial_vars) = self.initial_vars.as_mut() else {
                    return Err(MEraEngineError::new("CSV loaded too late".into()));
                };

                let Some(var_desc) = initial_vars.get_mut(Ascii::new_str(target)) else {
                    return Err(MEraEngineError::new(format!(
                        "variable `{target}` not found"
                    )));
                };
                if var_desc.dims.len() != 1 {
                    return Err(MEraEngineError::new(
                        "variable dimension mismatch; 2 columns are expected".into(),
                    ));
                }
                let mut initial_sval = vec![StrValue::default(); var_desc.dims[0] as _];

                let diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                for [index, name] in rows {
                    let offset = content.subslice_offset(index).unwrap();
                    let index_span = SrcSpan::new(SrcPos(offset as _), index.len() as _);

                    let Ok(index) = atoi_simd::parse_pos::<u32>(index.as_bytes()) else {
                        let mut diag = diag.clone();
                        diag.span_err(Default::default(), index_span, "invalid variable index");
                        self.ctx.emit_diag(diag);
                        continue;
                    };
                    let name = ArcStr::from(name);

                    // Add to *NAME array
                    if let Some(val) = initial_sval.get_mut(index as usize) {
                        *val = StrValue::new(name.clone());
                    } else {
                        let mut diag = diag.clone();
                        diag.span_err(
                            Default::default(),
                            index_span,
                            "variable index out of bounds",
                        );
                        self.ctx.emit_diag(diag);
                    }

                    // Add to reverse lookup index
                    if let Some(kind) = kind {
                        self.ctx
                            .csv_indices
                            .entry(Ascii::new(name))
                            .or_default()
                            .push((kind, index));
                    }
                }

                // Apply initial values
                var_desc.initial_sval = Some(initial_sval);

                Ok(())
            };

        let parse_u32 = |this: &mut Self, val: &str| -> Result<u32, ()> {
            crate::v2::routines::parse_int_literal(val.as_bytes())
                .and_then(|x| x.try_into().ok())
                .ok_or_else(|| {
                    let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                    let offset = content.subslice_offset(val).unwrap();
                    diag.span_err(
                        Default::default(),
                        SrcSpan::new(SrcPos(offset as _), val.len() as _),
                        "failed to parse u32",
                    );
                    this.ctx.emit_diag(diag);
                })
        };
        let parse_i64_tail = |this: &mut Self, val: &str| -> Result<i64, ()> {
            let mut val = val;
            let Some(r) = crate::v2::routines::read_int_literal_str_with_sign(&mut val) else {
                let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                let offset = content.subslice_offset(val).unwrap();
                diag.span_err(
                    Default::default(),
                    SrcSpan::new(SrcPos(offset as _), val.len() as _),
                    "failed to parse i64",
                );
                this.ctx.emit_diag(diag);
                return Err(());
            };
            val = val.trim_ascii_start();
            if !val.is_empty() {
                let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                let offset = content.subslice_offset(val).unwrap();
                diag.span_err(
                    Default::default(),
                    SrcSpan::new(SrcPos(offset as _), val.len() as _),
                    format!(
                        "invalid character `{}` after integer; did you mean `;`?",
                        val.chars().next().unwrap()
                    ),
                );
                this.ctx.emit_diag(diag);
            }
            Ok(r)
        };
        let resolve_csv_idx =
            |this: &mut Self, csv_kind: EraCsvVarKind, val: &str| -> Result<u32, ()> {
                let Some(idx) = val.parse().ok().or_else(|| {
                    // Not an integer, parse as CSV index name instead
                    this.ctx
                        .csv_indices
                        .get(Ascii::new_str(val))
                        .and_then(|x| x.iter().find(|x| x.0 == csv_kind))
                        .map(|x| x.1)
                }) else {
                    let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                    let offset = content.subslice_offset(val).unwrap();
                    diag.span_err(
                        Default::default(),
                        SrcSpan::new(SrcPos(offset as _), val.len() as _),
                        format!("invalid CSV variable index `{val}`"),
                    );
                    this.ctx.emit_diag(diag);
                    return Err(());
                };
                Ok(idx)
            };

        // Adds a system variable.
        let add_var_i64_str =
            |this: &mut Self, name: &str, val: &str, span: SrcSpan| -> Result<(), ()> {
                let val = parse_i64_tail(this, val)?;
                _ = this.ctx.variables.add_var(EraVarInfo {
                    name: Ascii::new(ArcStr::from(name)),
                    val: ArrayValue::new_int_0darr(val),
                    src_file: filename.clone(),
                    src_span: span,
                    is_const: true,
                    is_charadata: false,
                    is_global: false,
                    is_savedata: false,
                    never_trap: true,
                });
                Ok(())
            };
        let add_var_str =
            |this: &mut Self, name: &str, val: &str, span: SrcSpan| -> Result<(), ()> {
                _ = this.ctx.variables.add_var(EraVarInfo {
                    name: Ascii::new(ArcStr::from(name)),
                    val: ArrayValue::new_str_0darr(ArcStr::from(val)),
                    src_file: filename.clone(),
                    src_span: span,
                    is_const: true,
                    is_charadata: false,
                    is_global: false,
                    is_savedata: false,
                    never_trap: true,
                });
                Ok(())
            };
        let add_arr_i64_str =
            |this: &mut Self, name: &str, val: &str, span: SrcSpan| -> Result<(), ()> {
                let val = val
                    .split('/')
                    .map(|x| parse_i64_tail(this, x).map(IntValue::new))
                    .collect::<Result<Vec<_>, _>>()?;
                let val = ArrayValue::new_int_arr(smallvec![val.len() as _], val);
                _ = this.ctx.variables.add_var(EraVarInfo {
                    name: Ascii::new(ArcStr::from(name)),
                    val,
                    src_file: filename.clone(),
                    src_span: span,
                    is_const: true,
                    is_charadata: false,
                    is_global: false,
                    is_savedata: false,
                    never_trap: true,
                });
                Ok(())
            };

        match kind {
            EraCsvLoadKind::_Rename => {
                let rows = self.parse_csv::<2>(filename.clone(), content);
                for [out_replace, in_replace] in rows {
                    let offset = content.subslice_offset(in_replace).unwrap();
                    let data = EraDefineData {
                        filename: filename.clone(),
                        span: SrcSpan::new(SrcPos(offset as _), in_replace.len() as _),
                        data: out_replace.into(),
                    };
                    // TODO: Detect duplication?
                    self.ctx.global_replace.insert(in_replace.into(), data);
                }
            }
            EraCsvLoadKind::VariableSize => {
                if self.initial_vars.is_none() {
                    return Err(MEraEngineError::new("CSV loaded too late".into()));
                }

                let rows = self.parse_csv_loose(filename.clone(), content);
                let initial_vars = self.initial_vars.as_mut().unwrap();
                for row in rows {
                    if let [name, size] = row[..] {
                        let name_span = {
                            let offset = content.subslice_offset(name).unwrap();
                            SrcSpan::new(SrcPos(offset as _), name.len() as _)
                        };
                        let Ok(size) = size.parse() else {
                            let mut diag =
                                Diagnostic::with_src(filename.clone(), content.as_bytes());
                            let offset = content.subslice_offset(size).unwrap();
                            diag.span_err(
                                Default::default(),
                                SrcSpan::new(SrcPos(offset as _), size.len() as _),
                                "invalid variable size",
                            );
                            self.ctx.emit_diag(diag);
                            continue;
                        };
                        let Some(var_desc) = initial_vars.get_mut(Ascii::new_str(name)) else {
                            let mut diag =
                                Diagnostic::with_src(filename.clone(), content.as_bytes());
                            let offset = content.subslice_offset(name).unwrap();
                            diag.span_err(
                                Default::default(),
                                SrcSpan::new(SrcPos(offset as _), name.len() as _),
                                "variable not found for given line",
                            );
                            self.ctx.emit_diag(diag);
                            continue;
                        };
                        if var_desc.dims.len() != 1 {
                            let mut diag =
                                Diagnostic::with_src(filename.clone(), content.as_bytes());
                            let offset = content.subslice_offset(name).unwrap();
                            diag.span_err(
                                Default::default(),
                                SrcSpan::new(SrcPos(offset as _), name.len() as _),
                                "variable size line does not match variable dimensions",
                            );
                            self.ctx.emit_diag(diag);
                            continue;
                        }

                        var_desc.src_file = filename.clone();
                        var_desc.src_span = name_span;
                        var_desc.dims[0] = size;
                        // Apply dimension to associated variables
                        let vars_groups = [
                            ["ITEM", "ITEMNAME", "ITEMPRICE", "ITEMSALES"].as_slice(),
                            ["BASE", "BASENAME", "MAXBASE", "DOWNBASE", "LOSEBASE"].as_slice(),
                            [
                                "PALAM",
                                "PALAMNAME",
                                "JUEL",
                                // "JUELNAME",
                                "GOTJUEL",
                                "UP",
                                "DOWN",
                                "CUP",
                                "CDOWN",
                            ]
                            .as_slice(),
                            ["EX", "EXNAME", "NOWEX"].as_slice(),
                        ];
                        if let Some(group) = vars_groups.into_iter().find(|x| {
                            x.iter()
                                .copied()
                                .map(Ascii::new_str)
                                .contains(&Ascii::new_str(name))
                        }) {
                            for i in group.iter().copied() {
                                initial_vars.get_mut(Ascii::new_str(i)).unwrap().dims[0] = size;
                            }
                        } else {
                            if let Some(name) = name.strip_suffix("NAME") {
                                let name = Ascii::new_str(name);
                                if let Some(var_desc) = initial_vars.get_mut(name) {
                                    var_desc.dims[0] = size;
                                }
                            } else if let Some(var_desc) =
                                initial_vars.get_mut(Ascii::new_str(&format!("{name}NAME")))
                            {
                                var_desc.dims[0] = size;
                            }
                        }
                    } else if let [name, size1, size2] = row[..] {
                        let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                        let offset = content.subslice_offset(name).unwrap();
                        diag.span_err(
                            Default::default(),
                            SrcSpan::new(SrcPos(offset as _), name.len() as _),
                            "this variable size line is not yet supported",
                        );
                        self.ctx.emit_diag(diag);
                    } else {
                        let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                        let offset = content.subslice_offset(row[0]).unwrap();
                        diag.span_err(
                            Default::default(),
                            SrcSpan::new(SrcPos(offset as _), row[0].len() as _),
                            "invalid variable size line",
                        );
                        self.ctx.emit_diag(diag);
                    }
                }
            }
            EraCsvLoadKind::Chara_ => {
                use EraCsvVarKind::*;

                static RE: Lazy<Regex> =
                    Lazy::new(|| Regex::new(r"^(?:|.*[/\\])Chara(\d+).*\.(?i:csv)$").unwrap());
                let Some(csv_no) = RE.captures(&filename).and_then(|x| x[1].parse().ok()) else {
                    return Err(MEraEngineError::new("Chara csv number is invalid".into()));
                };

                // Fill character template
                let rows = self.parse_csv_loose(filename.clone(), content);
                let mut chara_template = EraCharaInitTemplate::default();
                chara_template.csv_no = csv_no;

                macro_rules! try_ {
                    ($expression:expr) => {{
                        let Ok(x) = $expression else {
                            continue;
                        };
                        x
                    }};
                }

                for cols in rows {
                    // Guaranees at least 2 columns (cols[0] and cols[1]) are present.
                    if cols.len() < 2 {
                        let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                        let offset = content.subslice_offset(&cols[0]).unwrap();
                        diag.span_err(
                            Default::default(),
                            SrcSpan::new(SrcPos(offset as _), cols[0].len() as _),
                            "too few columns in CSV row",
                        );
                        self.ctx.emit_diag(diag);
                        continue;
                    }

                    let get_col_str = |this: &mut Self, idx: usize| -> &str {
                        cols.get(idx).copied().unwrap_or_else(|| {
                            let mut diag =
                                Diagnostic::with_src(filename.clone(), content.as_bytes());
                            let offset = content.subslice_offset(&cols[0]).unwrap();
                            diag.span_err(
                                Default::default(),
                                SrcSpan::new(SrcPos(offset as _), cols[0].len() as _),
                                format!(
                                    "missing {} column in CSV row; assuming an empty string was given",
                                    Osize::from0(idx)
                                ),
                            );
                            this.ctx.emit_diag(diag);
                            ""
                        })
                    };
                    let get_col_i64 = |this: &mut Self, idx: usize| -> i64 {
                        let Some(val) = cols.get(idx) else {
                            let mut diag =
                                Diagnostic::with_src(filename.clone(), content.as_bytes());
                            let offset = content.subslice_offset(&cols[0]).unwrap();
                            diag.span_err(
                                Default::default(),
                                SrcSpan::new(SrcPos(offset as _), cols[0].len() as _),
                                format!(
                                    "missing {} column in CSV row; assuming 1 was given",
                                    Osize::from0(idx)
                                ),
                            );
                            this.ctx.emit_diag(diag);
                            return 1;
                        };
                        parse_i64_tail(this, val).unwrap_or(1)
                    };

                    match cols[0].to_ascii_uppercase().as_str() {
                        "NO" | "番号" => {
                            chara_template.no = try_!(parse_u32(self, cols[1]));
                        }
                        "NAME" | "名前" => {
                            chara_template.name = cols[1].into();
                        }
                        "CALLNAME" | "呼び名" => {
                            chara_template.callname = cols[1].into();
                        }
                        "NICKNAME" | "あだ名" => {
                            chara_template.nickname = cols[1].into();
                        }
                        "MASTERNAME" | "主人の呼び方" => {
                            chara_template.mastername = cols[1].into();
                        }
                        "MARK" | "刻印" => {
                            let idx = try_!(resolve_csv_idx(self, CsvMark, cols[1]));
                            let val = get_col_i64(self, 2);
                            chara_template.mark.insert(idx, val);
                        }
                        "EXP" | "経験" => {
                            let idx = try_!(resolve_csv_idx(self, CsvExp, cols[1]));
                            let val = get_col_i64(self, 2);
                            chara_template.exp.insert(idx, val);
                        }
                        "ABL" | "能力" => {
                            let idx = try_!(resolve_csv_idx(self, CsvAbl, cols[1]));
                            let val = get_col_i64(self, 2);
                            chara_template.abl.insert(idx, val);
                        }
                        "BASE" | "基礎" => {
                            let idx = try_!(resolve_csv_idx(self, CsvBase, cols[1]));
                            let val = get_col_i64(self, 2);
                            chara_template.maxbase.insert(idx, val);
                        }
                        "TALENT" | "素質" => {
                            let idx = try_!(resolve_csv_idx(self, CsvTalent, cols[1]));
                            let val = get_col_i64(self, 2);
                            chara_template.talent.insert(idx, val);
                        }
                        "RELATION" | "相性" => {
                            let idx = get_col_i64(self, 1);
                            let val = get_col_i64(self, 2);
                            chara_template.relation.insert(idx as _, val);
                        }
                        "CFLAG" | "フラグ" => {
                            let idx = try_!(resolve_csv_idx(self, CsvCFlag, cols[1]));
                            let val = get_col_i64(self, 2);
                            chara_template.cflag.insert(idx, val);
                        }
                        "EQUIP" | "装着物" => {
                            let idx = try_!(resolve_csv_idx(self, CsvEquip, cols[1]));
                            let val = get_col_i64(self, 2);
                            chara_template.equip.insert(idx, val);
                        }
                        "JUEL" | "珠" => {
                            let idx = try_!(resolve_csv_idx(self, CsvPalam, cols[1]));
                            let val = get_col_i64(self, 2);
                            chara_template.juel.insert(idx, val);
                        }
                        "CSTR" => {
                            let idx = try_!(resolve_csv_idx(self, CsvCStr, cols[1]));
                            let val = get_col_str(self, 2);
                            chara_template.cstr.insert(idx, val.into());
                        }
                        "ISASSI" | "助手" => continue,
                        _ => {
                            let mut diag =
                                Diagnostic::with_src(filename.clone(), content.as_bytes());
                            let offset = content.subslice_offset(&cols[0]).unwrap();
                            diag.span_err(
                                Default::default(),
                                SrcSpan::new(SrcPos(offset as _), cols[0].len() as _),
                                "unknown chara property in CSV row, ignoring",
                            );
                            self.ctx.emit_diag(diag);
                        }
                    }
                }

                // Add to character template list
                let key = chara_template.no;
                match self.ctx.chara_templates.entry(key) {
                    std::collections::btree_map::Entry::Vacant(entry) => {
                        entry.insert(chara_template);
                    }
                    std::collections::btree_map::Entry::Occupied(_) => {
                        return Err(MEraEngineError::new(format!(
                            "Character CSV id {key} already exists"
                        )));
                    }
                }
            }
            EraCsvLoadKind::Item => {
                if self.initial_vars.is_none() {
                    return Err(MEraEngineError::new("CSV loaded too late".into()));
                }

                let rows = self.parse_csv_loose(filename.clone(), content);
                let initial_vars = self.initial_vars.as_mut().unwrap();

                let target = "ITEMNAME";
                let target2 = "ITEMPRICE";
                let [name_vd, price_vd] = initial_vars
                    .get_many_mut([target, target2].map(Ascii::new_str))
                    .ok_or_else(|| MEraEngineError::new("variable not found".into()))?;
                let mut initial_nameval = vec![StrValue::default(); name_vd.dims[0] as _];
                let mut initial_priceval = vec![IntValue::default(); price_vd.dims[0] as _];

                let diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                for mut cols in rows {
                    cols.truncate(3);

                    let index = cols[0];
                    let offset = content.subslice_offset(index).unwrap();
                    let index_span = SrcSpan::new(SrcPos(offset as _), index.len() as _);

                    if cols.len() < 2 {
                        let mut diag = diag.clone();
                        diag.span_err(Default::default(), index_span, "too few columns in CSV row");
                        self.ctx.emit_diag(diag);
                        continue;
                    }

                    let Ok(index) = atoi_simd::parse_pos::<u32>(index.as_bytes()) else {
                        let mut diag = diag.clone();
                        diag.span_err(Default::default(), index_span, "invalid variable index");
                        self.ctx.emit_diag(diag);
                        continue;
                    };
                    let name = ArcStr::from(cols[1]);
                    let price = cols
                        .get(2)
                        .map(|x| {
                            x.parse().unwrap_or_else(|_| {
                                let mut diag = diag.clone();
                                let offset = content.subslice_offset(x).unwrap();
                                diag.span_err(
                                    Default::default(),
                                    SrcSpan::new(SrcPos(offset as _), x.len() as _),
                                    "failed to parse price",
                                );
                                self.ctx.emit_diag(diag);
                                0
                            })
                        })
                        .unwrap_or(0);

                    // Add to *NAME array
                    if let Some(val) = initial_nameval.get_mut(index as usize) {
                        *val = StrValue::new(name.clone());
                    } else {
                        let mut diag = diag.clone();
                        diag.span_err(
                            Default::default(),
                            index_span,
                            "variable index out of bounds",
                        );
                        self.ctx.emit_diag(diag);
                    }

                    // Add to *PRICE array
                    if let Some(val) = initial_priceval.get_mut(index as usize) {
                        *val = IntValue::new(price);
                    } else {
                        let mut diag = diag.clone();
                        diag.span_err(
                            Default::default(),
                            index_span,
                            "variable index out of bounds",
                        );
                        self.ctx.emit_diag(diag);
                    }

                    // Add to reverse lookup index
                    let kind = Some(EraCsvVarKind::CsvItem);
                    if let Some(kind) = kind {
                        self.ctx
                            .csv_indices
                            .entry(Ascii::new(name))
                            .or_default()
                            .push((kind, index));
                    }
                }

                // Apply initial values
                name_vd.initial_sval = Some(initial_nameval);
                price_vd.initial_ival = Some(initial_priceval);
            }
            EraCsvLoadKind::Abl => load_kvs("ABLNAME", Some(EraCsvVarKind::CsvAbl))?,
            EraCsvLoadKind::Exp => load_kvs("EXPNAME", Some(EraCsvVarKind::CsvExp))?,
            EraCsvLoadKind::Talent => load_kvs("TALENTNAME", Some(EraCsvVarKind::CsvTalent))?,
            EraCsvLoadKind::Palam => load_kvs("PALAMNAME", Some(EraCsvVarKind::CsvPalam))?,
            EraCsvLoadKind::Train => load_kvs("TRAINNAME", Some(EraCsvVarKind::CsvTrain))?,
            EraCsvLoadKind::Mark => load_kvs("MARKNAME", Some(EraCsvVarKind::CsvMark))?,
            EraCsvLoadKind::Base => load_kvs("BASENAME", Some(EraCsvVarKind::CsvBase))?,
            EraCsvLoadKind::Source => load_kvs("SOURCENAME", Some(EraCsvVarKind::CsvSource))?,
            EraCsvLoadKind::Ex => load_kvs("EXNAME", Some(EraCsvVarKind::CsvEx))?,
            EraCsvLoadKind::Str => load_kvs("STR", Some(EraCsvVarKind::CsvStr))?,
            EraCsvLoadKind::Equip => load_kvs("EQUIPNAME", Some(EraCsvVarKind::CsvEquip))?,
            EraCsvLoadKind::TEquip => load_kvs("TEQUIPNAME", Some(EraCsvVarKind::CsvTEquip))?,
            EraCsvLoadKind::Flag => load_kvs("FLAGNAME", Some(EraCsvVarKind::CsvFlag))?,
            EraCsvLoadKind::TFlag => load_kvs("TFLAGNAME", Some(EraCsvVarKind::CsvTFlag))?,
            EraCsvLoadKind::CFlag => load_kvs("CFLAGNAME", Some(EraCsvVarKind::CsvCFlag))?,
            EraCsvLoadKind::TCVar => load_kvs("TCVARNAME", Some(EraCsvVarKind::CsvTCVar))?,
            EraCsvLoadKind::CStr => load_kvs("CSTRNAME", Some(EraCsvVarKind::CsvCStr))?,
            EraCsvLoadKind::Stain => load_kvs("STAINNAME", Some(EraCsvVarKind::CsvStain))?,
            // EraCsvLoadKind::CDFlag1 => load_kvs("CDFLAG1NAME", Some(EraCsvVarKind::CsvCDFlag1))?,
            // EraCsvLoadKind::CDFlag2 => load_kvs("CDFLAG2NAME", Some(EraCsvVarKind::CsvCDFlag2))?,
            // EraCsvLoadKind::StrName => load_kvs("STRNAME", Some(EraCsvVarKind::CsvStrName))?,
            EraCsvLoadKind::TStr => load_kvs("TSTRNAME", Some(EraCsvVarKind::CsvTStr))?,
            EraCsvLoadKind::SaveStr => load_kvs("SAVESTRNAME", Some(EraCsvVarKind::CsvSaveStr))?,
            EraCsvLoadKind::Global => load_kvs("GLOBALNAME", Some(EraCsvVarKind::CsvGlobal))?,
            EraCsvLoadKind::Globals => load_kvs("GLOBALSNAME", Some(EraCsvVarKind::CsvGlobals))?,
            EraCsvLoadKind::ImageResources => {
                let dir_prefix = filename
                    .rsplit_once(&['\\', '/'])
                    .map(|x| &filename[..x.0.len() + 1])
                    .unwrap_or("");

                let mut anime_sprite_name = String::new();
                for row in self.parse_csv_loose(filename.clone(), content) {
                    // sprite,file_path,x,y,width,height
                    //
                    // or:
                    //
                    // sprite,ANIME,width,height
                    // sprite,file_path,x,y,width,height,xPos,yPos,delay
                    // ...

                    use crate::v2::routines::parse_int_literal;

                    let get_col_bytes = |idx: usize| row.get(idx).map_or(&[][..], |x| x.as_bytes());
                    let load_graphics = |this: &mut Self, file_path: &str| -> Result<i64, ()> {
                        let next_gid = (this.gid_file_cache.len() + 1) as i64;
                        match this.gid_file_cache.raw_entry_mut().from_key(file_path) {
                            hashbrown::hash_map::RawEntryMut::Occupied(e) => {
                                let gid = *e.get();
                                if gid > 0 {
                                    Ok(gid)
                                } else {
                                    Err(())
                                }
                            }
                            hashbrown::hash_map::RawEntryMut::Vacant(e) => {
                                if this.ctx.callback.on_gcreatefromfile(next_gid, file_path) != 0 {
                                    e.insert(file_path.into(), next_gid);
                                    Ok(next_gid)
                                } else {
                                    Err(())
                                }
                            }
                        }
                    };

                    if row.len() < 2 {
                        let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                        let offset = content.subslice_offset(&row[0]).unwrap();
                        diag.span_err(
                            Default::default(),
                            SrcSpan::new(SrcPos(offset as _), row[0].len() as _),
                            "too few columns in CSV row",
                        );
                        self.ctx.emit_diag(diag);
                        continue;
                    }
                    let sprite = row[0];
                    let file_path = row[1];
                    if sprite.is_empty() {
                        let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                        let offset = content.subslice_offset(&row[0]).unwrap();
                        diag.span_err(
                            Default::default(),
                            SrcSpan::new(SrcPos(offset as _), row[0].len() as _),
                            "empty sprite name",
                        );
                        self.ctx.emit_diag(diag);
                        continue;
                    }
                    if sprite == anime_sprite_name {
                        // Continue from previous anime sprite
                        let x = parse_int_literal(get_col_bytes(2)).unwrap_or(0);
                        let y = parse_int_literal(get_col_bytes(3)).unwrap_or(0);
                        let width = parse_int_literal(get_col_bytes(4)).unwrap_or(0);
                        let height = parse_int_literal(get_col_bytes(5)).unwrap_or(0);
                        let x_pos = parse_int_literal(get_col_bytes(6)).unwrap_or(0);
                        let y_pos = parse_int_literal(get_col_bytes(7)).unwrap_or(0);
                        let delay = parse_int_literal(get_col_bytes(8)).unwrap_or(0);
                        if let Ok(gid) = load_graphics(self, &format!("{dir_prefix}{file_path}")) {
                            self.ctx.callback.on_spriteanimeaddframe(
                                &anime_sprite_name,
                                gid,
                                x,
                                y,
                                width,
                                height,
                                x_pos,
                                y_pos,
                                delay,
                            );
                        }
                        continue;
                    }
                    // Otherwise, start a new sprite
                    if file_path.eq_ignore_ascii_case("ANIME") {
                        // Start a new anime sprite
                        anime_sprite_name = sprite.into();
                        let width = parse_int_literal(get_col_bytes(2)).unwrap_or(0);
                        let height = parse_int_literal(get_col_bytes(3)).unwrap_or(0);
                        (self.ctx.callback).on_spriteanimecreate(&anime_sprite_name, width, height);
                        continue;
                    } else {
                        // Start a new normal sprite
                        anime_sprite_name.clear();

                        let x = parse_int_literal(get_col_bytes(2)).unwrap_or(0);
                        let y = parse_int_literal(get_col_bytes(3)).unwrap_or(0);
                        let width = parse_int_literal(get_col_bytes(4)).unwrap_or(0);
                        let height = parse_int_literal(get_col_bytes(5)).unwrap_or(0);
                        if let Ok(gid) = load_graphics(self, &format!("{dir_prefix}{file_path}")) {
                            (self.ctx.callback).on_spritecreate(&sprite, gid, x, y, width, height);
                        }
                    }
                }
            }
            EraCsvLoadKind::GameBase => {
                for [index, value] in self.parse_csv::<2>(filename.clone(), content) {
                    let offset = content.subslice_offset(index).unwrap();
                    let index_span = SrcSpan::new(SrcPos(offset as _), index.len() as _);

                    let result = match index {
                        "コード" => {
                            add_var_i64_str(self, "GAMEBASE_GAMECODE", value, index_span)
                        }
                        "バージョン" => {
                            add_var_i64_str(self, "GAMEBASE_VERSION", value, index_span)
                        }
                        "バージョン違い認める" => {
                            add_var_i64_str(self, "GAMEBASE_ALLOWVERSION", value, index_span)
                        }
                        "最初からいるキャラ" => {
                            add_var_i64_str(self, "GAMEBASE_DEFAULTCHARA", value, index_span)
                        }
                        "アイテムなし" => {
                            add_var_i64_str(self, "GAMEBASE_NOITEM", value, index_span)
                        }
                        "タイトル" => {
                            add_var_str(self, "GAMEBASE_TITLE", value.into(), index_span)
                        }
                        "作者" => add_var_str(self, "GAMEBASE_AUTHER", value, index_span)
                            .and_then(|_| add_var_str(self, "GAMEBASE_AUTHOR", value, index_span)),
                        "製作年" => add_var_str(self, "GAMEBASE_YEAR", value.into(), index_span),
                        "追加情報" => {
                            add_var_str(self, "GAMEBASE_INFO", value.into(), index_span)
                        }
                        "ウィンドウタイトル" => {
                            // TODO: Window title
                            let mut diag =
                                Diagnostic::with_src(filename.clone(), content.as_bytes());
                            diag.span_err(
                                Default::default(),
                                index_span,
                                "game base item not yet supported; ignoring",
                            );
                            self.ctx.emit_diag(diag);
                            Ok(())
                        }
                        "動作に必要なEmueraのバージョン" => {
                            // TODO: Emuera version
                            let mut diag =
                                Diagnostic::with_src(filename.clone(), content.as_bytes());
                            diag.span_err(
                                Default::default(),
                                index_span,
                                "game base item not yet supported; ignoring",
                            );
                            self.ctx.emit_diag(diag);
                            Ok(())
                        }
                        _ => {
                            // TODO: Handle unknown GAMEBASE.CSV line
                            let mut diag =
                                Diagnostic::with_src(filename.clone(), content.as_bytes());
                            diag.span_err(
                                Default::default(),
                                index_span,
                                "game base item not yet supported; ignoring",
                            );
                            self.ctx.emit_diag(diag);
                            Ok(())
                        }
                    };
                    if result.is_err() {
                        continue;
                    }
                }

                // Add default values
                add_var_i64_str(self, "GAMEBASE_VERSION", "0", Default::default()).unwrap();
            }
            EraCsvLoadKind::_Replace => {
                for [index, value] in self.parse_csv::<2>(filename.clone(), content) {
                    let offset = content.subslice_offset(index).unwrap();
                    let index_span = SrcSpan::new(SrcPos(offset as _), index.len() as _);

                    let result = match index {
                        "汚れの初期値" => {
                            add_arr_i64_str(self, "DEFAULT_STAIN", value, index_span)
                        }
                        _ => {
                            // TODO: Handle unknown _Replace.csv line
                            let mut diag =
                                Diagnostic::with_src(filename.clone(), content.as_bytes());
                            diag.span_err(
                                Default::default(),
                                index_span,
                                "replace item not yet supported; ignoring",
                            );
                            self.ctx.emit_diag(diag);
                            Ok(())
                        }
                    };
                    if result.is_err() {
                        continue;
                    }
                }

                // Add default values
                add_arr_i64_str(self, "DEFAULT_STAIN", "0/0/2/1/8", Default::default()).unwrap();
            }
            _ => {
                return Err(MEraEngineError::new(
                    "CSV load kind not yet supported".into(),
                ));
            }
        }

        Ok(())
    }

    pub fn finish_load_csv(&mut self) -> Result<(), MEraEngineError> {
        let Some(initial_vars) = self.initial_vars.take() else {
            // Already loaded
            return Ok(());
        };

        for (name, mut var_desc) in initial_vars {
            let name = name.into_inner();
            if var_desc.is_charadata {
                var_desc.dims.insert(0, INITIAL_CHARA_CAP);
            }
            // NOTE: May need ensure_alloc() later
            let val = if var_desc.is_string {
                ArrayValue::new_str_arr(
                    var_desc.dims,
                    var_desc.initial_sval.unwrap_or_else(|| Vec::new()),
                )
            } else {
                ArrayValue::new_int_arr(
                    var_desc.dims,
                    var_desc.initial_ival.unwrap_or_else(|| Vec::new()),
                )
            };
            if self
                .ctx
                .variables
                .add_var(EraVarInfo {
                    name: Ascii::new(name.clone()),
                    val,
                    src_file: var_desc.src_file,
                    src_span: var_desc.src_span,
                    is_const: var_desc.is_const,
                    is_charadata: var_desc.is_charadata,
                    is_global: var_desc.is_global,
                    is_savedata: var_desc.is_savedata,
                    never_trap: true,
                })
                .is_none()
            {
                // TODO: Handle redefinition
                panic!("variable `{name}` was redefined; engine may have gone seriously wrong");
            }
        }

        Ok(())
    }

    pub fn load_erh(&mut self, filename: &str, content: &[u8]) -> Result<(), MEraEngineError> {
        self.load_erb_inner(filename, content, true)
    }

    #[cfg(feature = "legacy_cst")]
    pub fn cst_load_erh(&mut self, filename: &str, content: &[u8]) -> Result<(), MEraEngineError> {
        self.cst_load_erb_inner(filename, content, true)
    }

    pub fn finish_load_erh(&mut self) -> Result<(), MEraEngineError> {
        if self.is_header_finished {
            return Ok(());
        }

        self.is_header_finished = true;

        self.finish_load_erh_inner()
    }

    #[cfg(feature = "legacy_cst")]
    pub fn cst_finish_load_erh(&mut self) -> Result<(), MEraEngineError> {
        if self.is_header_finished {
            return Ok(());
        }

        self.is_header_finished = true;

        let result = self.cst_finish_load_erh_inner();
        self.ctx.active_source = ArcStr::default();

        result
    }

    pub fn load_erb(&mut self, filename: &str, content: &[u8]) -> Result<(), MEraEngineError> {
        self.load_erb_inner(filename, content, false)
    }

    #[cfg(feature = "legacy_cst")]
    pub fn cst_load_erb(&mut self, filename: &str, content: &[u8]) -> Result<(), MEraEngineError> {
        self.cst_load_erb_inner(filename, content, false)
    }

    pub fn start_async_erb_loader(&mut self) -> MEraEngineAsyncErbLoader {
        let (tx, rx) = std::sync::mpsc::channel();
        self.erb_loader_rx = Some(rx);
        MEraEngineAsyncErbLoader { tx }
    }

    pub fn wait_for_async_loader(&mut self) -> Result<(), MEraEngineError> {
        self.ensure_rayon_threadpool();

        // Handle parallel erb loading
        if let Some(erb_rx) = self.erb_loader_rx.take() {
            use crate::v2::parser::{EraParsedProgram, EraParser};

            if self.rayon_threadpool.is_some() {
                let srcs_map = Arc::into_inner(std::mem::take(&mut self.ctx.source_map)).unwrap();
                let s = &mut *scopeguard::guard((self, srcs_map), |s| {
                    let (this, srcs_map) = s;
                    this.ctx.source_map = Arc::new(srcs_map);
                });
                let rayon_threadpool = s.0.rayon_threadpool.as_ref().unwrap();

                let interner = s.0.ctx.interner();
                let EraCompilerCtx { callback, i } = &mut s.0.ctx;

                rayon_threadpool.in_place_scope(|_| {
                    let it = erb_rx.into_iter().par_idx_bridge().map(|(idx, item)| {
                        let MEraEngineAsyncErbLoaderMsg { filename, content } = item;

                        // Parse ERB file into AST
                        let mut lexer = EraLexer::new(filename.clone(), &content, false);
                        let mut is_str_var_fn = |x: &str| {
                            (i.variables)
                                .get_var(x)
                                .map_or(false, |x| x.kind().is_str())
                        };
                        let mut err_accum = EraDiagnosticAccumulator::new();
                        let mut parser = EraParser::new(
                            &mut err_accum,
                            i,
                            &mut lexer,
                            i.node_cache.interner(),
                            &i.global_replace,
                            &i.global_define,
                            &mut is_str_var_fn,
                            false,
                            false,
                        );
                        let EraParsedProgram {
                            root_node,
                            nodes,
                            macro_map,
                            defines: _,
                        } = parser.parse_program();

                        let src_file = EraSourceFile {
                            filename: filename.clone(),
                            text: Some(content.clone()),
                            // compressed_text: lz4_flex::compress_prepend_size(content.as_bytes()).into(),
                            compressed_text: None,
                            cst_root: None,
                            ast_data: Some((root_node, nodes)),
                            macro_map,
                            defines: Default::default(),
                            kind: EraSourceFileKind::Source,
                            newline_pos: Vec::new(),
                            is_transient: false,
                        };

                        (idx, (src_file, err_accum))
                    });
                    it.par_seq_for_each(|(src_file, mut err_accum)| {
                        let filename = src_file.filename.clone();
                        let source_map = &mut s.1;
                        let src_file = source_map.insert(filename.clone(), src_file);

                        if let Some(src_file) = src_file {
                            let content = src_file.text.as_ref().unwrap();
                            let mut diag =
                                Diagnostic::with_src(filename.clone(), content.as_bytes());
                            diag.span_err(
                                Default::default(),
                                Default::default(),
                                "file already loaded; overriding previous content",
                            );
                            let diag_resolver = DiagnosticResolver::new(None, None);
                            EraEmitDiagnostic::emit_diag(
                                callback,
                                DiagnosticProvider::new(diag, diag_resolver),
                            );
                        }

                        // Count files
                        s.0.erb_count += 1;

                        // Emit diagnostics
                        let diag_resolver = DiagnosticResolver::new(Some(&s.1), Some(interner));
                        err_accum.emit_all_to_with_resolver(callback, &diag_resolver);
                    });
                });
            } else {
                // Fall back to single-threaded loading
                for MEraEngineAsyncErbLoaderMsg { filename, content } in erb_rx {
                    // Add source map entry early so that we can refer to its source in error messages
                    let newline_pos = Vec::new();
                    // self.ctx.active_source = ArcStr::default();
                    let source_map = Arc::get_mut(&mut self.ctx.source_map).unwrap();
                    let src_file = source_map.insert(
                        filename.clone(),
                        EraSourceFile {
                            filename: filename.clone(),
                            text: Some(content.clone()),
                            // compressed_text: lz4_flex::compress_prepend_size(content.as_bytes()).into(),
                            compressed_text: None,
                            cst_root: None,
                            ast_data: None,
                            macro_map: Default::default(),
                            defines: Default::default(),
                            kind: EraSourceFileKind::Source,
                            newline_pos,
                            is_transient: false,
                        },
                    );

                    if let Some(src_file) = src_file {
                        let content = src_file.text.as_ref().unwrap();
                        let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                        diag.span_err(
                            Default::default(),
                            Default::default(),
                            "file already loaded; overriding previous content",
                        );
                        self.ctx.emit_diag(diag);
                    }

                    // Parse ERB file into AST
                    self.ctx.active_source = filename.clone();
                    // let node_cache = &mut self.node_cache;
                    let mut lexer = EraLexer::new(filename.clone(), &content, false);
                    let mut is_str_var_fn = |x: &str| {
                        (self.ctx.i.variables)
                            .get_var(x)
                            .map_or(false, |x| x.kind().is_str())
                    };
                    let mut parser = EraParser::new(
                        &mut self.ctx.callback,
                        &self.ctx.i,
                        &mut lexer,
                        self.ctx.i.node_cache.interner(),
                        &self.ctx.i.global_replace,
                        &self.ctx.i.global_define,
                        &mut is_str_var_fn,
                        false,
                        false,
                    );
                    let EraParsedProgram {
                        root_node,
                        nodes,
                        macro_map,
                        defines: _,
                    } = parser.parse_program();

                    // Complete source map entry
                    let source_map = Arc::get_mut(&mut self.ctx.source_map).unwrap();
                    let src_file = source_map.get_mut(&filename).unwrap();
                    src_file.ast_data = Some((root_node, nodes));
                    src_file.macro_map = macro_map;

                    // Count files
                    self.erb_count += 1;
                }
            }
        }

        Ok(())
    }

    pub fn build(mut self) -> Result<MEraEngine<T>, MEraEngineError> {
        _ = self.finish_load_csv()?;
        _ = self.finish_load_erh()?;

        // Load builtin source
        {
            const SYS_SRC: &str = include_str!("../sys_src.ERB");
            self.load_erb("<builtin>", SYS_SRC.as_bytes())?;
        }

        self.optimize_interner();

        self.ensure_rayon_threadpool();

        // Process .erb files
        let filenames = self
            .ctx
            .source_map
            .values()
            .filter(|x| x.kind == EraSourceFileKind::Source)
            .map(|x| x.filename.clone())
            .collect::<Vec<_>>();
        let mut codegen = EraCodeGenerator::new(
            &mut self.ctx,
            false,
            self.config.no_src_map,
            !self.config.no_src_map,
        );
        let progress_callback = |progress| {
            if let Some(callback) = &mut self.builder_callback {
                callback.on_build_progress(
                    "erb",
                    &MEraEngineBuildProgress {
                        current: progress,
                        total: self.erb_count,
                    },
                );
            }
        };
        if let Some(rayon_threadpool) = &self.rayon_threadpool {
            rayon_threadpool.in_place_scope(move |_| {
                codegen.compile_merge_many_programs(&filenames, progress_callback, true);
            });
        } else {
            codegen.compile_merge_many_programs(&filenames, progress_callback, false);
        }

        self.optimize_interner();

        if self.config.no_src_map {
            // let src_map = Arc::get_mut(&mut self.ctx.source_map).unwrap();
            // for (_, src_file) in src_map {
            //     src_file.final_root = None;
            // }
            // TODO: Reset node_cache when upstream is ready
            // let interner = self.ctx.node_cache.into_interner().unwrap();
            // self.ctx.node_cache = NodeCache::from_interner(interner);
        }

        let mut vm_state = EraVirtualMachineState::new();
        let mut vm = EraVirtualMachine::new(&mut self.ctx, &mut vm_state);

        // Register watching variables
        for var in self.watching_vars {
            vm.add_trap_var(var.as_ref());
        }

        Ok(MEraEngine {
            ctx: self.ctx,
            interner: self.interner,
            vm_state,
            config: self.config,
        })
    }

    #[cfg(feature = "legacy_cst")]
    pub fn cst_build(mut self) -> Result<MEraEngine<T>, MEraEngineError> {
        _ = self.finish_load_csv()?;
        _ = self.cst_finish_load_erh()?;

        // Load builtin source
        {
            const SYS_SRC: &str = include_str!("../sys_src.ERB");
            self.cst_load_erb("<builtin>", SYS_SRC.as_bytes())?;
        }

        self.optimize_interner();

        // Process .erb files
        let filenames = self
            .ctx
            .source_map
            .values()
            .filter(|x| !x.is_header)
            .map(|x| x.filename.clone())
            .collect::<Vec<_>>();
        let mut codegen = cst::codegen::EraCodeGenerator::new(
            &mut self.ctx,
            self.config.no_src_map,
            !self.config.no_src_map,
        );
        codegen.compile_merge_many_programs(&filenames, |progress| {
            if let Some(callback) = &mut self.builder_callback {
                callback.on_build_progress(
                    "erb",
                    &MEraEngineBuildProgress {
                        current: progress,
                        total: self.erb_count,
                    },
                );
            }
        });

        self.optimize_interner();

        if self.config.no_src_map {
            // let src_map = Arc::get_mut(&mut self.ctx.source_map).unwrap();
            // for (_, src_file) in src_map {
            //     src_file.final_root = None;
            // }
            // TODO: Reset node_cache when upstream is ready
            // let interner = self.ctx.node_cache.into_interner().unwrap();
            // self.ctx.node_cache = NodeCache::from_interner(interner);
        }

        // TODO: Call `optimize_resolution` on entering VM on the interner (which populates the
        //       resolution cache Vec<&str> from DashMap), which helps speed up the VM. Requires
        //       modification to the (multi-threaded) interner to allow this.

        // TODO: Handle watching variables

        Ok(MEraEngine {
            ctx: self.ctx,
            interner: self.interner,
            vm_state: EraVirtualMachineState::new(),
        })
    }

    pub fn register_variable(
        &mut self,
        name: &str,
        is_string: bool,
        dimension: u32,
        watch: bool,
    ) -> Result<(), MEraEngineError> {
        let val = if is_string {
            ArrayValue::new_str_arr(smallvec![dimension], Vec::new())
        } else {
            ArrayValue::new_int_arr(smallvec![dimension], Vec::new())
        };
        let name = ArcStr::from(name);
        let var_idx = self
            .ctx
            .variables
            .add_var(EraVarInfo {
                name: Ascii::new(name.clone()),
                val,
                src_file: Default::default(),
                src_span: Default::default(),
                is_const: false,
                is_charadata: false,
                is_global: false,
                is_savedata: false,
                never_trap: !watch,
            })
            .ok_or_else(|| MEraEngineError::new(format!("variable `{name}` already exists")))?;
        if watch {
            self.watching_vars.insert(Ascii::new(name));
        }
        Ok(())
    }

    pub fn ensure_rayon_threadpool(&mut self) {
        if self.rayon_threadpool.is_some() {
            return;
        }
        // let parallelism = NonZeroUsize::new(self.config.threads_cnt as _)
        //     .unwrap_or_else(|| std::thread::available_parallelism().unwrap_or(1));
        // if parallelism.get() > 1 {
        //     let pool = rayon::ThreadPoolBuilder::new()
        //         .num_threads(parallelism.get())
        //         .build()
        //         .unwrap();
        //     self.rayon_threadpool = Some(pool);
        // }
        if self.config.threads_cnt != 1 {
            let pool = rayon::ThreadPoolBuilder::new()
                .num_threads(self.config.threads_cnt as _)
                .build()
                .unwrap();
            self.rayon_threadpool = Some(pool);
        }
    }

    fn optimize_interner(&mut self) {
        // UNSAFETY: It is NOT safe.
        unsafe {
            self.interner.optimize_lookup_unchecked();
        }
    }

    fn load_erb_inner(
        &mut self,
        filename: &str,
        content: &[u8],
        is_header: bool,
    ) -> Result<(), MEraEngineError> {
        use crate::v2::parser::{EraParsedProgram, EraParser};

        if is_header && self.is_header_finished {
            return Err(MEraEngineError::new("header loaded too late".into()));
        }

        // Handle UTF-8 BOM
        let content = content
            .strip_prefix("\u{feff}".as_bytes())
            .unwrap_or(content);
        let content = simdutf8::basic::from_utf8(content)
            .map_or_else(|_| String::from_utf8_lossy(content), |s| Cow::Borrowed(s));
        let content: arcstr::ArcStr = content.into();
        let filename = ArcStr::from(filename);

        // Add source map entry early so that we can refer to its source in error messages
        let newline_pos = Vec::new();
        self.ctx.active_source = ArcStr::default();
        let source_map = Arc::get_mut(&mut self.ctx.source_map).unwrap();
        let src_file = source_map.insert(
            filename.clone(),
            EraSourceFile {
                filename: filename.clone(),
                text: Some(content.clone()),
                // compressed_text: lz4_flex::compress_prepend_size(content.as_bytes()).into(),
                compressed_text: None,
                cst_root: None,
                ast_data: None,
                macro_map: Default::default(),
                defines: Default::default(),
                kind: if is_header {
                    EraSourceFileKind::Header
                } else {
                    EraSourceFileKind::Source
                },
                newline_pos,
                is_transient: false,
            },
        );

        if let Some(src_file) = src_file {
            let content = src_file.text.as_ref().unwrap();
            let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
            diag.span_err(
                Default::default(),
                Default::default(),
                "file already loaded; overriding previous content",
            );
            self.ctx.emit_diag(diag);
        }

        // Parse ERB file into AST
        self.ctx.active_source = filename.clone();
        // let node_cache = &mut self.node_cache;
        let mut lexer = EraLexer::new(filename.clone(), &content, false);
        let mut is_str_var_fn = |x: &str| {
            (self.ctx.i.variables)
                .get_var(x)
                .map_or(false, |x| x.kind().is_str())
        };
        let mut parser = EraParser::new(
            &mut self.ctx.callback,
            &self.ctx.i,
            &mut lexer,
            self.ctx.i.node_cache.interner(),
            &self.ctx.i.global_replace,
            &self.ctx.i.global_define,
            &mut is_str_var_fn,
            is_header,
            false,
        );
        let EraParsedProgram {
            root_node,
            nodes,
            macro_map,
            defines,
        } = parser.parse_program();
        if is_header {
            // Merge defines into global defines
            let mut errors = Vec::new();
            self.ctx
                .global_define
                .extend(defines, |name, new_define, old_define| {
                    let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                    diag.span_err(
                        Default::default(),
                        new_define.span,
                        format!("redefinition of macro `{name}`"),
                    );
                    diag.span_note(
                        old_define.filename.clone(),
                        old_define.span,
                        "previous definition here",
                    );
                    errors.push(diag);
                });
            for diag in errors {
                self.ctx.emit_diag(diag);
            }
        }

        // Complete source map entry
        let source_map = Arc::get_mut(&mut self.ctx.source_map).unwrap();
        let src_file = source_map.get_mut(&filename).unwrap();
        src_file.ast_data = Some((root_node, nodes));
        src_file.macro_map = macro_map;

        // Count files
        if is_header {
            self.erh_count += 1;
        } else {
            self.erb_count += 1;
        }

        Ok(())
    }

    #[cfg(feature = "legacy_cst")]
    fn cst_load_erb_inner(
        &mut self,
        filename: &str,
        content: &[u8],
        is_header: bool,
    ) -> Result<(), MEraEngineError> {
        if is_header && self.is_header_finished {
            return Err(MEraEngineError::new("header loaded too late".into()));
        }

        // Handle UTF-8 BOM
        let content = content
            .strip_prefix("\u{feff}".as_bytes())
            .unwrap_or(content);
        let content = &content.to_str_lossy();
        let filename = ArcStr::from(filename);

        // Parse ERB file into AST
        self.ctx.active_source = filename.clone();
        // let node_cache = &mut self.node_cache;
        let mut lexer = EraLexer::new(filename.clone(), content, false);
        let mut is_str_var_fn = |x: &str| {
            (self.ctx.variables)
                .get_var(x)
                .map_or(false, |x| x.kind().is_str())
        };
        let mut parser = cst::parser::EraParser::new(
            &mut self.ctx.callback,
            &mut lexer,
            &mut self.ctx.i.node_cache,
            &self.ctx.i.global_replace,
            &self.ctx.i.global_define,
            &mut is_str_var_fn,
            is_header,
        );
        let cst::parser::EraParsedProgram {
            ast,
            macro_map,
            defines,
        } = parser.parse_program();
        if is_header {
            // Merge defines into global defines
            let mut errors = Vec::new();
            self.ctx
                .global_define
                .extend(defines, |name, new_define, old_define| {
                    let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                    diag.span_err(
                        Default::default(),
                        new_define.span,
                        format!("redefinition of macro `{name}`"),
                    );
                    diag.span_note(
                        old_define.filename.clone(),
                        old_define.span,
                        "previous definition here",
                    );
                    errors.push(diag);
                });
            for diag in errors {
                self.ctx.emit_diag(diag);
            }
        }
        let newline_pos = {
            let mut newline_pos = lexer.newline_positions();
            // // Fix newline positions
            // // NOTE: Newlines will never occur inside macros
            // // TODO: Optimize performance with the hint above
            // for pos in newline_pos.iter_mut() {
            //     let span = SrcSpan::new(*pos, 0);
            //     let span = macro_map.inverse_translate_span(span);
            //     *pos = span.start();
            // }
            newline_pos
        };
        self.ctx.active_source = ArcStr::default();

        // Add source map entry
        let source_map = Arc::get_mut(&mut self.ctx.source_map).unwrap();
        let src_file = source_map.insert(
            filename.clone(),
            EraSourceFile {
                filename: filename.clone(),
                text: None,
                // compressed_text: lz4_flex::compress_prepend_size(content.as_bytes()).into(),
                compressed_text: None,
                cst_root: Some(ast),
                ast_data: None,
                macro_map,
                defines: Default::default(),
                is_header,
                newline_pos,
            },
        );

        if src_file.is_some() {
            let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
            diag.span_err(
                Default::default(),
                Default::default(),
                "file already loaded; overriding previous content",
            );
            self.ctx.emit_diag(diag);
        }

        // Count files
        if is_header {
            self.erh_count += 1;
        } else {
            self.erb_count += 1;
        }

        Ok(())
    }

    #[cfg(feature = "legacy_cst")]
    fn cst_finish_load_erh_inner(&mut self) -> Result<(), MEraEngineError> {
        use crate::cst::ast::*;

        // Materialize .erh variable definitions
        let mut unresolved_vars = Vec::new();
        let source_map = Arc::clone(&self.ctx.source_map);
        let mut interp = EraInterpreter::new(&mut self.ctx, true);

        for (i, erh) in source_map.values().filter(|x| x.is_header).enumerate() {
            interp.get_ctx_mut().active_source = erh.filename.clone();

            let final_root = erh.cst_root.as_ref().unwrap().clone();
            let node = SyntaxNode::new_root(final_root);
            let program = EraProgramNode::cast(&node).unwrap();
            for decl in program.children() {
                let var_info = match interp.interpret_var_decl(decl) {
                    Ok(x) => {
                        if x.is_ref || x.is_dynamic {
                            let mut diag = interp.make_diag();
                            diag.span_err(
                                Default::default(),
                                decl.src_span(),
                                "global variable definition cannot be REF or DYNAMIC",
                            );
                            interp.get_ctx_mut().emit_diag(diag);
                            continue;
                        }
                        x.var_info
                    }
                    Err(EraInterpretError::VarNotFound(_)) => {
                        // Delay resoultion of variable definitions containing unresolved variables
                        unresolved_vars.push((erh.filename.clone(), decl.to_owned()));
                        continue;
                    }
                    _ => continue,
                };
                let var_name = var_info.name.clone();

                // Create variable
                // TODO: Report where the previous definition was
                if interp.get_ctx_mut().variables.add_var(var_info).is_none() {
                    let mut diag = interp.make_diag();
                    diag.span_err(
                        Default::default(),
                        decl.src_span(),
                        format!("variable `{var_name}` already defined"),
                    );
                    interp.get_ctx_mut().emit_diag(diag);
                    continue;
                }
            }

            if let Some(callback) = &mut self.builder_callback {
                callback.on_build_progress(
                    "erh",
                    &MEraEngineBuildProgress {
                        current: i + 1,
                        total: self.erh_count,
                    },
                );
            }
        }

        // Second pass for unresolved variables
        while !unresolved_vars.is_empty() {
            let mut has_progress = false;
            let mut failures = Vec::new();

            unresolved_vars.retain(|(filename, decl)| {
                interp.get_ctx_mut().active_source = filename.clone();

                let decl = decl.as_ref();

                let var_info = match interp.interpret_var_decl(decl) {
                    Ok(x) => {
                        if x.is_ref || x.is_dynamic {
                            let mut diag = interp.make_diag();
                            diag.span_err(
                                Default::default(),
                                decl.src_span(),
                                "global variable definition cannot be REF or DYNAMIC",
                            );
                            interp.get_ctx_mut().emit_diag(diag);
                            return false;
                        }
                        x.var_info
                    }
                    Err(EraInterpretError::VarNotFound(var)) => {
                        failures.push((filename.clone(), var.to_cloned()));
                        return true;
                    }
                    _ => return false,
                };
                let var_name = var_info.name.clone();

                // Create variable
                // TODO: Report where the previous definition was
                if interp.get_ctx_mut().variables.add_var(var_info).is_none() {
                    let mut diag = interp.make_diag();
                    diag.span_err(
                        Default::default(),
                        decl.src_span(),
                        format!("variable `{var_name}` already defined"),
                    );
                    interp.get_ctx_mut().emit_diag(diag);
                    return false;
                }

                has_progress = true;

                false
            });

            if !has_progress {
                for (filename, var) in failures {
                    let var_span = var.src_span();
                    let var = var.resolve_text(interp.get_ctx_mut().node_cache.interner());
                    let mut diag = interp.make_diag();
                    diag.span_err(filename, var_span, format!("undefined variable `{var}`"));
                    interp.get_ctx_mut().emit_diag(diag);
                }
                break;
            }
        }

        Ok(())
    }

    fn finish_load_erh_inner(&mut self) -> Result<(), MEraEngineError> {
        use crate::v2::interpret::*;
        use crate::v2::parser::*;

        // Materialize .erh variable definitions
        let mut unresolved_vars = Vec::new();
        let source_map = Arc::clone(&self.ctx.source_map);

        for (i, erh) in source_map
            .values()
            .filter(|x| x.kind == EraSourceFileKind::Header)
            .enumerate()
        {
            self.ctx.active_source = erh.filename.clone();
            let (program_ref, node_arena) = erh.ast_data.as_ref().unwrap();

            let EraNode::Program(program_children) = node_arena.get_node(*program_ref) else {
                unreachable!("ast_data is not a Program node");
            };
            for decl in node_arena.get_extra_data_view(program_children) {
                let decl = EraNodeRef(*decl);
                if let EraNode::DeclDefine(..) = node_arena.get_node(decl) {
                    continue;
                }
                let decl_span = node_arena.get_node_span(decl);
                let mut interp =
                    EraInterpreter::new(&mut self.ctx.callback, &self.ctx.i, node_arena, true);
                let var_info = match interp.interpret_var_decl(decl) {
                    Ok(x) => {
                        if x.is_ref || x.is_dynamic {
                            let mut diag = self.ctx.make_diag();
                            diag.span_err(
                                Default::default(),
                                decl_span,
                                "global variable definition cannot be REF or DYNAMIC",
                            );
                            self.ctx.emit_diag(diag);
                            continue;
                        }
                        x.var_info
                    }
                    Err(EraInterpretError::VarNotFound(..)) => {
                        // Delay resoultion of variable definitions containing unresolved variables
                        unresolved_vars.push((erh, decl));
                        continue;
                    }
                    _ => continue,
                };
                let var_name = var_info.name.clone();

                // Create variable
                // TODO: Report where the previous definition was
                if self.ctx.variables.add_var(var_info).is_none() {
                    let mut diag = self.ctx.make_diag();
                    diag.span_err(
                        Default::default(),
                        decl_span,
                        format!("variable `{var_name}` already defined"),
                    );
                    self.ctx.emit_diag(diag);
                    continue;
                }

                // Report progress
                if let Some(callback) = &mut self.builder_callback {
                    callback.on_build_progress(
                        "erh",
                        &MEraEngineBuildProgress {
                            current: i + 1,
                            total: self.erh_count,
                        },
                    );
                }
            }
        }

        // Second pass for unresolved variables
        while !unresolved_vars.is_empty() {
            let mut has_progress = false;
            let mut failures = Vec::new();

            unresolved_vars.retain(|&(erh, decl)| {
                self.ctx.active_source = erh.filename.clone();
                let (program_ref, node_arena) = erh.ast_data.as_ref().unwrap();
                let mut interp =
                    EraInterpreter::new(&mut self.ctx.callback, &self.ctx.i, node_arena, true);

                let decl_span = node_arena.get_node_span(decl);

                let var_info = match interp.interpret_var_decl(decl) {
                    Ok(x) => {
                        if x.is_ref || x.is_dynamic {
                            let mut diag = self.ctx.make_diag();
                            diag.span_err(
                                Default::default(),
                                decl_span,
                                "global variable definition cannot be REF or DYNAMIC",
                            );
                            self.ctx.emit_diag(diag);
                            return false;
                        }
                        x.var_info
                    }
                    Err(EraInterpretError::VarNotFound(var_name, var_span)) => {
                        failures.push((erh.filename.clone(), var_name, var_span));
                        return true;
                    }
                    _ => return false,
                };
                let var_name = var_info.name.clone();

                // Create variable
                // TODO: Report where the previous definition was
                if self.ctx.variables.add_var(var_info).is_none() {
                    let mut diag = self.ctx.make_diag();
                    diag.span_err(
                        Default::default(),
                        decl_span,
                        format!("variable `{var_name}` already defined"),
                    );
                    self.ctx.emit_diag(diag);
                    return false;
                }

                has_progress = true;

                false
            });

            if !has_progress {
                for (filename, var, var_span) in failures {
                    let mut diag = self.ctx.make_diag();
                    diag.span_err(filename, var_span, format!("undefined variable `{var}`"));
                    self.ctx.emit_diag(diag);
                }
                break;
            }
        }

        Ok(())
    }

    fn add_initial_vars(&mut self) {
        trait Adhoc {
            fn add_item(
                &mut self,
                name: ArcStr,
                dims: EraVarDims,
                is_string: bool,
                is_const: bool,
                is_charadata: bool,
                is_global: bool,
                is_savedata: bool,
            );
            fn add_int(&mut self, name: ArcStr, dims: EraVarDims, is_savedata: bool) {
                self.add_item(name, dims, false, false, false, false, is_savedata)
            }
            fn add_str(&mut self, name: ArcStr, dims: EraVarDims, is_savedata: bool) {
                self.add_item(name, dims, true, false, false, false, is_savedata)
            }
            fn add_const_int(&mut self, name: ArcStr, dims: EraVarDims, is_savedata: bool) {
                self.add_item(name, dims, false, true, false, false, is_savedata)
            }
            fn add_const_str(&mut self, name: ArcStr, dims: EraVarDims, is_savedata: bool) {
                self.add_item(name, dims, true, true, false, false, is_savedata)
            }
            fn add_chara_int(&mut self, name: ArcStr, dims: EraVarDims, is_savedata: bool) {
                self.add_item(name, dims, false, false, true, false, is_savedata)
            }
            fn add_chara_str(&mut self, name: ArcStr, dims: EraVarDims, is_savedata: bool) {
                self.add_item(name, dims, true, false, true, false, is_savedata)
            }
        }
        impl<T: MEraEngineSysCallback, U: MEraEngineBuilderCallback> Adhoc for MEraEngineBuilder<T, U> {
            fn add_item(
                &mut self,
                name: ArcStr,
                dims: EraVarDims,
                is_string: bool,
                is_const: bool,
                is_charadata: bool,
                is_global: bool,
                is_savedata: bool,
            ) {
                self.initial_vars.as_mut().unwrap().insert(
                    Ascii::new(name),
                    InitialVarDesc {
                        is_string,
                        dims,
                        src_file: Default::default(),
                        src_span: Default::default(),
                        is_const,
                        is_charadata,
                        is_global,
                        is_savedata,
                        initial_sval: None,
                        initial_ival: None,
                    },
                );
            }
        }

        // ………………………………………………
        // 数値配列型変数
        // ………………………………………………
        self.add_int(rcstr::literal!("DAY"), smallvec![1], true);
        self.add_int(rcstr::literal!("MONEY"), smallvec![1], true);
        self.add_int(rcstr::literal!("TIME"), smallvec![1], true);
        self.add_int(rcstr::literal!("ITEM"), smallvec![1], true);
        self.add_int(rcstr::literal!("ITEMSALES"), smallvec![1], true);
        self.add_int(rcstr::literal!("NOITEM"), smallvec![1], true);
        self.add_int(rcstr::literal!("BOUGHT"), smallvec![1], true);
        self.add_int(rcstr::literal!("PBAND"), smallvec![1], true);
        self.add_int(rcstr::literal!("FLAG"), smallvec![1], true);
        self.add_int(rcstr::literal!("TFLAG"), smallvec![1], true);
        self.add_int(rcstr::literal!("TARGET"), smallvec![1], true);
        self.add_int(rcstr::literal!("MASTER"), smallvec![1], true);
        self.add_int(rcstr::literal!("PLAYER"), smallvec![1], true);
        self.add_int(rcstr::literal!("ASSI"), smallvec![1], true);
        self.add_int(rcstr::literal!("ASSIPLAY"), smallvec![1], true);
        self.add_int(rcstr::literal!("UP"), smallvec![1], true);
        self.add_int(rcstr::literal!("DOWN"), smallvec![1], true);
        self.add_int(rcstr::literal!("LOSEBASE"), smallvec![1], true);
        self.add_int(rcstr::literal!("PALAMLV"), smallvec![1], true);
        self.add_int(rcstr::literal!("EXPLV"), smallvec![1], true);
        self.add_int(rcstr::literal!("EJAC"), smallvec![1], true);
        self.add_int(rcstr::literal!("PREVCOM"), smallvec![1], true);
        self.add_int(rcstr::literal!("SELECTCOM"), smallvec![1], true);
        self.add_int(rcstr::literal!("NEXTCOM"), smallvec![1], true);
        self.add_int(rcstr::literal!("RESULT"), smallvec![1], true);
        self.add_int(rcstr::literal!("COUNT"), smallvec![1], true);
        self.add_int(rcstr::literal!("A"), smallvec![1], true);
        self.add_int(rcstr::literal!("B"), smallvec![1], true);
        self.add_int(rcstr::literal!("C"), smallvec![1], true);
        self.add_int(rcstr::literal!("D"), smallvec![1], true);
        self.add_int(rcstr::literal!("E"), smallvec![1], true);
        self.add_int(rcstr::literal!("F"), smallvec![1], true);
        self.add_int(rcstr::literal!("G"), smallvec![1], true);
        self.add_int(rcstr::literal!("H"), smallvec![1], true);
        self.add_int(rcstr::literal!("I"), smallvec![1], true);
        self.add_int(rcstr::literal!("J"), smallvec![1], true);
        self.add_int(rcstr::literal!("K"), smallvec![1], true);
        self.add_int(rcstr::literal!("L"), smallvec![1], true);
        self.add_int(rcstr::literal!("M"), smallvec![1], true);
        self.add_int(rcstr::literal!("N"), smallvec![1], true);
        self.add_int(rcstr::literal!("O"), smallvec![1], true);
        self.add_int(rcstr::literal!("P"), smallvec![1], true);
        self.add_int(rcstr::literal!("Q"), smallvec![1], true);
        self.add_int(rcstr::literal!("R"), smallvec![1], true);
        self.add_int(rcstr::literal!("S"), smallvec![1], true);
        self.add_int(rcstr::literal!("T"), smallvec![1], true);
        self.add_int(rcstr::literal!("U"), smallvec![1], true);
        self.add_int(rcstr::literal!("V"), smallvec![1], true);
        self.add_int(rcstr::literal!("W"), smallvec![1], true);
        self.add_int(rcstr::literal!("X"), smallvec![1], true);
        self.add_int(rcstr::literal!("Y"), smallvec![1], true);
        self.add_int(rcstr::literal!("Z"), smallvec![1], true);
        self.add_const_int(rcstr::literal!("ITEMPRICE"), smallvec![1], false);
        // ………………………………………………
        // 文字列配列型変数
        // ………………………………………………
        self.add_str(rcstr::literal!("SAVESTR"), smallvec![1], true);
        self.add_str(rcstr::literal!("RESULTS"), smallvec![1], false);
        self.add_str(rcstr::literal!("TSTR"), smallvec![1], true);
        self.add_str(rcstr::literal!("STR"), smallvec![1], false);
        // ITEMNAMEとITEMPRICEは片方を変更すると他方も同じ値に変更されます
        self.add_const_str(rcstr::literal!("ITEMNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("ABLNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("EXPNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("TALENTNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("PALAMNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("TRAINNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("MARKNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("BASENAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("SOURCENAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("EXNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("EQUIPNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("TEQUIPNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("FLAGNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("CFLAGNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("TFLAGNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("TCVARNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("CSTRNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("STAINNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("STRNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("TSTRNAME"), smallvec![1], false);
        self.add_const_str(rcstr::literal!("SAVESTRNAME"), smallvec![1], false);
        // ………………………………………………
        // 角色変数
        // ………………………………………………
        self.add_chara_int(rcstr::literal!("BASE"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("MAXBASE"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("DOWNBASE"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("ABL"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("TALENT"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("EXP"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("MARK"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("PALAM"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("SOURCE"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("EX"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("NOWEX"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("CFLAG"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("JUEL"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("GOTJUEL"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("CUP"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("CDOWN"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("RELATION"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("EQUIP"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("TEQUIP"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("STAIN"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("TCVAR"), smallvec![1], true);
        // ………………………………………………
        // 角色文字列変数
        // ………………………………………………
        self.add_chara_str(rcstr::literal!("NAME"), smallvec![1], true);
        self.add_chara_str(rcstr::literal!("CALLNAME"), smallvec![1], true);
        self.add_chara_str(rcstr::literal!("NICKNAME"), smallvec![1], true);
        self.add_chara_str(rcstr::literal!("MASTERNAME"), smallvec![1], true);
        self.add_chara_str(rcstr::literal!("CSTR"), smallvec![1], true);
        // ………………………………………………
        // 特殊一時変数・一時文字列変数
        // ………………………………………………
        self.add_int(rcstr::literal!("LOCAL"), smallvec![1], false);
        self.add_str(rcstr::literal!("LOCALS"), smallvec![1], false);
        self.add_int(rcstr::literal!("ARG"), smallvec![1], false);
        self.add_str(rcstr::literal!("ARGS"), smallvec![1], false);
        self.add_item(
            rcstr::literal!("GLOBAL"),
            smallvec![1],
            false,
            false,
            false,
            true,
            true,
        );
        self.add_item(
            rcstr::literal!("GLOBALS"),
            smallvec![1],
            true,
            false,
            false,
            true,
            true,
        );
        // ………………………………………………
        // 二次元配列型変数
        // ………………………………………………
        // DITEMTYPE,1000,1000
        // DA,100,100
        // DB,100,100
        // DC,100,100
        // DD,100,100
        // DE,100,100
        // MISC variables
        self.add_chara_int(rcstr::literal!("NO"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("ISASSI"), smallvec![1], true);
        self.add_chara_int(rcstr::literal!("CDFLAG"), smallvec![1, 1], false);
        self.add_int(rcstr::literal!("DITEMTYPE"), smallvec![1, 1], false);
        self.add_int(rcstr::literal!("DA"), smallvec![1, 1], false);
        self.add_int(rcstr::literal!("DB"), smallvec![1, 1], false);
        self.add_int(rcstr::literal!("DC"), smallvec![1, 1], false);
        self.add_int(rcstr::literal!("DD"), smallvec![1, 1], false);
        self.add_int(rcstr::literal!("DE"), smallvec![1, 1], false);
        self.add_int(rcstr::literal!("TA"), smallvec![1, 1, 1], false);
        self.add_int(rcstr::literal!("TB"), smallvec![1, 1, 1], false);
    }

    fn parse_csv<'a, const COL_COUNT: usize>(
        &mut self,
        filename: ArcStr,
        content: &'a str,
    ) -> Vec<[&'a str; COL_COUNT]> {
        let loose_rows = self.parse_csv_loose(filename.clone(), content);
        let mut rows = Vec::new();
        for loose_row in &loose_rows {
            if loose_row.len() < COL_COUNT {
                let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                diag.span_err(
                    Default::default(),
                    SrcSpan::new(SrcPos(0), content.len() as _),
                    format!("expected {} columns, found {}", COL_COUNT, loose_row.len()),
                );
                self.ctx.emit_diag(diag);
                continue;
            }
            rows.push(loose_row[..COL_COUNT].try_into().unwrap());
        }
        rows
    }

    fn parse_csv_loose<'a>(&mut self, filename: ArcStr, content: &'a str) -> Vec<Vec<&'a str>> {
        let mut rows = Vec::new();
        let mut inhibit_newline_count = 0u32;

        for cont_line in content.lines() {
            // Remove comments
            let mut cont_line = if let Some(pos) = cont_line.find(';') {
                &cont_line[..pos]
            } else {
                cont_line
            };
            cont_line = cont_line.trim_ascii();
            if cont_line.starts_with('{') {
                cont_line = &cont_line[1..];
                inhibit_newline_count += 1;
            } else if cont_line.starts_with('}') {
                if inhibit_newline_count == 0 {
                    let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                    diag.span_err(
                        Default::default(),
                        SrcSpan::new(SrcPos(0), cont_line.len() as _),
                        "unmatched `}`",
                    );
                    self.ctx.emit_diag(diag);

                    continue;
                }
                cont_line = &cont_line[1..];
                inhibit_newline_count -= 1;
            }
            if cont_line.is_empty() {
                continue;
            }

            let cur_row = cont_line.split(',').map(|x| x.trim_ascii()).collect();
            rows.push(cur_row);
        }

        rows
    }
}

// RPC-friendly types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EraFuncFrameVarInfo {
    pub name: String,
    pub span: SrcSpan,
    pub is_ref: bool,
    pub is_const: bool,
    pub is_charadata: bool,
    pub in_local_frame: bool,
    /// Index of the variable in the frame.
    pub var_idx: u32,
    /// The type of the variable.
    pub var_kind: ArrayValueKind,
    /// The dimension count of the variable.
    pub dims_cnt: u8,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EraFuncFrameInfo {
    /// Argument slots for the function.
    pub args: Vec<EraFuncFrameArgInfo>,
    /// Declared variables in the function scope.
    pub vars: Vec<EraFuncFrameVarInfo>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EraFuncInfo {
    pub name: String,
    pub name_span: SrcSpan,
    pub frame_info: EraFuncFrameInfo,
    pub chunk_idx: u32,
    pub bc_offset: u32,
    pub bc_size: u32,
    pub ret_kind: ScalarValueKind,
}

impl EraFuncInfo {
    pub fn from_with(x: &crate::types::EraFuncInfo, i: &impl Interner) -> Self {
        EraFuncInfo {
            name: x.name.as_str().to_owned(),
            name_span: x.name_span,
            frame_info: EraFuncFrameInfo {
                args: x.frame_info.args.clone(),
                vars: x
                    .frame_info
                    .vars
                    .values()
                    .map(|x| EraFuncFrameVarInfo {
                        name: i.resolve(x.name).to_owned(),
                        span: x.span,
                        is_ref: x.is_ref,
                        is_const: x.is_const,
                        is_charadata: x.is_charadata,
                        in_local_frame: x.in_local_frame,
                        var_idx: x.var_idx,
                        var_kind: x.var_kind,
                        dims_cnt: x.dims_cnt,
                    })
                    .collect(),
            },
            chunk_idx: x.chunk_idx,
            bc_offset: x.bc_offset,
            bc_size: x.bc_size,
            ret_kind: x.ret_kind,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EraEngineVersion {
    pub version_str: &'static str,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EraEngineMemoryUsage {
    pub var_size: usize,
    pub code_size: usize,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EraEngineStackTrace {
    pub frames: Vec<EraFuncExecFrame>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EraSourceInfo {
    pub filename: String,
    pub span: SrcSpan,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EraChunkInfo {
    pub name: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EraDumpFunctionBytecodeEntry {
    pub offset: u32,
    pub opcode_str: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EraEvaluateExprResult {
    /// The value of the expression. When result is a compound value, this will be the digest
    /// (string result) of the value.
    pub value: ScalarValue,
    /// The children of the value, if it is a compound value.
    pub children: Vec<(String, ScalarValue)>,
    /// The unmodified offset argument.
    pub offset: u64,
    /// The unmodified count argument.
    pub count: u64,
    /// The total number of children in the value, if it is a compound value.
    pub children_total_count: Option<u64>,
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct EraEngineSnapshotKind: u32 {
        const GLOBAL_VAR = 0b0000_0001;
        const EXEC_STATE = 0b0000_0010;
        const SOURCE_CODE = 0b0000_0100;

        const ALL = 0x07;
        const _ = !0;
    }
}
impl Serialize for EraEngineSnapshotKind {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_u32(self.bits())
    }
}
impl<'de> Deserialize<'de> for EraEngineSnapshotKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(u32::deserialize(deserializer).map(Self::from_bits_truncate)?)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EraEngineSnapshotChunkHeader {
    pub kind: EraEngineSnapshotKind,
    pub size: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EraEngineDecodeBytecodeResult {
    pub bc: EraBytecodeKind,
    pub len: u8,
}

#[easy_jsonrpc::rpc]
pub trait MEraEngineRpc {
    fn reset_exec_to_ip(&mut self, ip: EraExecIp) -> Result<(), MEraEngineError>;
    fn get_func_info(&self, name: &str) -> Option<EraFuncInfo>;
    fn get_func_info_by_ip(&self, ip: EraExecIp) -> Option<EraFuncInfo>;
    fn get_src_info_from_ip(&self, ip: EraExecIp) -> Option<EraSourceInfo>;
    /// Get the IP of the first instruction in the chunk that contains the given source span.
    fn get_ip_from_src(&self, filename: &str, span: SrcSpan) -> Option<EraExecIp>;
    fn get_chunk_info(&self, idx: u32) -> Option<EraChunkInfo>;
    fn get_stack_trace(&self) -> EraEngineStackTrace;
    fn get_current_ip(&self) -> Option<EraExecIp>;
    fn get_file_source(&self, name: &str) -> Option<String>;
    fn get_version(&self) -> EraEngineVersion;
    fn get_mem_usage(&self) -> EraEngineMemoryUsage;
    fn resolve_src_span(
        &self,
        filename: &str,
        span: SrcSpan,
    ) -> Option<DiagnosticResolveSrcSpanResult>;
    fn get_loaded_files_list(&self) -> Vec<String>;
    fn read_bytecode(&self, chunk_idx: u32, offset: u32, size: u32) -> Option<Vec<u8>>;
    fn patch_bytecode(
        &mut self,
        chunk_idx: u32,
        offset: u32,
        data: Vec<u8>,
    ) -> Result<(), MEraEngineError>;
    // TODO: enumerate_scope_vars
    // fn enumerate_scope_vars(&self, scope_idx: Option<u32>) -> !;
    /// Evaluates an expression in the specified scope. The offset and count arguments are used to
    /// retrieve a range of sub items from the result, if the result is a compound value. The expression
    /// may have side effects, i.e. it may modify the global variables, but engine will make sure
    /// the VM state (such as call stack) remains unchanged after the call.
    ///
    /// * `scope_idx`: 0 => global, 1.. => local, None => current (topmost) scope
    fn evaluate_expr(
        &mut self,
        expr: &str,
        scope_idx: Option<u32>,
        offset: u64,
        count: u64,
        eval_limit: u64,
    ) -> Result<EraEvaluateExprResult, MEraEngineError>;
    fn get_functions_list(&self) -> Vec<String>;
    fn dump_function_bytecode(
        &self,
        name: &str,
    ) -> Result<Vec<EraDumpFunctionBytecodeEntry>, MEraEngineError>;
    fn dump_stack(&self) -> Vec<StackValue>;
    fn decode_bytecode(
        &self,
        bc: Vec<u8>,
    ) -> Result<EraEngineDecodeBytecodeResult, MEraEngineError>;
    /// Moves to the next IP that is safe to execute. Safe means that the stack
    /// transitions into a clean and safe state for execution starting from the
    /// new IP.
    fn goto_next_safe_ip(&mut self) -> Result<(), MEraEngineError>;
}

impl<Callback> MEraEngine<Callback> {
    pub fn get_version() -> &'static str {
        "MEraEngine in MEraEmuCore v0.3.1"
    }
}

macro_rules! mtry {
    ($x:expr) => {
        match $x {
            Ok(x) => x,
            Err(e) => return Err(MEraEngineError::from(format!("{:?}", e))),
        }
    };
}

impl<Callback: MEraEngineSysCallback> MEraEngine<Callback> {
    pub fn get_config(&self) -> &MEraEngineConfig {
        &self.config
    }

    pub fn set_config(&mut self, config: MEraEngineConfig) -> Result<(), MEraEngineError> {
        self.config = config;
        Ok(())
    }

    pub fn do_execution(
        &mut self,
        run_flag: &AtomicBool,
        max_inst_cnt: u64,
    ) -> Result<EraExecutionBreakReason, MEraEngineError> {
        let mut vm = EraVirtualMachine::new(&mut self.ctx, &mut self.vm_state);
        vm.set_enable_jit(self.config.vm_cache_strategy == MEraEngineVmCacheStrategy::FastJit);
        Ok(vm.execute(run_flag, max_inst_cnt))
    }

    pub fn do_rpc(&mut self, request: &str) -> String {
        use easy_jsonrpc::Handler;

        let json = match serde_json::from_str::<serde_json::Value>(request) {
            Ok(json) => json,
            Err(_) => {
                return r#"{"jsonrpc":"2.0","error":{"code":-32700,"message":"Parse error"},"id":null}"#
                    .to_string();
            }
        };
        match <dyn MEraEngineRpc>::handle_request(self, json) {
            easy_jsonrpc::MaybeReply::DontReply => String::new(),
            easy_jsonrpc::MaybeReply::Reply(reply) => reply.to_string(),
        }
    }

    pub fn take_snapshot(
        &self,
        parts_to_add: EraEngineSnapshotKind,
    ) -> Result<Vec<u8>, MEraEngineError> {
        Ok(mtry!(self.take_snapshot_inner(parts_to_add)))
    }

    pub fn restore_snapshot(&mut self, snapshot: &[u8]) -> Result<(), MEraEngineError> {
        Ok(mtry!(self.restore_snapshot_inner(snapshot)))
    }

    fn take_snapshot_inner(&self, parts_to_add: EraEngineSnapshotKind) -> anyhow::Result<Vec<u8>> {
        use byteorder::{WriteBytesExt, LE};

        let mut snapshot = Vec::new();
        for kind in [
            EraEngineSnapshotKind::SOURCE_CODE,
            EraEngineSnapshotKind::GLOBAL_VAR,
            EraEngineSnapshotKind::EXEC_STATE,
        ] {
            if !parts_to_add.contains(kind) {
                continue;
            }
            snapshot.write_u32::<LE>(kind.bits())?;
            let pos = snapshot.len();
            snapshot.write_u32::<LE>(0)?;
            let mut sink = ZstdSink::new(&mut snapshot);
            match kind {
                EraEngineSnapshotKind::GLOBAL_VAR => {
                    self.ctx.serialize_global_var_into(&mut sink)?;
                }
                EraEngineSnapshotKind::SOURCE_CODE => {
                    self.ctx.serialize_source_code_into(&mut sink)?;
                }
                EraEngineSnapshotKind::EXEC_STATE => {
                    let value = (&*self.ctx.transient_ctx, &self.vm_state);
                    bincode::serialize_into(&mut sink, &value)?;
                }
                _ => unreachable!(),
            }
            drop(sink);
            let size = snapshot.len() - pos - 4;
            (&mut snapshot[pos..pos + 4]).write_u32::<LE>(size as u32)?;
        }
        Ok(snapshot)
    }

    pub fn restore_snapshot_inner(&mut self, snapshot: &[u8]) -> anyhow::Result<()> {
        use byteorder::{ReadBytesExt, LE};

        let mut snapshot = snapshot;
        while !snapshot.is_empty() {
            let kind = EraEngineSnapshotKind::from_bits_truncate(snapshot.read_u32::<LE>()?);
            let size = snapshot.read_u32::<LE>()? as usize;
            let chunk = &snapshot[..size];
            snapshot = &snapshot[size..];
            let mut chunk = ZstdSource::new(chunk);
            match kind {
                EraEngineSnapshotKind::GLOBAL_VAR => {
                    self.ctx.deserialize_global_var_from(&mut chunk)?;
                }
                EraEngineSnapshotKind::SOURCE_CODE => {
                    self.ctx.deserialize_source_code_from(&mut chunk, true)?;
                }
                EraEngineSnapshotKind::EXEC_STATE => {
                    let (transient_ctx, vm_state) = bincode::deserialize_from(&mut chunk)?;
                    self.ctx.transient_ctx = Arc::new(transient_ctx);
                    self.vm_state = vm_state;
                }
                _ => return Err(anyhow::anyhow!("unknown snapshot kind")),
            }
        }
        Ok(())
    }

    fn resolve_variable_place(&self, place: VariablePlaceRef) -> Option<&ArrayValue> {
        let var_idx = place.index as usize;
        if place.is_dynamic {
            self.vm_state.get_var_stack_value(var_idx)
        } else {
            self.ctx.variables.get_var_by_idx(var_idx)
        }
    }

    fn resolve_variable_place_mut(&mut self, place: VariablePlaceRef) -> Option<&mut ArrayValue> {
        let var_idx = place.index as usize;
        if place.is_dynamic {
            self.vm_state.get_var_stack_value_mut(var_idx)
        } else {
            self.ctx.variables.get_var_by_idx_mut(var_idx)
        }
    }

    // NOTE: Returns None if the variable is not found.
    fn try_evaluate_var_at_scope(
        &self,
        name: &str,
        scope_idx: Option<u32>,
    ) -> anyhow::Result<Option<&ArrayValue>> {
        let scope_idx = scope_idx
            .map(|x| x as usize)
            .unwrap_or_else(|| self.vm_state.get_exec_frames().len());
        let exec_frame = if scope_idx > 0 {
            let exec_frame = (self.vm_state)
                .get_exec_frames()
                .get(scope_idx - 1)
                .context("scope not found")?;
            Some(exec_frame)
        } else {
            None
        };

        // Check local scope first
        if let Some(exec_frame) = exec_frame {
            let func_info = self
                .ctx
                .func_info_from_chunk_pos(exec_frame.ip.chunk as _, exec_frame.ip.offset as _)
                .context("function info not found")?;
            if let Some(var_info) = func_info.frame_info.vars.get(Ascii::new_str(name)) {
                return Ok(Some(if var_info.in_local_frame {
                    let var_idx = var_info.var_idx as usize + exec_frame.stack_start as usize;
                    let var = (self.vm_state)
                        .get_stack_value(var_idx)
                        .context("variable not found in local frame")?;
                    let var = var
                        .as_arr_ref()
                        .cloned()
                        .context("variable is not an array")?;
                    self.resolve_variable_place(var)
                        .with_context(|| format!("array {:?} not found", var))?
                } else {
                    let var_idx = var_info.var_idx as usize;
                    let var = (self.ctx.variables)
                        .get_var_by_idx(var_idx)
                        .context("variable not found in global frame")?;
                    var
                }));
            }
        }

        // Check global scope
        if let Some(var) = self.ctx.variables.get_var(name) {
            return Ok(Some(var));
        }

        Ok(None)
    }
}

impl<Callback: MEraEngineSysCallback> MEraEngineRpc for MEraEngine<Callback> {
    fn reset_exec_to_ip(&mut self, ip: EraExecIp) -> Result<(), MEraEngineError> {
        self.vm_state.reset_exec_to_ip(ip);
        Ok(())
    }

    fn get_func_info(&self, name: &str) -> Option<EraFuncInfo> {
        let i = self.ctx.interner();
        self.ctx
            .func_entries
            .get(Ascii::new_str(name))
            .and_then(Option::as_ref)
            .map(|x| EraFuncInfo::from_with(x, i))
    }

    fn get_func_info_by_ip(&self, ip: EraExecIp) -> Option<EraFuncInfo> {
        self.ctx
            .func_info_from_chunk_pos(ip.chunk as _, ip.offset as _)
            .map(|x| EraFuncInfo::from_with(x, self.ctx.interner()))
    }

    fn get_src_info_from_ip(&self, ip: EraExecIp) -> Option<EraSourceInfo> {
        self.ctx
            .source_info_from_chunk_pos(ip.chunk as _, ip.offset as _)
            .map(|(filename, span)| EraSourceInfo {
                filename: filename.to_string(),
                span,
            })
    }

    fn get_ip_from_src(&self, filename: &str, span: SrcSpan) -> Option<EraExecIp> {
        // WARN: It currently only performs a best-effort search, and will never be exact.
        let (bc_chunk_idx, bc_chunk) = self
            .ctx
            .bc_chunks
            .iter()
            .enumerate()
            .find(|(_, x)| x.name.as_str() == filename)?;
        let mut bc_offset = None;
        for (i, x) in bc_chunk.src_spans_iter().enumerate() {
            if span.contains_pos(x.start()) {
                // Best match - the first instruction in the span
                bc_offset = Some(i);
                break;
            } else if x.intersects(span) && bc_offset.is_none() {
                // Second best match - the first instruction that intersects the span
                bc_offset = Some(i);
            }
        }
        Some(EraExecIp {
            chunk: bc_chunk_idx as _,
            offset: bc_offset? as _,
        })
    }

    fn get_chunk_info(&self, idx: u32) -> Option<EraChunkInfo> {
        self.ctx.bc_chunks.get(idx as usize).map(|x| EraChunkInfo {
            name: x.name.to_string(),
        })
    }

    fn get_stack_trace(&self) -> EraEngineStackTrace {
        let frames = self.vm_state.get_exec_frames().to_owned();
        EraEngineStackTrace { frames }
    }

    fn get_current_ip(&self) -> Option<EraExecIp> {
        self.vm_state.get_exec_frames().last().map(|x| x.ip)
    }

    fn get_file_source(&self, name: &str) -> Option<String> {
        self.ctx
            .source_map
            .get(name)
            .and_then(|x| x.text.as_ref().map(|x| x.to_string()))
    }

    fn get_version(&self) -> EraEngineVersion {
        EraEngineVersion {
            version_str: MEraEngine::<Callback>::get_version(),
        }
    }

    fn get_mem_usage(&self) -> EraEngineMemoryUsage {
        EraEngineMemoryUsage {
            var_size: self.ctx.variables.iter().map(|x| x.val.mem_usage()).sum(),
            code_size: self.ctx.bc_chunks.iter().map(|x| x.mem_usage()).sum(),
        }
    }

    fn resolve_src_span(
        &self,
        filename: &str,
        span: SrcSpan,
    ) -> Option<DiagnosticResolveSrcSpanResult> {
        self.ctx.resolve_src_span(filename, span)
    }

    fn get_loaded_files_list(&self) -> Vec<String> {
        self.ctx.source_map.keys().map(|x| x.to_string()).collect()
    }

    fn read_bytecode(&self, chunk_idx: u32, offset: u32, size: u32) -> Option<Vec<u8>> {
        self.ctx.bc_chunks.get(chunk_idx as usize).and_then(|x| {
            let offset = offset as usize;
            let size = size as usize;
            x.get_bc().get(offset..offset + size).map(|x| x.to_vec())
        })
    }

    fn patch_bytecode(
        &mut self,
        chunk_idx: u32,
        offset: u32,
        data: Vec<u8>,
    ) -> Result<(), MEraEngineError> {
        let chunk_idx = chunk_idx as usize;
        let offset = offset as usize;
        let chunk = Arc::get_mut(&mut self.ctx.bc_chunks)
            .unwrap()
            .get_mut(chunk_idx)
            .ok_or_else(|| MEraEngineError::new("chunk not found".to_owned()))?;
        let Some(bc_slice) = chunk.get_bc_mut().get_mut(offset..offset + data.len()) else {
            return Err(MEraEngineError::new(
                "patched data exceeds chunk size".to_owned(),
            ));
        };
        bc_slice.copy_from_slice(&data);
        Ok(())
    }

    fn evaluate_expr(
        &mut self,
        expr: &str,
        scope_idx: Option<u32>,
        offset: u64,
        count: u64,
        eval_limit: u64,
    ) -> Result<EraEvaluateExprResult, MEraEngineError> {
        if let Some(var) = self.try_evaluate_var_at_scope(expr, scope_idx)? {
            let mut children = Vec::new();
            let children_total_count = var.dims().iter().map(|&x| x as u64).product();
            let digest;
            let digest_limit = 4;
            let has_ellipsis = children_total_count > digest_limit;
            let ellipsis_str = if has_ellipsis { ", ..." } else { "" };
            let vals_range = offset as usize..(offset + count).min(children_total_count) as usize;
            match var.as_unpacked() {
                FlatArrayValueRef::ArrInt(x) => {
                    // NOTE: Must chain with infinite zeros in case the variable is not `ensure_alloc`ed.
                    digest = format!(
                        "[{:?}{}] (ArrInt{:?})",
                        x.vals
                            .iter()
                            .map(|x| x.val)
                            .chain(std::iter::repeat(0))
                            .take(digest_limit.min(children_total_count) as _)
                            .format(", "),
                        ellipsis_str,
                        x.dims
                    );
                    for i in vals_range {
                        let val = x.flat_get_val_safe(i).map(|x| x.val).unwrap();
                        children.push((format!("[{i}]"), ScalarValue::Int(val)));
                    }
                }
                FlatArrayValueRef::ArrStr(x) => {
                    // NOTE: Must chain with infinite zeros in case the variable is not `ensure_alloc`ed.
                    digest = format!(
                        "[{:?}{}] (ArrStr{:?})",
                        x.vals
                            .iter()
                            .map(|x| x.val.as_str())
                            .chain(std::iter::repeat(""))
                            .take(digest_limit.min(children_total_count) as _)
                            .format(", "),
                        ellipsis_str,
                        x.dims
                    );
                    for i in vals_range {
                        let val = x.flat_get_val_safe(i).map(|x| x.val.to_string()).unwrap();
                        children.push((format!("[{i}]"), ScalarValue::Str(val)));
                    }
                }
            }
            return Ok(EraEvaluateExprResult {
                value: ScalarValue::Str(digest),
                children,
                offset,
                count,
                children_total_count: Some(children_total_count),
            });
        }

        // TODO: Finish `evaluate_expr`
        Err(MEraEngineError::new(
            "expr eval not yet implemented".to_owned(),
        ))
    }

    fn get_functions_list(&self) -> Vec<String> {
        self.ctx
            .func_entries
            .keys()
            .map(|x| x.to_string())
            .collect()
    }

    fn dump_function_bytecode(
        &self,
        name: &str,
    ) -> Result<Vec<EraDumpFunctionBytecodeEntry>, MEraEngineError> {
        let func_info = self
            .ctx
            .func_entries
            .get(Ascii::new_str(name))
            .and_then(Option::as_ref)
            .ok_or_else(|| MEraEngineError::new("function not found".to_owned()))?;
        let chunk = self
            .ctx
            .bc_chunks
            .get(func_info.chunk_idx as usize)
            .ok_or_else(|| MEraEngineError::new("chunk not found".to_owned()))?;
        let bc_start = func_info.bc_offset as usize;
        let bc_end = bc_start + func_info.bc_size as usize;
        let mut bc = &chunk.get_bc()[bc_start..bc_end];
        let mut lines = Vec::new();
        loop {
            use crate::util::SubsliceOffset;
            let bc_offset = chunk.get_bc().subslice_offset(bc).unwrap();
            let Ok(cur_inst) = EraBytecodeKind::from_reader(&mut bc) else {
                break;
            };
            let mut opcode_str = format!("{:?}", cur_inst);
            if let EraBytecodeKind::LoadConstStr { idx } = cur_inst {
                let s =
                    TokenKey::try_from_u32(idx).and_then(|x| self.ctx.interner().try_resolve(x));
                if let Some(s) = s {
                    opcode_str += &format!(" ; {:?}", s);
                } else {
                    opcode_str += " ; <invalid>";
                }
            } else if let EraBytecodeKind::JumpWW { offset }
            | EraBytecodeKind::JumpIfWW { offset }
            | EraBytecodeKind::JumpIfNotWW { offset } = cur_inst
            {
                opcode_str += &format!(
                    " ; -> {}",
                    bc_offset.wrapping_add_signed(offset as _) as u32
                );
            }
            lines.push(EraDumpFunctionBytecodeEntry {
                offset: bc_offset as _,
                opcode_str,
            });
        }
        if !bc.is_empty() {
            lines.push(EraDumpFunctionBytecodeEntry {
                offset: u32::MAX,
                opcode_str: format!("; WARNING: {} bytes of trailing data not decoded", bc.len()),
            });
        }
        Ok(lines)
    }

    fn dump_stack(&self) -> Vec<StackValue> {
        self.vm_state.get_stack().to_owned()
    }

    fn decode_bytecode(
        &self,
        bc: Vec<u8>,
    ) -> Result<EraEngineDecodeBytecodeResult, MEraEngineError> {
        let mut bc = bc.as_slice();
        let orig_bc = bc;
        match EraBytecodeKind::from_reader(&mut bc) {
            Ok(cur_inst) => Ok(EraEngineDecodeBytecodeResult {
                bc: cur_inst,
                len: orig_bc.subslice_offset(bc).unwrap() as _,
            }),
            Err(e) => Err(MEraEngineError::new(format!("{:?}", e))),
        }
    }

    fn goto_next_safe_ip(&mut self) -> Result<(), MEraEngineError> {
        self.goto_next_safe_ip_inner()
            .map_err(|e| MEraEngineError::new(format!("{:?}", e)))
    }
}

impl<Callback: MEraEngineSysCallback> MEraEngine<Callback> {
    fn goto_next_safe_ip_inner(&mut self) -> anyhow::Result<()> {
        use hashbrown::hash_map::Entry;

        let cur_ip = self.vm_state.get_cur_ip().context("no current IP")?;
        let func_info = self
            .ctx
            .func_info_from_chunk_pos(cur_ip.chunk as _, cur_ip.offset as _)
            .context("function info not found")?;
        let bc = (self.ctx.bc_chunks)
            .get(cur_ip.chunk as usize)
            .context("chunk not found")?;
        let bc = {
            let start = func_info.bc_offset as usize;
            let end = start + func_info.bc_size as usize;
            &bc.get_bc()[start..end]
        };
        let func_local_vars_count = (func_info.frame_info.vars.iter())
            .filter(|(_, x)| x.in_local_frame)
            .count() as u32;

        // Build stack balance map
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        struct InstInfo {
            /// The stack length before executing the instruction.
            pub stack_balance: u32,
            /// The stack length to restore to if going to rollback from this instruction.
            pub base_stack_balance: u32,
        }
        let mut bc_infos = HashMap::new();
        bc_infos.insert(
            0,
            InstInfo {
                stack_balance: 0,
                base_stack_balance: 0,
            },
        );
        let mut queue = VecDeque::new();
        queue.push_back(0);

        #[cfg(debug_assertions)]
        #[cfg(windows)]
        let mut counters = HashMap::new();

        while let Some(ip) = queue.pop_front() {
            let bc = &bc[ip..];
            if bc.is_empty() {
                // Reached the end of the function.
                continue;
            }
            let (cur_inst, cur_inst_len) =
                EraBytecodeKind::with_len_from_bytes(bc).context("bytecode decode error")?;
            let cur_inst_info = bc_infos[&ip];
            let stack_delta = if let Some(stack_delta) = cur_inst.stack_influence() {
                stack_delta
            } else if let EraBytecodeKind::CallFun { args_cnt, func_idx } = cur_inst {
                // NB: Not handled by `stack_influence`, because it depends on the function
                // signature (return value).

                let func_idx = func_idx as usize;
                let Some(func_info) = (self.ctx.func_entries)
                    .get_index(func_idx)
                    .and_then(|x| x.1.as_ref())
                else {
                    continue;
                };
                -(args_cnt as i32 - func_info.ret_kind.is_value() as i32)
            } else if matches!(
                cur_inst,
                EraBytecodeKind::FailWithMsg | EraBytecodeKind::Quit | EraBytecodeKind::Throw
            ) {
                // HACK: Assume we can go past these instructions.
                -((cur_inst_info.stack_balance - cur_inst_info.base_stack_balance) as i32)
            } else {
                // Control flow cannot go past this instruction. Stop here.
                continue;
            };
            let Some(stack_balance) = cur_inst_info.stack_balance.checked_add_signed(stack_delta)
            else {
                // Wrong path; maybe need analysis from other paths to determine the correct stack balance.
                continue;
                // anyhow::bail!(
                //     "cannot add stack balance {} with {}: underflow\nwhile processing IP {} {:?} {:?}",
                //     cur_inst_info.stack_balance,
                //     stack_delta,
                //     ip,
                //     cur_inst,
                //     cur_inst_info
                // );
            };
            let next_inst_info = InstInfo {
                stack_balance,
                base_stack_balance: if matches!(cur_inst, EraBytecodeKind::ForLoopNoStep) {
                    // Currently, only for loops introduce a new stack balance.
                    cur_inst_info.stack_balance
                } else {
                    (cur_inst_info.base_stack_balance)
                        .max(func_local_vars_count)
                        .min(stack_balance)
                },
            };
            // Fill next instruction info
            let mut apply_at = |new_ip| -> anyhow::Result<()> {
                #[cfg(debug_assertions)]
                #[cfg(windows)]
                unsafe {
                    counters.entry(ip).and_modify(|x| *x += 1).or_insert(1);
                    let msg = format!(
                        "IP {} -> {} {:?} {} times\n    (stack {:?} -> {:?})\n\0",
                        ip, new_ip, cur_inst, counters[&ip], cur_inst_info, next_inst_info
                    );
                    windows_sys::Win32::System::Diagnostics::Debug::OutputDebugStringA(
                        msg.as_ptr(),
                    );
                }

                // match bc_infos.entry(new_ip) {
                //     Entry::Occupied(e) => {
                //         // Already visited this instruction. Check whether the stack balance is the same.
                //         if e.get() != &next_inst_info {
                //             // Stack balance conflict!
                //             anyhow::bail!(
                //                 "stack balance conflict at IP {}; {:?} != {:?}; originated from {}({:?})",
                //                 new_ip,
                //                 next_inst_info,
                //                 e.get(),
                //                 ip,
                //                 cur_inst_info
                //             );
                //         }
                //     }
                //     Entry::Vacant(e) => {
                //         // New instruction. Add to the queue.
                //         e.insert(next_inst_info);
                //         queue.push_back(new_ip);
                //     }
                // }
                match bc_infos.entry(new_ip) {
                    Entry::Occupied(e)
                        if next_inst_info.stack_balance > e.get().stack_balance
                            || next_inst_info.base_stack_balance > e.get().base_stack_balance =>
                    {
                        // Stack balance updated. Add to the queue.
                        e.replace_entry(next_inst_info);
                        queue.push_back(new_ip);
                    }
                    Entry::Vacant(e) => {
                        // New instruction. Add to the queue.
                        e.insert(next_inst_info);
                        queue.push_back(new_ip);
                    }
                    _ => (),
                }
                Ok(())
            };
            match cur_inst {
                EraBytecodeKind::JumpWW { offset } => {
                    apply_at(ip.wrapping_add_signed(offset as _))?
                }
                EraBytecodeKind::JumpIfWW { offset } | EraBytecodeKind::JumpIfNotWW { offset } => {
                    apply_at(ip.wrapping_add_signed(offset as _))?;
                    apply_at(ip + cur_inst_len as usize)?;
                }
                _ => apply_at(ip + cur_inst_len as usize)?,
            }
        }

        // Now transition to the next safe IP by searching down from the current IP.
        let mut offset = (cur_ip.offset - func_info.bc_offset) as usize;
        let this_bc_info = bc_infos
            .get(&offset)
            .with_context(|| format!("IP not found: {}", offset))?;
        while let Some(cur_inst) = EraBytecodeKind::from_bytes(&bc[offset..]) {
            let bc_info = bc_infos
                .get(&offset)
                .with_context(|| format!("IP not found: {}", offset))?;
            if bc_info.stack_balance == this_bc_info.base_stack_balance {
                // Found a safe IP.
                let new_ip = EraExecIp {
                    chunk: cur_ip.chunk,
                    offset: func_info.bc_offset + offset as u32,
                };
                let delta = this_bc_info.stack_balance - this_bc_info.base_stack_balance;
                let new_len = self.vm_state.stack_len() - delta as usize;
                self.vm_state.set_cur_ip(new_ip).unwrap();
                self.vm_state.truncate_stack(new_len);
                return Ok(());
            }
            offset += cur_inst.bytes_len();
        }

        anyhow::bail!("no safe IP found");
    }
}
