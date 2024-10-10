use std::{
    any,
    cell::RefCell,
    collections::BTreeMap,
    ops::{ControlFlow, DerefMut},
    sync::atomic::AtomicBool,
};

use bstr::ByteSlice;
use either::Either;
use fxhash::FxBuildHasher;
use hashbrown::{HashMap, HashSet};
use indoc::indoc;
use itertools::Itertools;
use num_ordinal::{ordinal0, Ordinal, Osize};
use once_cell::sync::Lazy;
use rclite::Rc;
use rcstr::ArcStr;
use regex::Regex;
use safer_ffi::derive_ReprC;
use smallvec::smallvec;

use crate::{
    types::*,
    v2::interpret::{EraInterpretError, EraInterpreter},
};

use crate::util::*;

#[derive(Debug)]
struct InitialVarDesc {
    pub is_string: bool,
    pub dims: EraVarDims,
    pub is_const: bool,
    pub is_charadata: bool,
    pub is_global: bool,
    pub initial_sval: Option<Vec<StrValue>>,
    pub initial_ival: Option<Vec<IntValue>>,
}

pub struct MEraEngine<Callback> {
    ctx: EraCompilerCtx<Callback>,
}

// TODO: Scale charas variables dynamically; use default capacity of 128 then.
pub const MAX_CHARA_COUNT: u32 = 512;

#[derive(thiserror::Error, Debug)]
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

#[derive(Clone)]
pub struct MEraEngineConfig {
    /// Memory limit in bytes. 0 means no limit.
    pub memory_limit: u64,
    /// Number of threads to use for parallel compilation. 0 means auto.
    pub threads_cnt: u32,
}
impl Default for MEraEngineConfig {
    fn default() -> Self {
        MEraEngineConfig {
            memory_limit: 0,
            threads_cnt: 0,
        }
    }
}

pub use crate::types::EraCompilerHostFile as MEraEngineHostFile;

use super::{codegen::EraCodeGenerator, lexer::EraLexer, parser::EraParser};

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
    fn on_html_print(&mut self, content: &str) {}
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
    fn emit_diag(&mut self, diag: &crate::types::DiagnosticProvider) {
        self.on_error(diag)
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

    fn on_html_print(&mut self, content: &str) {
        self.on_html_print(content)
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
#[repr(C)]
#[derive(Default, Debug, Clone)]
pub struct EraExecIpInfo {
    chunk: u32,
    offset: u32,
}

#[derive_ReprC]
#[repr(C)]
#[derive(Default, Debug, Clone)]
pub struct EraFuncInfo {
    pub entry: EraExecIpInfo,
    pub args_cnt: u32,
}

// pub struct EngineStackTraceFrame<'a> {
//     pub file_name: &'a str,
//     pub func_name: &'a str,
//     pub ip: EraExecIpInfo,
//     pub src_info: ExecSourceInfo,
// }
// pub struct EngineStackTrace<'a> {
//     pub frames: Vec<EngineStackTraceFrame<'a>>,
// }

#[derive(Debug)]
pub struct EngineMemoryUsage {
    pub var_size: usize,
    pub code_size: usize,
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
    ctx: EraCompilerCtx<SysCallback>,
    builder_callback: Option<BuilderCallback>,
    config: MEraEngineConfig,
    watching_vars: HashSet<Ascii<ArcStr>, FxBuildHasher>,
    initial_vars: Option<HashMap<Ascii<ArcStr>, InitialVarDesc, FxBuildHasher>>,
    node_cache: cstree::build::NodeCache<'static>,
    /// Whether the headers have been compiled.
    is_header_finished: bool,
    erh_count: usize,
    erb_count: usize,
}

impl<T: MEraEngineSysCallback> MEraEngineBuilder<T, EmptyCallback> {
    pub fn new(callback: T) -> MEraEngineBuilder<T, EmptyCallback> {
        let ctx = EraCompilerCtx::new(callback);
        let mut this = MEraEngineBuilder {
            ctx,
            builder_callback: Default::default(),
            config: Default::default(),
            watching_vars: Default::default(),
            initial_vars: Some(Default::default()),
            node_cache: Default::default(),
            is_header_finished: false,
            erh_count: 0,
            erb_count: 0,
        };

        // Initialize default global variables
        this.add_initial_vars();

        this
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
            node_cache: self.node_cache,
            is_header_finished: self.is_header_finished,
            erh_count: self.erh_count,
            erb_count: self.erb_count,
        }
    }
    pub fn with_config(mut self, new_config: MEraEngineConfig) -> Self {
        self.config = new_config;
        self
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
        let content = &content.to_str_lossy();
        let filename = ArcStr::from(filename);

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
            val.parse().map_err(|_| {
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
        let parse_i64_tail = |this: &mut Self, mut val: &str| -> Result<i64, ()> {
            // HACK: Ignore bad characters after integers
            if let Some(pos) = val.find(|x| !matches!(x, '0'..='9' | ' ' | '\t' | '-')) {
                let (left, right) = val.split_at(pos);
                let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                let offset = content.subslice_offset(right).unwrap();
                diag.span_err(
                    Default::default(),
                    SrcSpan::new(SrcPos(offset as _), right.len() as _),
                    format!(
                        "invalid character `{}` after integer; did you mean `;`?",
                        right.chars().next().unwrap()
                    ),
                );
                this.ctx.emit_diag(diag);
                val = left.trim();
            }
            val.parse().map_err(|_| {
                let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                let offset = content.subslice_offset(val).unwrap();
                diag.span_err(
                    Default::default(),
                    SrcSpan::new(SrcPos(offset as _), val.len() as _),
                    "failed to parse i64",
                );
                this.ctx.emit_diag(diag);
            })
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
        let add_var_i64_str = |this: &mut Self, name: &str, val: &str| -> Result<(), ()> {
            let val = parse_i64_tail(this, val)?;
            _ = this.ctx.variables.add_var(EraVarInfo {
                name: Ascii::new(ArcStr::from(name)),
                val: Value::new_int_0darr(val),
                is_const: true,
                is_charadata: false,
                is_global: false,
                never_trap: true,
            });
            Ok(())
        };
        let add_var_str = |this: &mut Self, name: &str, val: &str| -> Result<(), ()> {
            _ = this.ctx.variables.add_var(EraVarInfo {
                name: Ascii::new(ArcStr::from(name)),
                val: Value::new_str_0darr(ArcStr::from(val)),
                is_const: true,
                is_charadata: false,
                is_global: false,
                never_trap: true,
            });
            Ok(())
        };
        let add_arr_i64_str = |this: &mut Self, name: &str, val: &str| -> Result<(), ()> {
            let val = val
                .split('/')
                .map(|x| parse_i64_tail(this, x).map(IntValue::new))
                .collect::<Result<Vec<_>, _>>()?;
            let val = Value::new_int_arr(smallvec![val.len() as _], val);
            _ = this.ctx.variables.add_var(EraVarInfo {
                name: Ascii::new(ArcStr::from(name)),
                val,
                is_const: true,
                is_charadata: false,
                is_global: false,
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
                let Some(initial_vars) = self.initial_vars.as_mut() else {
                    return Err(MEraEngineError::new("CSV loaded too late".into()));
                };

                let rows = Self::parse_csv_loose(content);
                for row in rows {
                    if let [name, size] = row[..] {
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
                let rows = Self::parse_csv_loose(content);
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
                let Some(initial_vars) = self.initial_vars.as_mut() else {
                    return Err(MEraEngineError::new("CSV loaded too late".into()));
                };

                let target = "ITEMNAME";
                let target2 = "ITEMPRICE";
                let [name_vd, price_vd] = initial_vars
                    .get_many_mut([target, target2].map(Ascii::new_str))
                    .ok_or_else(|| MEraEngineError::new("variable not found".into()))?;
                let mut initial_nameval = vec![StrValue::default(); name_vd.dims[0] as _];
                let mut initial_priceval = vec![IntValue::default(); price_vd.dims[0] as _];

                let diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                for mut cols in Self::parse_csv_loose(content) {
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
            EraCsvLoadKind::Str => load_kvs("STRNAME", Some(EraCsvVarKind::CsvStr))?,
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
                    .map(|x| x.0)
                    .unwrap_or("");
                // TODO: Use self.callback to pre-allocate graphics & sprites
                todo!()
            }
            EraCsvLoadKind::GameBase => {
                for [index, value] in self.parse_csv::<2>(filename.clone(), content) {
                    let offset = content.subslice_offset(index).unwrap();
                    let index_span = SrcSpan::new(SrcPos(offset as _), index.len() as _);

                    let result = match index {
                        "コード" => add_var_i64_str(self, "GAMEBASE_GAMECODE", value),
                        "バージョン" => add_var_i64_str(self, "GAMEBASE_VERSION", value),
                        "バージョン違い認める" => {
                            add_var_i64_str(self, "GAMEBASE_ALLOWVERSION", value)
                        }
                        "最初からいるキャラ" => {
                            add_var_i64_str(self, "GAMEBASE_DEFAULTCHARA", value)
                        }
                        "アイテムなし" => add_var_i64_str(self, "GAMEBASE_NOITEM", value),
                        "タイトル" => add_var_str(self, "GAMEBASE_TITLE", value.into()),
                        "作者" => add_var_str(self, "GAMEBASE_AUTHER", value)
                            .and_then(|_| add_var_str(self, "GAMEBASE_AUTHOR", value)),
                        "製作年" => add_var_str(self, "GAMEBASE_YEAR", value.into()),
                        "追加情報" => add_var_str(self, "GAMEBASE_INFO", value.into()),
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
                add_var_i64_str(self, "GAMEBASE_VERSION", "0").unwrap();
            }
            EraCsvLoadKind::_Replace => {
                for [index, value] in self.parse_csv::<2>(filename.clone(), content) {
                    let offset = content.subslice_offset(index).unwrap();
                    let index_span = SrcSpan::new(SrcPos(offset as _), index.len() as _);

                    let result = match index {
                        "汚れの初期値" => add_arr_i64_str(self, "DEFAULT_STAIN", value),
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
                add_arr_i64_str(self, "DEFAULT_STAIN", "0/0/2/1/8").unwrap();
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
                var_desc.dims.insert(0, MAX_CHARA_COUNT);
            }
            let val = if var_desc.is_string {
                Value::new_str_arr(
                    var_desc.dims,
                    var_desc
                        .initial_sval
                        .unwrap_or_else(|| vec![Default::default()]),
                )
            } else {
                Value::new_int_arr(
                    var_desc.dims,
                    var_desc
                        .initial_ival
                        .unwrap_or_else(|| vec![Default::default()]),
                )
            };
            if self
                .ctx
                .variables
                .add_var(EraVarInfo {
                    name: Ascii::new(name.clone()),
                    val,
                    is_const: var_desc.is_const,
                    is_charadata: var_desc.is_charadata,
                    is_global: var_desc.is_global,
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

    pub fn finish_load_erh(&mut self) -> Result<(), MEraEngineError> {
        if self.is_header_finished {
            return Ok(());
        }

        self.is_header_finished = true;

        // HACK: Set up node cache
        std::mem::swap(&mut self.node_cache, &mut self.ctx.node_cache);
        let result = self.finish_load_erh_inner();
        std::mem::swap(&mut self.node_cache, &mut self.ctx.node_cache);
        self.ctx.active_source = ArcStr::default();

        result
    }

    pub fn load_erb(&mut self, filename: &str, content: &[u8]) -> Result<(), MEraEngineError> {
        self.load_erb_inner(filename, content, false)
    }

    pub fn build(mut self) -> Result<MEraEngine<T>, MEraEngineError> {
        _ = self.finish_load_csv()?;
        _ = self.finish_load_erh()?;

        // Load builtin source
        {
            const SYS_SRC: &str = include_str!("../sys_src.ERB");
            self.load_erb("<builtin>", SYS_SRC.as_bytes())?;
        }

        self.ctx.node_cache = self.node_cache;

        // Process .erb files
        let filenames = self
            .ctx
            .source_map
            .values()
            .filter(|x| !x.is_header)
            .map(|x| x.filename.clone())
            .collect::<Vec<_>>();
        let mut codegen = EraCodeGenerator::new(&mut self.ctx);
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

        // TODO: Call `optimize_resolution` on entering VM on the interner (which populates the
        //       resolution cache Vec<&str> from DashMap), which helps speed up the VM. Requires
        //       modification to the (multi-threaded) interner to allow this.

        // TODO: Handle watching variables

        Ok(MEraEngine { ctx: self.ctx })
    }

    pub fn register_variable(
        &mut self,
        name: &str,
        is_string: bool,
        dimension: u32,
        watch: bool,
    ) -> Result<(), MEraEngineError> {
        let val = if is_string {
            Value::new_str_arr(smallvec![dimension], Vec::new())
        } else {
            Value::new_int_arr(smallvec![dimension], Vec::new())
        };
        let name = ArcStr::from(name);
        let var_idx = self
            .ctx
            .variables
            .add_var(EraVarInfo {
                name: Ascii::new(name.clone()),
                val,
                is_const: false,
                is_charadata: false,
                is_global: false,
                never_trap: !watch,
            })
            .ok_or_else(|| MEraEngineError::new(format!("variable `{name}` already exists")))?;
        if watch {
            self.watching_vars.insert(Ascii::new(name));
        }
        Ok(())
    }

    fn load_erb_inner(
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
        let node_cache = &mut self.node_cache;
        let mut lexer = EraLexer::new(&mut self.ctx, content);
        let mut parser = EraParser::new(&mut lexer, node_cache, is_header);
        let (ast, macro_map) = parser.parse_program();
        let newline_pos = {
            let mut newline_pos = lexer.newline_positions();
            // Fix newline positions
            // NOTE: Newlines will never occur inside macros
            // TODO: Optimize performance with the hint above
            for pos in newline_pos.iter_mut() {
                let span = SrcSpan::new(*pos, 0);
                let span = macro_map.inverse_translate_span(span);
                *pos = span.start();
            }
            newline_pos
        };
        self.ctx.active_source = ArcStr::default();

        // Add source map entry
        let source_map = Rc::get_mut(&mut self.ctx.source_map).unwrap();
        let src_file = source_map.insert(
            filename.clone(),
            EraSourceFile {
                filename: filename.clone(),
                final_root: ast,
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

    fn finish_load_erh_inner(&mut self) -> Result<(), MEraEngineError> {
        use super::ast::*;

        // Materialize .erh variable definitions
        let mut unresolved_vars = Vec::new();
        let source_map = Rc::clone(&self.ctx.source_map);
        let mut interp = EraInterpreter::new(&mut self.ctx, true);

        for (i, erh) in source_map.values().filter(|x| x.is_header).enumerate() {
            interp.get_ctx_mut().active_source = erh.filename.clone();

            let node = cstree::syntax::SyntaxNode::<EraTokenKind>::new_root(erh.final_root.clone());
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
            );
            fn add_int(&mut self, name: ArcStr, dims: EraVarDims) {
                self.add_item(name, dims, false, false, false, false)
            }
            fn add_str(&mut self, name: ArcStr, dims: EraVarDims) {
                self.add_item(name, dims, true, false, false, false)
            }
            fn add_const_int(&mut self, name: ArcStr, dims: EraVarDims) {
                self.add_item(name, dims, false, true, false, false)
            }
            fn add_const_str(&mut self, name: ArcStr, dims: EraVarDims) {
                self.add_item(name, dims, true, true, false, false)
            }
            fn add_chara_int(&mut self, name: ArcStr, dims: EraVarDims) {
                self.add_item(name, dims, false, false, true, false)
            }
            fn add_chara_str(&mut self, name: ArcStr, dims: EraVarDims) {
                self.add_item(name, dims, true, false, true, false)
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
            ) {
                self.initial_vars.as_mut().unwrap().insert(
                    Ascii::new(name),
                    InitialVarDesc {
                        is_string,
                        dims,
                        is_const,
                        is_charadata,
                        is_global,
                        initial_sval: None,
                        initial_ival: None,
                    },
                );
            }
        }

        // ………………………………………………
        // 数値配列型変数
        // ………………………………………………
        self.add_int(rcstr::literal!("DAY"), smallvec![1]);
        self.add_int(rcstr::literal!("MONEY"), smallvec![1]);
        self.add_int(rcstr::literal!("TIME"), smallvec![1]);
        self.add_int(rcstr::literal!("ITEM"), smallvec![1]);
        self.add_int(rcstr::literal!("ITEMSALES"), smallvec![1]);
        self.add_int(rcstr::literal!("NOITEM"), smallvec![1]);
        self.add_int(rcstr::literal!("BOUGHT"), smallvec![1]);
        self.add_int(rcstr::literal!("PBAND"), smallvec![1]);
        self.add_int(rcstr::literal!("FLAG"), smallvec![1]);
        self.add_int(rcstr::literal!("TFLAG"), smallvec![1]);
        self.add_int(rcstr::literal!("TARGET"), smallvec![1]);
        self.add_int(rcstr::literal!("MASTER"), smallvec![1]);
        self.add_int(rcstr::literal!("PLAYER"), smallvec![1]);
        self.add_int(rcstr::literal!("ASSI"), smallvec![1]);
        self.add_int(rcstr::literal!("ASSIPLAY"), smallvec![1]);
        self.add_int(rcstr::literal!("UP"), smallvec![1]);
        self.add_int(rcstr::literal!("DOWN"), smallvec![1]);
        self.add_int(rcstr::literal!("LOSEBASE"), smallvec![1]);
        self.add_int(rcstr::literal!("PALAMLV"), smallvec![1]);
        self.add_int(rcstr::literal!("EXPLV"), smallvec![1]);
        self.add_int(rcstr::literal!("EJAC"), smallvec![1]);
        self.add_int(rcstr::literal!("PREVCOM"), smallvec![1]);
        self.add_int(rcstr::literal!("SELECTCOM"), smallvec![1]);
        self.add_int(rcstr::literal!("NEXTCOM"), smallvec![1]);
        self.add_int(rcstr::literal!("RESULT"), smallvec![1]);
        self.add_int(rcstr::literal!("COUNT"), smallvec![1]);
        self.add_int(rcstr::literal!("A"), smallvec![1]);
        self.add_int(rcstr::literal!("B"), smallvec![1]);
        self.add_int(rcstr::literal!("C"), smallvec![1]);
        self.add_int(rcstr::literal!("D"), smallvec![1]);
        self.add_int(rcstr::literal!("E"), smallvec![1]);
        self.add_int(rcstr::literal!("F"), smallvec![1]);
        self.add_int(rcstr::literal!("G"), smallvec![1]);
        self.add_int(rcstr::literal!("H"), smallvec![1]);
        self.add_int(rcstr::literal!("I"), smallvec![1]);
        self.add_int(rcstr::literal!("J"), smallvec![1]);
        self.add_int(rcstr::literal!("K"), smallvec![1]);
        self.add_int(rcstr::literal!("L"), smallvec![1]);
        self.add_int(rcstr::literal!("M"), smallvec![1]);
        self.add_int(rcstr::literal!("N"), smallvec![1]);
        self.add_int(rcstr::literal!("O"), smallvec![1]);
        self.add_int(rcstr::literal!("P"), smallvec![1]);
        self.add_int(rcstr::literal!("Q"), smallvec![1]);
        self.add_int(rcstr::literal!("R"), smallvec![1]);
        self.add_int(rcstr::literal!("S"), smallvec![1]);
        self.add_int(rcstr::literal!("T"), smallvec![1]);
        self.add_int(rcstr::literal!("U"), smallvec![1]);
        self.add_int(rcstr::literal!("V"), smallvec![1]);
        self.add_int(rcstr::literal!("W"), smallvec![1]);
        self.add_int(rcstr::literal!("X"), smallvec![1]);
        self.add_int(rcstr::literal!("Y"), smallvec![1]);
        self.add_int(rcstr::literal!("Z"), smallvec![1]);
        self.add_const_int(rcstr::literal!("ITEMPRICE"), smallvec![1]);
        // ………………………………………………
        // 文字列配列型変数
        // ………………………………………………
        self.add_str(rcstr::literal!("SAVESTR"), smallvec![1]);
        self.add_str(rcstr::literal!("RESULTS"), smallvec![1]);
        self.add_str(rcstr::literal!("TSTR"), smallvec![1]);
        self.add_str(rcstr::literal!("STR"), smallvec![1]);
        // ITEMNAMEとITEMPRICEは片方を変更すると他方も同じ値に変更されます
        self.add_const_str(rcstr::literal!("ITEMNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("ABLNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("EXPNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("TALENTNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("PALAMNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("TRAINNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("MARKNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("BASENAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("SOURCENAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("EXNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("EQUIPNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("TEQUIPNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("FLAGNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("CFLAGNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("TFLAGNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("TCVARNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("CSTRNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("STAINNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("STRNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("TSTRNAME"), smallvec![1]);
        self.add_const_str(rcstr::literal!("SAVESTRNAME"), smallvec![1]);
        // ………………………………………………
        // 角色変数
        // ………………………………………………
        self.add_chara_int(rcstr::literal!("BASE"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("MAXBASE"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("DOWNBASE"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("ABL"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("TALENT"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("EXP"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("MARK"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("PALAM"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("SOURCE"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("EX"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("NOWEX"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("CFLAG"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("JUEL"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("GOTJUEL"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("CUP"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("CDOWN"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("RELATION"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("EQUIP"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("TEQUIP"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("STAIN"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("TCVAR"), smallvec![1]);
        // ………………………………………………
        // 角色文字列変数
        // ………………………………………………
        self.add_chara_str(rcstr::literal!("NAME"), smallvec![1]);
        self.add_chara_str(rcstr::literal!("CALLNAME"), smallvec![1]);
        self.add_chara_str(rcstr::literal!("NICKNAME"), smallvec![1]);
        self.add_chara_str(rcstr::literal!("MASTERNAME"), smallvec![1]);
        self.add_chara_str(rcstr::literal!("CSTR"), smallvec![1]);
        // ………………………………………………
        // 特殊一時変数・一時文字列変数
        // ………………………………………………
        self.add_int(rcstr::literal!("LOCAL"), smallvec![1]);
        self.add_str(rcstr::literal!("LOCALS"), smallvec![1]);
        self.add_int(rcstr::literal!("ARG"), smallvec![1]);
        self.add_str(rcstr::literal!("ARGS"), smallvec![1]);
        self.add_item(
            rcstr::literal!("GLOBAL"),
            smallvec![1],
            false,
            false,
            false,
            true,
        );
        self.add_item(
            rcstr::literal!("GLOBALS"),
            smallvec![1],
            true,
            false,
            false,
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
        self.add_chara_int(rcstr::literal!("NO"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("ISASSI"), smallvec![1]);
        self.add_chara_int(rcstr::literal!("CDFLAG"), smallvec![1, 1]);
        self.add_int(rcstr::literal!("DITEMTYPE"), smallvec![1, 1]);
        self.add_int(rcstr::literal!("DA"), smallvec![1, 1]);
        self.add_int(rcstr::literal!("DB"), smallvec![1, 1]);
        self.add_int(rcstr::literal!("DC"), smallvec![1, 1]);
        self.add_int(rcstr::literal!("DD"), smallvec![1, 1]);
        self.add_int(rcstr::literal!("DE"), smallvec![1, 1]);
        self.add_int(rcstr::literal!("TA"), smallvec![1, 1, 1]);
        self.add_int(rcstr::literal!("TB"), smallvec![1, 1, 1]);
    }

    fn parse_csv<'a, const COL_COUNT: usize>(
        &mut self,
        filename: ArcStr,
        content: &'a str,
    ) -> Vec<[&'a str; COL_COUNT]> {
        use crate::util::SubsliceOffset;

        let mut rows = Vec::new();

        'outer: for cont_line in content.lines() {
            // Remove comments
            let cont_line = if let Some(pos) = cont_line.find(';') {
                &cont_line[..pos]
            } else {
                cont_line
            };

            if cont_line.trim_ascii().is_empty() {
                continue;
            }

            let mut cur_row = [""; COL_COUNT];
            cur_row[0] = cont_line;
            // NOTE: Excessive columns are allowed and combined into the last column.
            for i in 1..COL_COUNT {
                let Some((cur_col, rest)) = cur_row[i - 1].split_once(',') else {
                    let mut diag = Diagnostic::with_src(filename.clone(), content.as_bytes());
                    let offset = cont_line.subslice_offset(cur_row[i - 1]).unwrap();
                    diag.span_err(
                        Default::default(),
                        SrcSpan::new(SrcPos(offset as _), cur_row[i - 1].len() as _),
                        format!("expected {} columns, found {}", COL_COUNT, i),
                    );
                    self.ctx.emit_diag(diag);
                    continue 'outer;
                };
                (cur_row[i - 1], cur_row[i]) = (cur_col.trim_ascii(), rest.trim_ascii());
            }

            // HACK: Remove trailing comma (& rows)
            if let Some(left) = cur_row[COL_COUNT - 1].split_once(',') {
                cur_row[COL_COUNT - 1] = left.0.trim_ascii();
            }

            rows.push(cur_row);
        }

        rows
    }

    fn parse_csv_loose(content: &str) -> Vec<Vec<&str>> {
        let mut rows = Vec::new();

        for cont_line in content.lines() {
            // Remove comments
            let cont_line = if let Some(pos) = cont_line.find(';') {
                &cont_line[..pos]
            } else {
                cont_line
            };

            if cont_line.trim_ascii().is_empty() {
                continue;
            }

            let cur_row = cont_line.split(',').map(|x| x.trim_ascii()).collect();

            rows.push(cur_row);
        }

        rows
    }
}
