use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap, HashSet},
    ops::DerefMut,
    rc::Rc,
    sync::atomic::AtomicBool,
};

use indoc::indoc;
use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::Regex;

use crate::{
    bytecode::{EraCharaInitTemplate, EraCsvVarKind, IntValue, SourcePosInfo, StrValue, Value},
    compiler::{EraBytecodeCompilation, EraCompilerFileInput},
    lexer::EraLexerTempStorage,
    parser::EraParserSlimVarTypeInfo,
    vm::{EraVarPool, EraVirtualMachine},
};

use crate::util::*;

#[derive(Debug)]
struct InitialVarDesc {
    pub is_string: bool,
    pub dims: smallvec::SmallVec<[u32; 3]>,
    pub is_const: bool,
    pub is_charadata: bool,
    pub is_global: bool,
    pub initial_sval: Option<Vec<StrValue>>,
    pub initial_ival: Option<Vec<IntValue>>,
}

pub struct MEraEngine<'a> {
    file_inputs: Vec<EraCompilerFileInput>,
    vm: Option<EraVirtualMachine>,
    callback: Box<dyn MEraEngineSysCallback + 'a>,
    config: MEraEngineConfig,
    global_vars: EraVarPool,
    watching_vars: HashSet<Ascii<arcstr::ArcStr>>,
    replace_list: HashMap<Box<[u8]>, Box<[u8]>>,
    define_list: HashMap<Box<[u8]>, Box<[u8]>>,
    chara_list: BTreeMap<u32, EraCharaInitTemplate>,
    initial_vars: Option<hashbrown::HashMap<Ascii<String>, InitialVarDesc>>,
    contextual_indices: HashMap<Ascii<arcstr::ArcStr>, Vec<(EraCsvVarKind, u32)>>,
}

pub const MAX_CHARA_COUNT: u32 = 768;

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
    /// Whether to use integers with infinite precision. Incompatible with Emuera.
    pub use_inf_prec_int: bool,
    pub memory_limit: u64,
}
impl Default for MEraEngineConfig {
    fn default() -> Self {
        MEraEngineConfig {
            use_inf_prec_int: false,
            memory_limit: 0,
        }
    }
}

// NOTE: This struct does not expose bytecode info
#[derive(Debug, Clone)]
pub struct EraScriptErrorInfo<'a> {
    pub filename: &'a str,
    pub src_info: ExecSourceInfo,
    pub is_error: bool,
    pub msg: &'a str,
}

pub trait MEraEngineSysCallback {
    /// Callback for script compilation errors.
    fn on_compile_error(&mut self, info: &EraScriptErrorInfo);
    /// Callback for script execution errors.
    fn on_execute_error(&mut self, info: &EraScriptErrorInfo);
    /// Callback for RAND statements. Note that you should always return a fully
    /// filled random u64; the engine will internally cache entropy to reduce
    /// the total amount of syscalls.
    fn on_get_rand(&mut self) -> u64;
    /// Callback for PRINT family statements.
    fn on_print(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags);
    //fn on_debugprint(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags);
    /// Callback for HTML_PRINT statements.
    fn on_html_print(&mut self, content: &str);
    fn on_wait(&mut self, any_key: bool, is_force: bool);
    fn on_twait(&mut self, duration: i64, is_force: bool);
    fn on_input_int(
        &mut self,
        default_value: Option<i64>,
        can_click: bool,
        allow_skip: bool,
    ) -> Option<i64>;
    fn on_input_str(
        &mut self,
        default_value: Option<&str>,
        can_click: bool,
        allow_skip: bool,
    ) -> Option<String>;
    fn on_tinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<i64>;
    fn on_tinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<String>;
    fn on_oneinput_int(&mut self, default_value: Option<i64>) -> Option<i64>;
    fn on_oneinput_str(&mut self, default_value: Option<&str>) -> Option<String>;
    fn on_toneinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<i64>;
    fn on_toneinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<String>;
    fn on_reuselastline(&mut self, content: &str);
    fn on_clearline(&mut self, count: i64);
    fn on_print_button(
        &mut self,
        content: &str,
        value: &str,
        flags: crate::bytecode::PrintExtendedFlags,
    );
    /// Callbacks for variable getters & setters. May return a string to report as execution errors.
    fn on_var_get_int(&mut self, name: &str, idx: usize) -> Result<i64, anyhow::Error>;
    fn on_var_get_str(&mut self, name: &str, idx: usize) -> Result<String, anyhow::Error>;
    fn on_var_set_int(&mut self, name: &str, idx: usize, val: i64) -> Result<(), anyhow::Error>;
    fn on_var_set_str(&mut self, name: &str, idx: usize, val: &str) -> Result<(), anyhow::Error>;
    // Graphics subsystem
    fn on_gcreate(&mut self, gid: i64, width: i64, height: i64) -> i64;
    fn on_gcreatefromfile(&mut self, gid: i64, path: &str) -> i64;
    fn on_gdispose(&mut self, gid: i64) -> i64;
    fn on_gcreated(&mut self, gid: i64) -> i64;
    fn on_gdrawsprite(
        &mut self,
        gid: i64,
        sprite_name: &str,
        dest_x: i64,
        dest_y: i64,
        dest_width: i64,
        dest_height: i64,
        color_matrix: Option<&crate::vm::EraColorMatrix>,
    ) -> i64;
    fn on_gclear(&mut self, gid: i64, color: i64) -> i64;
    fn on_spritecreate(
        &mut self,
        name: &str,
        gid: i64,
        x: i64,
        y: i64,
        width: i64,
        height: i64,
    ) -> i64;
    fn on_spritedispose(&mut self, name: &str) -> i64;
    fn on_spritecreated(&mut self, name: &str) -> i64;
    fn on_spriteanimecreate(&mut self, name: &str, width: i64, height: i64) -> i64;
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
    ) -> i64;
    fn on_spritewidth(&mut self, name: &str) -> i64;
    fn on_spriteheight(&mut self, name: &str) -> i64;
    // Filesystem subsystem
    fn on_open_host_file(
        &mut self,
        path: &str,
        can_write: bool,
    ) -> anyhow::Result<Box<dyn crate::vm::EraVirtualMachineHostFile>>;
    fn on_check_host_file_exists(&mut self, path: &str) -> anyhow::Result<bool>;
    fn on_delete_host_file(&mut self, path: &str) -> anyhow::Result<()>;
    fn on_list_host_file(&mut self, path: &str) -> anyhow::Result<Vec<String>>;
    // Others
    fn on_check_font(&mut self, font_name: &str) -> i64;
    // NOTE: Returns UTC timestamp (in milliseconds).
    fn on_get_host_time(&mut self) -> u64;
    fn on_get_config_int(&mut self, name: &str) -> anyhow::Result<i64>;
    fn on_get_config_str(&mut self, name: &str) -> anyhow::Result<String>;
    // NOTE: Returns { b15 = <key down>, b0 = <key triggered> }. For key codes, refer
    //       to https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes.
    fn on_get_key_state(&mut self, key_code: i64) -> i64;
}

// #[derive_ReprC]
// #[repr(C)]
#[derive(Default, Debug, Clone)]
pub struct ExecSourceInfo {
    pub line: u32,
    pub column: u32,
    // offset: u64,
}
impl From<SourcePosInfo> for ExecSourceInfo {
    fn from(value: SourcePosInfo) -> Self {
        ExecSourceInfo {
            line: value.line,
            column: value.column,
        }
    }
}

#[safer_ffi::derive_ReprC]
#[repr(C)]
#[derive(Default, Debug, Clone)]
pub struct EraExecIpInfo {
    chunk: u32,
    offset: u32,
}

#[safer_ffi::derive_ReprC]
#[repr(C)]
#[derive(Default, Debug, Clone)]
pub struct EraFuncInfo {
    pub entry: EraExecIpInfo,
    pub args_cnt: u32,
}

struct ReplExprResult {
    value: crate::bytecode::Value,
}

pub struct EngineStackTraceFrame<'a> {
    pub file_name: &'a str,
    pub func_name: &'a str,
    pub ip: EraExecIpInfo,
    pub src_info: ExecSourceInfo,
}
pub struct EngineStackTrace<'a> {
    pub frames: Vec<EngineStackTraceFrame<'a>>,
}

#[safer_ffi::derive_ReprC]
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

struct EmptyCallback;
impl MEraEngineSysCallback for EmptyCallback {
    fn on_compile_error(&mut self, info: &EraScriptErrorInfo) {}
    fn on_execute_error(&mut self, info: &EraScriptErrorInfo) {}
    fn on_get_rand(&mut self) -> u64 {
        0
    }
    fn on_print(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags) {}
    //fn on_debugprint(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags) {}
    fn on_html_print(&mut self, content: &str) {}
    fn on_wait(&mut self, any_key: bool, is_force: bool) {}
    fn on_twait(&mut self, duration: i64, is_force: bool) {}
    fn on_input_int(
        &mut self,
        default_value: Option<i64>,
        can_click: bool,
        allow_skip: bool,
    ) -> Option<i64> {
        None
    }
    fn on_input_str(
        &mut self,
        default_value: Option<&str>,
        can_click: bool,
        allow_skip: bool,
    ) -> Option<String> {
        None
    }
    fn on_tinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<i64> {
        None
    }
    fn on_tinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<String> {
        None
    }
    fn on_oneinput_int(&mut self, default_value: Option<i64>) -> Option<i64> {
        None
    }
    fn on_oneinput_str(&mut self, default_value: Option<&str>) -> Option<String> {
        None
    }
    fn on_toneinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<i64> {
        None
    }
    fn on_toneinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<String> {
        None
    }
    fn on_reuselastline(&mut self, content: &str) {}
    fn on_clearline(&mut self, count: i64) {}
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
    fn on_print_button(
        &mut self,
        content: &str,
        value: &str,
        flags: crate::bytecode::PrintExtendedFlags,
    ) {
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
        color_matrix: Option<&crate::vm::EraColorMatrix>,
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
    ) -> anyhow::Result<Box<dyn crate::vm::EraVirtualMachineHostFile>> {
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
    fn on_get_host_time(&mut self) -> u64 {
        0
    }
    fn on_get_config_int(&mut self, name: &str) -> anyhow::Result<i64> {
        anyhow::bail!("no such config entry");
    }
    fn on_get_config_str(&mut self, name: &str) -> anyhow::Result<String> {
        anyhow::bail!("no such config entry");
    }
    fn on_get_key_state(&mut self, key_code: i64) -> i64 {
        0
    }
}

impl<'a> MEraEngine<'a> {
    pub fn new() -> Self {
        type T1 = hashbrown::HashMap<Ascii<String>, InitialVarDesc>;
        let mut iv: T1 = Default::default();
        trait Adhoc {
            fn add_item(
                &mut self,
                name: &str,
                dims: smallvec::SmallVec<[u32; 3]>,
                is_string: bool,
                is_const: bool,
                is_charadata: bool,
                is_global: bool,
            );
            fn add_int(&mut self, name: &str, dims: smallvec::SmallVec<[u32; 3]>) {
                self.add_item(name, dims, false, false, false, false)
            }
            fn add_str(&mut self, name: &str, dims: smallvec::SmallVec<[u32; 3]>) {
                self.add_item(name, dims, true, false, false, false)
            }
            fn add_const_int(&mut self, name: &str, dims: smallvec::SmallVec<[u32; 3]>) {
                self.add_item(name, dims, false, true, false, false)
            }
            fn add_const_str(&mut self, name: &str, dims: smallvec::SmallVec<[u32; 3]>) {
                self.add_item(name, dims, true, true, false, false)
            }
            fn add_chara_int(&mut self, name: &str, dims: smallvec::SmallVec<[u32; 3]>) {
                self.add_item(name, dims, false, false, true, false)
            }
            fn add_chara_str(&mut self, name: &str, dims: smallvec::SmallVec<[u32; 3]>) {
                self.add_item(name, dims, true, false, true, false)
            }
        }
        impl Adhoc for T1 {
            fn add_item(
                &mut self,
                name: &str,
                dims: smallvec::SmallVec<[u32; 3]>,
                is_string: bool,
                is_const: bool,
                is_charadata: bool,
                is_global: bool,
            ) {
                self.insert(
                    name.to_owned().into(),
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
        iv.add_int("DAY", smallvec::smallvec![1]);
        iv.add_int("MONEY", smallvec::smallvec![1]);
        iv.add_int("TIME", smallvec::smallvec![1]);
        iv.add_int("ITEM", smallvec::smallvec![1]);
        iv.add_int("ITEMSALES", smallvec::smallvec![1]);
        iv.add_int("NOITEM", smallvec::smallvec![1]);
        iv.add_int("BOUGHT", smallvec::smallvec![1]);
        iv.add_int("PBAND", smallvec::smallvec![1]);
        iv.add_int("FLAG", smallvec::smallvec![1]);
        iv.add_int("TFLAG", smallvec::smallvec![1]);
        iv.add_int("TARGET", smallvec::smallvec![1]);
        iv.add_int("MASTER", smallvec::smallvec![1]);
        iv.add_int("PLAYER", smallvec::smallvec![1]);
        iv.add_int("ASSI", smallvec::smallvec![1]);
        iv.add_int("ASSIPLAY", smallvec::smallvec![1]);
        iv.add_int("UP", smallvec::smallvec![1]);
        iv.add_int("DOWN", smallvec::smallvec![1]);
        iv.add_int("LOSEBASE", smallvec::smallvec![1]);
        iv.add_int("PALAMLV", smallvec::smallvec![1]);
        iv.add_int("EXPLV", smallvec::smallvec![1]);
        iv.add_int("EJAC", smallvec::smallvec![1]);
        iv.add_int("PREVCOM", smallvec::smallvec![1]);
        iv.add_int("SELECTCOM", smallvec::smallvec![1]);
        iv.add_int("NEXTCOM", smallvec::smallvec![1]);
        iv.add_int("RESULT", smallvec::smallvec![1]);
        iv.add_int("COUNT", smallvec::smallvec![1]);
        iv.add_int("A", smallvec::smallvec![1]);
        iv.add_int("B", smallvec::smallvec![1]);
        iv.add_int("C", smallvec::smallvec![1]);
        iv.add_int("D", smallvec::smallvec![1]);
        iv.add_int("E", smallvec::smallvec![1]);
        iv.add_int("F", smallvec::smallvec![1]);
        iv.add_int("G", smallvec::smallvec![1]);
        iv.add_int("H", smallvec::smallvec![1]);
        iv.add_int("I", smallvec::smallvec![1]);
        iv.add_int("J", smallvec::smallvec![1]);
        iv.add_int("K", smallvec::smallvec![1]);
        iv.add_int("L", smallvec::smallvec![1]);
        iv.add_int("M", smallvec::smallvec![1]);
        iv.add_int("N", smallvec::smallvec![1]);
        iv.add_int("O", smallvec::smallvec![1]);
        iv.add_int("P", smallvec::smallvec![1]);
        iv.add_int("Q", smallvec::smallvec![1]);
        iv.add_int("R", smallvec::smallvec![1]);
        iv.add_int("S", smallvec::smallvec![1]);
        iv.add_int("T", smallvec::smallvec![1]);
        iv.add_int("U", smallvec::smallvec![1]);
        iv.add_int("V", smallvec::smallvec![1]);
        iv.add_int("W", smallvec::smallvec![1]);
        iv.add_int("X", smallvec::smallvec![1]);
        iv.add_int("Y", smallvec::smallvec![1]);
        iv.add_int("Z", smallvec::smallvec![1]);
        iv.add_const_int("ITEMPRICE", smallvec::smallvec![1]);
        // ………………………………………………
        // 文字列配列型変数
        // ………………………………………………
        iv.add_str("SAVESTR", smallvec::smallvec![1]);
        iv.add_str("RESULTS", smallvec::smallvec![1]);
        iv.add_str("TSTR", smallvec::smallvec![1]);
        iv.add_str("STR", smallvec::smallvec![1]);
        // ITEMNAMEとITEMPRICEは片方を変更すると他方も同じ値に変更されます
        iv.add_const_str("ITEMNAME", smallvec::smallvec![1]);
        iv.add_const_str("ABLNAME", smallvec::smallvec![1]);
        iv.add_const_str("EXPNAME", smallvec::smallvec![1]);
        iv.add_const_str("TALENTNAME", smallvec::smallvec![1]);
        iv.add_const_str("PALAMNAME", smallvec::smallvec![1]);
        iv.add_const_str("TRAINNAME", smallvec::smallvec![1]);
        iv.add_const_str("MARKNAME", smallvec::smallvec![1]);
        iv.add_const_str("BASENAME", smallvec::smallvec![1]);
        iv.add_const_str("SOURCENAME", smallvec::smallvec![1]);
        iv.add_const_str("EXNAME", smallvec::smallvec![1]);
        iv.add_const_str("EQUIPNAME", smallvec::smallvec![1]);
        iv.add_const_str("TEQUIPNAME", smallvec::smallvec![1]);
        iv.add_const_str("FLAGNAME", smallvec::smallvec![1]);
        iv.add_const_str("CFLAGNAME", smallvec::smallvec![1]);
        iv.add_const_str("TFLAGNAME", smallvec::smallvec![1]);
        iv.add_const_str("TCVARNAME", smallvec::smallvec![1]);
        iv.add_const_str("CSTRNAME", smallvec::smallvec![1]);
        iv.add_const_str("STAINNAME", smallvec::smallvec![1]);
        iv.add_const_str("STRNAME", smallvec::smallvec![1]);
        iv.add_const_str("TSTRNAME", smallvec::smallvec![1]);
        iv.add_const_str("SAVESTRNAME", smallvec::smallvec![1]);
        // ………………………………………………
        // 角色変数
        // ………………………………………………
        iv.add_chara_int("BASE", smallvec::smallvec![1]);
        iv.add_chara_int("MAXBASE", smallvec::smallvec![1]);
        iv.add_chara_int("DOWNBASE", smallvec::smallvec![1]);
        iv.add_chara_int("ABL", smallvec::smallvec![1]);
        iv.add_chara_int("TALENT", smallvec::smallvec![1]);
        iv.add_chara_int("EXP", smallvec::smallvec![1]);
        iv.add_chara_int("MARK", smallvec::smallvec![1]);
        iv.add_chara_int("PALAM", smallvec::smallvec![1]);
        iv.add_chara_int("SOURCE", smallvec::smallvec![1]);
        iv.add_chara_int("EX", smallvec::smallvec![1]);
        iv.add_chara_int("NOWEX", smallvec::smallvec![1]);
        iv.add_chara_int("CFLAG", smallvec::smallvec![1]);
        iv.add_chara_int("JUEL", smallvec::smallvec![1]);
        iv.add_chara_int("GOTJUEL", smallvec::smallvec![1]);
        iv.add_chara_int("CUP", smallvec::smallvec![1]);
        iv.add_chara_int("CDOWN", smallvec::smallvec![1]);
        iv.add_chara_int("RELATION", smallvec::smallvec![1]);
        iv.add_chara_int("EQUIP", smallvec::smallvec![1]);
        iv.add_chara_int("TEQUIP", smallvec::smallvec![1]);
        iv.add_chara_int("STAIN", smallvec::smallvec![1]);
        iv.add_chara_int("TCVAR", smallvec::smallvec![1]);
        // ………………………………………………
        // 角色文字列変数
        // ………………………………………………
        iv.add_chara_str("NAME", smallvec::smallvec![1]);
        iv.add_chara_str("CALLNAME", smallvec::smallvec![1]);
        iv.add_chara_str("NICKNAME", smallvec::smallvec![1]);
        iv.add_chara_str("MASTERNAME", smallvec::smallvec![1]);
        iv.add_chara_str("CSTR", smallvec::smallvec![1]);
        // ………………………………………………
        // 特殊一時変数・一時文字列変数
        // ………………………………………………
        iv.add_int("LOCAL", smallvec::smallvec![1]);
        iv.add_str("LOCALS", smallvec::smallvec![1]);
        iv.add_int("ARG", smallvec::smallvec![1]);
        iv.add_str("ARGS", smallvec::smallvec![1]);
        iv.add_int("GLOBAL", smallvec::smallvec![1]);
        iv.add_str("GLOBALS", smallvec::smallvec![1]);
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
        iv.add_chara_int("NO", smallvec::smallvec![1]);
        iv.add_chara_int("ISASSI", smallvec::smallvec![1]);
        iv.add_chara_int("CDFLAG", smallvec::smallvec![1, 1]);
        iv.add_int("DITEMTYPE", smallvec::smallvec![1, 1]);
        iv.add_int("DA", smallvec::smallvec![1, 1]);
        iv.add_int("DB", smallvec::smallvec![1, 1]);
        iv.add_int("DC", smallvec::smallvec![1, 1]);
        iv.add_int("DD", smallvec::smallvec![1, 1]);
        iv.add_int("DE", smallvec::smallvec![1, 1]);
        iv.add_int("TA", smallvec::smallvec![1, 1, 1]);
        iv.add_int("TB", smallvec::smallvec![1, 1, 1]);

        MEraEngine {
            file_inputs: Vec::new(),
            vm: None,
            callback: Box::new(EmptyCallback),
            config: Default::default(),
            global_vars: EraVarPool::default(),
            watching_vars: HashSet::new(),
            replace_list: HashMap::new(),
            define_list: HashMap::new(),
            chara_list: BTreeMap::new(),
            initial_vars: Some(iv),
            contextual_indices: HashMap::new(),
        }
    }
    pub fn install_sys_callback<'b>(&'b mut self, callback: Box<dyn MEraEngineSysCallback + 'a>)
    where
        'a: 'b,
    {
        self.callback = callback;
    }
    pub fn get_config(&self) -> MEraEngineConfig {
        self.config.clone()
    }
    pub fn set_config(&mut self, config: MEraEngineConfig) {
        self.config = config;
    }
    pub fn load_csv(
        &mut self,
        filename: &str,
        content: &[u8],
        kind: EraCsvLoadKind,
    ) -> Result<(), MEraEngineError> {
        let mut load_2_fn = |target_name: &str, kind: Option<EraCsvVarKind>| {
            let Some(iv) = &mut self.initial_vars else {
                return Err(MEraEngineError::new("csv loaded too late".to_owned()));
            };
            let Some(var_desc) = iv.get_mut(Ascii::new_str(target_name)) else {
                panic!("csv loader: invalid target name `{target_name}`");
            };
            if var_desc.dims.len() != 1 {
                return Err(MEraEngineError::new(
                    "variable dimension mismatch".to_owned(),
                ));
            }
            let items = match crate::csv::parse_csv::<2>(content) {
                Ok(x) => x,
                Err((src_info, e)) => {
                    self.callback.on_compile_error(&EraScriptErrorInfo {
                        filename,
                        src_info: src_info.into(),
                        is_error: true,
                        msg: &e,
                    });
                    return Err(MEraEngineError::new("invalid csv input".to_owned()));
                }
            };
            let mut initial_sval = vec![
                StrValue {
                    val: arcstr::ArcStr::new()
                };
                var_desc.dims[0] as _
            ];
            for item in items {
                let [index, name] = item;
                // Add to *NAME array
                let index: u32 = match atoi_simd::parse_pos(&index) {
                    Ok(x) => x,
                    Err(e) => return Err(MEraEngineError::new(format!("{e}"))),
                };
                let name: arcstr::ArcStr = String::from_utf8_lossy(&name).into();
                initial_sval[index as usize] = StrValue { val: name.clone() };
                // Add to define list
                if let Some(kind) = kind {
                    self.contextual_indices
                        .entry(Ascii::new(name))
                        .or_default()
                        .push((kind, index));
                }
            }
            var_desc.initial_sval = Some(initial_sval);
            Ok(())
        };
        struct Ctx<'a> {
            global_vars: &'a mut EraVarPool,
        }
        impl<'a> Ctx<'a> {
            fn new(this: &'a mut MEraEngine) -> Ctx<'a> {
                Ctx {
                    global_vars: &mut this.global_vars,
                }
            }
            fn parse_i64(&mut self, value: &str) -> Result<i64, MEraEngineError> {
                match atoi_simd::parse(value.as_bytes()) {
                    Ok(x) => Ok(x),
                    Err(e) => Err(MEraEngineError::new(format!("{e}"))),
                }
            }
            fn add_var_i64(&mut self, name: &str, value: &str) -> Result<(), MEraEngineError> {
                let value = self.parse_i64(value)?;
                _ = self.global_vars.add_var_ex(
                    name,
                    &Value::new_int_0darr(value),
                    true,
                    false,
                    false,
                    true,
                );
                Ok(())
            }
            fn add_var_str(
                &mut self,
                name: &str,
                value: arcstr::ArcStr,
            ) -> Result<(), MEraEngineError> {
                _ = self.global_vars.add_var_ex(
                    name,
                    &Value::new_str_0darr(value),
                    true,
                    false,
                    false,
                    true,
                );
                Ok(())
            }
            fn add_arr_i64(&mut self, name: &str, value: &str) -> Result<(), MEraEngineError> {
                let value = value
                    .split('/')
                    .map(|x| self.parse_i64(x).map(|val| IntValue { val }))
                    .collect::<Result<Vec<_>, _>>()?;
                let value = Value::new_int_arr(smallvec::smallvec![value.len() as _], value);
                _ = self
                    .global_vars
                    .add_var_ex(name, &value, true, false, false, true);
                Ok(())
            }
        }
        match kind {
            EraCsvLoadKind::_Rename => {
                //self.replace_list.clear();
                let rows = match crate::csv::parse_csv::<2>(content) {
                    Ok(x) => x,
                    Err((src_info, msg)) => {
                        (self.callback.on_compile_error(&EraScriptErrorInfo {
                            filename,
                            src_info: src_info.into(),
                            is_error: true,
                            msg: msg.as_str(),
                        }));
                        return Err(MEraEngineError::new(msg));
                    }
                };
                for [out_replace, in_replace] in rows {
                    // TODO: Detect duplication?
                    self.replace_list.insert(in_replace, out_replace);
                }
            }
            EraCsvLoadKind::VariableSize => {
                let rows = crate::csv::parse_csv_loose(content).unwrap();
                for (irow, row) in rows {
                    let src_info: ExecSourceInfo = SourcePosInfo {
                        line: irow,
                        column: 1,
                    }
                    .into();
                    match row.len() {
                        2 => {
                            let name = Ascii::new_str(&row[0]);
                            let size: u32 = match row[1].parse() {
                                Ok(x) => x,
                                Err(e) => {
                                    self.callback.on_compile_error(&EraScriptErrorInfo {
                                        filename,
                                        src_info,
                                        is_error: true,
                                        msg: &format!("invalid variable size: {e}"),
                                    });
                                    continue;
                                }
                            };
                            let Some(iv) = &mut self.initial_vars else {
                                return Err(MEraEngineError::new("csv loaded too late".to_owned()));
                            };
                            let Some(var_desc) = iv.get_mut(name) else {
                                self.callback.on_compile_error(&EraScriptErrorInfo {
                                    filename,
                                    src_info,
                                    is_error: true,
                                    msg: &format!("unsupported variable size line"),
                                });
                                continue;
                            };
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
                            if let Some(group) = vars_groups
                                .into_iter()
                                .find(|x| x.iter().copied().map(Ascii::new_str).any(|x| x == name))
                            {
                                for i in group.iter().copied() {
                                    iv.get_mut(Ascii::new_str(i)).unwrap().dims[0] = size;
                                }
                            } else {
                                if let Some(name) = name.as_ref().strip_suffix("NAME") {
                                    let name = Ascii::new_str(name);
                                    if let Some(var_desc) = iv.get_mut(name) {
                                        var_desc.dims[0] = size;
                                    }
                                } else if let Some(var_desc) =
                                    iv.get_mut(&Ascii::new(format!("{name}NAME")))
                                {
                                    var_desc.dims[0] = size;
                                }
                            }
                        }
                        3 => {
                            self.callback.on_compile_error(&EraScriptErrorInfo {
                                filename,
                                src_info,
                                is_error: true,
                                msg: &format!("unsupported variable size line"),
                            });
                        }
                        _ => return Err(MEraEngineError::new(format!("invalid csv line {irow}"))),
                    }
                }
            }
            EraCsvLoadKind::Chara_ => {
                use EraCsvVarKind::*;

                static RE: Lazy<Regex> =
                    Lazy::new(|| Regex::new(r"^(?:|.*[/\\])Chara(\d+).*\.(?i:csv)$").unwrap());
                let Some(csv_no) = RE.captures(filename) else {
                    return Err(MEraEngineError::new(
                        "Chara csv must be numbered".to_owned(),
                    ));
                };
                let Ok(csv_no) = csv_no[1].parse() else {
                    return Err(MEraEngineError::new(
                        "Chara csv number is invalid".to_owned(),
                    ));
                };
                // Fill character template
                let rows = crate::csv::parse_csv_loose(content).unwrap();
                let mut chara_template = EraCharaInitTemplate::default();
                chara_template.csv_no = csv_no;
                for (line_n, mut cols) in rows {
                    let src_info = SourcePosInfo {
                        line: line_n,
                        column: 1,
                    };
                    if cols.len() < 2 {
                        self.callback.on_compile_error(&EraScriptErrorInfo {
                            filename,
                            src_info: src_info.into(),
                            is_error: true,
                            msg: "too few columns in CSV line",
                        });
                        continue;
                    }
                    cols[0].make_ascii_uppercase();
                    let parse_u32 = |this: &mut Self, val: &str| -> Result<u32, _> {
                        match val.parse() {
                            Ok(x) => Ok(x),
                            Err(_) => {
                                this.callback.on_compile_error(&EraScriptErrorInfo {
                                    filename,
                                    src_info: src_info.into(),
                                    is_error: true,
                                    msg: "failed to parse u32",
                                });
                                Err(MEraEngineError::new("failed to parse u32".to_owned()))
                            }
                        }
                    };
                    let parse_i64 = |this: &mut Self, mut val: &str| -> Result<i64, _> {
                        // HACK: Ignore bad characters after integers
                        if let Some(pos) = val.find(|x| !matches!(x, '0'..='9' | ' ' | '\t' | '-'))
                        {
                            let (left, right) = val.split_at(pos);
                            this.callback.on_compile_error(&EraScriptErrorInfo {
                                filename,
                                src_info: src_info.into(),
                                is_error: true,
                                msg: &format!(
                                    "`{}` is not `;`; did you misspell?",
                                    right.chars().next().unwrap()
                                ),
                            });
                            val = left.trim();
                        }
                        match val.parse() {
                            Ok(x) => Ok(x),
                            Err(_) => {
                                this.callback.on_compile_error(&EraScriptErrorInfo {
                                    filename,
                                    src_info: src_info.into(),
                                    is_error: true,
                                    msg: "failed to parse i64",
                                });
                                Err(MEraEngineError::new("failed to parse i64".to_owned()))
                            }
                        }
                    };
                    let get_contextual_idx =
                        |this: &mut Self, csv_kind, val: &str| -> Result<u32, _> {
                            let Some(idx) = val.parse().ok().or_else(|| {
                                // Not an integer, parse as CSV index name instead
                                this.contextual_indices
                                    .get(Ascii::new_str(val))
                                    .and_then(|x| x.iter().find(|x| x.0 == csv_kind))
                                    .map(|x| x.1)
                            }) else {
                                let msg = format!("invalid CSV variable index `{val}`");
                                this.callback.on_compile_error(&EraScriptErrorInfo {
                                    filename,
                                    src_info: src_info.into(),
                                    is_error: true,
                                    msg: &msg,
                                });
                                return Err(MEraEngineError::new(msg));
                            };
                            Ok(idx)
                        };
                    let get_col_2_str = |this: &mut Self,
                                         cols: &mut [String]|
                     -> Result<String, MEraEngineError> {
                        match cols.get_mut(2) {
                            Some(x) if !x.is_empty() => Ok(std::mem::take(x)),
                            _ => {
                                this.callback.on_compile_error(&EraScriptErrorInfo {
                                    filename,
                                    src_info: src_info.into(),
                                    is_error: true,
                                    msg: "the 3rd column is missing; assuming an empty string was given",
                                });
                                Ok(String::new())
                            }
                        }
                    };
                    let get_col_i64 = |this: &mut Self,
                                       index: usize,
                                       cols: &[String]|
                     -> Result<i64, MEraEngineError> {
                        match cols.get(index) {
                            Some(x) if !x.is_empty() => Ok(parse_i64(this, x)?),
                            _ => {
                                this.callback.on_compile_error(&EraScriptErrorInfo {
                                    filename,
                                    src_info: src_info.into(),
                                    is_error: true,
                                    msg: "the 3rd column is missing; assuming 1 was given",
                                });
                                Ok(1)
                            }
                        }
                    };
                    let get_col_2_i64 =
                        |this: &mut Self, cols: &[String]| -> Result<i64, MEraEngineError> {
                            get_col_i64(this, 2, cols)
                        };
                    match cols[0].as_str() {
                        "NO" | "番号" => {
                            chara_template.no = parse_u32(self, &cols[1])?;
                        }
                        "NAME" | "名前" => {
                            chara_template.name = (&cols[1]).into();
                        }
                        "CALLNAME" | "呼び名" => {
                            chara_template.callname = (&cols[1]).into();
                        }
                        "NICKNAME" | "あだ名" => {
                            chara_template.nickname = (&cols[1]).into();
                        }
                        "MASTERNAME" | "主人の呼び方" => {
                            chara_template.mastername = (&cols[1]).into();
                        }
                        "MARK" | "刻印" => {
                            let idx = get_contextual_idx(self, CsvMark, &cols[1])?;
                            let val = get_col_2_i64(self, &mut cols)?;
                            chara_template.mark.insert(idx, val);
                        }
                        "EXP" | "経験" => {
                            let idx = get_contextual_idx(self, CsvExp, &cols[1])?;
                            let val = get_col_2_i64(self, &mut cols)?;
                            chara_template.exp.insert(idx, val);
                        }
                        "ABL" | "能力" => {
                            let idx = get_contextual_idx(self, CsvAbl, &cols[1])?;
                            let val = get_col_2_i64(self, &mut cols)?;
                            chara_template.abl.insert(idx, val);
                        }
                        "BASE" | "基礎" => {
                            let idx = get_contextual_idx(self, CsvBase, &cols[1])?;
                            let val = get_col_2_i64(self, &mut cols)?;
                            chara_template.maxbase.insert(idx, val);
                        }
                        "TALENT" | "素質" => {
                            let idx = get_contextual_idx(self, CsvTalent, &cols[1])?;
                            let val = get_col_2_i64(self, &mut cols)?;
                            chara_template.talent.insert(idx, val);
                        }
                        "RELATION" | "相性" => {
                            let idx = get_col_i64(self, 1, &cols)?;
                            let val = get_col_2_i64(self, &mut cols)?;
                            chara_template.relation.insert(idx as _, val);
                        }
                        "CFLAG" | "フラグ" => {
                            let idx = get_contextual_idx(self, CsvCFlag, &cols[1])?;
                            let val = get_col_2_i64(self, &mut cols)?;
                            chara_template.cflag.insert(idx, val);
                        }
                        "EQUIP" | "装着物" => {
                            let idx = get_contextual_idx(self, CsvEquip, &cols[1])?;
                            let val = get_col_2_i64(self, &mut cols)?;
                            chara_template.equip.insert(idx, val);
                        }
                        "JUEL" | "珠" => {
                            let idx = get_contextual_idx(self, CsvPalam, &cols[1])?;
                            let val = get_col_2_i64(self, &mut cols)?;
                            chara_template.juel.insert(idx, val);
                        }
                        "CSTR" => {
                            let idx = get_contextual_idx(self, CsvCStr, &cols[1])?;
                            let val = get_col_2_str(self, &mut cols)?;
                            chara_template.cstr.insert(idx, val.into());
                        }
                        "ISASSI" | "助手" => continue,
                        _ => todo!(),
                    }
                }
                // Add to character template list
                let key = chara_template.no;
                match self.chara_list.entry(key) {
                    std::collections::btree_map::Entry::Occupied(_) => {
                        return Err(MEraEngineError::new(format!(
                            "Character CSV id {key} already exists"
                        )));
                    }
                    std::collections::btree_map::Entry::Vacant(e) => {
                        e.insert(chara_template);
                    }
                }
            }
            EraCsvLoadKind::Abl => load_2_fn("ABLNAME", Some(EraCsvVarKind::CsvAbl))?,
            EraCsvLoadKind::Exp => load_2_fn("EXPNAME", Some(EraCsvVarKind::CsvExp))?,
            EraCsvLoadKind::Talent => load_2_fn("TALENTNAME", Some(EraCsvVarKind::CsvTalent))?,
            EraCsvLoadKind::Palam => load_2_fn("PALAMNAME", Some(EraCsvVarKind::CsvPalam))?,
            EraCsvLoadKind::Train => load_2_fn("TRAINNAME", Some(EraCsvVarKind::CsvTrain))?,
            EraCsvLoadKind::Mark => load_2_fn("MARKNAME", Some(EraCsvVarKind::CsvMark))?,
            EraCsvLoadKind::Item => {
                let target_name = "ITEMNAME";
                let target2_name = "ITEMPRICE";
                let Some(iv) = &mut self.initial_vars else {
                    return Err(MEraEngineError::new("csv loaded too late".to_owned()));
                };
                let [name_vd, price_vd] = iv
                    .get_many_mut([target_name, target2_name].map(Ascii::new_str))
                    .unwrap();
                let items = match crate::csv::parse_csv_loose(content) {
                    Ok(x) => x,
                    Err(_) => unreachable!(),
                };
                let mut initial_nameval = vec![
                    StrValue {
                        val: arcstr::ArcStr::new()
                    };
                    name_vd.dims[0] as _
                ];
                let mut initial_priceval = vec![IntValue { val: 0 }; price_vd.dims[0] as _];
                for (line_num, mut item) in items {
                    let src_info = SourcePosInfo {
                        line: line_num,
                        column: 1,
                    };
                    item.truncate(3);
                    let [index, name, price] = match TryInto::<[_; 3]>::try_into(item) {
                        Ok(x) => x,
                        Err(e) => match TryInto::<[_; 2]>::try_into(e) {
                            Ok([index, name]) => [index, name, "0".to_owned()],
                            Err(_) => {
                                self.callback.on_compile_error(&EraScriptErrorInfo {
                                    filename,
                                    src_info: src_info.into(),
                                    is_error: true,
                                    msg: "too few columns in CSV line",
                                });
                                return Err(MEraEngineError::new("invalid csv input".to_owned()));
                            }
                        },
                    };
                    // Add to ITEMNAME and ITEMPRICE
                    let index: u32 = match atoi_simd::parse_pos(index.as_bytes()) {
                        Ok(x) => x,
                        Err(e) => return Err(MEraEngineError::new(format!("{e}"))),
                    };
                    let name: arcstr::ArcStr = name.into();
                    let price: u32 = match atoi_simd::parse_pos(price.as_bytes()) {
                        Ok(x) => x,
                        Err(e) => return Err(MEraEngineError::new(format!("{e}"))),
                    };
                    initial_nameval[index as usize] = StrValue { val: name.clone() };
                    initial_priceval[index as usize] = IntValue { val: price as _ };
                    // Add to define list
                    self.contextual_indices
                        .entry(Ascii::new(name))
                        .or_default()
                        .push((EraCsvVarKind::CsvItem, index));
                }
                name_vd.initial_sval = Some(initial_nameval);
                price_vd.initial_ival = Some(initial_priceval);
            }
            EraCsvLoadKind::Base => load_2_fn("BASENAME", Some(EraCsvVarKind::CsvBase))?,
            EraCsvLoadKind::Source => load_2_fn("SOURCENAME", Some(EraCsvVarKind::CsvSource))?,
            EraCsvLoadKind::Ex => load_2_fn("EXNAME", Some(EraCsvVarKind::CsvEx))?,
            EraCsvLoadKind::Str => load_2_fn("STR", Some(EraCsvVarKind::CsvStr))?,
            EraCsvLoadKind::Equip => load_2_fn("EQUIPNAME", Some(EraCsvVarKind::CsvEquip))?,
            EraCsvLoadKind::TEquip => load_2_fn("TEQUIPNAME", Some(EraCsvVarKind::CsvTEquip))?,
            EraCsvLoadKind::Flag => load_2_fn("FLAGNAME", Some(EraCsvVarKind::CsvFlag))?,
            EraCsvLoadKind::TFlag => load_2_fn("TFLAGNAME", Some(EraCsvVarKind::CsvTFlag))?,
            EraCsvLoadKind::CFlag => load_2_fn("CFLAGNAME", Some(EraCsvVarKind::CsvCFlag))?,
            EraCsvLoadKind::TCVar => load_2_fn("TCVARNAME", Some(EraCsvVarKind::CsvTCVar))?,
            EraCsvLoadKind::CStr => load_2_fn("CSTRNAME", Some(EraCsvVarKind::CsvCStr))?,
            EraCsvLoadKind::Stain => load_2_fn("STAINNAME", Some(EraCsvVarKind::CsvStain))?,
            // EraCsvLoadKind::CDFlag1 => load_2_fn("CDFLAGNAME1", false)?,
            // EraCsvLoadKind::CDFlag2 => load_2_fn("CDFLAGNAME2", false)?,
            EraCsvLoadKind::StrName => load_2_fn("STRNAME", Some(EraCsvVarKind::CsvStr))?,
            EraCsvLoadKind::TStr => load_2_fn("TSTRNAME", Some(EraCsvVarKind::CsvTStr))?,
            EraCsvLoadKind::SaveStr => load_2_fn("SAVESTRNAME", Some(EraCsvVarKind::CsvSaveStr))?,
            EraCsvLoadKind::Global => load_2_fn("GLOBALNAME", Some(EraCsvVarKind::CsvGlobal))?,
            EraCsvLoadKind::Globals => load_2_fn("GLOBALSNAME", Some(EraCsvVarKind::CsvGlobals))?,
            EraCsvLoadKind::ImageResources => {
                let dir_prefix = filename
                    .rsplit_once(&['\\', '/'])
                    .map(|x| x.0)
                    .unwrap_or("");
                // TODO: Use self.callback to pre-allocate graphics & sprites
                todo!()
            }
            EraCsvLoadKind::GameBase => {
                let rows = match crate::csv::parse_csv::<2>(content) {
                    Ok(x) => x,
                    Err((src_info, msg)) => {
                        (self.callback.on_compile_error(&EraScriptErrorInfo {
                            filename,
                            src_info: src_info.into(),
                            is_error: true,
                            msg: msg.as_str(),
                        }));
                        return Err(MEraEngineError::new(msg));
                    }
                };

                let mut ctx = Ctx::new(self);

                for [index, value] in rows {
                    let index = String::from_utf8_lossy(&index);
                    let value = String::from_utf8_lossy(&value);
                    match index.as_ref() {
                        "コード" => {
                            ctx.add_var_i64("GAMEBASE_GAMECODE", &value)?;
                        }
                        "バージョン" => {
                            let _ = ctx.add_var_i64("GAMEBASE_VERSION", &value);
                        }
                        "バージョン違い認める" => {
                            ctx.add_var_i64("GAMEBASE_ALLOWVERSION", &value)?;
                        }
                        "最初からいるキャラ" => {
                            ctx.add_var_i64("GAMEBASE_DEFAULTCHARA", &value)?;
                        }
                        "アイテムなし" => {
                            ctx.add_var_i64("GAMEBASE_NOITEM", &value)?;
                        }
                        "タイトル" => {
                            ctx.add_var_str("GAMEBASE_TITLE", value.into())?;
                        }
                        "作者" => {
                            let value: arcstr::ArcStr = value.into();
                            ctx.add_var_str("GAMEBASE_AUTHER", value.clone())?;
                            ctx.add_var_str("GAMEBASE_AUTHOR", value)?;
                        }
                        "製作年" => {
                            ctx.add_var_str("GAMEBASE_YEAR", value.into())?;
                        }
                        "追加情報" => {
                            ctx.add_var_str("GAMEBASE_INFO", value.into())?;
                        }
                        "ウィンドウタイトル" => {
                            // TODO...
                            return Err(MEraEngineError::new(
                                "`ウィンドウタイトル` is unsupported".to_owned(),
                            ));
                        }
                        "動作に必要なEmueraのバージョン" => {
                            // TODO...
                            return Err(MEraEngineError::new(
                                "`動作に必要なEmueraのバージョン` is unsupported".to_owned(),
                            ));
                        }
                        _ => {
                            // TODO: Handle unknown GAMEBASE.CSV line
                        }
                    }
                }

                ctx.add_var_i64("GAMEBASE_VERSION", "0").unwrap();
            }
            EraCsvLoadKind::_Replace => {
                let rows = match crate::csv::parse_csv::<2>(content) {
                    Ok(x) => x,
                    Err((src_info, msg)) => {
                        (self.callback.on_compile_error(&EraScriptErrorInfo {
                            filename,
                            src_info: src_info.into(),
                            is_error: true,
                            msg: msg.as_str(),
                        }));
                        return Err(MEraEngineError::new(msg));
                    }
                };

                let mut ctx = Ctx::new(self);

                for [index, value] in rows {
                    let index = String::from_utf8_lossy(&index);
                    let value = String::from_utf8_lossy(&value);
                    match index.as_ref() {
                        "汚れの初期値" => ctx.add_arr_i64("DEFAULT_STAIN", &value)?,
                        _ => {
                            // TODO: Handle unknown _Replace.csv line
                        }
                    }
                }

                ctx.add_arr_i64("DEFAULT_STAIN", "0/0/2/1/8").unwrap();
            }
            _ => {
                return Err(MEraEngineError::new(
                    "csv loading not yet implemented".to_owned(),
                ));
            }
        }
        Ok(())
    }
    pub fn load_erh(&mut self, filename: &str, content: &[u8]) -> Result<(), MEraEngineError> {
        self.load_erb(filename, content)
    }
    pub fn load_erb(&mut self, filename: &str, content: &[u8]) -> Result<(), MEraEngineError> {
        use crate::parser::{EraDecl, EraSharpDecl};
        // TODO: Handle UTF-8 BOM?

        if let Some(initial_vars) = self.initial_vars.take() {
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
                    .global_vars
                    .add_var_ex(
                        &name,
                        &val,
                        var_desc.is_const,
                        var_desc.is_charadata,
                        var_desc.is_global,
                        true,
                    )
                    .is_none()
                {
                    panic!("variable `{name}` was redefined");
                }
            }
        }

        let mut temp_storage = EraLexerTempStorage::default();
        let callback = RefCell::new(self.callback.deref_mut());
        let mut lexer = crate::lexer::EraLexer::new(
            content,
            &self.replace_list,
            &mut self.define_list,
            &mut temp_storage,
            |e| {
                callback.borrow_mut().on_compile_error(&EraScriptErrorInfo {
                    filename,
                    src_info: e.src_info.into(),
                    is_error: e.is_error,
                    msg: &e.msg,
                });
            },
        );
        let mut parser = crate::parser::EraParser::new(|e| {
            callback.borrow_mut().on_compile_error(&EraScriptErrorInfo {
                filename,
                src_info: e.src_info.into(),
                is_error: e.is_error,
                msg: &e.msg,
            });
        });
        let root_ast = match parser.parse(&mut lexer, &mut self.global_vars) {
            Some(x) => x,
            None => {
                return Err(MEraEngineError::new(
                    "could not produce root AST node".to_owned(),
                ));
            }
        };
        for decl in &root_ast.decls {
            match decl {
                EraDecl::SharpDecl(EraSharpDecl::VarDecl(x)) => {
                    // self.global_vars.insert(
                    //     CaselessString::new(x.name.clone()),
                    //     EraParserSlimVarTypeInfo {
                    //         is_string: x.is_string,
                    //     },
                    // );
                }
                _ => (),
            }
        }
        self.file_inputs.push(EraCompilerFileInput {
            file_name: filename.to_owned(),
            root_node: root_ast,
        });
        Ok(())
    }
    /// Registers a global variable for interop between native and script. Takes over the ownership
    /// of variables by passing get / set operations to callbacks. Note that only simple variables
    /// are supported.
    ///
    /// The following variables should be taken care of as they are relied upon by the engine
    /// (non-exhaustive list):
    /// * @COLOR (0xaarrggbb)
    /// * @BGCOLOR
    /// * @FONT
    /// * WINDOW_TITLE
    pub fn register_global_var(
        &mut self,
        name: &str,
        is_string: bool,
        dimension: usize,
        watch: bool,
    ) -> Result<(), MEraEngineError> {
        let val = if is_string {
            crate::bytecode::Value::new_str_arr(smallvec::smallvec![dimension as _], Vec::new())
        } else {
            crate::bytecode::Value::new_int_arr(smallvec::smallvec![dimension as _], Vec::new())
        };
        let var_idx = self
            .global_vars
            .add_var(name, &val)
            .ok_or_else(|| MEraEngineError::new("variable already used".to_owned()))?;
        if watch {
            self.watching_vars
                .insert(self.global_vars.get_var_info(var_idx).unwrap().name.clone());
        }
        Ok(())
    }
    /// Mark the completion of source code loading. All previously loaded code will be assembled
    /// and compiled into bytecode.
    pub fn finialize_load_srcs(&mut self) -> Result<(), MEraEngineError> {
        use crate::vm::EraExecIp;

        // Add built-in functions invoking foundation intrinsics, which are
        // used by LOADGAME, BEGIN TRAIN, etc.
        self.load_builtin_srcs()?;

        let callback = RefCell::new(self.callback.deref_mut());
        let mut compiler = crate::compiler::EraCompiler::new(|e| {
            callback.borrow_mut().on_compile_error(&EraScriptErrorInfo {
                filename: &e.file_name,
                src_info: e.src_info.into(),
                is_error: e.is_error,
                msg: &e.msg,
            });
        });
        // let registered_vars: Vec<_> = self
        //     .registered_vars
        //     .iter()
        //     .map(|x| x.name.as_str().to_owned())
        //     .collect();
        let compilation = match compiler.compile_all(
            std::mem::take(&mut self.file_inputs),
            std::mem::take(&mut self.global_vars),
            &self.contextual_indices,
        ) {
            Some(x) => x,
            None => {
                return Err(MEraEngineError::new(
                    "could not compile AST nodes".to_owned(),
                ));
            }
        };
        // Set start of execution
        let entry_info = match compilation
            .func_names
            .get(Ascii::new_str("SYSPROC_BEGIN_TITLE"))
        {
            Some(x) => *x,
            None => {
                return Err(MEraEngineError::new("entry function not found".to_owned()));
            }
        };
        let entry_info = &compilation.funcs[entry_info];
        assert_eq!(entry_info.args.len(), 0);
        let entry_ip = EraExecIp {
            chunk: entry_info.chunk_idx as _,
            offset: entry_info.offset as _,
        };
        let mut vm =
            crate::vm::EraVirtualMachine::new(compilation, std::mem::take(&mut self.chara_list));
        vm.reset_exec_and_ip(entry_ip);
        for var in std::mem::take(&mut self.watching_vars) {
            vm.register_var_callback(var.as_ref()).ok_or_else(|| {
                MEraEngineError::new("cannot register variable callback".to_owned())
            })?;
        }
        self.vm = Some(vm);
        Ok(())
    }
    pub fn do_execution(
        &mut self,
        stop_flag: &AtomicBool,
        max_inst_cnt: u64,
    ) -> Result<bool, MEraEngineError> {
        let vm = match self.vm.as_mut() {
            Some(x) => x,
            None => {
                return Err(MEraEngineError::new(
                    "virtual machine not created yet".to_owned(),
                ));
            }
        };

        struct AdhocCallback<'a> {
            callback: &'a mut dyn MEraEngineSysCallback,
            contextual_indices: &'a mut HashMap<Ascii<arcstr::ArcStr>, Vec<(EraCsvVarKind, u32)>>,
        }
        impl crate::vm::EraVirtualMachineCallback for AdhocCallback<'_> {
            fn on_execution_error(&mut self, error: crate::vm::EraRuntimeErrorInfo) {
                self.callback.on_execute_error(&EraScriptErrorInfo {
                    filename: &error.file_name,
                    src_info: error.src_info.into(),
                    is_error: error.is_error,
                    msg: &error.msg,
                })
            }
            fn on_get_rand(&mut self) -> u64 {
                self.callback.on_get_rand()
            }
            fn on_print(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags) {
                self.callback.on_print(content, flags)
            }
            // TODO: on_debugprint is a callback?
            fn on_debugprint(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags) {
                todo!()
            }
            fn on_html_print(&mut self, content: &str) {
                self.callback.on_html_print(content)
            }
            fn on_wait(&mut self, any_key: bool, is_force: bool) {
                self.callback.on_wait(any_key, is_force)
            }
            fn on_twait(&mut self, duration: i64, is_force: bool) {
                self.callback.on_twait(duration, is_force)
            }
            fn on_input_int(
                &mut self,
                default_value: Option<i64>,
                can_click: bool,
                allow_skip: bool,
            ) -> Option<i64> {
                self.callback
                    .on_input_int(default_value, can_click, allow_skip)
            }
            fn on_input_str(
                &mut self,
                default_value: Option<&str>,
                can_click: bool,
                allow_skip: bool,
            ) -> Option<String> {
                self.callback
                    .on_input_str(default_value, can_click, allow_skip)
            }
            fn on_tinput_int(
                &mut self,
                time_limit: i64,
                default_value: i64,
                show_prompt: bool,
                expiry_msg: &str,
                can_click: bool,
            ) -> Option<i64> {
                self.callback.on_tinput_int(
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
            ) -> Option<String> {
                self.callback.on_tinput_str(
                    time_limit,
                    default_value,
                    show_prompt,
                    expiry_msg,
                    can_click,
                )
            }
            fn on_oneinput_int(&mut self, default_value: Option<i64>) -> Option<i64> {
                self.callback.on_oneinput_int(default_value)
            }
            fn on_oneinput_str(&mut self, default_value: Option<&str>) -> Option<String> {
                self.callback.on_oneinput_str(default_value)
            }
            fn on_toneinput_int(
                &mut self,
                time_limit: i64,
                default_value: i64,
                show_prompt: bool,
                expiry_msg: &str,
                can_click: bool,
            ) -> Option<i64> {
                self.callback.on_toneinput_int(
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
            ) -> Option<String> {
                self.callback.on_toneinput_str(
                    time_limit,
                    default_value,
                    show_prompt,
                    expiry_msg,
                    can_click,
                )
            }
            fn on_reuselastline(&mut self, content: &str) {
                self.callback.on_reuselastline(content)
            }
            fn on_clearline(&mut self, count: i64) {
                self.callback.on_clearline(count)
            }
            fn on_var_get_int(&mut self, name: &str, idx: usize) -> Result<i64, anyhow::Error> {
                self.callback.on_var_get_int(name, idx)
            }
            fn on_var_get_str(&mut self, name: &str, idx: usize) -> Result<String, anyhow::Error> {
                self.callback.on_var_get_str(name, idx)
            }
            fn on_var_set_int(
                &mut self,
                name: &str,
                idx: usize,
                val: i64,
            ) -> Result<(), anyhow::Error> {
                self.callback.on_var_set_int(name, idx, val)
            }
            fn on_var_set_str(
                &mut self,
                name: &str,
                idx: usize,
                val: &str,
            ) -> Result<(), anyhow::Error> {
                self.callback.on_var_set_str(name, idx, val)
            }
            fn on_print_button(
                &mut self,
                content: &str,
                value: &str,
                flags: crate::bytecode::PrintExtendedFlags,
            ) {
                self.callback.on_print_button(content, value, flags)
            }
            // Graphics
            fn on_gcreate(&mut self, gid: i64, width: i64, height: i64) -> i64 {
                self.callback.on_gcreate(gid, width, height)
            }
            fn on_gcreatefromfile(&mut self, gid: i64, path: &str) -> i64 {
                self.callback.on_gcreatefromfile(gid, path)
            }
            fn on_gdispose(&mut self, gid: i64) -> i64 {
                self.callback.on_gdispose(gid)
            }
            fn on_gcreated(&mut self, gid: i64) -> i64 {
                self.callback.on_gcreated(gid)
            }
            fn on_gdrawsprite(
                &mut self,
                gid: i64,
                sprite_name: &str,
                dest_x: i64,
                dest_y: i64,
                dest_width: i64,
                dest_height: i64,
                color_matrix: Option<&crate::vm::EraColorMatrix>,
            ) -> i64 {
                self.callback.on_gdrawsprite(
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
                self.callback.on_gclear(gid, color)
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
                self.callback
                    .on_spritecreate(name, gid, x, y, width, height)
            }
            fn on_spritedispose(&mut self, name: &str) -> i64 {
                self.callback.on_spritedispose(name)
            }
            fn on_spritecreated(&mut self, name: &str) -> i64 {
                self.callback.on_spritecreated(name)
            }
            fn on_spriteanimecreate(&mut self, name: &str, width: i64, height: i64) -> i64 {
                self.callback.on_spriteanimecreate(name, width, height)
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
                self.callback.on_spriteanimeaddframe(
                    name, gid, x, y, width, height, offset_x, offset_y, delay,
                )
            }
            fn on_spritewidth(&mut self, name: &str) -> i64 {
                self.callback.on_spritewidth(name)
            }
            fn on_spriteheight(&mut self, name: &str) -> i64 {
                self.callback.on_spriteheight(name)
            }
            // Filesystem subsystem
            fn on_open_host_file(
                &mut self,
                path: &str,
                can_write: bool,
            ) -> anyhow::Result<Box<dyn crate::vm::EraVirtualMachineHostFile>> {
                self.callback.on_open_host_file(path, can_write)
            }
            fn on_check_host_file_exists(&mut self, path: &str) -> anyhow::Result<bool> {
                self.callback.on_check_host_file_exists(path)
            }
            fn on_delete_host_file(&mut self, path: &str) -> anyhow::Result<()> {
                self.callback.on_delete_host_file(path)
            }
            fn on_list_host_file(&mut self, path: &str) -> anyhow::Result<Vec<String>> {
                self.callback.on_list_host_file(path)
            }
            // Others
            fn on_check_font(&mut self, font_name: &str) -> i64 {
                self.callback.on_check_font(font_name)
            }
            fn on_get_host_time(&mut self) -> u64 {
                self.callback.on_get_host_time()
            }
            fn on_get_config_int(&mut self, name: &str) -> anyhow::Result<i64> {
                self.callback.on_get_config_int(name)
            }
            fn on_get_config_str(&mut self, name: &str) -> anyhow::Result<String> {
                self.callback.on_get_config_str(name)
            }
            fn on_get_key_state(&mut self, key_code: i64) -> i64 {
                self.callback.on_get_key_state(key_code)
            }
            // Private
            fn on_csv_get_num(&mut self, kind: EraCsvVarKind, name: &str) -> Option<u32> {
                self.contextual_indices
                    .get(Ascii::new_str(name))
                    .and_then(|x| x.iter().find(|x| x.0 == kind))
                    .map(|x| x.1)
            }
        }

        let can_progress = vm.execute(
            stop_flag,
            max_inst_cnt,
            &mut AdhocCallback {
                callback: self.callback.deref_mut(),
                contextual_indices: &mut self.contextual_indices,
            },
        );
        Ok(can_progress)
    }
    pub fn get_is_halted(&self) -> bool {
        self.vm.as_ref().map(|x| x.get_is_halted()).unwrap_or(true)
    }
    // NOTE: This is a convenience helper that creates a temporary code chunk and
    //       points ip to it, then returns evaluated result. The engine will ensure
    //       integrity of the ip no matter whether the execution failed.
    pub fn repl_exec_expr(&mut self, expr: &[u8]) -> Result<ReplExprResult, MEraEngineError> {
        // TODO...
        unimplemented!()
    }
    // pub fn get_exec_ip(&self) -> (ExecIpInfo, ExecSourceInfo) {
    //     // TODO...
    // }
    // pub fn set_exec_ip(&mut self, ip: &ExecIpInfo) -> Result<(), MEraEngineError> {
    //     // TODO...
    // }
    /// Set execution to start from the specified function. All previous execution
    /// stacks are discarded.
    pub fn reset_exec_to_ip(&mut self, ip: EraExecIpInfo) -> Result<(), MEraEngineError> {
        use crate::vm::EraExecIp;

        let vm = match self.vm.as_mut() {
            Some(x) => x,
            None => {
                return Err("virtual machine not created yet".to_owned().into());
            }
        };

        let ip = EraExecIp {
            chunk: ip.chunk as _,
            offset: ip.offset as _,
        };
        vm.reset_exec_and_ip(ip);
        Ok(())
    }
    pub fn get_func_info(&self, name: &str) -> Result<EraFuncInfo, MEraEngineError> {
        let vm = match self.vm.as_ref() {
            Some(x) => x,
            None => {
                return Err("virtual machine not created yet".to_owned().into());
            }
        };
        let Some(func_info) = vm.get_func_info(name) else {
            return Err("function not found".to_owned().into());
        };
        Ok(EraFuncInfo {
            entry: EraExecIpInfo {
                chunk: func_info.chunk_idx,
                offset: func_info.offset,
            },
            args_cnt: func_info.args.len() as _,
        })
    }
    pub fn get_stack_trace(&self) -> Result<EngineStackTrace, MEraEngineError> {
        let vm = match self.vm.as_ref() {
            Some(x) => x,
            None => {
                return Err(MEraEngineError::new(
                    "virtual machine not created yet".to_owned(),
                ));
            }
        };

        let frames = vm
            .get_stack_trace()
            .frames
            .into_iter()
            .map(|x| {
                let func_info = vm.func_info_from_ip(x.ip).unwrap();
                let chunk = vm.get_chunk(x.ip.chunk).unwrap();
                EngineStackTraceFrame {
                    file_name: &chunk.name,
                    func_name: func_info.name.as_ref(),
                    ip: EraExecIpInfo {
                        chunk: x.ip.chunk as _,
                        offset: x.ip.offset as _,
                    },
                    src_info: chunk.source_info_at(x.ip.offset).unwrap().into(),
                }
            })
            .collect();
        Ok(EngineStackTrace { frames })
    }
    pub fn get_file_source(&self, todo: ()) -> Result<(), MEraEngineError> {
        // NOTE: We don't return user-provided sources, only those which
        //       are built into engine or generated on the fly

        // TODO...
        unimplemented!()
    }
    pub fn get_version() -> &'static str {
        "MEraEngine in MEraEmuCore v0.1.0"
    }
    fn load_builtin_srcs(&mut self) -> Result<(), MEraEngineError> {
        // TODO: Expose builtin source code to user for debugging

        const SYS_SRC: &str = include_str!("sys_src.ERB");

        self.load_erb("<builtin>", SYS_SRC.as_bytes())?;

        Ok(())
    }
}
