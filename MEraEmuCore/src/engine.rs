use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap, HashSet},
    ops::DerefMut,
    rc::Rc,
    sync::atomic::AtomicBool,
};

use once_cell::sync::Lazy;
use regex::Regex;

use crate::{
    bytecode::{IntValue, SourcePosInfo, StrValue, Value},
    compiler::{EraBytecodeCompilation, EraCompilerFileInput},
    lexer::EraLexerTempStorage,
    parser::EraParserSlimVarTypeInfo,
    vm::{EraVarPool, EraVirtualMachine},
};

use crate::util::*;

struct InitialVarDesc {
    pub is_string: bool,
    pub dims: Vec<u32>,
    pub is_const: bool,
    pub is_charadata: bool,
    pub initial_sval: Option<Vec<Rc<StrValue>>>,
    pub initial_ival: Option<Vec<IntValue>>,
}

pub struct MEraEngine<'a> {
    file_inputs: Vec<EraCompilerFileInput>,
    vm: Option<EraVirtualMachine>,
    callback: Box<dyn MEraEngineSysCallback + 'a>,
    config: MEraEngineConfig,
    global_vars: EraVarPool,
    watching_vars: HashSet<Rc<CaselessStr>>,
    replace_list: HashMap<Box<[u8]>, Box<[u8]>>,
    define_list: HashMap<Box<[u8]>, Box<[u8]>>,
    chara_list: BTreeMap<usize, Vec<(Box<[u8]>, Box<[u8]>, Box<[u8]>)>>,
    initial_vars: Option<hashbrown::HashMap<Ascii<String>, InitialVarDesc>>,
    contextual_indices: HashMap<Ascii<String>, u32>,
}

pub const MAX_CHARA_COUNT: u32 = 1024;

#[derive(thiserror::Error, Debug)]
#[error("{msg}")]
pub struct MEraEngineError {
    msg: String,
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
    fn on_read_file(&mut self, path: &str) -> anyhow::Result<Vec<u8>>;
    fn on_write_file(&mut self, path: &str, data: Vec<u8>) -> anyhow::Result<()>;
    fn on_list_file(&mut self, path: &str) -> anyhow::Result<Vec<String>>;
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

pub struct ExecIpInfo {
    filename: String,
    offset: u64,
}

struct EraFuncInfo {
    entry: ExecIpInfo,
}

struct ReplExprResult {
    value: crate::bytecode::Value,
}

#[repr(u8)]
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
    fn on_read_file(&mut self, path: &str) -> anyhow::Result<Vec<u8>> {
        anyhow::bail!("no such file");
    }
    fn on_write_file(&mut self, path: &str, data: Vec<u8>) -> anyhow::Result<()> {
        anyhow::bail!("no such file");
    }
    fn on_list_file(&mut self, path: &str) -> anyhow::Result<Vec<String>> {
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
                dims: Vec<u32>,
                is_string: bool,
                is_const: bool,
                is_charadata: bool,
            );
            fn add_int(&mut self, name: &str, dims: Vec<u32>) {
                self.add_item(name, dims, false, false, false)
            }
            fn add_str(&mut self, name: &str, dims: Vec<u32>) {
                self.add_item(name, dims, true, false, false)
            }
            fn add_const_int(&mut self, name: &str, dims: Vec<u32>) {
                self.add_item(name, dims, false, true, false)
            }
            fn add_const_str(&mut self, name: &str, dims: Vec<u32>) {
                self.add_item(name, dims, true, true, false)
            }
            fn add_chara_int(&mut self, name: &str, dims: Vec<u32>) {
                self.add_item(name, dims, false, false, true)
            }
            fn add_chara_str(&mut self, name: &str, dims: Vec<u32>) {
                self.add_item(name, dims, true, false, true)
            }
        }
        impl Adhoc for T1 {
            fn add_item(
                &mut self,
                name: &str,
                dims: Vec<u32>,
                is_string: bool,
                is_const: bool,
                is_charadata: bool,
            ) {
                self.insert(
                    name.to_owned().into(),
                    InitialVarDesc {
                        is_string,
                        dims,
                        is_const,
                        is_charadata,
                        initial_sval: None,
                        initial_ival: None,
                    },
                );
            }
        }
        // ………………………………………………
        // 数値配列型変数
        // ………………………………………………
        iv.add_int("DAY", vec![1]);
        iv.add_int("MONEY", vec![1]);
        iv.add_int("TIME", vec![1]);
        iv.add_int("ITEM", vec![1]);
        iv.add_int("ITEMSALES", vec![1]);
        iv.add_int("NOITEM", vec![1]);
        iv.add_int("BOUGHT", vec![1]);
        iv.add_int("PBAND", vec![1]);
        iv.add_int("FLAG", vec![1]);
        iv.add_int("TFLAG", vec![1]);
        iv.add_int("TARGET", vec![1]);
        iv.add_int("MASTER", vec![1]);
        iv.add_int("PLAYER", vec![1]);
        iv.add_int("ASSI", vec![1]);
        iv.add_int("ASSIPLAY", vec![1]);
        iv.add_int("UP", vec![1]);
        iv.add_int("DOWN", vec![1]);
        iv.add_int("LOSEBASE", vec![1]);
        iv.add_int("PALAMLV", vec![1]);
        iv.add_int("EXPLV", vec![1]);
        iv.add_int("EJAC", vec![1]);
        iv.add_int("PREVCOM", vec![1]);
        iv.add_int("SELECTCOM", vec![1]);
        iv.add_int("NEXTCOM", vec![1]);
        iv.add_int("RESULT", vec![1]);
        iv.add_int("COUNT", vec![1]);
        iv.add_int("A", vec![1]);
        iv.add_int("B", vec![1]);
        iv.add_int("C", vec![1]);
        iv.add_int("D", vec![1]);
        iv.add_int("E", vec![1]);
        iv.add_int("F", vec![1]);
        iv.add_int("G", vec![1]);
        iv.add_int("H", vec![1]);
        iv.add_int("I", vec![1]);
        iv.add_int("J", vec![1]);
        iv.add_int("K", vec![1]);
        iv.add_int("L", vec![1]);
        iv.add_int("M", vec![1]);
        iv.add_int("N", vec![1]);
        iv.add_int("O", vec![1]);
        iv.add_int("P", vec![1]);
        iv.add_int("Q", vec![1]);
        iv.add_int("R", vec![1]);
        iv.add_int("S", vec![1]);
        iv.add_int("T", vec![1]);
        iv.add_int("U", vec![1]);
        iv.add_int("V", vec![1]);
        iv.add_int("W", vec![1]);
        iv.add_int("X", vec![1]);
        iv.add_int("Y", vec![1]);
        iv.add_int("Z", vec![1]);
        iv.add_int("ITEMPRICE", vec![1]);
        // ………………………………………………
        // 文字列配列型変数
        // ………………………………………………
        iv.add_str("SAVESTR", vec![1]);
        iv.add_str("RESULTS", vec![1]);
        iv.add_str("TSTR", vec![1]);
        iv.add_str("STR", vec![1]);
        // ITEMNAMEとITEMPRICEは片方を変更すると他方も同じ値に変更されます
        iv.add_str("ITEMNAME", vec![1]);
        iv.add_str("ABLNAME", vec![1]);
        iv.add_str("EXPNAME", vec![1]);
        iv.add_str("TALENTNAME", vec![1]);
        iv.add_str("PALAMNAME", vec![1]);
        iv.add_str("TRAINNAME", vec![1]);
        iv.add_str("MARKNAME", vec![1]);
        iv.add_str("BASENAME", vec![1]);
        iv.add_str("SOURCENAME", vec![1]);
        iv.add_str("EXNAME", vec![1]);
        iv.add_str("EQUIPNAME", vec![1]);
        iv.add_str("TEQUIPNAME", vec![1]);
        iv.add_str("FLAGNAME", vec![1]);
        iv.add_str("CFLAGNAME", vec![1]);
        iv.add_str("TFLAGNAME", vec![1]);
        iv.add_str("TCVARNAME", vec![1]);
        iv.add_str("CSTRNAME", vec![1]);
        iv.add_str("STAINNAME", vec![1]);
        iv.add_str("STRNAME", vec![1]);
        iv.add_str("TSTRNAME", vec![1]);
        iv.add_str("SAVESTRNAME", vec![1]);
        // ………………………………………………
        // 角色変数
        // ………………………………………………
        iv.add_chara_int("BASE", vec![1]);
        iv.add_chara_int("MAXBASE", vec![1]);
        iv.add_chara_int("DOWNBASE", vec![1]);
        iv.add_chara_int("ABL", vec![1]);
        iv.add_chara_int("TALENT", vec![1]);
        iv.add_chara_int("EXP", vec![1]);
        iv.add_chara_int("MARK", vec![1]);
        iv.add_chara_int("PALAM", vec![1]);
        iv.add_chara_int("SOURCE", vec![1]);
        iv.add_chara_int("EX", vec![1]);
        iv.add_chara_int("NOWEX", vec![1]);
        iv.add_chara_int("CFLAG", vec![1]);
        iv.add_chara_int("JUEL", vec![1]);
        iv.add_chara_int("GOTJUEL", vec![1]);
        iv.add_chara_int("CUP", vec![1]);
        iv.add_chara_int("CDOWN", vec![1]);
        iv.add_chara_int("RELATION", vec![1]);
        iv.add_chara_int("EQUIP", vec![1]);
        iv.add_chara_int("TEQUIP", vec![1]);
        iv.add_chara_int("STAIN", vec![1]);
        iv.add_chara_int("TCVAR", vec![1]);
        // ………………………………………………
        // 角色文字列変数
        // ………………………………………………
        iv.add_chara_str("NAME", vec![1]);
        iv.add_chara_str("CALLNAME", vec![1]);
        iv.add_chara_str("NICKNAME", vec![1]);
        iv.add_chara_str("MASTERNAME", vec![1]);
        iv.add_chara_str("CSTR", vec![1]);
        // ………………………………………………
        // 特殊一時変数・一時文字列変数
        // ………………………………………………
        // LOCAL,500
        // LOCALS,100
        // ARG,200
        // ARGS,100
        iv.add_int("GLOBAL", vec![1]);
        iv.add_str("GLOBALS", vec![1]);
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
        iv.add_chara_int("NO", vec![1]);
        iv.add_chara_int("ISASSI", vec![1]);

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
        let mut load_2_fn = |target_name: &str, no_define: bool| {
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
            let mut initial_sval =
                vec![Rc::new(StrValue { val: String::new() }); var_desc.dims[0] as _];
            for item in items {
                let [index, name] = item;
                // Add to *NAME array
                let index: u32 = match atoi_simd::parse_pos(&index) {
                    Ok(x) => x,
                    Err(e) => return Err(MEraEngineError::new(format!("{e}"))),
                };
                let name = String::from_utf8_lossy(&name).into_owned();
                initial_sval[index as usize] = Rc::new(StrValue { val: name.clone() });
                // Add to define list
                if !no_define {
                    self.contextual_indices.insert(Ascii::new(name), index);
                }
            }
            var_desc.initial_sval = Some(initial_sval);
            Ok(())
        };
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
                static RE: Lazy<Regex> =
                    Lazy::new(|| Regex::new(r"^(?:|.*[/\\])Chara(\d+).*\.(?i:csv)$").unwrap());
                let Some(chara_id) = RE.captures(filename) else {
                    return Err(MEraEngineError::new(
                        "Chara csv must be numbered".to_owned(),
                    ));
                };
                let Ok(chara_id) = chara_id[1].parse() else {
                    return Err(MEraEngineError::new(
                        "Chara csv number is invalid".to_owned(),
                    ));
                };
                match self.chara_list.entry(chara_id) {
                    std::collections::btree_map::Entry::Occupied(_) => {
                        return Err(MEraEngineError::new(format!(
                            "Character id {chara_id} already exists"
                        )));
                    }
                    std::collections::btree_map::Entry::Vacant(e) => {
                        //e.insert(todo!());
                    }
                }
            }
            EraCsvLoadKind::Abl => load_2_fn("ABLNAME", false)?,
            EraCsvLoadKind::Exp => load_2_fn("EXPNAME", false)?,
            EraCsvLoadKind::Talent => load_2_fn("TALENTNAME", false)?,
            EraCsvLoadKind::Palam => load_2_fn("PALAMNAME", false)?,
            EraCsvLoadKind::Train => load_2_fn("TRAINNAME", false)?,
            EraCsvLoadKind::Mark => load_2_fn("MARKNAME", false)?,
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
                let mut initial_nameval =
                    vec![Rc::new(StrValue { val: String::new() }); name_vd.dims[0] as _];
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
                    let price: u32 = match atoi_simd::parse_pos(price.as_bytes()) {
                        Ok(x) => x,
                        Err(e) => return Err(MEraEngineError::new(format!("{e}"))),
                    };
                    initial_nameval[index as usize] = Rc::new(StrValue { val: name.clone() });
                    initial_priceval[index as usize] = IntValue { val: price as _ };
                    // Add to define list
                    self.contextual_indices.insert(Ascii::new(name), index);
                }
                name_vd.initial_sval = Some(initial_nameval);
                price_vd.initial_ival = Some(initial_priceval);
            }
            EraCsvLoadKind::Base => load_2_fn("BASENAME", false)?,
            EraCsvLoadKind::Source => load_2_fn("SOURCENAME", false)?,
            EraCsvLoadKind::Ex => load_2_fn("EXNAME", false)?,
            EraCsvLoadKind::Str => load_2_fn("STR", true)?,
            EraCsvLoadKind::Equip => load_2_fn("EQUIPNAME", false)?,
            EraCsvLoadKind::TEquip => load_2_fn("TEQUIPNAME", false)?,
            EraCsvLoadKind::Flag => load_2_fn("FLAGNAME", false)?,
            EraCsvLoadKind::TFlag => load_2_fn("TFLAGNAME", false)?,
            EraCsvLoadKind::CFlag => load_2_fn("CFLAGNAME", false)?,
            EraCsvLoadKind::TCVar => load_2_fn("TCVARNAME", false)?,
            EraCsvLoadKind::CStr => load_2_fn("CSTRNAME", false)?,
            EraCsvLoadKind::Stain => load_2_fn("STAINNAME", false)?,
            // EraCsvLoadKind::CDFlag1 => load_2_fn("CDFLAGNAME1", false)?,
            // EraCsvLoadKind::CDFlag2 => load_2_fn("CDFLAGNAME2", false)?,
            EraCsvLoadKind::StrName => load_2_fn("STRNAME", false)?,
            EraCsvLoadKind::TStr => load_2_fn("TSTRNAME", false)?,
            EraCsvLoadKind::SaveStr => load_2_fn("SAVESTRNAME", false)?,
            EraCsvLoadKind::Global => load_2_fn("GLOBALNAME", false)?,
            EraCsvLoadKind::Globals => load_2_fn("GLOBALSNAME", false)?,
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
                    fn add_var_i64(
                        &mut self,
                        name: &str,
                        value: &str,
                    ) -> Result<(), MEraEngineError> {
                        let value = self.parse_i64(value)?;
                        _ = self.global_vars.add_var_ex(
                            name,
                            Value::new_int_0darr(value),
                            true,
                            false,
                        );
                        Ok(())
                    }
                    fn add_var_str(
                        &mut self,
                        name: &str,
                        value: String,
                    ) -> Result<(), MEraEngineError> {
                        _ = self.global_vars.add_var_ex(
                            name,
                            Value::new_str_0darr(value),
                            true,
                            false,
                        );
                        Ok(())
                    }
                }
                let mut ctx = Ctx::new(self);

                ctx.add_var_i64("GAMEBASE_VERSION", "0")?;

                for [index, value] in rows {
                    let index = String::from_utf8_lossy(&index);
                    let value = String::from_utf8_lossy(&value);
                    match index.as_ref() {
                        "コード" => {
                            ctx.add_var_i64("GAMEBASE_GAMECODE", &value)?;
                        }
                        "バージョン" => match ctx.add_var_i64("GAMEBASE_VERSION", &value) {
                            Ok(()) => (),
                            Err(_) => (),
                        },
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
                            ctx.add_var_str("GAMEBASE_TITLE", value.into_owned())?;
                        }
                        "作者" => {
                            let value = value.into_owned();
                            ctx.add_var_str("GAMEBASE_AUTHER", value.clone())?;
                            ctx.add_var_str("GAMEBASE_AUTHOR", value)?;
                        }
                        "製作年" => {
                            ctx.add_var_str("GAMEBASE_YEAR", value.into_owned())?;
                        }
                        "追加情報" => {
                            ctx.add_var_str("GAMEBASE_INFO", value.into_owned())?;
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
                    Value::new_str_arr(var_desc.dims, var_desc.initial_sval.unwrap_or_default())
                } else {
                    Value::new_int_arr(var_desc.dims, var_desc.initial_ival.unwrap_or_default())
                };
                if self
                    .global_vars
                    .add_var_ex(&name, val, var_desc.is_const, var_desc.is_charadata)
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
            crate::bytecode::Value::new_str_arr(vec![dimension as _], Vec::new())
        } else {
            crate::bytecode::Value::new_int_arr(vec![dimension as _], Vec::new())
        };
        let var_idx = self
            .global_vars
            .add_var(name, val)
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
        use crate::vm::{EraExecIp, EraVarPool};
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
            std::mem::take(&mut self.contextual_indices),
        ) {
            Some(x) => x,
            None => {
                return Err(MEraEngineError::new(
                    "could not compile AST nodes".to_owned(),
                ));
            }
        };
        let entry_info = match compilation.func_names.get(CaselessStr::new("SYSTEM_TITLE")) {
            Some(x) => *x,
            None => {
                return Err(MEraEngineError::new("entry function not found".to_owned()));
            }
        };
        let entry_info = &compilation.funcs[entry_info];
        let entry_ip = EraExecIp {
            chunk: entry_info.chunk_idx as _,
            offset: entry_info.offset as _,
        };
        let mut vm = crate::vm::EraVirtualMachine::new(compilation);
        vm.reset_exec_and_ip(entry_ip);
        for var in std::mem::take(&mut self.watching_vars) {
            vm.register_var_callback(var.as_str()).ok_or_else(|| {
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
            contextual_indices: &'a mut HashMap<Ascii<String>, u32>,
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
            fn on_read_file(&mut self, path: &str) -> anyhow::Result<Vec<u8>> {
                self.callback.on_read_file(path)
            }
            fn on_write_file(&mut self, path: &str, data: Vec<u8>) -> anyhow::Result<()> {
                self.callback.on_write_file(path, data)
            }
            fn on_list_file(&mut self, path: &str) -> anyhow::Result<Vec<String>> {
                self.callback.on_list_file(path)
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
            fn on_csv_get_num(&mut self, name: &str) -> Option<u32> {
                self.contextual_indices.get(Ascii::new_str(name)).copied()
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
    pub fn set_exec_to_func(&mut self, name: &str) -> Result<(), MEraEngineError> {
        // TODO...
        unimplemented!()
    }
    pub fn get_func_info(&self, func: &str) -> Option<EraFuncInfo> {
        // TODO...
        unimplemented!()
    }
    pub fn get_version(&self) -> String {
        "MEraEngine in MEraEmuCore v0.1.0".to_owned()
    }
}
