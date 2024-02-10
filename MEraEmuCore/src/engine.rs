use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    ops::DerefMut,
    rc::Rc,
    sync::atomic::AtomicBool,
};

use crate::{
    bytecode::SourcePosInfo,
    compiler::{EraBytecodeCompilation, EraCompilerFileInput},
    parser::EraParserSlimVarTypeInfo,
    vm::{EraVarPool, EraVirtualMachine},
};

use crate::util::*;

pub struct MEraEngine<'a> {
    global_vars: HashMap<CaselessString, EraParserSlimVarTypeInfo>,
    file_inputs: Vec<EraCompilerFileInput>,
    vm: Option<EraVirtualMachine>,
    callback: Box<dyn MEraEngineSysCallback + 'a>,
    config: MEraEngineConfig,
    //registered_vars: Vec<(String, bool, usize)>,
    registered_vars: EraVarPool,
    watching_vars: HashSet<Rc<CaselessStr>>,
}

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
    /// Callbacks for variable getters & setters. May return a string to report as execution errors.
    fn on_var_get_int(&mut self, name: &str, idx: usize) -> Result<i64, anyhow::Error>;
    fn on_var_get_str(&mut self, name: &str, idx: usize) -> Result<String, anyhow::Error>;
    fn on_var_set_int(&mut self, name: &str, idx: usize, val: i64) -> Result<(), anyhow::Error>;
    fn on_var_set_str(&mut self, name: &str, idx: usize, val: &str) -> Result<(), anyhow::Error>;
    /// Callback for PRINT family statements.
    fn on_print(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags);
    /// Callback for HTML_PRINT statements.
    fn on_html_print(&mut self, content: &str);
    fn on_wait(&mut self, XXXXXXXXXXXX: u32);
    fn on_input(&mut self, XXXXXXXXXXXX: u32);
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
}

struct EmptyCallback;
impl MEraEngineSysCallback for EmptyCallback {
    fn on_compile_error(&mut self, info: &EraScriptErrorInfo) {}
    fn on_execute_error(&mut self, info: &EraScriptErrorInfo) {}
    fn on_get_rand(&mut self) -> u64 {
        0
    }
    fn on_html_print(&mut self, content: &str) {}
    fn on_input(&mut self, XXXXXXXXXXXX: u32) {}
    fn on_print(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags) {}
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
    fn on_wait(&mut self, XXXXXXXXXXXX: u32) {}
}

impl<'a> MEraEngine<'a> {
    pub fn new() -> Self {
        MEraEngine {
            global_vars: HashMap::new(),
            file_inputs: Vec::new(),
            vm: None,
            callback: Box::new(EmptyCallback),
            config: Default::default(),
            registered_vars: EraVarPool::default(),
            watching_vars: HashSet::new(),
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
        // TODO...
        unimplemented!()
    }
    pub fn load_erh(&mut self, filename: &str, content: &[u8]) -> Result<(), MEraEngineError> {
        self.load_erb(filename, content)
    }
    pub fn load_erb(&mut self, filename: &str, content: &[u8]) -> Result<(), MEraEngineError> {
        use crate::parser::{EraDecl, EraSharpDecl};
        // TODO: Handle UTF-8 BOM?
        let callback = RefCell::new(self.callback.deref_mut());
        let mut lexer = crate::lexer::EraLexer::new(content, |e| {
            callback.borrow_mut().on_compile_error(&EraScriptErrorInfo {
                filename,
                src_info: e.src_info.into(),
                is_error: e.is_error,
                msg: &e.msg,
            });
        });
        let mut parser = crate::parser::EraParser::new(|e| {
            callback.borrow_mut().on_compile_error(&EraScriptErrorInfo {
                filename,
                src_info: e.src_info.into(),
                is_error: e.is_error,
                msg: &e.msg,
            });
        });
        let root_ast = match parser.parse(&mut lexer, &self.global_vars) {
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
                    self.global_vars.insert(
                        CaselessString::new(x.name.clone()),
                        EraParserSlimVarTypeInfo {
                            is_string: x.is_string,
                        },
                    );
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
            .registered_vars
            .add_var(name, val)
            .ok_or_else(|| MEraEngineError::new("variable already used".to_owned()))?;
        if watch {
            self.watching_vars.insert(
                self.registered_vars
                    .get_var_info(var_idx)
                    .unwrap()
                    .name
                    .clone(),
            );
        }
        self.global_vars.insert(
            CaselessString::new(name.to_owned()),
            EraParserSlimVarTypeInfo { is_string },
        );
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
            std::mem::take(&mut self.registered_vars),
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
        //for var in registered_vars {
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
        }

        let can_progress = vm.execute(
            stop_flag,
            max_inst_cnt,
            &mut AdhocCallback {
                callback: self.callback.deref_mut(),
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
