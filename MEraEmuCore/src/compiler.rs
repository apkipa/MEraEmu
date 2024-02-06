use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    rc::Rc,
};

use crate::{
    bytecode::{EraBytecodePrimaryType, PrintExtendedFlags, ValueKind},
    lexer::{EraTokenKind, EraTokenLite},
    parser::{EraCommandStmt, EraFunDecl, EraStmt, EraTermExpr, EraVarExpr},
    util::*,
};

use crate::{
    bytecode::{IntValue, SourcePosInfo, StrValue, Value},
    parser::{EraDecl, EraExpr, EraLiteral, EraRootASTNode, EraSharpDecl, EraVarDecl},
    vm::EraVarPool,
};

#[derive(Debug, Clone, Copy)]
pub struct SlimSourcePosInfo {
    pub line: u16,
    pub column: u16,
}
impl From<SlimSourcePosInfo> for SourcePosInfo {
    fn from(value: SlimSourcePosInfo) -> Self {
        SourcePosInfo {
            line: value.line.into(),
            column: value.column.into(),
        }
    }
}
impl From<SourcePosInfo> for SlimSourcePosInfo {
    fn from(value: SourcePosInfo) -> Self {
        // TODO: Check overflow
        SlimSourcePosInfo {
            line: value.line as _,
            column: value.column as _,
        }
    }
}

#[derive(Default)]
pub struct EraConstantPool {
    pub vals: Vec<Value>,
}

pub struct EraFuncBytecodeInfo {
    pub name: Rc<CaselessStr>,
    pub chunk_idx: u32,
    pub offset: u32,
    pub args_count: u32,
}

#[derive(Default)]
struct EraFuncPool {
    // Mapping from names to indices.
    func_names: HashMap<Rc<CaselessStr>, usize>,
    funcs: Vec<EraFuncInfo>,
}
struct EraFuncInfo {
    name: Rc<CaselessStr>,
    params: Vec<EraFuncArgInfo>,
    local_vars: HashMap<Rc<CaselessStr>, EraFuncLocalVarInfo>,
    local_frame_size: usize,
    local_size: Option<u32>,
}
struct EraFuncArgInfo {
    // Default ArrInt / ArrStr value indicates ref argument
    target_var: (Rc<CaselessStr>, Vec<u32>),
    default_val: Value, // @FUN(value = <default_val>)
    src_info: SourcePosInfo,
}

struct EraFuncLocalVarInfo {
    init_val: Value, // #DIM value = <init_val>
    has_init: bool,  // Is <init_val> present?
    idx_in_frame: usize,
    is_in_global_frame: bool,
    is_const: bool,
    is_dynamic: bool,
    is_ref: bool,
    src_info: SourcePosInfo,
}

pub struct EraBytecodeChunk {
    // TODO: Optimize debugging information presentation
    pub bytecode: Vec<u8>,
    // src_infos[bytecode_offset] => src_info
    pub src_infos: Vec<SlimSourcePosInfo>,
    pub name: Rc<str>,
    pub constants: EraConstantPool,
}
struct EraBytecodeChunkBacktrackHolder {
    base_idx: usize,
    write_fn: &'static dyn Fn(&mut EraBytecodeChunk, usize, isize) -> Option<()>,
}
impl EraBytecodeChunk {
    fn append_u8(&mut self, value: u8, src_info: SourcePosInfo) {
        self.bytecode.push(value);
        self.src_infos.push(src_info.into());
    }
    fn append_u16(&mut self, value: u16, src_info: SourcePosInfo) {
        self.bytecode.extend_from_slice(&value.to_ne_bytes());
        self.src_infos.extend(
            std::iter::repeat(SlimSourcePosInfo::from(src_info))
                .take(std::mem::size_of_val(&value)),
        );
    }
    fn append_u32(&mut self, value: u32, src_info: SourcePosInfo) {
        self.bytecode.extend_from_slice(&value.to_ne_bytes());
        self.src_infos.extend(
            std::iter::repeat(SlimSourcePosInfo::from(src_info))
                .take(std::mem::size_of_val(&value)),
        );
    }
    pub fn read_u8(&self, idx: usize) -> Option<u8> {
        self.bytecode.get(idx).copied()
    }
    pub fn read_u16(&self, idx: usize) -> Option<u16> {
        let idx2 = idx + std::mem::size_of::<u16>();
        self.bytecode
            .get(idx..idx2)
            .map(|x| u16::from_ne_bytes(x.try_into().unwrap()))
    }
    pub fn read_u32(&self, idx: usize) -> Option<u32> {
        let idx2 = idx + std::mem::size_of::<u32>();
        self.bytecode
            .get(idx..idx2)
            .map(|x| u32::from_ne_bytes(x.try_into().unwrap()))
    }
    // Useful for backtracking
    pub fn overwrite_u8(&mut self, value: u8, idx: usize) -> Option<()> {
        *self.bytecode.get_mut(idx)? = value;
        Some(())
    }
    // Useful for backtracking
    pub fn overwrite_u16(&mut self, value: u16, idx: usize) -> Option<()> {
        let idx2 = idx + std::mem::size_of::<u16>();
        self.bytecode
            .get_mut(idx..idx2)?
            .copy_from_slice(&value.to_ne_bytes());
        Some(())
    }
    // Useful for backtracking
    pub fn overwrite_u32(&mut self, value: u32, idx: usize) -> Option<()> {
        let idx2 = idx + std::mem::size_of::<u32>();
        self.bytecode
            .get_mut(idx..idx2)?
            .copy_from_slice(&value.to_ne_bytes());
        Some(())
    }
    pub fn cur_bytes_cnt(&self) -> usize {
        self.bytecode.len()
    }
    pub fn get_constants_cnt(&self) -> usize {
        self.constants.vals.len()
    }
    pub fn get_name(&self) -> &str {
        &self.name
    }
    pub fn source_info_at(&self, idx: usize) -> Option<SourcePosInfo> {
        self.src_infos.get(idx).map(|x| SourcePosInfo::from(*x))
    }
    fn add_constant(&mut self, value: Value) -> usize {
        let idx = self.constants.vals.len();
        self.constants.vals.push(value);
        idx
    }
    // WARN: Do NOT modify the returned value
    pub fn get_constant(&self, idx: usize) -> Option<&Value> {
        self.constants.vals.get(idx)
    }
    // NOTE: emit_* functions are helpers for writing bytecodes
    fn emit_bytecode(&mut self, value: EraBytecodePrimaryType, src_info: SourcePosInfo) {
        self.append_u8(value.to_i(), src_info)
    }
    fn emit_load_const(&mut self, value: Value, src_info: SourcePosInfo) {
        // TODO: Implement constant deduplication?
        let const_idx = self.add_constant(value);
        if let Ok(idx) = const_idx.try_into() {
            self.emit_bytecode(EraBytecodePrimaryType::LoadConst, src_info);
            self.append_u8(idx, src_info);
        } else if let Ok(idx) = const_idx.try_into() {
            self.emit_bytecode(EraBytecodePrimaryType::LoadConstW, src_info);
            self.append_u16(idx, src_info);
        } else if let Ok(idx) = const_idx.try_into() {
            self.emit_bytecode(EraBytecodePrimaryType::LoadConstWW, src_info);
            self.append_u32(idx, src_info);
        } else {
            panic!("too many constants in constant pool");
        }
    }
    // Returned value can be used for backtracking
    // TODO: Determine suitable imm sizes by spliting code into relocatable basic blocks
    fn emit_jump_hold(&mut self, src_info: SourcePosInfo) -> EraBytecodeChunkBacktrackHolder {
        let base_idx = self.cur_bytes_cnt();
        self.emit_bytecode(EraBytecodePrimaryType::JumpW, src_info);
        self.append_u16(0xffff, src_info);
        EraBytecodeChunkBacktrackHolder {
            base_idx,
            write_fn: &|this, idx, offset| this.overwrite_u16(offset.try_into().ok()?, idx),
        }
    }
    fn emit_jump_cond_hold(&mut self, src_info: SourcePosInfo) -> EraBytecodeChunkBacktrackHolder {
        let base_idx = self.cur_bytes_cnt();
        self.emit_bytecode(EraBytecodePrimaryType::JumpCondW, src_info);
        self.append_u16(0xffff, src_info);
        EraBytecodeChunkBacktrackHolder {
            base_idx,
            write_fn: &|this, idx, offset| this.overwrite_u16(offset.try_into().ok()?, idx),
        }
    }
    fn emit_jump(&mut self, offset: isize, src_info: SourcePosInfo) {
        if let Ok(offset) = i8::try_from(offset) {
            self.emit_bytecode(EraBytecodePrimaryType::Jump, src_info);
            self.append_u8(offset as _, src_info);
        } else if let Ok(offset) = i16::try_from(offset) {
            self.emit_bytecode(EraBytecodePrimaryType::JumpW, src_info);
            self.append_u16(offset as _, src_info);
        } else {
            panic!("jump too far to be encoded in bytecode");
        }
    }
    fn emit_jump_cond(&mut self, offset: isize, src_info: SourcePosInfo) {
        if let Ok(offset) = i8::try_from(offset) {
            self.emit_bytecode(EraBytecodePrimaryType::JumpCond, src_info);
            self.append_u8(offset as _, src_info);
        } else if let Ok(offset) = i16::try_from(offset) {
            self.emit_bytecode(EraBytecodePrimaryType::JumpCondW, src_info);
            self.append_u16(offset as _, src_info);
        } else {
            panic!("jump too far to be encoded in bytecode");
        }
    }
}
impl EraBytecodeChunkBacktrackHolder {
    fn complete(self, chunk: &mut EraBytecodeChunk) -> Option<()> {
        let offset = isize::try_from(chunk.cur_bytes_cnt())
            .ok()?
            .checked_sub_unsigned(self.base_idx)?;
        // 1 is bytecode size
        (self.write_fn)(chunk, self.base_idx + 1, offset)
    }
}

pub struct EraBytecodeCompilation {
    pub func_names: HashMap<Rc<CaselessStr>, usize>,
    pub funcs: Vec<EraFuncBytecodeInfo>,
    pub chunks: Vec<EraBytecodeChunk>,
    pub global_vars: EraVarPool,
}

pub struct EraCompilerFileInput {
    pub file_name: String,
    pub root_node: EraRootASTNode,
}

pub struct EraCompiler<ErrReportFn> {
    err_report_fn: ErrReportFn,
}

pub struct EraCompileErrorInfo {
    pub file_name: String,
    pub src_info: SourcePosInfo,
    pub is_error: bool,
    pub msg: String,
}

pub struct EraCompilerImpl<'a, ErrReportFn> {
    err_report_fn: &'a mut ErrReportFn,
    file_name: String,
    // funcs: EraFuncPool,
    vars: EraVarPool,
    // local_vars: Option<EraFunctionFrame>,
    intern_vals: HashMap<either::Either<Rc<IntValue>, Rc<StrValue>>, ()>,
}

impl<T: FnMut(&EraCompileErrorInfo)> EraCompiler<T> {
    pub fn new(err_report_fn: T) -> Self {
        Self { err_report_fn }
    }
    pub fn compile_all(
        &mut self,
        inputs: Vec<EraCompilerFileInput>,
        global_vars: EraVarPool,
    ) -> Option<EraBytecodeCompilation> {
        EraCompilerImpl::new(self).compile_all(inputs, global_vars)
    }
}

impl<'a, T: FnMut(&EraCompileErrorInfo)> EraCompilerImpl<'a, T> {
    fn new(parser: &'a mut EraCompiler<T>) -> Self {
        EraCompilerImpl {
            err_report_fn: &mut parser.err_report_fn,
            file_name: String::new(),
            // funcs: Default::default(),
            vars: Default::default(),
            intern_vals: HashMap::new(),
        }
    }
    fn compile_all(
        &mut self,
        inputs: Vec<EraCompilerFileInput>,
        global_vars: EraVarPool,
    ) -> Option<EraBytecodeCompilation> {
        let mut const_vars = HashSet::new();
        let mut fun_decls = Vec::new();
        let mut global_funcs = EraFuncPool::default();

        self.vars = global_vars;

        // Build global variables and functions list
        for input in inputs {
            self.file_name = input.file_name;

            for decl in input.root_node.decls {
                match decl {
                    EraDecl::SharpDecl(x) => match x {
                        EraSharpDecl::VarDecl(x) => {
                            let src_info = x.src_info;
                            if x.is_dynamic || x.is_ref {
                                self.report_err(
                                    src_info,
                                    true,
                                    "invalid qualifier for file-scope variable",
                                );
                                return None;
                            }
                            let is_const = x.is_const;
                            let (name, var_val) = self.materialize_var_decl(x)?;
                            let var_idx = match self.vars.add_var(&name, var_val) {
                                Some(x) => x,
                                None => {
                                    self.report_err(
                                        src_info,
                                        true,
                                        format!("redefinition of variable `{name}`"),
                                    );
                                    return None;
                                }
                            };
                            if is_const {
                                const_vars.insert(name);
                            }
                        }
                        _ => {
                            self.report_err(
                                x.source_pos_info(),
                                false,
                                "this declaration should not appear here; ignoring",
                            );
                        }
                    },
                    EraDecl::FunDecl(x) => {
                        fun_decls.push((self.file_name.clone(), x));
                    }
                }
            }
        }

        // Clear cache
        self.file_name = String::new();

        // Parse function signatures
        for (file_name, fun) in fun_decls.iter_mut() {
            std::mem::swap(&mut self.file_name, file_name);

            let func_name = CaselessStr::new(fun.name.deref()).into();
            let fun_idx = global_funcs.funcs.len();
            let mut local_vars = HashMap::new();
            let mut params = Vec::new();
            if global_funcs
                .func_names
                .insert(Rc::clone(&func_name), fun_idx)
                .is_some()
            {
                self.report_err(
                    fun.src_info,
                    true,
                    format!("redefinition of function `{func_name}`"),
                );
                return None;
            }
            self.vars.add_var(
                &format!("LOCAL@{}", func_name),
                Value::new_int_arr(vec![16], Vec::new()),
            );
            self.vars.add_var(
                &format!("LOCALS@{}", func_name),
                Value::new_str_arr(vec![16], Vec::new()),
            );
            self.vars.add_var(
                &format!("ARG@{}", func_name),
                Value::new_int_arr(vec![16], Vec::new()),
            );
            self.vars.add_var(
                &format!("ARGS@{}", func_name),
                Value::new_str_arr(vec![16], Vec::new()),
            );

            // Local variables
            let mut local_frame_var_counter = fun.params.len();
            for decl in std::mem::take(&mut fun.decls) {
                let src_info;
                let (var_name, mut local_decl) = match decl {
                    EraSharpDecl::VarDecl(x) => {
                        src_info = x.src_info;
                        let has_init;
                        let init_val = if x.is_ref {
                            if !x.dims.is_empty() {
                                self.report_err(
                                    src_info,
                                    true,
                                    "ref-qualified function parameter must not have dimensions",
                                );
                                return None;
                            }
                            if !x.inits.is_empty() {
                                self.report_err(
                                    src_info,
                                    true,
                                    "ref-qualified function parameter must not have init values",
                                );
                                return None;
                            }
                            has_init = false;
                            if x.is_string {
                                Value::new_str_arr(vec![0], Vec::new())
                            } else {
                                Value::new_int_arr(vec![0], Vec::new())
                            }
                        } else {
                            has_init = !x.inits.is_empty();
                            let arr_len = x.dims.iter().fold(1, |acc, &x| acc * x);
                            if (arr_len as usize) < x.inits.len() {
                                self.report_err(
                                    src_info,
                                    false,
                                    "excessive elements in initializer",
                                );
                            }
                            if x.is_string {
                                let inits = x
                                    .inits
                                    .into_iter()
                                    .map(|x| {
                                        self.evaluate_constant(x)
                                            .and_then(|x| self.unwrap_str_constant(x))
                                            // TODO: Perform interning
                                            .map(|val| Rc::new(StrValue { val }))
                                    })
                                    .collect::<Option<Vec<_>>>()?;
                                Value::new_str_arr(x.dims, inits)
                            } else {
                                let inits = x
                                    .inits
                                    .into_iter()
                                    .map(|x| {
                                        self.evaluate_constant(x)
                                            .and_then(|x| self.unwrap_int_constant(x))
                                            .map(|val| Rc::new(IntValue { val }))
                                    })
                                    .collect::<Option<Vec<_>>>()?;
                                Value::new_int_arr(x.dims, inits)
                            }
                        };
                        (
                            x.name,
                            EraFuncLocalVarInfo {
                                init_val,
                                has_init,
                                idx_in_frame: usize::MAX, //Stub
                                is_in_global_frame: false,
                                is_const: x.is_const,
                                is_dynamic: x.is_dynamic,
                                is_ref: x.is_ref,
                                src_info,
                            },
                        )
                    }
                    _ => continue,
                };
                // Insert into global / local var pool
                if !local_decl.is_dynamic && !local_decl.is_ref {
                    let var_name = Self::decorate_as_func_local_name(&var_name, func_name.as_str());
                    local_decl.idx_in_frame =
                        match self.vars.add_var(&var_name, local_decl.init_val.clone()) {
                            Some(x) => x,
                            None => {
                                self.report_err(
                                    fun.src_info,
                                    true,
                                    format!("redefinition of global variable `{var_name}`"),
                                );
                                return None;
                            }
                        };
                    local_decl.is_in_global_frame = true;
                } else if local_decl.is_ref {
                    // Stub marker; must be reassigned later
                    local_decl.idx_in_frame = usize::MAX;
                } else {
                    local_decl.idx_in_frame = local_frame_var_counter;
                    local_frame_var_counter += 1;
                }
                if local_vars
                    .insert(CaselessStr::new(&var_name).into(), local_decl)
                    .is_some()
                {
                    self.report_err(
                        src_info,
                        true,
                        format!("redefinition of variable `{var_name}`"),
                    );
                    return None;
                }
            }

            // Parameters
            for (param_idx, param) in std::mem::take(&mut fun.params).into_iter().enumerate() {
                let arg_info = match param {
                    EraExpr::Binary(
                        lhs,
                        EraTokenLite {
                            kind: EraTokenKind::Assign | EraTokenKind::ExprAssign,
                            src_info,
                        },
                        rhs,
                    ) => match *lhs {
                        EraExpr::Term(EraTermExpr::Var(lhs)) => {
                            // Check parameter type
                            let kind = local_vars
                                .get(CaselessStr::new(&lhs.name))
                                .map(|x| match x.is_ref {
                                    true => x.init_val.kind().with_arr(),
                                    false => x.init_val.kind().without_arr(),
                                })
                                .or_else(|| {
                                    self.vars.get_var(&lhs.name).map(|x| x.kind().without_arr())
                                });
                            let kind = match kind {
                                Some(x) => x,
                                None => {
                                    self.report_err(
                                        lhs.src_info,
                                        true,
                                        format!("undefined variable `{}`", lhs.name),
                                    );
                                    return None;
                                }
                            };
                            // TODO: Check index overflow
                            let idxs = lhs
                                .idxs
                                .into_iter()
                                .map(|x| {
                                    self.evaluate_constant(x)
                                        .and_then(|x| self.unwrap_int_constant(x).map(|x| x as _))
                                })
                                .collect::<Option<Vec<_>>>()?;
                            let rhs = self.evaluate_constant(*rhs)?;
                            //let rhs = Self::materialize_constant(rhs);
                            let rhs = match kind {
                                ValueKind::ArrInt | ValueKind::ArrStr => {
                                    self.report_err(
                                        src_info,
                                        true,
                                        "ref-qualified parameter must not have default value",
                                    );
                                    return None;
                                }
                                ValueKind::Int => {
                                    let rhs = self.unwrap_int_constant(rhs)?;
                                    self.new_value_int(rhs)
                                }
                                ValueKind::Str => {
                                    let rhs = self.unwrap_str_constant(rhs)?;
                                    self.new_value_str(rhs)
                                }
                            };

                            EraFuncArgInfo {
                                target_var: (CaselessStr::new(&lhs.name).into(), idxs),
                                default_val: rhs,
                                src_info: lhs.src_info,
                            }
                        }
                        _ => {
                            self.report_err(
                                lhs.source_pos_info(),
                                true,
                                format!("invalid expression as function parameter"),
                            );
                            return None;
                        }
                    },
                    EraExpr::Term(EraTermExpr::Var(lhs)) => {
                        let kind = local_vars
                            .get_mut(CaselessStr::new(&lhs.name))
                            .map(|x| match x.is_ref {
                                true => {
                                    // Replace with actual parameter position
                                    x.idx_in_frame = param_idx as _;
                                    x.init_val.kind().with_arr()
                                }
                                false => x.init_val.kind().without_arr(),
                            })
                            .or_else(|| {
                                self.vars.get_var(&lhs.name).map(|x| x.kind().without_arr())
                            });
                        let kind = match kind {
                            Some(x) => x,
                            None => {
                                self.report_err(
                                    lhs.src_info,
                                    true,
                                    format!("undefined variable `{}`", lhs.name),
                                );
                                return None;
                            }
                        };
                        // TODO: Check index overflow
                        let idxs = lhs
                            .idxs
                            .into_iter()
                            .map(|x| {
                                self.evaluate_constant(x)
                                    .and_then(|x| self.unwrap_int_constant(x).map(|x| x as _))
                            })
                            .collect::<Option<Vec<_>>>()?;

                        let rhs = match kind {
                            ValueKind::ArrInt => Value::new_int_arr(vec![0], Vec::new()),
                            ValueKind::ArrStr => Value::new_str_arr(vec![0], Vec::new()),
                            ValueKind::Int => self.new_value_int(0),
                            ValueKind::Str => self.new_value_str(String::new()),
                        };

                        EraFuncArgInfo {
                            target_var: (CaselessStr::new(&lhs.name).into(), idxs),
                            default_val: rhs,
                            src_info: lhs.src_info,
                        }
                    }
                    _ => {
                        self.report_err(
                            param.source_pos_info(),
                            true,
                            format!("invalid expression as function parameter"),
                        );
                        return None;
                    }
                };
                params.push(arg_info);
            }

            // TODO: LocalSize support

            global_funcs.funcs.push(EraFuncInfo {
                name: func_name,
                params,
                local_vars,
                local_frame_size: local_frame_var_counter,
                local_size: None,
            });

            std::mem::swap(&mut self.file_name, file_name);
        }

        let mut funcs = Vec::new();
        let mut chunks_idxs: HashMap<String, usize> = HashMap::new();
        let mut chunks = Vec::new();

        // Compile function bodies
        for (file_name, func_decl) in fun_decls {
            self.file_name = file_name;

            let chunk_idx = *chunks_idxs
                .entry(self.file_name.clone())
                .or_insert_with(|| {
                    let idx = chunks.len();
                    chunks.push(EraBytecodeChunk {
                        bytecode: Vec::new(),
                        src_infos: Vec::new(),
                        name: self.file_name.deref().into(),
                        constants: Default::default(),
                    });
                    idx
                });
            let chunk = &mut chunks[chunk_idx];
            let func_idx = funcs.len();
            let mut func_info = self.compile_function(
                &global_funcs,
                &global_funcs.funcs[func_idx],
                func_decl,
                chunk,
            )?;
            // TODO: Check overflow
            func_info.chunk_idx = chunk_idx as _;
            funcs.push(func_info);
        }

        // let global_funcs = std::mem::take(&mut self.funcs);
        let global_vars = std::mem::take(&mut self.vars);

        Some(EraBytecodeCompilation {
            func_names: global_funcs.func_names,
            funcs,
            chunks,
            global_vars,
        })
    }
    #[must_use]
    fn compile_function(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        func_decl: EraFunDecl,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<EraFuncBytecodeInfo> {
        use EraBytecodePrimaryType::*;

        let bytecode_info = EraFuncBytecodeInfo {
            name: CaselessStr::new(&func_decl.name).into(),
            chunk_idx: u32::MAX, // Stub
            offset: chunk.cur_bytes_cnt() as _,
            args_count: func_info.params.len() as _,
        };

        // Init local variables (including non-DYNAMIC ones)
        for (name, var_info) in func_info.local_vars.iter() {
            let src_info = var_info.src_info;
            if var_info.is_in_global_frame {
                if var_info.has_init {
                    chunk.emit_load_const(Value::new_int(var_info.idx_in_frame as _), src_info);
                    chunk.emit_bytecode(GetGlobal, src_info);
                    chunk.emit_load_const(var_info.init_val.clone(), src_info);
                    chunk.emit_bytecode(CopyArrayContent, src_info);
                }
            } else if var_info.is_ref {
                if var_info.idx_in_frame == usize::MAX {
                    self.report_err(
                        src_info,
                        true,
                        "ref-qualified variable was not bound to any valid parameter",
                    );
                    return None;
                }
            } else {
                chunk.emit_load_const(var_info.init_val.clone(), src_info);
                chunk.emit_bytecode(DeepClone, src_info);
            }
        }

        // Assign parameters
        for (param_idx, param_info) in func_info.params.iter().enumerate() {
            // TODO: Optimize performance

            if param_info.default_val.kind().is_arr() {
                // Skip ref parameter
                continue;
            }

            let src_info = param_info.src_info;
            let lhs = EraVarExpr {
                name: param_info.target_var.0.to_string(),
                idxs: param_info
                    .target_var
                    .1
                    .iter()
                    .map(|x| EraExpr::new_int(*x as _, src_info))
                    .collect(),
                src_info,
            };
            self.compile_expression_assign(
                funcs,
                func_info,
                lhs,
                |this, chunk| {
                    chunk.emit_load_const(this.new_value_int(param_idx as _), src_info);
                    chunk.emit_bytecode(GetLocal, src_info);
                    Some(())
                },
                chunk,
            )?;
            chunk.emit_bytecode(Pop, src_info);
        }

        // Compile function body
        for stmt in func_decl.body {
            self.compile_statement(funcs, func_info, stmt, chunk)?;
        }

        // Return
        match func_decl.kind {
            crate::parser::EraFunKind::Function => {
                let val = self.new_value_int(0);
                chunk.emit_load_const(val, func_decl.src_info);
                chunk.emit_bytecode(ReturnInteger, func_decl.src_info);
            }
            crate::parser::EraFunKind::FunctionS => {
                let val = self.new_value_str(String::new());
                chunk.emit_load_const(val, func_decl.src_info);
                chunk.emit_bytecode(ReturnString, func_decl.src_info);
            }
            crate::parser::EraFunKind::Procedure => {
                chunk.emit_bytecode(ReturnVoid, func_decl.src_info);
            }
        }

        Some(bytecode_info)
    }
    #[must_use]
    fn compile_statement(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        stmt: EraStmt,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<()> {
        use EraBytecodePrimaryType::*;
        use EraCommandStmt as Cmd;

        let stmt = match stmt {
            EraStmt::Expr(x) => {
                // Evaluate and discard values
                let src_info = x.source_pos_info();
                self.compile_expression(funcs, func_info, x, chunk)?;
                chunk.emit_bytecode(Pop, src_info);
                return Some(());
            }
            EraStmt::Command(x) => x,
        };

        match stmt {
            Cmd::Print(x) => {
                let src_info = x.src_info;
                let args_cnt = x.vals.len();
                for v in x.vals {
                    let src_info = v.source_pos_info();
                    self.compile_expression(funcs, func_info, v, chunk)?;
                    chunk.emit_bytecode(ConvertToString, src_info);
                }
                chunk.emit_bytecode(BuildString, src_info);
                // TODO: BuildString check overflow
                chunk.append_u8(args_cnt as _, src_info);

                match u8::from(x.flags) {
                    x if x == PrintExtendedFlags::new().into() => {
                        chunk.emit_bytecode(Print, src_info);
                    }
                    x if x == PrintExtendedFlags::new().with_is_line(true).into() => {
                        chunk.emit_bytecode(PrintLine, src_info);
                    }
                    x => {
                        chunk.emit_bytecode(PrintExtended, src_info);
                        chunk.append_u8(x, src_info);
                    }
                }
            }
            Cmd::Quit(x) => {
                chunk.emit_bytecode(Quit, x.src_info);
            }
            Cmd::If(x) => {
                // TODO: Optimize when there is no else body
                self.compile_expression(funcs, func_info, x.cond, chunk)?;
                let backtrack_then = chunk.emit_jump_cond_hold(x.src_info);
                for stmt in x.else_body {
                    self.compile_statement(funcs, func_info, stmt, chunk)?;
                }
                // HACK: Steal source pos info from last statement
                let last_src_info = chunk.source_info_at(chunk.cur_bytes_cnt() - 1).unwrap();
                let backtrack_done = chunk.emit_jump_hold(last_src_info);
                if backtrack_then.complete(chunk).is_none() {
                    self.report_err(x.src_info, true, "jump too far to be encoded in bytecode");
                    return None;
                }
                for stmt in x.body {
                    self.compile_statement(funcs, func_info, stmt, chunk)?;
                }
                if backtrack_done.complete(chunk).is_none() {
                    self.report_err(x.src_info, true, "jump too far to be encoded in bytecode");
                    return None;
                }
            }
            Cmd::While(x) => {
                let start_index = chunk.cur_bytes_cnt();
                self.compile_expression(funcs, func_info, x.cond, chunk)?;
                chunk.emit_bytecode(Negate, x.src_info);
                let backtrack_done = chunk.emit_jump_cond_hold(x.src_info);
                for stmt in x.body {
                    self.compile_statement(funcs, func_info, stmt, chunk)?;
                }
                let done_index = chunk.cur_bytes_cnt();
                // HACK: Steal source pos info from last statement
                let last_src_info = chunk.source_info_at(chunk.cur_bytes_cnt() - 1).unwrap();
                chunk.emit_jump(-((done_index - start_index) as isize), last_src_info);
                if backtrack_done.complete(chunk).is_none() {
                    self.report_err(x.src_info, true, "jump too far to be encoded in bytecode");
                    return None;
                }
            }
            _ => {
                self.report_err(stmt.source_pos_info(), true, "unsupported statement");
                return None;
            }
        }

        Some(())
    }
    #[must_use]
    fn compile_expression(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        expr: EraExpr,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<()> {
        use EraBytecodePrimaryType::*;

        match expr {
            EraExpr::Binary(lhs, op, rhs) => {
                if let EraTokenKind::Assign | EraTokenKind::ExprAssign = op.kind {
                    match *lhs {
                        EraExpr::Term(EraTermExpr::Var(lhs)) => {
                            self.compile_expression_assign(
                                funcs,
                                func_info,
                                lhs,
                                |this, chunk| {
                                    this.compile_expression(funcs, func_info, *rhs, chunk)
                                },
                                chunk,
                            )?;
                        }
                        _ => {
                            self.report_err(
                                lhs.source_pos_info(),
                                true,
                                "invalid assignment target",
                            );
                            return None;
                        }
                    }
                } else {
                    self.compile_expression(funcs, func_info, *lhs, chunk)?;
                    self.compile_expression(funcs, func_info, *rhs, chunk)?;
                    match op.kind {
                        EraTokenKind::Plus => chunk.emit_bytecode(Add, op.src_info),
                        EraTokenKind::Minus => chunk.emit_bytecode(Subtract, op.src_info),
                        EraTokenKind::Multiply => chunk.emit_bytecode(Multiply, op.src_info),
                        EraTokenKind::Divide => chunk.emit_bytecode(Divide, op.src_info),
                        EraTokenKind::Percentage => chunk.emit_bytecode(Modulo, op.src_info),
                        EraTokenKind::CmpEq => chunk.emit_bytecode(CompareEq, op.src_info),
                        EraTokenKind::CmpL => chunk.emit_bytecode(CompareL, op.src_info),
                        EraTokenKind::CmpLEq => chunk.emit_bytecode(CompareLEq, op.src_info),
                        EraTokenKind::CmpNEq => {
                            chunk.emit_bytecode(CompareEq, op.src_info);
                            chunk.emit_bytecode(Negate, op.src_info);
                        }
                        EraTokenKind::CmpG => {
                            chunk.emit_bytecode(CompareLEq, op.src_info);
                            chunk.emit_bytecode(Negate, op.src_info);
                        }
                        EraTokenKind::CmpGEq => {
                            chunk.emit_bytecode(CompareL, op.src_info);
                            chunk.emit_bytecode(Negate, op.src_info);
                        }
                        // TODO...
                        _ => {
                            self.report_err(op.src_info, true, "invalid arithmetic kind");
                            return None;
                        }
                    }
                }
            }
            EraExpr::Term(x) => match x {
                EraTermExpr::Literal(x) => {
                    match x {
                        EraLiteral::Integer(x, src_info) => {
                            if let Ok(x) = i8::try_from(x) {
                                chunk.emit_bytecode(LoadIntegerImm8, src_info);
                                chunk.append_u8(x as _, src_info);
                            } else {
                                chunk.emit_load_const(self.new_value_int(x), src_info);
                            }
                        },
                        EraLiteral::String(x, src_info) => chunk.emit_load_const(self.new_value_str(x), src_info),
                    };
                }
                EraTermExpr::StrForm(x) => {
                    let src_info = x.src_info;
                    let parts_cnt = x.parts.len();
                    for part in x.parts {
                        match part {
                            crate::parser::EraStrFormExprPart::Expression(x) => {
                                let src_info = x.source_pos_info();
                                self.compile_expression(funcs, func_info, x, chunk)?;
                                chunk.emit_bytecode(ConvertToString, src_info);
                            }
                            crate::parser::EraStrFormExprPart::Literal(x, src_info) => {
                                let x = self.new_value_str(x);
                                chunk.emit_load_const(x, src_info);
                            }
                        }
                    }
                    chunk.emit_bytecode(BuildString, src_info);
                    // TODO: BuildString check overflow
                    chunk.append_u8(parts_cnt as _, src_info);
                }
                EraTermExpr::Var(x) => {
                    self.compile_expression_array(funcs, func_info, x, true, chunk)?;
                }
            },
            EraExpr::PreUnary(op, rhs) => match op.kind {
                EraTokenKind::Plus => self.compile_expression(funcs, func_info, *rhs, chunk)?,
                EraTokenKind::Minus => {
                    self.compile_expression(funcs, func_info, *rhs, chunk)?;
                    chunk.emit_bytecode(Negate, op.src_info);
                }
                EraTokenKind::Increment | EraTokenKind::Decrement => todo!(),
                _ => {
                    self.report_err(op.src_info, true, "invalid arithmetic kind");
                    return None;
                }
            },
            // TODO...
            _ => {
                self.report_err(expr.source_pos_info(), true, "unknown expression kind");
                return None;
            }
        }
        Some(())
    }
    #[must_use]
    fn compile_expression_assign(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        mut lhs: EraVarExpr,
        rhs: impl FnOnce(&mut Self, &mut EraBytecodeChunk) -> Option<()>,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<()> {
        use EraBytecodePrimaryType::*;

        let src_info = lhs.src_info;
        let idxs = std::mem::take(&mut lhs.idxs);
        let idx_len = idxs.len();
        self.compile_expression_array(funcs, func_info, lhs, false, chunk)?;
        for idx in idxs {
            self.compile_expression(funcs, func_info, idx, chunk)?;
        }
        rhs(self, chunk)?;
        chunk.emit_bytecode(SetMDArrayVal, src_info);
        chunk.append_u8(idx_len as _, src_info);

        Some(())
    }
    #[must_use]
    fn compile_expression_array(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        var_expr: EraVarExpr,
        decay_arr: bool,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<()> {
        use EraBytecodePrimaryType::*;

        let src_info = var_expr.src_info;
        let is_in_global_frame;
        let var_idx;
        if let Some(var) = func_info.local_vars.get(CaselessStr::new(&var_expr.name)) {
            is_in_global_frame = var.is_in_global_frame;
            var_idx = var.idx_in_frame;
        } else if let Some(var) = self.vars.get_var_idx(&var_expr.name) {
            is_in_global_frame = true;
            var_idx = var;
        } else {
            // HACK: For undefined identifiers, do special handling (such as RAND pesudo-array access)
            if var_expr.name.eq_ignore_ascii_case("RAND") {
                if var_expr.idxs.len() != 1 {
                    self.report_err(
                        src_info,
                        true,
                        format!("`RAND` requires exactly one parameter"),
                    );
                    return None;
                }
                let rand_max = var_expr.idxs.into_iter().next().unwrap();
                self.compile_expression(funcs, func_info, rand_max, chunk)?;
                chunk.emit_bytecode(GetRandomMax, src_info);
                return Some(());
            } else {
                self.report_err(
                    src_info,
                    true,
                    format!("undefined variable `{}`", var_expr.name),
                );
                return None;
            }
        }
        chunk.emit_load_const(self.new_value_int(var_idx as _), src_info);
        if is_in_global_frame {
            chunk.emit_bytecode(GetGlobal, src_info);
        } else {
            chunk.emit_bytecode(GetLocal, src_info);
        }
        // TODO: Check array size & type
        // Decay array values to primitive values
        if decay_arr {
            let idx_len = var_expr.idxs.len();
            for idx in var_expr.idxs {
                self.compile_expression(funcs, func_info, idx, chunk)?;
            }
            chunk.emit_bytecode(GetMDArrayVal, src_info);
            chunk.append_u8(idx_len as _, src_info);
        }
        Some(())
    }
    fn decorate_as_func_local_name(name: &str, func_name: &str) -> String {
        format!("$LOCALVAR_{name}@{func_name}")
    }
    fn report_err<V: Into<String>>(
        &mut self,
        //file_name: String,
        src_info: SourcePosInfo,
        is_error: bool,
        msg: V,
    ) {
        (self.err_report_fn)(&EraCompileErrorInfo {
            //file_name,
            file_name: self.file_name.clone(),
            src_info,
            is_error,
            msg: msg.into(),
        });
    }
    fn evaluate_constant(&mut self, expr: EraExpr) -> Option<EraLiteral> {
        match expr.try_evaluate_constant() {
            Ok(x) => Some(x),
            Err(x) => {
                self.report_err(x.src_info, x.is_error, x.msg);
                None
            }
        }
    }
    fn materialize_constant(&mut self, literal: EraLiteral) -> Value {
        match literal {
            EraLiteral::Integer(x, _) => self.new_value_int(x),
            EraLiteral::String(x, _) => self.new_value_str(x),
        }
    }
    fn unwrap_int_constant(&mut self, literal: EraLiteral) -> Option<i64> {
        match literal {
            EraLiteral::Integer(x, _) => Some(x),
            EraLiteral::String(_, src_info) => {
                self.report_err(src_info, true, "expected integer value, found string");
                None
            }
        }
    }
    fn unwrap_str_constant(&mut self, literal: EraLiteral) -> Option<String> {
        match literal {
            EraLiteral::Integer(_, src_info) => {
                self.report_err(src_info, true, "expected string value, found integer");
                None
            }
            EraLiteral::String(x, _) => Some(x),
        }
    }
    fn materialize_var_decl(
        &mut self,
        mut decl: EraVarDecl,
    ) -> Option<(String, crate::bytecode::Value)> {
        if decl.dims.is_empty() {
            let dim = (decl.inits.len() as u32).max(1);
            decl.dims.push(dim);
        }
        let inits = decl
            .inits
            .into_iter()
            .map(|x| self.evaluate_constant(x))
            .collect::<Option<Vec<_>>>()?;
        Some((
            decl.name,
            if decl.is_string {
                let inits = inits
                    .into_iter()
                    .map(|x| match x {
                        EraLiteral::Integer(_, src_info) => {
                            self.report_err(src_info, true, "expected string value, found integer");
                            None
                        }
                        // TODO: Perform interning
                        EraLiteral::String(val, _) => Some(Rc::new(StrValue { val })),
                    })
                    .collect::<Option<Vec<_>>>()?;
                Value::new_str_arr(decl.dims, inits)
            } else {
                let inits = inits
                    .into_iter()
                    .map(|x| match x {
                        EraLiteral::Integer(val, _) => Some(Rc::new(IntValue { val })),
                        EraLiteral::String(_, src_info) => {
                            self.report_err(src_info, true, "expected integer value, found string");
                            None
                        }
                    })
                    .collect::<Option<Vec<_>>>()?;
                Value::new_int_arr(decl.dims, inits)
            },
        ))
    }
    fn new_value_int(&mut self, value: i64) -> Value {
        use std::collections::hash_map::Entry::*;
        let value = IntValue { val: value };
        match self.intern_vals.entry(either::Either::Left(Rc::new(value))) {
            Occupied(x) => Value::new_int_rc(x.key().clone().unwrap_left()),
            Vacant(x) => {
                let r = Value::new_int_rc(x.key().clone().unwrap_left());
                x.insert(());
                r
            }
        }
    }
    fn new_value_str(&mut self, value: String) -> Value {
        use std::collections::hash_map::Entry::*;
        let value = StrValue { val: value };
        match self
            .intern_vals
            .entry(either::Either::Right(Rc::new(value)))
        {
            Occupied(x) => Value::new_str_rc(x.key().clone().unwrap_right()),
            Vacant(x) => {
                let r = Value::new_str_rc(x.key().clone().unwrap_right());
                x.insert(());
                r
            }
        }
    }
}
