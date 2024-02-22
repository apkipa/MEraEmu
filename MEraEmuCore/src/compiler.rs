use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    rc::Rc,
};

use crate::{
    bytecode::{EraBytecodePrimaryType, FlatValue, PrintExtendedFlags, ValueKind},
    lexer::{EraTokenKind, EraTokenLite},
    parser::{EraCommandStmt, EraFunDecl, EraFunKind, EraStmt, EraTermExpr, EraVarExpr},
    routine,
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

macro_rules! bail_opt {
    ($self:expr, $src_info:expr, $is_error:expr, $msg:expr) => {{
        $self.report_err($src_info, $is_error, $msg);
        return None;
    }};
    ($self:expr, $src_info:expr, $msg:expr) => {{
        $self.report_err($src_info, true, $msg);
        return None;
    }};
    ($self:expr, [$(($src_info:expr, $is_error:expr, $msg:expr)),+]) => {{
        $($self.report_err($src_info, $is_error, $msg);)+
        return None;
    }};
    ($self:expr, [$(($src_info:expr, $msg:expr)),+]) => {{
        $($self.report_err($src_info, true, $msg);)+
        return None;
    }};
    ($self:expr, [($src_info0:expr, $msg0:expr), $(($file_name:expr, $src_info:expr, $msg:expr)),+]) => {{
        $self.report_err($src_info0, true, $msg0);
        $($self.report_far_err($file_name, $src_info, true, $msg);)+
        return None;
    }};
}

#[derive(Default)]
pub struct EraConstantPool {
    pub vals: Vec<Value>,
}

pub struct EraFuncBytecodeInfo {
    pub name: Rc<CaselessStr>,
    pub chunk_idx: u32,
    pub offset: u32,
    pub args: Vec<Value>,
    pub func_kind: EraFunKind,
}

#[derive(Default)]
struct EraFuncPool {
    // Mapping from names to indices.
    func_names: HashMap<Rc<CaselessStr>, usize>,
    funcs: Vec<EraFuncInfo>,
}
struct EraFuncInfo {
    name: Rc<CaselessStr>,
    kind: EraFunKind,
    params: Vec<EraFuncArgInfo>,
    local_vars: HashMap<Rc<CaselessStr>, EraFuncLocalVarInfo>,
    local_frame_size: usize,
    local_size: Option<u32>,
    file_name: Rc<str>,
    src_info: SourcePosInfo,
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
struct EraBytecodeChunkSnapshot {
    len: usize,
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
            write_fn: &|this, idx, offset| {
                this.overwrite_u16(i16::try_from(offset).ok()? as _, idx)
            },
        }
    }
    fn emit_jump_cond_hold(&mut self, src_info: SourcePosInfo) -> EraBytecodeChunkBacktrackHolder {
        let base_idx = self.cur_bytes_cnt();
        self.emit_bytecode(EraBytecodePrimaryType::JumpCondW, src_info);
        self.append_u16(0xffff, src_info);
        EraBytecodeChunkBacktrackHolder {
            base_idx,
            write_fn: &|this, idx, offset| {
                this.overwrite_u16(i16::try_from(offset).ok()? as _, idx)
            },
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
    fn emit_duplicate_n(&mut self, count: u8, src_info: SourcePosInfo) {
        self.emit_bytecode(EraBytecodePrimaryType::DuplicateN, src_info);
        self.append_u8(count, src_info);
    }
    fn emit_duplicate_one_n(&mut self, count: u8, src_info: SourcePosInfo) {
        self.emit_bytecode(EraBytecodePrimaryType::DuplicateOneN, src_info);
        self.append_u8(count, src_info);
    }
    fn take_snapshot(&self) -> EraBytecodeChunkSnapshot {
        EraBytecodeChunkSnapshot {
            len: self.bytecode.len(),
        }
    }
}
impl EraBytecodeChunkBacktrackHolder {
    fn source_pos_info(&self, chunk: &EraBytecodeChunk) -> Option<SourcePosInfo> {
        chunk.source_info_at(self.base_idx)
    }
    fn complete(self, chunk: &mut EraBytecodeChunk) -> Option<()> {
        let pos = chunk.cur_bytes_cnt();
        self.complete_at(chunk, pos)
    }
    fn complete_at(self, chunk: &mut EraBytecodeChunk, pos: usize) -> Option<()> {
        let offset = isize::try_from(pos)
            .ok()?
            .checked_sub_unsigned(self.base_idx)?;
        // 1 is bytecode size
        (self.write_fn)(chunk, self.base_idx + 1, offset)
    }
}
impl EraBytecodeChunkSnapshot {
    fn regret(self, chunk: &mut EraBytecodeChunk) {
        chunk.bytecode.truncate(self.len);
        chunk.src_infos.truncate(self.len);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
// NOTE: We deliberately do not expose array types
enum EraExpressionValueKind {
    TVoid,
    TInteger,
    TString,
}
impl EraExpressionValueKind {
    fn expect_int<T: FnMut(&EraCompileErrorInfo)>(
        self,
        that: &mut EraCompilerImpl<'_, T>,
        si: SourcePosInfo,
    ) -> Option<()> {
        if matches!(self, Self::TInteger) {
            Some(())
        } else {
            that.report_err(si, true, "expected expression of type integer");
            None
        }
    }
    fn expect_str<T: FnMut(&EraCompileErrorInfo)>(
        self,
        that: &mut EraCompilerImpl<'_, T>,
        si: SourcePosInfo,
    ) -> Option<()> {
        if matches!(self, Self::TString) {
            Some(())
        } else {
            that.report_err(si, true, "expected expression of type string");
            None
        }
    }
    fn expect_void<T: FnMut(&EraCompileErrorInfo)>(
        self,
        that: &mut EraCompilerImpl<'_, T>,
        si: SourcePosInfo,
    ) -> Option<()> {
        if matches!(self, Self::TVoid) {
            Some(())
        } else {
            that.report_err(si, true, "expected expression of type void");
            None
        }
    }
}

struct EraLoopStructCodeMetadata {
    continue_queue: Vec<EraBytecodeChunkBacktrackHolder>,
    done_queue: Vec<EraBytecodeChunkBacktrackHolder>,
}
impl EraLoopStructCodeMetadata {
    fn new() -> Self {
        EraLoopStructCodeMetadata {
            continue_queue: Vec::new(),
            done_queue: Vec::new(),
        }
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

struct EraGotoLabelInfo {
    stack_balance: usize,
    pos: usize,
    src_info: SourcePosInfo,
}
struct EraGotoJumpInfo {
    backtrack: EraBytecodeChunkBacktrackHolder,
    stack_balance: usize,
    target: CaselessString,
}

pub struct EraCompilerImpl<'a, ErrReportFn> {
    err_report_fn: &'a mut ErrReportFn,
    file_name: Rc<str>,
    // funcs: EraFuncPool,
    vars: EraVarPool,
    // local_vars: Option<EraFunctionFrame>,
    loop_structs: Vec<EraLoopStructCodeMetadata>,
    intern_vals: HashMap<either::Either<Rc<IntValue>, Rc<StrValue>>, ()>,
    stack_balance: usize,
    goto_labels: HashMap<CaselessString, EraGotoLabelInfo>,
    goto_backtracks: Vec<EraGotoJumpInfo>,
    final_return_backtracks: Vec<EraBytecodeChunkBacktrackHolder>,
    contextual_indices: HashMap<Ascii<String>, u32>,
}

impl<T: FnMut(&EraCompileErrorInfo)> EraCompiler<T> {
    pub fn new(err_report_fn: T) -> Self {
        Self { err_report_fn }
    }
    pub fn compile_all(
        &mut self,
        inputs: Vec<EraCompilerFileInput>,
        global_vars: EraVarPool,
        contextual_indices: HashMap<Ascii<String>, u32>,
    ) -> Option<EraBytecodeCompilation> {
        EraCompilerImpl::new(self, contextual_indices).compile_all(inputs, global_vars)
    }
}

impl<'a, T: FnMut(&EraCompileErrorInfo)> EraCompilerImpl<'a, T> {
    fn new(
        parser: &'a mut EraCompiler<T>,
        contextual_indices: HashMap<Ascii<String>, u32>,
    ) -> Self {
        EraCompilerImpl {
            err_report_fn: &mut parser.err_report_fn,
            file_name: "".into(),
            vars: Default::default(),
            loop_structs: Vec::new(),
            intern_vals: HashMap::new(),
            stack_balance: 0,
            goto_labels: HashMap::new(),
            goto_backtracks: Vec::new(),
            final_return_backtracks: Vec::new(),
            contextual_indices,
        }
    }
    fn compile_all(
        &mut self,
        inputs: Vec<EraCompilerFileInput>,
        global_vars: EraVarPool,
    ) -> Option<EraBytecodeCompilation> {
        // let mut const_vars = HashSet::new();
        let mut fun_decls = Vec::new();
        let mut global_funcs = EraFuncPool::default();

        self.vars = global_vars;

        let empty_str = self.file_name.clone();

        // Build global variables and functions list
        for input in inputs {
            self.file_name = input.file_name.into();

            for decl in input.root_node.decls {
                match decl {
                    EraDecl::SharpDecl(x) => match x {
                        EraSharpDecl::VarDecl(x) => {
                            // let src_info = x.src_info;
                            // if x.is_dynamic || x.is_ref {
                            //     self.report_err(
                            //         src_info,
                            //         true,
                            //         "invalid qualifier for file-scope variable",
                            //     );
                            //     return None;
                            // }
                            // let is_const = x.is_const;
                            // let (name, var_val) = self.materialize_var_decl(x)?;
                            // let var_idx = match self.vars.add_var(&name, var_val) {
                            //     Some(x) => x,
                            //     None => {
                            //         self.report_err(
                            //             src_info,
                            //             true,
                            //             format!("redefinition of variable `{name}`"),
                            //         );
                            //         return None;
                            //     }
                            // };
                            // if is_const {
                            //     const_vars.insert(name);
                            // }
                        }
                        EraSharpDecl::DefineDecl(_) => (),
                        _ => {
                            self.report_err(
                                x.source_pos_info(),
                                false,
                                "this kind of declaration should not appear here; ignoring",
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
        self.file_name = empty_str;

        // Parse function signatures
        for (file_name, fun) in fun_decls.iter_mut() {
            std::mem::swap(&mut self.file_name, file_name);

            // Rewrite event function name
            if let Some(kind) = fun.event_kind {
                use crate::parser::EraEventFunKind::*;
                fun.name += match kind {
                    Only => "@ONLY",
                    Pri => "@PRI",
                    Normal => "@NORMAL",
                    Later => "@LATER",
                };
            }

            let func_name = CaselessStr::new(fun.name.deref()).into();
            let fun_idx = global_funcs.funcs.len();
            let mut local_vars = HashMap::new();
            let mut params = Vec::new();
            match global_funcs.func_names.entry(Rc::clone(&func_name)) {
                std::collections::hash_map::Entry::Occupied(e) => {
                    let old_func = *e.get();
                    let old_func = &global_funcs.funcs[old_func];
                    // HACK: Ignore error and skip this function
                    // TODO: Refactor error reporting mechanism
                    _ = (|| -> Option<()> {
                        bail_opt!(
                            self,
                            [
                                (
                                    fun.src_info,
                                    format!("redefinition of function `{func_name}`")
                                ),
                                (
                                    &old_func.file_name,
                                    old_func.src_info,
                                    "note: see previous definition of function"
                                )
                            ]
                        );
                    })();
                    continue;
                }
                std::collections::hash_map::Entry::Vacant(e) => {
                    e.insert(fun_idx);
                }
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
                    EraSharpDecl::VarDecl(mut x) => {
                        src_info = x.src_info;
                        let has_init;
                        let init_val = if x.is_ref {
                            if !x.dims.is_empty() && x.dims[0] != 0 {
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
                            if x.dims.is_empty() {
                                x.dims = vec![(x.inits.len() as u32).max(1)];
                            }
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
                                            .map(|val| IntValue { val })
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
                if self.vars.get_var(&var_name).is_some() {
                    self.report_err(
                        local_decl.src_info,
                        false,
                        "this declaration shadows a global variable with the same name",
                    );
                }
                if !local_decl.is_dynamic && !local_decl.is_ref {
                    let var_name = Self::decorate_as_func_local_name(&var_name, func_name.as_str());
                    local_decl.idx_in_frame = match self
                        .vars
                        .add_var(&var_name, local_decl.init_val.deep_clone())
                    {
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
                            let lhs_name = CaselessStr::new(&lhs.name);
                            let kind = local_vars
                                .get(lhs_name)
                                .map(|x| match x.is_ref {
                                    true => x.init_val.kind().with_arr(),
                                    false => x.init_val.kind().without_arr(),
                                })
                                .or_else(|| {
                                    self.vars.get_var(&lhs.name).map(|x| x.kind().without_arr())
                                })
                                .or_else(|| {
                                    self.vars
                                        .get_var(&format!("{}@{}", lhs_name, func_name))
                                        .map(|x| x.kind().without_arr())
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
                                target_var: (lhs_name.into(), idxs),
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
                        let lhs_name = CaselessStr::new(&lhs.name);
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
                            })
                            .or_else(|| {
                                self.vars
                                    .get_var(&format!("{}@{}", lhs_name, func_name))
                                    .map(|x| x.kind().without_arr())
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
                            target_var: (lhs_name.into(), idxs),
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

            // TODO: LocalSize & LocalSSize support

            global_funcs.funcs.push(EraFuncInfo {
                name: func_name,
                kind: fun.kind,
                params,
                local_vars,
                local_frame_size: local_frame_var_counter,
                local_size: None,
                file_name: self.file_name.clone(),
                src_info: fun.src_info,
            });

            std::mem::swap(&mut self.file_name, file_name);
        }

        let mut funcs = Vec::new();
        let mut chunks_idxs: HashMap<Rc<str>, usize> = HashMap::new();
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
            args: func_info
                .params
                .iter()
                .map(|x| x.default_val.clone())
                .collect(),
            func_kind: func_decl.kind,
        };

        // Reset GOTO context
        self.stack_balance = 0;
        self.goto_labels = HashMap::new();
        self.goto_backtracks = Vec::new();

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

            let rhs_kind = match param_info.default_val.kind() {
                ValueKind::Int => EraExpressionValueKind::TInteger,
                ValueKind::Str => EraExpressionValueKind::TString,
                ValueKind::ArrInt | ValueKind::ArrStr => {
                    // Skip ref parameter
                    continue;
                }
            };

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
                    Some(rhs_kind)
                },
                chunk,
            )?;
            chunk.emit_bytecode(Pop, src_info);
        }

        // Compile function body
        for stmt in func_decl.body {
            self.compile_statement(funcs, func_info, func_decl.kind, stmt, chunk)?;
        }

        // Return
        for bt in std::mem::take(&mut self.final_return_backtracks) {
            bt.complete(chunk)?;
        }
        match func_decl.kind {
            EraFunKind::Function => {
                let val = self.new_value_int(0);
                chunk.emit_load_const(val, func_decl.src_info);
                chunk.emit_bytecode(ReturnInteger, func_decl.src_info);
            }
            EraFunKind::FunctionS => {
                let val = self.new_value_str(String::new());
                chunk.emit_load_const(val, func_decl.src_info);
                chunk.emit_bytecode(ReturnString, func_decl.src_info);
            }
            EraFunKind::Procedure => {
                chunk.emit_bytecode(ReturnVoid, func_decl.src_info);
            }
        }

        // Complete GOTOs
        _ = std::mem::take(&mut self.stack_balance);
        let goto_labels = std::mem::take(&mut self.goto_labels);
        let goto_backtracks = std::mem::take(&mut self.goto_backtracks);
        for backtrack in goto_backtracks {
            let src_si = backtrack.backtrack.source_pos_info(chunk)?;
            let dst_si;
            let Some(target) = goto_labels.get(&backtrack.target) else {
                bail_opt!(
                    self,
                    [
                        (
                            src_si,
                            format!(
                                "label `${}` not defined in current function",
                                backtrack.target
                            )
                        ),
                        (func_info.src_info, "note: function definition starts here")
                    ]
                );
            };
            dst_si = target.src_info;
            if backtrack.stack_balance != target.stack_balance {
                bail_opt!(
                    self,
                    [
                        (
                            src_si,
                            "this jump will corrupt the integrity of control flow"
                        ),
                        (dst_si, "note: see the destination of this jump")
                    ]
                );
            }
            backtrack.backtrack.complete_at(chunk, target.pos)?;
        }

        Some(bytecode_info)
    }
    #[must_use]
    fn compile_statement(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        func_kind: EraFunKind,
        stmt: EraStmt,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<()> {
        use EraBytecodePrimaryType::*;
        use EraCommandStmt as Cmd;
        use EraExpressionValueKind::*;

        let stmt = match stmt {
            EraStmt::Expr(x) => {
                // Evaluate and discard values
                let src_info = x.source_pos_info();
                self.compile_expression(funcs, func_info, x, chunk)?;
                chunk.emit_bytecode(Pop, src_info);
                return Some(());
            }
            EraStmt::Command(x) => x,
            EraStmt::Label(x) => {
                let label = EraGotoLabelInfo {
                    stack_balance: self.stack_balance,
                    pos: chunk.cur_bytes_cnt(),
                    src_info: x.src_info,
                };
                if let Some(label) = self.goto_labels.insert(x.name.into(), label) {
                    let si = label.src_info;
                    bail_opt!(
                        self,
                        [
                            (x.src_info, "redefinition of label"),
                            (si, "note: see the previous definition of label")
                        ]
                    );
                }
                return Some(());
            }
            EraStmt::RowAssign(x) => {
                todo!()
            }
            EraStmt::Invalid(src_info) => {
                chunk.emit_load_const(self.new_value_str("invalid statement".to_owned()), src_info);
                chunk.emit_bytecode(InvalidWithMessage, src_info);
                return Some(());
            }
        };

        match stmt {
            Cmd::DebugPrint(x) => {
                // TODO: Cmd::DebugPrint
                // Ignore debug print for now
            }
            Cmd::Print(x) => {
                let src_info = x.src_info;
                let args_cnt = x.vals.len();
                for v in x.vals {
                    let src_info = v.source_pos_info();
                    match self.compile_expression(funcs, func_info, v, chunk)? {
                        TInteger => chunk.emit_bytecode(ConvertToString, src_info),
                        TString => (),
                        TVoid => bail_opt!(self, src_info, "cannot print void"),
                    }
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
            Cmd::PrintData(x) => {
                todo!();
            }
            Cmd::Wait(x) => {
                chunk.emit_bytecode(Wait, x.src_info);
            }
            Cmd::If(x) => {
                // TODO: Optimize when there is no else body
                self.compile_expression(funcs, func_info, x.cond, chunk)?;
                let backtrack_then = chunk.emit_jump_cond_hold(x.src_info);
                for stmt in x.else_body {
                    self.compile_statement(funcs, func_info, func_kind, stmt, chunk)?;
                }
                // HACK: Steal source pos info from last statement
                let last_src_info = chunk.source_info_at(chunk.cur_bytes_cnt() - 1).unwrap();
                let backtrack_done = chunk.emit_jump_hold(last_src_info);
                if backtrack_then.complete(chunk).is_none() {
                    self.report_err(x.src_info, true, "jump too far to be encoded in bytecode");
                    return None;
                }
                for stmt in x.body {
                    self.compile_statement(funcs, func_info, func_kind, stmt, chunk)?;
                }
                if backtrack_done.complete(chunk).is_none() {
                    self.report_err(x.src_info, true, "jump too far to be encoded in bytecode");
                    return None;
                }
            }
            Cmd::Quit(x) => {
                chunk.emit_bytecode(Quit, x.src_info);
            }
            Cmd::SelectCase(x) => {
                self.stack_balance += 1;

                let cond_k = self.compile_expression(funcs, func_info, x.cond, chunk)?;
                if let TVoid = cond_k {
                    bail_opt!(
                        self,
                        x.src_info,
                        "SELECTCASE does not accept void expressions"
                    );
                }
                let mut bt_dones = Vec::new();
                let mut bt_else: Option<EraBytecodeChunkBacktrackHolder> = None;
                for (case_conds, case_body) in x.cases {
                    if let Some(bt) = bt_else.take() {
                        bt.complete(chunk)?;
                    }
                    let mut bt_bodies = Vec::new();
                    for case_cond in case_conds {
                        use crate::parser::EraSelectCaseStmtCondition::*;
                        chunk.emit_bytecode(Duplicate, x.src_info);
                        match case_cond {
                            Single(x) => {
                                let x_si = x.source_pos_info();
                                if cond_k != self.compile_expression(funcs, func_info, x, chunk)? {
                                    bail_opt!(self, x_si, "case expression type mismatch");
                                }
                                chunk.emit_bytecode(CompareEq, x_si);
                            }
                            Range(lhs, rhs) => {
                                // No short-circuit within single range?
                                let lhs_si = lhs.source_pos_info();
                                if cond_k
                                    != self.compile_expression(funcs, func_info, lhs, chunk)?
                                {
                                    bail_opt!(self, lhs_si, "case expression type mismatch");
                                }
                                chunk.emit_bytecode(CompareL, lhs_si);
                                chunk.emit_bytecode(LogicalNot, lhs_si);
                                chunk.emit_bytecode(DuplicateN, lhs_si);
                                chunk.append_u8(2, lhs_si);
                                chunk.emit_bytecode(Pop, lhs_si);
                                let rhs_si = rhs.source_pos_info();
                                if cond_k
                                    != self.compile_expression(funcs, func_info, rhs, chunk)?
                                {
                                    bail_opt!(self, rhs_si, "case expression type mismatch");
                                }
                                chunk.emit_bytecode(CompareLEq, rhs_si);
                                chunk.emit_bytecode(BitAnd, rhs_si);
                            }
                            Condition(op, x) => {
                                let x_si = x.source_pos_info();
                                if cond_k != self.compile_expression(funcs, func_info, x, chunk)? {
                                    bail_opt!(self, x_si, "case expression type mismatch");
                                }
                                let op_si = op.src_info;
                                match op.kind {
                                    EraTokenKind::CmpL => chunk.emit_bytecode(CompareL, op_si),
                                    EraTokenKind::CmpLEq => chunk.emit_bytecode(CompareLEq, op_si),
                                    EraTokenKind::CmpEq => chunk.emit_bytecode(CompareEq, op_si),
                                    EraTokenKind::CmpNEq => {
                                        chunk.emit_bytecode(CompareEq, op_si);
                                        chunk.emit_bytecode(LogicalNot, op_si);
                                    }
                                    EraTokenKind::CmpG => {
                                        chunk.emit_bytecode(CompareLEq, op_si);
                                        chunk.emit_bytecode(LogicalNot, op_si);
                                    }
                                    EraTokenKind::CmpGEq => {
                                        chunk.emit_bytecode(CompareL, op_si);
                                        chunk.emit_bytecode(LogicalNot, op_si);
                                    }
                                    _ => {
                                        bail_opt!(self, op_si, "unsupported operator as case")
                                    }
                                }
                            }
                        }
                        bt_bodies.push(chunk.emit_jump_cond_hold(x.src_info));
                    }
                    bt_else = Some(chunk.emit_jump_hold(x.src_info));
                    for bt in bt_bodies {
                        bt.complete(chunk)?;
                    }
                    for stmt in case_body {
                        self.compile_statement(funcs, func_info, func_kind, stmt, chunk)?;
                    }
                    bt_dones.push(chunk.emit_jump_hold(x.src_info));
                }
                if let Some(bt) = bt_else {
                    bt.complete(chunk)?;
                }
                for stmt in x.case_else {
                    self.compile_statement(funcs, func_info, func_kind, stmt, chunk)?;
                }
                for bt in bt_dones {
                    bt.complete(chunk)?;
                }
                chunk.emit_bytecode(Pop, x.src_info);

                self.stack_balance -= 1;
            }
            Cmd::While(x) => {
                let start_pos = chunk.cur_bytes_cnt();
                self.loop_structs.push(EraLoopStructCodeMetadata::new());
                self.compile_expression(funcs, func_info, x.cond, chunk)?;
                chunk.emit_bytecode(LogicalNot, x.src_info);
                let backtrack_done = chunk.emit_jump_cond_hold(x.src_info);
                for stmt in x.body {
                    self.compile_statement(funcs, func_info, func_kind, stmt, chunk)?;
                }
                let done_index = chunk.cur_bytes_cnt();
                // HACK: Steal source pos info from last statement
                let last_src_info = chunk.source_info_at(chunk.cur_bytes_cnt() - 1).unwrap();
                chunk.emit_jump(-((done_index - start_pos) as isize), last_src_info);
                if backtrack_done.complete(chunk).is_none() {
                    bail_opt!(self, x.src_info, "jump too far to be encoded in bytecode");
                }
                let loop_struct = self.loop_structs.pop().unwrap();
                for bt in loop_struct.continue_queue {
                    bt.complete_at(chunk, start_pos)?;
                }
                for bt in loop_struct.done_queue {
                    bt.complete(chunk)?;
                }
            }
            Cmd::Call(x) => match x.func {
                // Try to optimize into a static call
                EraExpr::Term(EraTermExpr::Literal(EraLiteral::String(func, si))) => match self
                    .compile_static_fun_call(funcs, func_info, &func, x.args, si, chunk)?
                {
                    TInteger | TString => chunk.emit_bytecode(Pop, x.src_info),
                    TVoid => (),
                },
                _ => {
                    self.compile_dynamic_fun_call(funcs, func_info, x.func, x.args, chunk)?;
                    chunk.emit_bytecode(Pop, x.src_info);
                }
            },
            Cmd::TryCall(x) => {
                // Enforce dynamic call
                self.compile_dynamic_fun_call(funcs, func_info, x.func, x.args, chunk)?;
                chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::TryCCall(x) => {
                // Enforce dynamic call
                self.compile_dynamic_fun_call(funcs, func_info, x.func, x.args, chunk)?;
                let bt_then = chunk.emit_jump_cond_hold(x.src_info);
                for stmt in x.catch_body {
                    self.compile_statement(funcs, func_info, func_kind, stmt, chunk)?;
                }
                // HACK: Steal source pos info from last statement
                let last_src_info = chunk.source_info_at(chunk.cur_bytes_cnt() - 1).unwrap();
                let bt_done = chunk.emit_jump_hold(last_src_info);
                if bt_then.complete(chunk).is_none() {
                    bail_opt!(self, x.src_info, "jump too far to be encoded in bytecode");
                }
                for stmt in x.then_body {
                    self.compile_statement(funcs, func_info, func_kind, stmt, chunk)?;
                }
                if bt_done.complete(chunk).is_none() {
                    bail_opt!(self, x.src_info, "jump too far to be encoded in bytecode");
                }
            }
            Cmd::Jump(x) => {
                let si = x.src_info;
                self.compile_statement(
                    funcs,
                    func_info,
                    func_kind,
                    EraStmt::Command(Cmd::Call(x)),
                    chunk,
                )?;
                self.final_return_backtracks.push(chunk.emit_jump_hold(si));
            }
            Cmd::TryJump(x) => {
                let si = x.src_info;
                self.compile_statement(
                    funcs,
                    func_info,
                    func_kind,
                    EraStmt::Command(Cmd::TryCall(x)),
                    chunk,
                )?;
                self.final_return_backtracks.push(chunk.emit_jump_hold(si));
            }
            Cmd::TryCJump(x) => {
                let si = x.src_info;
                self.compile_statement(
                    funcs,
                    func_info,
                    func_kind,
                    EraStmt::Command(Cmd::TryCCall(x)),
                    chunk,
                )?;
                self.final_return_backtracks.push(chunk.emit_jump_hold(si));
            }
            Cmd::Return(x) => {
                // NOTE: EraBasic only assigns values to RESULT:*
                if let EraFunKind::Procedure = func_kind {
                    // TODO: Optimize RETURN
                    for (val_idx, val) in x.vals.into_iter().enumerate() {
                        let src_info = val.source_pos_info();
                        let dst = EraVarExpr {
                            name: "RESULT".to_owned(),
                            idxs: vec![EraExpr::Term(EraTermExpr::Literal(EraLiteral::Integer(
                                val_idx as _,
                                src_info,
                            )))],
                            src_info: val.source_pos_info(),
                        };
                        _ = self.compile_expression_assign(
                            funcs,
                            func_info,
                            dst,
                            |this, chunk| {
                                let v = this.compile_expression(funcs, func_info, val, chunk)?;
                                if let TString = v {
                                    chunk.emit_bytecode(ConvertToInteger, src_info);
                                }
                                Some(TInteger)
                            },
                            chunk,
                        )?;
                    }
                    chunk.emit_bytecode(ReturnVoid, x.src_info);
                } else {
                    let src_info = x.src_info;
                    let Ok([val]) = TryInto::<[_; 1]>::try_into(x.vals) else {
                        bail_opt!(self, src_info, "invalid return type for current function");
                    };
                    let val_kind = self.compile_expression(funcs, func_info, val, chunk)?;
                    match (func_kind, val_kind) {
                        (EraFunKind::Function, TInteger) => {
                            chunk.emit_bytecode(ReturnInteger, src_info);
                        }
                        (EraFunKind::FunctionS, TString) => {
                            chunk.emit_bytecode(ReturnString, src_info);
                        }
                        _ => bail_opt!(self, src_info, "invalid return type for current function"),
                    }
                }
            }
            Cmd::Continue(x) => {
                let Some(loop_struct) = self.loop_structs.last_mut() else {
                    bail_opt!(self, x.src_info, "loop controlling statements are valid only in the context of a loop structure");
                };
                // let pos_delta = (chunk.cur_bytes_cnt() - loop_struct.continue_pos) as isize;
                // chunk.emit_jump(-pos_delta, x.src_info);
                loop_struct
                    .continue_queue
                    .push(chunk.emit_jump_hold(x.src_info));
            }
            Cmd::Break(x) => {
                let Some(loop_struct) = self.loop_structs.last_mut() else {
                    bail_opt!(self, x.src_info, "loop controlling statements are valid only in the context of a loop structure");
                };
                loop_struct
                    .done_queue
                    .push(chunk.emit_jump_hold(x.src_info));
            }
            Cmd::Throw(x) => {
                self.compile_expression(funcs, func_info, x.val, chunk)?;
                chunk.emit_bytecode(Throw, x.src_info);
            }
            Cmd::Repeat(x) => {
                // for (loopCount = ?, COUNT = 0; COUNT < loopCount; COUNT++) {}
                // Use stack to store temporary loopCount
                self.stack_balance += 1;

                match self.compile_expression(funcs, func_info, x.loop_cnt, chunk)? {
                    TInteger => (),
                    TString | TVoid => {
                        bail_opt!(self, x.src_info, "invalid expression for REPEAT condition")
                    }
                }
                let count_var = EraVarExpr {
                    name: "COUNT".to_owned(),
                    idxs: Vec::new(),
                    src_info: x.src_info,
                };
                self.compile_expression_assign(
                    funcs,
                    func_info,
                    count_var.clone(),
                    |this, chunk| {
                        chunk.emit_load_const(Value::new_int(0), x.src_info);
                        Some(TInteger)
                    },
                    chunk,
                )?;
                chunk.emit_bytecode(Pop, x.src_info);
                let bt_body = chunk.emit_jump_hold(x.src_info);
                let start_pos = chunk.cur_bytes_cnt();
                self.loop_structs.push(EraLoopStructCodeMetadata::new());
                self.compile_expression(
                    funcs,
                    func_info,
                    EraExpr::PreUnary(
                        EraTokenLite {
                            kind: EraTokenKind::Increment,
                            src_info: x.src_info,
                        },
                        Box::new(EraExpr::Term(EraTermExpr::Var(count_var.clone()))),
                    ),
                    chunk,
                )?;
                chunk.emit_bytecode(Pop, x.src_info);
                bt_body.complete(chunk)?;
                chunk.emit_bytecode(Duplicate, x.src_info);
                self.compile_expression_array(funcs, func_info, count_var.clone(), true, chunk)?;
                chunk.emit_bytecode(CompareLEq, x.src_info);
                let bt_done = chunk.emit_jump_cond_hold(x.src_info);
                for stmt in x.body {
                    self.compile_statement(funcs, func_info, func_kind, stmt, chunk)?;
                }
                {
                    let offset = -((chunk.cur_bytes_cnt() - start_pos) as isize);
                    chunk.emit_jump(offset, x.src_info);
                }
                let loop_struct = self.loop_structs.pop().unwrap();
                for bt in loop_struct.continue_queue {
                    bt.complete_at(chunk, start_pos)?;
                }
                for bt in loop_struct.done_queue {
                    bt.complete(chunk)?;
                }
                // NOTE: Emulates Eramaker behavior (inc COUNT even when break'ing)
                self.compile_expression(
                    funcs,
                    func_info,
                    EraExpr::PreUnary(
                        EraTokenLite {
                            kind: EraTokenKind::Increment,
                            src_info: x.src_info,
                        },
                        Box::new(EraExpr::Term(EraTermExpr::Var(count_var.clone()))),
                    ),
                    chunk,
                )?;
                bt_done.complete(chunk)?;
                chunk.emit_bytecode(Pop, x.src_info);

                self.stack_balance -= 1;
            }
            Cmd::Goto(x) => {
                let backtrack = chunk.emit_jump_hold(x.src_info);
                self.goto_backtracks.push(EraGotoJumpInfo {
                    backtrack,
                    stack_balance: self.stack_balance,
                    target: x.target.into(),
                });
            }
            Cmd::For(x) => {
                // var(2) + end(1) + step(1)
                self.stack_balance += 4;

                match self.compile_expression_array_with_idx(funcs, func_info, x.var, chunk)? {
                    TInteger => (),
                    TString | TVoid => {
                        bail_opt!(self, x.src_info, "invalid expression for FOR statement")
                    }
                }
                chunk.emit_duplicate_n(2, x.src_info);
                match self.compile_expression(funcs, func_info, x.start, chunk)? {
                    TInteger => (),
                    TString | TVoid => {
                        bail_opt!(self, x.src_info, "invalid expression for FOR statement")
                    }
                }
                chunk.emit_bytecode(SetArrayVal, x.src_info);
                chunk.emit_bytecode(Pop, x.src_info);
                match self.compile_expression(funcs, func_info, x.end, chunk)? {
                    TInteger => (),
                    TString | TVoid => {
                        bail_opt!(self, x.src_info, "invalid expression for FOR statement")
                    }
                }
                match self.compile_expression(funcs, func_info, x.step, chunk)? {
                    TInteger => (),
                    TString | TVoid => {
                        bail_opt!(self, x.src_info, "invalid expression for FOR statement")
                    }
                }
                let emit_ternary_fn = |chunk: &mut EraBytecodeChunk,
                                       src_info,
                                       then_fn: &mut dyn Fn(
                    &mut EraBytecodeChunk,
                    SourcePosInfo,
                ),
                                       else_fn: &mut dyn Fn(
                    &mut EraBytecodeChunk,
                    SourcePosInfo,
                )| {
                    // Assuming stack has condition pushed
                    let bt_then = chunk.emit_jump_cond_hold(src_info);
                    else_fn(chunk, src_info);
                    let bt_else = chunk.emit_jump_hold(src_info);
                    bt_then.complete(chunk);
                    then_fn(chunk, src_info);
                    bt_else.complete(chunk);
                };
                let zero_val = self.new_value_int(0);
                let emit_cond_fn = |chunk: &mut EraBytecodeChunk, src_info| {
                    chunk.emit_duplicate_n(2, src_info);
                    chunk.emit_load_const(zero_val.clone(), src_info);
                    chunk.emit_bytecode(CompareL, src_info);
                    emit_ternary_fn(
                        chunk,
                        x.src_info,
                        &mut |chunk, src_info| {
                            chunk.emit_duplicate_one_n(5, src_info);
                            chunk.emit_duplicate_one_n(5, src_info);
                            chunk.emit_bytecode(GetArrayVal, src_info);
                            chunk.emit_bytecode(CompareL, src_info);
                            chunk.emit_bytecode(LogicalNot, src_info);
                        },
                        &mut |chunk, src_info| {
                            chunk.emit_duplicate_one_n(5, src_info);
                            chunk.emit_duplicate_one_n(5, src_info);
                            chunk.emit_bytecode(GetArrayVal, src_info);
                            chunk.emit_bytecode(CompareLEq, src_info);
                        },
                    );
                };
                let emit_step_fn = |chunk: &mut EraBytecodeChunk, src_info| {
                    chunk.emit_duplicate_one_n(4, src_info);
                    chunk.emit_duplicate_one_n(4, src_info);
                    chunk.emit_duplicate_n(2, src_info);
                    chunk.emit_bytecode(GetArrayVal, src_info);
                    chunk.emit_duplicate_one_n(4, src_info);
                    chunk.emit_bytecode(Add, src_info);
                    chunk.emit_bytecode(SetArrayVal, src_info);
                    chunk.emit_bytecode(Pop, src_info);
                };
                let bt_body = chunk.emit_jump_hold(x.src_info);
                let start_pos = chunk.cur_bytes_cnt();
                self.loop_structs.push(EraLoopStructCodeMetadata::new());
                emit_step_fn(chunk, x.src_info);
                bt_body.complete(chunk)?;
                emit_cond_fn(chunk, x.src_info);
                let bt_done = chunk.emit_jump_cond_hold(x.src_info);
                for stmt in x.body {
                    self.compile_statement(funcs, func_info, func_kind, stmt, chunk)?;
                }
                {
                    let offset = -((chunk.cur_bytes_cnt() - start_pos) as isize);
                    chunk.emit_jump(offset, x.src_info);
                }
                let loop_struct = self.loop_structs.pop().unwrap();
                for bt in loop_struct.continue_queue {
                    bt.complete_at(chunk, start_pos)?;
                }
                for bt in loop_struct.done_queue {
                    bt.complete(chunk)?;
                }
                // NOTE: Emulates Eramaker behavior (inc COUNT even when break'ing)
                emit_step_fn(chunk, x.src_info);
                bt_done.complete(chunk)?;
                chunk.emit_bytecode(Pop, x.src_info);
                chunk.emit_bytecode(Pop, x.src_info);
                chunk.emit_bytecode(Pop, x.src_info);
                chunk.emit_bytecode(Pop, x.src_info);

                self.stack_balance -= 4;
            }
            Cmd::DoLoop(x) => {
                let si = x.src_info;
                let start_pos = chunk.cur_bytes_cnt();
                self.loop_structs.push(EraLoopStructCodeMetadata::new());
                for stmt in x.body {
                    self.compile_statement(funcs, func_info, func_kind, stmt, chunk)?;
                }
                let continue_pos = chunk.cur_bytes_cnt();
                self.compile_expression(funcs, func_info, x.cond, chunk)?;
                chunk.emit_jump_cond_hold(si).complete_at(chunk, start_pos);
                let loop_struct = self.loop_structs.pop().unwrap();
                for bt in loop_struct.continue_queue {
                    bt.complete_at(chunk, continue_pos)?;
                }
                for bt in loop_struct.done_queue {
                    bt.complete(chunk)?;
                }
            }
            Cmd::Split(x) => {
                self.expression_str(funcs, func_info, x.input, chunk)?;
                self.expression_str(funcs, func_info, x.separator, chunk)?;
                self.array_str(funcs, func_info, x.dest, chunk)?;
                self.array_int(funcs, func_info, x.dest_count, chunk)?;
                chunk.emit_bytecode(SplitString, x.src_info);
            }
            Cmd::ResultCmdCall(x) => {
                let mut should_assign = false;
                // TODO: Refactor Cmd::ResultCmdCall
                match self.compile_builtin_fun_call(
                    funcs, func_info, &x.name, x.args, x.src_info, true, chunk,
                ) {
                    Some(_) => {
                        if should_assign {
                            chunk.emit_bytecode(SetMDArrayVal, x.src_info);
                            chunk.emit_bytecode(Pop, x.src_info);
                        }
                    }
                    None => bail_opt!(self, x.src_info, "invalid statement call"),
                }
            }
            // TODO...
            Cmd::Swap(x) => {
                let k1 = self.compile_expression_array_with_idx(funcs, func_info, x.v1, chunk)?;
                let k2 = self.compile_expression_array_with_idx(funcs, func_info, x.v2, chunk)?;
                if k1 != k2 || matches!(k1, TVoid) {
                    bail_opt!(self, x.src_info, "invalid SWAP operands");
                }
                chunk.emit_duplicate_one_n(4, x.src_info);
                chunk.emit_duplicate_one_n(4, x.src_info);
                chunk.emit_bytecode(GetArrayVal, x.src_info);
                chunk.emit_duplicate_n(5, x.src_info);
                chunk.emit_bytecode(Pop, x.src_info);
                chunk.emit_bytecode(GetArrayVal, x.src_info);
                chunk.emit_bytecode(SetArrayVal, x.src_info);
                chunk.emit_bytecode(Pop, x.src_info);
                chunk.emit_bytecode(SetArrayVal, x.src_info);
                chunk.emit_bytecode(Pop, x.src_info);
                chunk.emit_bytecode(Pop, x.src_info);
                chunk.emit_bytecode(Pop, x.src_info);
            }
            // TODO...
            _ => bail_opt!(
                self,
                stmt.source_pos_info(),
                format!("unsupported statement: {stmt:?}")
            ),
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
    ) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;
        use EraExpressionValueKind::*;

        let val_kind = match expr {
            EraExpr::Binary(lhs, op, rhs) => {
                if let EraTokenKind::Assign | EraTokenKind::ExprAssign = op.kind {
                    match *lhs {
                        EraExpr::Term(EraTermExpr::Var(lhs)) => self.compile_expression_assign(
                            funcs,
                            func_info,
                            lhs,
                            |this, chunk| this.compile_expression(funcs, func_info, *rhs, chunk),
                            chunk,
                        )?,
                        _ => {
                            self.report_err(
                                lhs.source_pos_info(),
                                true,
                                "invalid assignment target",
                            );
                            return None;
                        }
                    }
                } else if let EraTokenKind::LogicalAnd | EraTokenKind::LogicalOr = op.kind {
                    let lhs_k = self.compile_expression(funcs, func_info, *lhs, chunk)?;
                    match lhs_k {
                        TInteger => (),
                        _ => bail_opt!(
                            self,
                            op.src_info,
                            true,
                            "only integers can participate in logical arithmetic at the moment"
                        ),
                    }
                    chunk.emit_bytecode(Duplicate, op.src_info);
                    if let EraTokenKind::LogicalAnd = op.kind {
                        chunk.emit_bytecode(LogicalNot, op.src_info);
                    }
                    let bt_done = chunk.emit_jump_cond_hold(op.src_info);
                    chunk.emit_bytecode(Pop, op.src_info);
                    let rhs_k = self.compile_expression(funcs, func_info, *rhs, chunk)?;
                    bt_done.complete(chunk);
                    if lhs_k != rhs_k {
                        bail_opt!(
                            self,
                            op.src_info,
                            true,
                            "conditional arithmetic requires operands of same type"
                        );
                    }
                    rhs_k
                } else if let EraTokenKind::PlusAssign
                | EraTokenKind::MinusAssign
                | EraTokenKind::MultiplyAssign
                | EraTokenKind::DivideAssign
                | EraTokenKind::ModuloAssign = op.kind
                {
                    match *lhs {
                        EraExpr::Term(EraTermExpr::Var(lhs)) => {
                            let lhs_k = self
                                .compile_expression_array_with_idx(funcs, func_info, lhs, chunk)?;
                            chunk.emit_bytecode(DuplicateN, op.src_info);
                            chunk.append_u8(2, op.src_info);
                            chunk.emit_bytecode(GetArrayVal, op.src_info);
                            let rhs_k = self.compile_expression(funcs, func_info, *rhs, chunk)?;
                            let result_k = match op.kind {
                                EraTokenKind::PlusAssign => match (lhs_k, rhs_k) {
                                    (TInteger, TInteger) | (TString, TString) => {
                                        chunk.emit_bytecode(Add, op.src_info);
                                        lhs_k
                                    }
                                    _ => bail_opt!(
                                        self,
                                        op.src_info,
                                        true,
                                        "operands must be of same primitive types"
                                    ),
                                },
                                EraTokenKind::MinusAssign => match (lhs_k, rhs_k) {
                                    (TInteger, TInteger) => {
                                        chunk.emit_bytecode(Subtract, op.src_info);
                                        lhs_k
                                    }
                                    _ => bail_opt!(
                                        self,
                                        op.src_info,
                                        true,
                                        "operands must be integers"
                                    ),
                                },
                                EraTokenKind::MultiplyAssign => match (lhs_k, rhs_k) {
                                    (TInteger, TInteger) => {
                                        chunk.emit_bytecode(Multiply, op.src_info);
                                        lhs_k
                                    }
                                    _ => bail_opt!(
                                        self,
                                        op.src_info,
                                        true,
                                        "operands must be integers"
                                    ),
                                },
                                EraTokenKind::DivideAssign => match (lhs_k, rhs_k) {
                                    (TInteger, TInteger) => {
                                        chunk.emit_bytecode(Divide, op.src_info);
                                        lhs_k
                                    }
                                    _ => bail_opt!(
                                        self,
                                        op.src_info,
                                        true,
                                        "operands must be integers"
                                    ),
                                },
                                EraTokenKind::ModuloAssign => match (lhs_k, rhs_k) {
                                    (TInteger, TInteger) => {
                                        chunk.emit_bytecode(Modulo, op.src_info);
                                        lhs_k
                                    }
                                    _ => bail_opt!(
                                        self,
                                        op.src_info,
                                        true,
                                        "operands must be integers"
                                    ),
                                },
                                _ => unreachable!(),
                            };
                            chunk.emit_bytecode(SetArrayVal, op.src_info);
                            result_k
                        }
                        _ => bail_opt!(self, rhs.source_pos_info(), "invalid assignment target"),
                    }
                } else {
                    let lhs = self.compile_expression(funcs, func_info, *lhs, chunk)?;
                    let rhs = self.compile_expression(funcs, func_info, *rhs, chunk)?;
                    match op.kind {
                        EraTokenKind::Plus => match (lhs, rhs) {
                            (TInteger, TInteger) | (TString, TString) => {
                                chunk.emit_bytecode(Add, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(
                                self,
                                op.src_info,
                                true,
                                "operands must be of same primitive types"
                            ),
                        },
                        EraTokenKind::Minus => match (lhs, rhs) {
                            (TInteger, TInteger) => {
                                chunk.emit_bytecode(Subtract, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(self, op.src_info, true, "operands must be integers"),
                        },
                        EraTokenKind::Multiply => match (lhs, rhs) {
                            (TInteger, TInteger) => {
                                chunk.emit_bytecode(Multiply, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(self, op.src_info, true, "operands must be integers"),
                        },
                        EraTokenKind::Divide => match (lhs, rhs) {
                            (TInteger, TInteger) => {
                                chunk.emit_bytecode(Divide, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(self, op.src_info, true, "operands must be integers"),
                        },
                        EraTokenKind::Percentage => match (lhs, rhs) {
                            (TInteger, TInteger) => {
                                chunk.emit_bytecode(Modulo, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(self, op.src_info, true, "operands must be integers"),
                        },
                        EraTokenKind::CmpEq => match (lhs, rhs) {
                            (TInteger, TInteger) | (TString, TString) => {
                                chunk.emit_bytecode(CompareEq, op.src_info);
                                TInteger
                            }
                            _ => bail_opt!(
                                self,
                                op.src_info,
                                true,
                                "operands must be of same primitive types"
                            ),
                        },
                        EraTokenKind::CmpL => match (lhs, rhs) {
                            (TInteger, TInteger) | (TString, TString) => {
                                chunk.emit_bytecode(CompareL, op.src_info);
                                TInteger
                            }
                            _ => bail_opt!(
                                self,
                                op.src_info,
                                true,
                                "operands must be of same primitive types"
                            ),
                        },
                        EraTokenKind::CmpLEq => match (lhs, rhs) {
                            (TInteger, TInteger) | (TString, TString) => {
                                chunk.emit_bytecode(CompareLEq, op.src_info);
                                TInteger
                            }
                            _ => bail_opt!(
                                self,
                                op.src_info,
                                true,
                                "operands must be of same primitive types"
                            ),
                        },
                        EraTokenKind::CmpNEq => match (lhs, rhs) {
                            (TInteger, TInteger) | (TString, TString) => {
                                chunk.emit_bytecode(CompareEq, op.src_info);
                                chunk.emit_bytecode(LogicalNot, op.src_info);
                                TInteger
                            }
                            _ => bail_opt!(
                                self,
                                op.src_info,
                                true,
                                "operands must be of same primitive types"
                            ),
                        },
                        EraTokenKind::CmpG => match (lhs, rhs) {
                            (TInteger, TInteger) | (TString, TString) => {
                                chunk.emit_bytecode(CompareLEq, op.src_info);
                                chunk.emit_bytecode(LogicalNot, op.src_info);
                                TInteger
                            }
                            _ => bail_opt!(
                                self,
                                op.src_info,
                                true,
                                "operands must be of same primitive types"
                            ),
                        },
                        EraTokenKind::CmpGEq => match (lhs, rhs) {
                            (TInteger, TInteger) | (TString, TString) => {
                                chunk.emit_bytecode(CompareL, op.src_info);
                                chunk.emit_bytecode(LogicalNot, op.src_info);
                                TInteger
                            }
                            _ => bail_opt!(
                                self,
                                op.src_info,
                                true,
                                "operands must be of same primitive types"
                            ),
                        },
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
                            // if let Ok(x) = i8::try_from(x) {
                            //     chunk.emit_bytecode(LoadIntegerImm8, src_info);
                            //     chunk.append_u8(x as _, src_info);
                            // } else {
                            //     chunk.emit_load_const(self.new_value_int(x), src_info);
                            // }
                            chunk.emit_load_const(self.new_value_int(x), src_info);
                            TInteger
                        }
                        EraLiteral::String(x, src_info) => {
                            chunk.emit_load_const(self.new_value_str(x), src_info);
                            TString
                        }
                    }
                }
                EraTermExpr::StrForm(x) => {
                    let src_info = x.src_info;
                    let parts_cnt = x.parts.len();
                    for part in x.parts {
                        match part {
                            crate::parser::EraStrFormExprPart::Expression(x) => {
                                let src_info = x.source_pos_info();
                                if let TInteger =
                                    self.compile_expression(funcs, func_info, x.expr, chunk)?
                                {
                                    chunk.emit_bytecode(ConvertToString, src_info);
                                }
                                if let Some(width) = x.width {
                                    let width_si = width.source_pos_info();
                                    let TInteger =
                                        self.compile_expression(funcs, func_info, width, chunk)?
                                    else {
                                        bail_opt!(
                                            self,
                                            width_si,
                                            "width must be an integer expression"
                                        );
                                    };
                                    chunk.emit_bytecode(PadString, src_info);
                                    chunk.append_u8(x.alignment.into(), src_info);
                                }
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
                    TString
                }
                EraTermExpr::Var(x) => {
                    self.compile_expression_array(funcs, func_info, x, true, chunk)?
                }
            },
            EraExpr::PreUnary(op, rhs) => match op.kind {
                EraTokenKind::Plus => self.compile_expression(funcs, func_info, *rhs, chunk)?,
                EraTokenKind::Minus => {
                    let rhs = self.compile_expression(funcs, func_info, *rhs, chunk)?;
                    chunk.emit_bytecode(Negate, op.src_info);
                    rhs
                }
                EraTokenKind::Increment | EraTokenKind::Decrement => match *rhs {
                    EraExpr::Term(EraTermExpr::Var(rhs)) => {
                        match self
                            .compile_expression_array_with_idx(funcs, func_info, rhs, chunk)?
                        {
                            TInteger => (),
                            _ => bail_opt!(
                                self,
                                op.src_info,
                                "invalid value type for this arithmetic"
                            ),
                        }
                        chunk.emit_bytecode(DuplicateN, op.src_info);
                        chunk.append_u8(2, op.src_info);
                        chunk.emit_bytecode(GetArrayVal, op.src_info);
                        chunk.emit_load_const(self.new_value_int(1), op.src_info);
                        match op.kind {
                            EraTokenKind::Increment => chunk.emit_bytecode(Add, op.src_info),
                            EraTokenKind::Decrement => chunk.emit_bytecode(Subtract, op.src_info),
                            _ => unreachable!(),
                        }
                        chunk.emit_bytecode(SetArrayVal, op.src_info);
                        TInteger
                    }
                    _ => {
                        self.report_err(rhs.source_pos_info(), true, "invalid assignment target");
                        return None;
                    }
                },
                _ => {
                    self.report_err(op.src_info, true, "invalid arithmetic kind");
                    return None;
                }
            },
            EraExpr::PostUnary(lhs, op) => match op.kind {
                EraTokenKind::Increment | EraTokenKind::Decrement => match *lhs {
                    EraExpr::Term(EraTermExpr::Var(lhs)) => {
                        match self
                            .compile_expression_array_with_idx(funcs, func_info, lhs, chunk)?
                        {
                            TInteger => (),
                            _ => bail_opt!(self, op.src_info, "invalid type for this arithmetic"),
                        }
                        chunk.emit_bytecode(DuplicateN, op.src_info);
                        chunk.append_u8(2, op.src_info);
                        chunk.emit_bytecode(GetArrayVal, op.src_info);
                        chunk.emit_load_const(self.new_value_int(1), op.src_info);
                        match op.kind {
                            EraTokenKind::Increment => chunk.emit_bytecode(Add, op.src_info),
                            EraTokenKind::Decrement => chunk.emit_bytecode(Subtract, op.src_info),
                            _ => unreachable!(),
                        }
                        chunk.emit_bytecode(SetArrayVal, op.src_info);
                        chunk.emit_load_const(self.new_value_int(1), op.src_info);
                        match op.kind {
                            EraTokenKind::Increment => chunk.emit_bytecode(Subtract, op.src_info),
                            EraTokenKind::Decrement => chunk.emit_bytecode(Add, op.src_info),
                            _ => unreachable!(),
                        }
                        TInteger
                    }
                    _ => {
                        self.report_err(lhs.source_pos_info(), true, "invalid assignment target");
                        return None;
                    }
                },
                _ => bail_opt!(self, op.src_info, true, "invalid arithmetic kind"),
            },
            EraExpr::Ternary(lhs, op1, mhs, op2, rhs) => {
                let lhs_si = lhs.source_pos_info();
                match self.compile_expression(funcs, func_info, *lhs, chunk)? {
                    TInteger | TString => (),
                    _ => bail_opt!(self, lhs_si, true, "condition must not be void"),
                }
                let bt_then = chunk.emit_jump_cond_hold(op1.src_info);
                let rhs_k = self.compile_expression(funcs, func_info, *rhs, chunk)?;
                let bt_done = chunk.emit_jump_hold(op2.src_info);
                bt_then.complete(chunk)?;
                let mhs_k = self.compile_expression(funcs, func_info, *mhs, chunk)?;
                bt_done.complete(chunk)?;
                match (mhs_k, rhs_k) {
                    (TInteger, TInteger) => TInteger,
                    (TString, TString) => TString,
                    (TVoid, TVoid) => TVoid,
                    _ => bail_opt!(self, op2.src_info, true, "invalid branch expression type"),
                }
            }
            EraExpr::FunCall(lhs, args) => match *lhs {
                // EraExpr::Term(EraTermExpr::Literal(EraLiteral::String(lhs, lhs_si))) => {
                //     self.compile_static_fun_call(funcs, func_info, &lhs, args, lhs_si, chunk)?
                // }
                EraExpr::Term(EraTermExpr::Var(lhs)) => self.compile_static_fun_call(
                    funcs,
                    func_info,
                    &lhs.name,
                    args,
                    lhs.src_info,
                    chunk,
                )?,
                _ => bail_opt!(
                    self,
                    lhs.source_pos_info(),
                    "dynamic call is forbidden in this context"
                ),
            },
            // TODO...
            _ => {
                self.report_err(expr.source_pos_info(), true, "unknown expression kind");
                return None;
            }
        };

        Some(val_kind)
    }
    #[must_use]
    fn compile_expression_assign(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        mut lhs: EraVarExpr,
        rhs: impl FnOnce(&mut Self, &mut EraBytecodeChunk) -> Option<EraExpressionValueKind>,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;

        let src_info = lhs.src_info;
        let idxs = std::mem::take(&mut lhs.idxs);
        let idx_len = idxs.len();
        self.compile_expression_array(funcs, func_info, lhs, false, chunk)?;
        for idx in idxs {
            self.compile_expression(funcs, func_info, idx, chunk)?;
        }
        let val_kind = rhs(self, chunk)?;
        chunk.emit_bytecode(SetMDArrayVal, src_info);
        chunk.append_u8(idx_len as _, src_info);

        Some(val_kind)
    }
    #[must_use]
    fn compile_expression_array(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        var_expr: EraVarExpr,
        decay_arr: bool,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;
        use EraExpressionValueKind::*;

        let src_info = var_expr.src_info;
        let is_in_global_frame;
        let var_idx;
        let var_kind;
        let var_dims_cnt;
        if let Some(var) = func_info.local_vars.get(CaselessStr::new(&var_expr.name)) {
            (var_kind, var_dims_cnt) = match var.init_val.clone().into_unpacked() {
                FlatValue::ArrInt(x) => (TInteger, x.borrow().dims.len()),
                FlatValue::ArrStr(x) => (TString, x.borrow().dims.len()),
                _ => unreachable!(),
            };
            is_in_global_frame = var.is_in_global_frame;
            var_idx = var.idx_in_frame;
        } else if let Some(var) = self.vars.get_var_idx(&var_expr.name).or_else(|| {
            self.vars
                .get_var_idx(&format!("{}@{}", &var_expr.name, func_info.name.as_str(),))
        }) {
            (var_kind, var_dims_cnt) = match self
                .vars
                .get_var_info(var)
                .unwrap()
                .val
                .clone()
                .into_unpacked()
            {
                FlatValue::ArrInt(x) => (TInteger, x.borrow().dims.len()),
                FlatValue::ArrStr(x) => (TString, x.borrow().dims.len()),
                _ => unreachable!(),
            };
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
                let src_info = rand_max.source_pos_info();
                match self.compile_expression(funcs, func_info, rand_max, chunk)? {
                    EraExpressionValueKind::TInteger => (),
                    _ => bail_opt!(self, src_info, true, "array indices must be integer"),
                }
                chunk.emit_bytecode(GetRandomMax, src_info);
                return Some(EraExpressionValueKind::TInteger);
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
        if decay_arr && var_dims_cnt > 1 && var_expr.idxs.len() < var_dims_cnt {
            self.report_err(src_info, false, "non-compliant use of array variable");
        }
        // Decay array values to primitive values
        if decay_arr {
            let idx_len = var_expr.idxs.len();
            for idx in var_expr.idxs {
                let src_info = idx.source_pos_info();
                let idx_kind = match idx {
                    // HACK: Contextual indices replacing
                    EraExpr::Term(EraTermExpr::Var(x))
                        if x.idxs.is_empty()
                            && routine::is_csv_var(&var_expr.name)
                            && self
                                .contextual_indices
                                .contains_key(Ascii::new_str(&x.name)) =>
                    {
                        let idx = self
                            .contextual_indices
                            .get(Ascii::new_str(&x.name))
                            .copied()
                            .unwrap();
                        chunk.emit_load_const(self.new_value_int(idx as _), x.src_info);
                        TInteger
                    }
                    _ => self.compile_expression(funcs, func_info, idx, chunk)?,
                };
                match idx_kind {
                    TInteger => (),
                    _ => bail_opt!(self, src_info, true, "array indices must be integer"),
                }
            }
            chunk.emit_bytecode(GetMDArrayVal, src_info);
            chunk.append_u8(idx_len as _, src_info);
        }
        Some(var_kind)
    }
    #[must_use]
    fn compile_expression_array_with_idx(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        var_expr: EraVarExpr,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;
        use EraExpressionValueKind::*;

        let src_info = var_expr.src_info;
        let is_in_global_frame;
        let var_idx;
        let var_kind;
        let var_dims;
        if let Some(var) = func_info.local_vars.get(CaselessStr::new(&var_expr.name)) {
            match var.init_val.clone().into_unpacked() {
                FlatValue::ArrInt(x) => {
                    var_kind = EraExpressionValueKind::TInteger;
                    var_dims = x.borrow().dims.clone();
                }
                FlatValue::ArrStr(x) => {
                    var_kind = EraExpressionValueKind::TString;
                    var_dims = x.borrow().dims.clone();
                }
                _ => unimplemented!(),
            }
            is_in_global_frame = var.is_in_global_frame;
            var_idx = var.idx_in_frame;
        } else if let Some(var) = self.vars.get_var_idx(&var_expr.name) {
            let var_info = self.vars.get_var_info(var).unwrap();
            match var_info.val.clone().into_unpacked() {
                FlatValue::ArrInt(x) => {
                    var_kind = EraExpressionValueKind::TInteger;
                    var_dims = x.borrow().dims.clone();
                }
                FlatValue::ArrStr(x) => {
                    var_kind = EraExpressionValueKind::TString;
                    var_dims = x.borrow().dims.clone();
                }
                _ => unimplemented!(),
            }
            is_in_global_frame = true;
            var_idx = var;
        } else {
            // NOTE: Cannot be used to access pesudo array (like RAND)
            bail_opt!(
                self,
                src_info,
                true,
                format!("undefined variable `{}`", var_expr.name)
            );
        }
        chunk.emit_load_const(self.new_value_int(var_idx as _), src_info);
        if is_in_global_frame {
            chunk.emit_bytecode(GetGlobal, src_info);
        } else {
            chunk.emit_bytecode(GetLocal, src_info);
        }
        // TODO: Check array size & type
        if var_dims.len() > 1 && var_expr.idxs.len() < var_dims.len() {
            self.report_err(src_info, false, "non-compliant use of array variable");
        }
        // FIXME: Insert checks for indices overflow for MD arrays
        // Decay array values to primitive values
        let idx_len = var_expr.idxs.len();
        if idx_len > var_dims.len() {
            bail_opt!(self, src_info, "too many indices into array");
        }
        chunk.emit_load_const(self.new_value_int(0), src_info);
        let stride = var_dims.iter().skip(idx_len).fold(1, |acc, &x| acc * x);
        let strides = var_dims
            .iter()
            .take(idx_len)
            .rev()
            .scan(stride, |acc, x| {
                let r = Some(*acc);
                *acc *= x;
                r
            })
            .collect::<Vec<_>>();
        for (idx, stride) in var_expr.idxs.into_iter().zip(strides.into_iter().rev()) {
            let src_info = idx.source_pos_info();

            let idx_kind = match idx {
                // HACK: Contextual indices replacing
                EraExpr::Term(EraTermExpr::Var(x))
                    if x.idxs.is_empty()
                        && routine::is_csv_var(&var_expr.name)
                        && self
                            .contextual_indices
                            .contains_key(Ascii::new_str(&x.name)) =>
                {
                    let idx = self
                        .contextual_indices
                        .get(Ascii::new_str(&x.name))
                        .copied()
                        .unwrap();
                    chunk.emit_load_const(self.new_value_int(idx as _), x.src_info);
                    TInteger
                }
                _ => self.compile_expression(funcs, func_info, idx, chunk)?,
            };
            match idx_kind {
                TInteger => (),
                _ => bail_opt!(self, src_info, true, "array indices must be integer"),
            }

            chunk.emit_load_const(self.new_value_int(stride as _), src_info);
            chunk.emit_bytecode(Multiply, src_info);
            chunk.emit_bytecode(Add, src_info);
        }
        Some(var_kind)
    }
    #[must_use]
    fn compile_static_fun_call(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        target: &str,
        args: Vec<Option<EraExpr>>,
        target_src_info: SourcePosInfo,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;

        let Some(&target_idx) = funcs.func_names.get(CaselessStr::new(&target)) else {
            // Check for built-in functions as fallback
            match self.compile_builtin_fun_call(
                funcs,
                func_info,
                target,
                args,
                target_src_info,
                false,
                chunk,
            ) {
                Some(x) => return Some(x),
                None => bail_opt!(
                    self,
                    target_src_info,
                    format!("function `{target}` is undefined or has no matching overloads")
                ),
            }
        };
        let target = funcs.funcs.get(target_idx).unwrap();
        let args_len = args.len();
        if args_len > target.params.len() {
            bail_opt!(
                self,
                [
                    (target_src_info, "incorrect parameter count"),
                    (
                        &target.file_name,
                        target.src_info,
                        "note: see signature of callee here"
                    )
                ]
            );
        }
        for (arg, param) in args.into_iter().zip(target.params.iter()) {
            let Some(arg) = arg else {
                // Omitted argument
                // HACK: Steal source info from last line
                let src_info = chunk.source_info_at(chunk.cur_bytes_cnt() - 1).unwrap();
                if param.default_val.kind().is_arr() {
                    bail_opt!(
                        self,
                        [
                            (src_info, "reference parameter cannot be omitted"),
                            (
                                &target.file_name,
                                target.src_info,
                                "note: see signature of callee here"
                            )
                        ]
                    );
                }
                chunk.emit_load_const(param.default_val.clone(), src_info);
                continue;
            };
            let arg_si = arg.source_pos_info();
            let param_kind = param.default_val.kind();
            match param_kind {
                ValueKind::ArrInt | ValueKind::ArrStr => {
                    let EraExpr::Term(EraTermExpr::Var(var_expr)) = arg else {
                        bail_opt!(
                            self,
                            [
                                (arg_si, "argument cannot be coerced into a reference"),
                                (
                                    &target.file_name,
                                    target.src_info,
                                    "note: see signature of callee here"
                                )
                            ]
                        );
                    };
                    let arg_kind =
                        self.compile_expression_array(funcs, func_info, var_expr, false, chunk)?;
                    match (arg_kind, param_kind) {
                        (EraExpressionValueKind::TInteger, ValueKind::ArrInt)
                        | (EraExpressionValueKind::TString, ValueKind::ArrStr) => (),
                        _ => bail_opt!(
                            self,
                            [
                                (arg_si, "incompatible argument type"),
                                (
                                    &target.file_name,
                                    target.src_info,
                                    "note: see signature of callee here"
                                )
                            ]
                        ),
                    }
                }
                ValueKind::Int | ValueKind::Str => {
                    let arg_kind = self.compile_expression(funcs, func_info, arg, chunk)?;
                    match (arg_kind, param_kind) {
                        (EraExpressionValueKind::TInteger, ValueKind::Int)
                        | (EraExpressionValueKind::TString, ValueKind::Str) => (),
                        _ => bail_opt!(
                            self,
                            [
                                (arg_si, "incompatible argument type"),
                                (
                                    &target.file_name,
                                    target.src_info,
                                    "see signature of callee here"
                                )
                            ]
                        ),
                    }
                }
            }
        }
        for param in target.params.iter().skip(args_len) {
            // HACK: Steal source info from last line
            let src_info = chunk.source_info_at(chunk.cur_bytes_cnt() - 1).unwrap();
            if param.default_val.kind().is_arr() {
                bail_opt!(
                    self,
                    [
                        (src_info, "reference parameter cannot be omitted"),
                        (
                            &target.file_name,
                            target.src_info,
                            "note: see signature of callee here"
                        )
                    ]
                );
            }
            chunk.emit_load_const(param.default_val.clone(), src_info);
        }
        chunk.emit_load_const(Value::new_int(target_idx as _), target_src_info);
        chunk.emit_bytecode(FunCall, target_src_info);
        chunk.append_u8(target.params.len() as _, target_src_info);

        Some(match target.kind {
            EraFunKind::Procedure => EraExpressionValueKind::TVoid,
            EraFunKind::Function => EraExpressionValueKind::TInteger,
            EraFunKind::FunctionS => EraExpressionValueKind::TString,
        })
    }
    #[must_use]
    fn compile_dynamic_fun_call(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        target: EraExpr,
        args: Vec<Option<EraExpr>>,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;
        use EraExpressionValueKind::*;

        // NOTE: We know nothing about the target, so we must emit args pack which contains
        //       enough information to carry out args resolution at run-time.
        let args_len = args.len();
        let target_si = target.source_pos_info();
        for arg in args {
            let Some(arg) = arg else {
                bail_opt!(
                    self,
                    target_si,
                    "omission of arguments are unsupported at the moment"
                );
            };
            let arg_si = arg.source_pos_info();
            if let EraExpr::Term(EraTermExpr::Var(x)) = arg {
                self.compile_expression_array_with_idx(funcs, func_info, x, chunk)?;
            } else {
                chunk.emit_load_const(self.new_value_int(0), arg_si);
                self.compile_expression(funcs, func_info, arg, chunk)?;
            }
        }
        let TString = self.compile_expression(funcs, func_info, target, chunk)? else {
            bail_opt!(self, target_si, "invalid type as dynamic function callee");
        };
        chunk.emit_bytecode(TryFunCall, target_si);
        chunk.append_u8(args_len as _, target_si);

        Some(TInteger)
    }
    #[must_use]
    fn compile_builtin_fun_call(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        target: &str,
        args: Vec<Option<EraExpr>>,
        src_info: SourcePosInfo,
        is_cmd: bool,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;
        use EraExpressionValueKind::*;
        use ValueKind::*;

        struct Ctx<'c, 'this, 'a, 'target, T> {
            this: &'this mut EraCompilerImpl<'a, T>,
            chunk: &'c mut EraBytecodeChunk,
            target: &'target str,
            ret_kind: EraExpressionValueKind,
            src_info: SourcePosInfo,
            assign_to_result: bool,
        }
        impl<T: FnMut(&EraCompileErrorInfo)> Ctx<'_, '_, '_, '_, T> {
            fn result(&mut self) -> Option<()> {
                if !self.assign_to_result {
                    return Some(());
                }
                self.this.var_result(self.src_info, self.chunk)?;
                self.chunk
                    .emit_load_const(self.this.new_value_int(0), self.src_info);
                self.ret_kind = TInteger;
                Some(())
            }
            fn results(&mut self) -> Option<()> {
                if !self.assign_to_result {
                    return Some(());
                }
                self.this.var_results(self.src_info, self.chunk)?;
                self.chunk
                    .emit_load_const(self.this.new_value_int(0), self.src_info);
                self.ret_kind = TString;
                Some(())
            }
            fn unpack_args<const N: usize>(
                &mut self,
                args: Vec<Option<EraExpr>>,
            ) -> Option<[Option<EraExpr>; N]> {
                if args.len() != N {
                    bail_opt!(
                        self.this,
                        self.src_info,
                        format!(
                            "function `{}` expects {} parameters, but {} were given",
                            self.target,
                            N,
                            args.len()
                        )
                    );
                }
                let mut it = args.into_iter();
                Some(std::array::from_fn(|_| it.next().unwrap()))
            }
            fn unpack_some_args<const N: usize>(
                &mut self,
                args: Vec<Option<EraExpr>>,
            ) -> Option<[EraExpr; N]> {
                let args = self.unpack_args::<N>(args)?;
                for (idx, arg) in args.iter().enumerate() {
                    if arg.is_none() {
                        bail_opt!(
                            self.this,
                            self.src_info,
                            format!("argument {} cannot be omitted", idx + 1)
                        );
                    }
                }
                Some(args.map(|x| x.unwrap()))
            }
        }
        impl<T> Drop for Ctx<'_, '_, '_, '_, T> {
            fn drop(&mut self) {
                match self.ret_kind {
                    TInteger | TString => {
                        self.chunk.emit_bytecode(SetArrayVal, self.src_info);
                        self.chunk.emit_bytecode(Pop, self.src_info);
                    }
                    TVoid => (),
                }
            }
        }
        let mut ctx = Ctx {
            this: self,
            chunk,
            ret_kind: TVoid,
            target,
            src_info,
            assign_to_result: is_cmd,
        };

        match target.to_ascii_uppercase().as_bytes() {
            b"GCREATE" => todo!(),
            b"GDISPOSE" => todo!(),
            b"GDRAWSPRITE" => todo!(),
            b"GCLEAR" => todo!(),
            b"SPRITECREATE" => todo!(),
            b"SPRITEDISPOSE" => todo!(),
            b"SPRITEANIMECREATE" => todo!(),
            b"SPRITEANIMEADDFRAME" => todo!(),
            b"GETBIT" => todo!(),
            b"GETSTYLE" => todo!(),
            b"CHKFONT" => todo!(),
            b"GETFONT" => todo!(),
            b"REPLACE" => todo!(),
            b"SUBSTRING" => todo!(),
            b"SUBSTRINGU" => todo!(),
            b"STRFIND" => todo!(),
            b"STRFINDU" => todo!(),
            b"STRLENS" => todo!(),
            b"STRLENSU" => todo!(),
            b"STRCOUNT" => todo!(),
            b"CUREENTREDRAW" => todo!(),
            b"CURRENTALIGN" => todo!(),
            b"CHKCHARADATA" => todo!(),
            b"SAVETEXT" => todo!(),
            b"MAX" => {
                ctx.result()?;
                let [a1, a2] = ctx.unpack_some_args(args)?;
                ctx.this.expression_int(funcs, func_info, a1, ctx.chunk)?;
                ctx.this.expression_int(funcs, func_info, a2, ctx.chunk)?;
                ctx.chunk.emit_bytecode(MaximumInt, src_info);
            }
            b"MIN" => {
                ctx.result()?;
                let [a1, a2] = ctx.unpack_some_args(args)?;
                ctx.this.expression_int(funcs, func_info, a1, ctx.chunk)?;
                ctx.this.expression_int(funcs, func_info, a2, ctx.chunk)?;
                ctx.chunk.emit_bytecode(MinimumInt, src_info);
            }
            b"LIMIT" => {
                ctx.result()?;
                let [a, amax, amin] = ctx.unpack_some_args(args)?;
                ctx.this.expression_int(funcs, func_info, a, ctx.chunk)?;
                ctx.this.expression_int(funcs, func_info, amax, ctx.chunk)?;
                ctx.this.expression_int(funcs, func_info, amin, ctx.chunk)?;
                ctx.chunk.emit_bytecode(ClampInt, src_info);
            }
            b"INRANGE" => {
                ctx.result()?;
                let [a, amax, amin] = ctx.unpack_some_args(args)?;
                ctx.this.expression_int(funcs, func_info, a, ctx.chunk)?;
                ctx.this.expression_int(funcs, func_info, amax, ctx.chunk)?;
                ctx.this.expression_int(funcs, func_info, amin, ctx.chunk)?;
                ctx.chunk.emit_bytecode(InRangeInt, src_info);
            }
            b"FINDCHARA" => todo!(),
            b"FINDLASTCHARA" => todo!(),
            _ => bail_opt!(
                ctx.this,
                src_info,
                format!("function `{target}` is undefined or has no matching overloads")
            ),
        }

        Some(ctx.ret_kind)
    }
    #[must_use]
    fn decorate_as_func_local_name(name: &str, func_name: &str) -> String {
        format!("$LOCALVAR_{name}@{func_name}")
    }
    fn expression_int(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        expr: EraExpr,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<()> {
        use EraExpressionValueKind::*;
        let si = expr.source_pos_info();
        let TInteger = self.compile_expression(funcs, func_info, expr, chunk)? else {
            bail_opt!(self, si, "expected an integer expression");
        };
        Some(())
    }
    fn expression_str(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        expr: EraExpr,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<()> {
        use EraExpressionValueKind::*;
        let si = expr.source_pos_info();
        let TString = self.compile_expression(funcs, func_info, expr, chunk)? else {
            bail_opt!(self, si, "expected a string expression");
        };
        Some(())
    }
    fn array_int(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        var_expr: EraVarExpr,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<()> {
        use EraExpressionValueKind::*;
        let si = var_expr.src_info;
        let TInteger = self.compile_expression_array_with_idx(funcs, func_info, var_expr, chunk)?
        else {
            bail_opt!(self, si, "expected an integer variable");
        };
        Some(())
    }
    fn array_str(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        var_expr: EraVarExpr,
        chunk: &mut EraBytecodeChunk,
    ) -> Option<()> {
        use EraExpressionValueKind::*;
        let si = var_expr.src_info;
        let TString = self.compile_expression_array_with_idx(funcs, func_info, var_expr, chunk)?
        else {
            bail_opt!(self, si, "expected a string variable");
        };
        Some(())
    }
    fn var_result(&mut self, src_info: SourcePosInfo, chunk: &mut EraBytecodeChunk) -> Option<()> {
        let idx = self.vars.get_var_idx("RESULT").unwrap();
        chunk.emit_load_const(self.new_value_int(idx as _), src_info);
        chunk.emit_bytecode(EraBytecodePrimaryType::GetGlobal, src_info);
        Some(())
    }
    fn var_results(&mut self, src_info: SourcePosInfo, chunk: &mut EraBytecodeChunk) -> Option<()> {
        let idx = self.vars.get_var_idx("RESULTS").unwrap();
        chunk.emit_load_const(self.new_value_int(idx as _), src_info);
        chunk.emit_bytecode(EraBytecodePrimaryType::GetGlobal, src_info);
        Some(())
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
            file_name: self.file_name.deref().to_owned(),
            src_info,
            is_error,
            msg: msg.into(),
        });
    }
    fn report_far_err<V: Into<String>>(
        &mut self,
        file_name: &str,
        src_info: SourcePosInfo,
        is_error: bool,
        msg: V,
    ) {
        (self.err_report_fn)(&EraCompileErrorInfo {
            file_name: file_name.to_owned(),
            src_info,
            is_error,
            msg: msg.into(),
        });
    }
    fn evaluate_constant(&mut self, expr: EraExpr) -> Option<EraLiteral> {
        match expr.try_evaluate_constant(&self.vars) {
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
                        EraLiteral::Integer(val, _) => Some(IntValue { val }),
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
