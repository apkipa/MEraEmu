use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    rc::Rc,
};

use smallvec::SmallVec;

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
    vars: EraVarPool,
    intern_vals: HashMap<either::Either<Rc<IntValue>, Rc<StrValue>>, ()>,
    contextual_indices: HashMap<Ascii<String>, u32>,
}

pub struct EraCompilerImplFunctionSite<'p, 'a, ErrReportFn> {
    p: &'p mut EraCompilerImpl<'a, ErrReportFn>,
    funcs: &'p EraFuncPool,
    func_info: &'p EraFuncInfo,
    chunk: &'p mut EraBytecodeChunk,
    loop_structs: Vec<EraLoopStructCodeMetadata>,
    stack_balance: usize,
    goto_labels: HashMap<CaselessString, EraGotoLabelInfo>,
    goto_backtracks: Vec<EraGotoJumpInfo>,
    final_return_backtracks: Vec<EraBytecodeChunkBacktrackHolder>,
}

#[derive(Debug, Clone, Copy)]
enum EraPseudoVarKind {
    Rand,
}

#[derive(Debug, Clone, Copy)]
enum EraVarArrCompileInfo {
    Pseudo(EraPseudoVarKind),
    Normal(EraExpressionValueKind, u8),
}
#[derive(Debug, Clone)]
enum EraVarArrCompileInfoWithDims {
    Pseudo(EraPseudoVarKind),
    Normal(EraExpressionValueKind, smallvec::SmallVec<[u32; 3]>),
}
impl EraVarArrCompileInfo {
    fn from_fat(value: &EraVarArrCompileInfoWithDims) -> Self {
        use EraVarArrCompileInfoWithDims::*;
        match value {
            Pseudo(kind) => Self::Pseudo(*kind),
            Normal(kind, dims) => Self::Normal(*kind, dims.len().try_into().unwrap()),
        }
    }
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
            intern_vals: HashMap::new(),
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
        EraCompilerImplFunctionSite::new(self, funcs, func_info, chunk).function(func_decl)
    }
    #[must_use]
    fn decorate_as_func_local_name(name: &str, func_name: &str) -> String {
        format!("$LOCALVAR_{name}@{func_name}")
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

impl<'p, 'a, T: FnMut(&EraCompileErrorInfo)> EraCompilerImplFunctionSite<'p, 'a, T> {
    fn new(
        parent: &'p mut EraCompilerImpl<'a, T>,
        funcs: &'p EraFuncPool,
        func_info: &'p EraFuncInfo,
        chunk: &'p mut EraBytecodeChunk,
    ) -> Self {
        EraCompilerImplFunctionSite {
            p: parent,
            funcs,
            func_info,
            chunk,
            loop_structs: Vec::new(),
            stack_balance: 0,
            goto_labels: HashMap::new(),
            goto_backtracks: Vec::new(),
            final_return_backtracks: Vec::new(),
        }
    }
    fn function(&mut self, func_decl: EraFunDecl) -> Option<EraFuncBytecodeInfo> {
        use EraBytecodePrimaryType::*;

        let bytecode_info = EraFuncBytecodeInfo {
            name: CaselessStr::new(&func_decl.name).into(),
            chunk_idx: u32::MAX, // Stub
            offset: self.chunk.cur_bytes_cnt() as _,
            args: self
                .func_info
                .params
                .iter()
                .map(|x| x.default_val.clone())
                .collect(),
            func_kind: func_decl.kind,
        };

        // Init local variables (including non-DYNAMIC ones)
        for (name, var_info) in self.func_info.local_vars.iter() {
            let src_info = var_info.src_info;
            if var_info.is_in_global_frame {
                if var_info.has_init {
                    self.chunk
                        .emit_load_const(Value::new_int(var_info.idx_in_frame as _), src_info);
                    self.chunk.emit_bytecode(GetGlobal, src_info);
                    self.chunk
                        .emit_load_const(var_info.init_val.clone(), src_info);
                    self.chunk.emit_bytecode(CopyArrayContent, src_info);
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
                self.chunk
                    .emit_load_const(var_info.init_val.clone(), src_info);
                self.chunk.emit_bytecode(DeepClone, src_info);
            }
        }

        // Assign parameters
        for (param_idx, param_info) in self.func_info.params.iter().enumerate() {
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
            self.arr_set(&lhs.name, lhs.idxs, lhs.src_info, |this| {
                this.chunk
                    .emit_load_const(this.p.new_value_int(param_idx as _), src_info);
                this.chunk.emit_bytecode(GetLocal, src_info);
                Some(rhs_kind)
            })?;
            self.chunk.emit_bytecode(Pop, src_info);
        }

        // Compile function body
        for stmt in func_decl.body {
            self.statement(func_decl.kind, stmt)?;
        }

        // Return
        for bt in std::mem::take(&mut self.final_return_backtracks) {
            bt.complete(self.chunk)?;
        }
        match func_decl.kind {
            EraFunKind::Function => {
                let val = self.p.new_value_int(0);
                self.chunk.emit_load_const(val, func_decl.src_info);
                self.chunk.emit_bytecode(ReturnInteger, func_decl.src_info);
            }
            EraFunKind::FunctionS => {
                let val = self.p.new_value_str(String::new());
                self.chunk.emit_load_const(val, func_decl.src_info);
                self.chunk.emit_bytecode(ReturnString, func_decl.src_info);
            }
            EraFunKind::Procedure => {
                self.chunk.emit_bytecode(ReturnVoid, func_decl.src_info);
            }
        }

        // Complete GOTOs
        _ = std::mem::take(&mut self.stack_balance);
        let goto_labels = std::mem::take(&mut self.goto_labels);
        let goto_backtracks = std::mem::take(&mut self.goto_backtracks);
        for backtrack in goto_backtracks {
            let src_si = backtrack.backtrack.source_pos_info(self.chunk)?;
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
                        (
                            self.func_info.src_info,
                            "note: function definition starts here"
                        )
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
            backtrack.backtrack.complete_at(self.chunk, target.pos)?;
        }

        Some(bytecode_info)
    }
    #[must_use]
    fn expression(&mut self, expr: EraExpr) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;
        use EraExpressionValueKind::*;

        let val_kind = match expr {
            EraExpr::Binary(lhs, op, rhs) => {
                if let EraTokenKind::Assign | EraTokenKind::ExprAssign = op.kind {
                    match *lhs {
                        EraExpr::Term(EraTermExpr::Var(lhs)) => {
                            self.arr_set(&lhs.name, lhs.idxs, lhs.src_info, |this| {
                                this.expression(*rhs)
                            })?
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
                } else if let EraTokenKind::LogicalAnd | EraTokenKind::LogicalOr = op.kind {
                    let lhs_k = self.expression(*lhs)?;
                    match lhs_k {
                        TInteger => (),
                        _ => bail_opt!(
                            self,
                            op.src_info,
                            true,
                            "only integers can participate in logical arithmetic at the moment"
                        ),
                    }
                    self.chunk.emit_bytecode(Duplicate, op.src_info);
                    if let EraTokenKind::LogicalAnd = op.kind {
                        self.chunk.emit_bytecode(LogicalNot, op.src_info);
                    }
                    let bt_done = self.chunk.emit_jump_cond_hold(op.src_info);
                    self.chunk.emit_bytecode(Pop, op.src_info);
                    let rhs_k = self.expression(*rhs)?;
                    bt_done.complete(self.chunk);
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
                            let lhs_k = self.arr_idx(&lhs.name, lhs.idxs, lhs.src_info)?;
                            self.chunk.emit_bytecode(DuplicateN, op.src_info);
                            self.chunk.append_u8(2, op.src_info);
                            self.chunk.emit_bytecode(GetArrayVal, op.src_info);
                            let rhs_k = self.expression(*rhs)?;
                            let result_k = match op.kind {
                                EraTokenKind::PlusAssign => match (lhs_k, rhs_k) {
                                    (TInteger, TInteger) | (TString, TString) => {
                                        self.chunk.emit_bytecode(Add, op.src_info);
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
                                        self.chunk.emit_bytecode(Subtract, op.src_info);
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
                                        self.chunk.emit_bytecode(Multiply, op.src_info);
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
                                        self.chunk.emit_bytecode(Divide, op.src_info);
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
                                        self.chunk.emit_bytecode(Modulo, op.src_info);
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
                            self.chunk.emit_bytecode(SetArrayVal, op.src_info);
                            result_k
                        }
                        _ => bail_opt!(self, rhs.source_pos_info(), "invalid assignment target"),
                    }
                } else {
                    let lhs = self.expression(*lhs)?;
                    let rhs = self.expression(*rhs)?;
                    match op.kind {
                        EraTokenKind::Plus => match (lhs, rhs) {
                            (TInteger, TInteger) | (TString, TString) => {
                                self.chunk.emit_bytecode(Add, op.src_info);
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
                                self.chunk.emit_bytecode(Subtract, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(self, op.src_info, true, "operands must be integers"),
                        },
                        EraTokenKind::Multiply => match (lhs, rhs) {
                            (TInteger, TInteger) => {
                                self.chunk.emit_bytecode(Multiply, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(self, op.src_info, true, "operands must be integers"),
                        },
                        EraTokenKind::Divide => match (lhs, rhs) {
                            (TInteger, TInteger) => {
                                self.chunk.emit_bytecode(Divide, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(self, op.src_info, true, "operands must be integers"),
                        },
                        EraTokenKind::Percentage => match (lhs, rhs) {
                            (TInteger, TInteger) => {
                                self.chunk.emit_bytecode(Modulo, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(self, op.src_info, true, "operands must be integers"),
                        },
                        EraTokenKind::CmpEq => match (lhs, rhs) {
                            (TInteger, TInteger) | (TString, TString) => {
                                self.chunk.emit_bytecode(CompareEq, op.src_info);
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
                                self.chunk.emit_bytecode(CompareL, op.src_info);
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
                                self.chunk.emit_bytecode(CompareLEq, op.src_info);
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
                                self.chunk.emit_bytecode(CompareEq, op.src_info);
                                self.chunk.emit_bytecode(LogicalNot, op.src_info);
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
                                self.chunk.emit_bytecode(CompareLEq, op.src_info);
                                self.chunk.emit_bytecode(LogicalNot, op.src_info);
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
                                self.chunk.emit_bytecode(CompareL, op.src_info);
                                self.chunk.emit_bytecode(LogicalNot, op.src_info);
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
                            self.chunk
                                .emit_load_const(self.p.new_value_int(x), src_info);
                            TInteger
                        }
                        EraLiteral::String(x, src_info) => {
                            self.chunk
                                .emit_load_const(self.p.new_value_str(x), src_info);
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
                                if let TInteger = self.expression(x.expr)? {
                                    self.chunk.emit_bytecode(ConvertToString, src_info);
                                }
                                if let Some(width) = x.width {
                                    let width_si = width.source_pos_info();
                                    let TInteger = self.expression(width)? else {
                                        bail_opt!(
                                            self,
                                            width_si,
                                            "width must be an integer expression"
                                        );
                                    };
                                    self.chunk.emit_bytecode(PadString, src_info);
                                    self.chunk.append_u8(x.alignment.into(), src_info);
                                }
                            }
                            crate::parser::EraStrFormExprPart::Literal(x, src_info) => {
                                let x = self.p.new_value_str(x);
                                self.chunk.emit_load_const(x, src_info);
                            }
                        }
                    }
                    self.chunk.emit_bytecode(BuildString, src_info);
                    // TODO: BuildString check overflow
                    self.chunk.append_u8(parts_cnt as _, src_info);
                    TString
                }
                EraTermExpr::Var(x) => self.arr_get(&x.name, x.idxs, x.src_info)?,
            },
            EraExpr::PreUnary(op, rhs) => match op.kind {
                EraTokenKind::Plus => self.expression(*rhs)?,
                EraTokenKind::Minus => {
                    let rhs = self.expression(*rhs)?;
                    self.chunk.emit_bytecode(Negate, op.src_info);
                    rhs
                }
                EraTokenKind::Increment | EraTokenKind::Decrement => match *rhs {
                    EraExpr::Term(EraTermExpr::Var(rhs)) => {
                        match self.arr_idx(&rhs.name, rhs.idxs, rhs.src_info)? {
                            TInteger => (),
                            _ => bail_opt!(
                                self,
                                op.src_info,
                                "invalid value type for this arithmetic"
                            ),
                        }
                        self.chunk.emit_bytecode(DuplicateN, op.src_info);
                        self.chunk.append_u8(2, op.src_info);
                        self.chunk.emit_bytecode(GetArrayVal, op.src_info);
                        self.chunk
                            .emit_load_const(self.p.new_value_int(1), op.src_info);
                        match op.kind {
                            EraTokenKind::Increment => self.chunk.emit_bytecode(Add, op.src_info),
                            EraTokenKind::Decrement => {
                                self.chunk.emit_bytecode(Subtract, op.src_info)
                            }
                            _ => unreachable!(),
                        }
                        self.chunk.emit_bytecode(SetArrayVal, op.src_info);
                        TInteger
                    }
                    _ => bail_opt!(
                        self,
                        rhs.source_pos_info(),
                        true,
                        "invalid assignment target"
                    ),
                },
                _ => bail_opt!(self, op.src_info, true, "invalid arithmetic kind"),
            },
            EraExpr::PostUnary(lhs, op) => match op.kind {
                EraTokenKind::Increment | EraTokenKind::Decrement => match *lhs {
                    EraExpr::Term(EraTermExpr::Var(lhs)) => {
                        match self.arr_idx(&lhs.name, lhs.idxs, lhs.src_info)? {
                            TInteger => (),
                            _ => bail_opt!(self, op.src_info, "invalid type for this arithmetic"),
                        }
                        self.chunk.emit_bytecode(DuplicateN, op.src_info);
                        self.chunk.append_u8(2, op.src_info);
                        self.chunk.emit_bytecode(GetArrayVal, op.src_info);
                        self.chunk
                            .emit_load_const(self.p.new_value_int(1), op.src_info);
                        match op.kind {
                            EraTokenKind::Increment => self.chunk.emit_bytecode(Add, op.src_info),
                            EraTokenKind::Decrement => {
                                self.chunk.emit_bytecode(Subtract, op.src_info)
                            }
                            _ => unreachable!(),
                        }
                        self.chunk.emit_bytecode(SetArrayVal, op.src_info);
                        self.chunk
                            .emit_load_const(self.p.new_value_int(1), op.src_info);
                        match op.kind {
                            EraTokenKind::Increment => {
                                self.chunk.emit_bytecode(Subtract, op.src_info)
                            }
                            EraTokenKind::Decrement => self.chunk.emit_bytecode(Add, op.src_info),
                            _ => unreachable!(),
                        }
                        TInteger
                    }
                    _ => bail_opt!(
                        self,
                        lhs.source_pos_info(),
                        true,
                        "invalid assignment target"
                    ),
                },
                _ => bail_opt!(self, op.src_info, true, "invalid arithmetic kind"),
            },
            EraExpr::Ternary(lhs, op1, mhs, op2, rhs) => {
                let lhs_si = lhs.source_pos_info();
                match self.expression(*lhs)? {
                    TInteger | TString => (),
                    _ => bail_opt!(self, lhs_si, true, "condition must not be void"),
                }
                let bt_then = self.chunk.emit_jump_cond_hold(op1.src_info);
                let rhs_k = self.expression(*rhs)?;
                let bt_done = self.chunk.emit_jump_hold(op2.src_info);
                bt_then.complete(self.chunk)?;
                let mhs_k = self.expression(*mhs)?;
                bt_done.complete(self.chunk)?;
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
                EraExpr::Term(EraTermExpr::Var(lhs)) => {
                    self.static_fun_call(&lhs.name, args, lhs.src_info)?
                }
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
    fn static_fun_call(
        &mut self,
        target: &str,
        args: Vec<Option<EraExpr>>,
        target_src_info: SourcePosInfo,
    ) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;

        let Some(&target_idx) = self.funcs.func_names.get(CaselessStr::new(&target)) else {
            // Check for built-in functions as fallback
            return Some(self.builtin_fun_call(target, args, target_src_info, false)?);
        };
        let target = self.funcs.funcs.get(target_idx).unwrap();
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
                let src_info = self
                    .chunk
                    .source_info_at(self.chunk.cur_bytes_cnt() - 1)
                    .unwrap();
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
                self.chunk
                    .emit_load_const(param.default_val.clone(), src_info);
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
                    let arg_kind = self.var_arr_no_pseudo(&var_expr.name, var_expr.src_info)?;
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
                    let arg_kind = self.expression(arg)?;
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
            let src_info = self
                .chunk
                .source_info_at(self.chunk.cur_bytes_cnt() - 1)
                .unwrap();
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
            self.chunk
                .emit_load_const(param.default_val.clone(), src_info);
        }
        self.chunk
            .emit_load_const(self.p.new_value_int(target_idx as _), target_src_info);
        self.chunk.emit_bytecode(FunCall, target_src_info);
        self.chunk
            .append_u8(target.params.len() as _, target_src_info);

        Some(match target.kind {
            EraFunKind::Procedure => EraExpressionValueKind::TVoid,
            EraFunKind::Function => EraExpressionValueKind::TInteger,
            EraFunKind::FunctionS => EraExpressionValueKind::TString,
        })
    }
    #[must_use]
    fn dynamic_fun_call(
        &mut self,
        target: EraExpr,
        args: Vec<Option<EraExpr>>,
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
                self.arr_idx(&x.name, x.idxs, x.src_info)?;
            } else {
                self.chunk.emit_load_const(self.p.new_value_int(0), arg_si);
                self.expression(arg)?;
            }
        }
        let TString = self.expression(target)? else {
            bail_opt!(self, target_si, "invalid type as dynamic function callee");
        };
        self.chunk.emit_bytecode(TryFunCall, target_si);
        self.chunk.append_u8(args_len as _, target_si);

        Some(TInteger)
    }
    #[must_use]
    fn builtin_fun_call(
        &mut self,
        target: &str,
        args: Vec<Option<EraExpr>>,
        src_info: SourcePosInfo,
        is_cmd: bool,
    ) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;
        use EraExpressionValueKind::*;
        use ValueKind::*;

        struct Ctx<'this, 'p, 'a, 'target, T> {
            this: &'this mut EraCompilerImplFunctionSite<'p, 'a, T>,
            target: &'target str,
            ret_kind: EraExpressionValueKind,
            src_info: SourcePosInfo,
            assign_to_result: bool,
        }
        impl<T: FnMut(&EraCompileErrorInfo)> Ctx<'_, '_, '_, '_, T> {
            fn result(&mut self) -> Option<()> {
                self.ret_kind = TInteger;
                if !self.assign_to_result {
                    return Some(());
                }
                self.this.var_result(self.src_info)?;
                self.this
                    .chunk
                    .emit_load_const(self.this.p.new_value_int(0), self.src_info);
                Some(())
            }
            fn results(&mut self) -> Option<()> {
                self.ret_kind = TString;
                if !self.assign_to_result {
                    return Some(());
                }
                self.this.var_results(self.src_info)?;
                self.this
                    .chunk
                    .emit_load_const(self.this.p.new_value_int(0), self.src_info);
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
                if !self.assign_to_result {
                    return;
                }
                match self.ret_kind {
                    TInteger | TString => {
                        self.this.chunk.emit_bytecode(SetArrayVal, self.src_info);
                        self.this.chunk.emit_bytecode(Pop, self.src_info);
                    }
                    TVoid => (),
                }
            }
        }
        let mut ctx = Ctx {
            this: self,
            ret_kind: TVoid,
            target,
            src_info,
            assign_to_result: is_cmd,
        };

        match target.to_ascii_uppercase().as_bytes() {
            b"GCREATE" => {
                ctx.result()?;
                let [gid, width, height] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(gid)?;
                ctx.this.expr_int(width)?;
                ctx.this.expr_int(height)?;
                ctx.this.chunk.emit_bytecode(GCreate, src_info);
            }
            b"GCREATEFROMFILE" => {
                ctx.result()?;
                let [gid, file_path] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(gid)?;
                ctx.this.expr_str(file_path)?;
                ctx.this.chunk.emit_bytecode(GCreateFromFile, src_info);
            }
            b"GDISPOSE" => {
                ctx.result()?;
                let [gid] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(gid)?;
                ctx.this.chunk.emit_bytecode(GDispose, src_info);
            }
            b"GCREATED" => {
                ctx.result()?;
                let [gid] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(gid)?;
                ctx.this.chunk.emit_bytecode(GCreated, src_info);
            }
            b"GDRAWSPRITE" => {
                ctx.result()?;
                let (gid, sprite_name, dest_x, dest_y, dest_width, dest_height);
                let mut color_matrix = None;
                match args.len() {
                    2 => {
                        [gid, sprite_name] = ctx.unpack_some_args(args)?;
                        [dest_x, dest_y] =
                            [EraExpr::new_int(0, src_info), EraExpr::new_int(0, src_info)];
                        [dest_width, dest_height] = [
                            EraExpr::new_int(-1, src_info),
                            EraExpr::new_int(-1, src_info),
                        ];
                    }
                    4 => {
                        [gid, sprite_name, dest_x, dest_y] = ctx.unpack_some_args(args)?;
                        [dest_width, dest_height] = [
                            EraExpr::new_int(-1, src_info),
                            EraExpr::new_int(-1, src_info),
                        ];
                    }
                    6 => {
                        [gid, sprite_name, dest_x, dest_y, dest_width, dest_height] =
                            ctx.unpack_some_args(args)?;
                    }
                    7 => {
                        let a_cm;
                        [
                            gid,
                            sprite_name,
                            dest_x,
                            dest_y,
                            dest_width,
                            dest_height,
                            a_cm,
                        ] = ctx.unpack_some_args(args)?;
                        let EraExpr::Term(EraTermExpr::Var(mut clr_mat)) = a_cm else {
                            bail_opt!(ctx.this, src_info, "the 7th parameter must be an array");
                        };
                        clr_mat.idxs.clear();
                        color_matrix = Some(clr_mat);
                    }
                    _ => bail_opt!(
                        ctx.this,
                        ctx.src_info,
                        format!(
                            "no overload of `{}` accepts {} arguments",
                            ctx.target,
                            args.len()
                        )
                    ),
                }
                ctx.this.expr_int(gid)?;
                ctx.this.expr_str(sprite_name)?;
                ctx.this.expr_int(dest_x)?;
                ctx.this.expr_int(dest_y)?;
                ctx.this.expr_int(dest_width)?;
                ctx.this.expr_int(dest_height)?;
                if let Some(color_matrix) = color_matrix {
                    ctx.this
                        .var_arr_int(&color_matrix.name, color_matrix.src_info)?;
                    ctx.this.chunk.emit_bytecode(Pop, src_info);
                    ctx.this
                        .chunk
                        .emit_bytecode(GDrawSpriteWithColorMatrix, src_info);
                } else {
                    ctx.this.chunk.emit_bytecode(GDrawSprite, src_info);
                }
            }
            b"GCLEAR" => {
                ctx.result()?;
                let [gid, color] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(gid)?;
                ctx.this.expr_int(color)?;
                ctx.this.chunk.emit_bytecode(GClear, src_info);
            }
            b"SPRITECREATE" => {
                ctx.result()?;
                let (sprite_name, gid, x, y, width, height);
                match args.len() {
                    2 => {
                        [sprite_name, gid] = ctx.unpack_some_args(args)?;
                        [x, y] = [EraExpr::new_int(0, src_info), EraExpr::new_int(0, src_info)];
                        [width, height] = [
                            EraExpr::new_int(-1, src_info),
                            EraExpr::new_int(-1, src_info),
                        ];
                    }
                    6 => {
                        [sprite_name, gid, x, y, width, height] = ctx.unpack_some_args(args)?;
                    }
                    _ => bail_opt!(
                        ctx.this,
                        ctx.src_info,
                        format!(
                            "no overload of `{}` accepts {} arguments",
                            ctx.target,
                            args.len()
                        )
                    ),
                }
                ctx.this.expr_str(sprite_name)?;
                ctx.this.expr_int(gid)?;
                ctx.this.expr_int(x)?;
                ctx.this.expr_int(y)?;
                ctx.this.expr_int(width)?;
                ctx.this.expr_int(height)?;
                ctx.this.chunk.emit_bytecode(SpriteCreate, src_info);
            }
            b"SPRITEDISPOSE" => {
                ctx.result()?;
                let [name] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(name)?;
                ctx.this.chunk.emit_bytecode(SpriteDispose, src_info);
            }
            b"SPRITECREATED" => {
                ctx.result()?;
                let [name] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(name)?;
                ctx.this.chunk.emit_bytecode(SpriteCreated, src_info);
            }
            b"SPRITEANIMECREATE" => {
                ctx.result()?;
                let [name, width, height] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(name)?;
                ctx.this.expr_int(width)?;
                ctx.this.expr_int(height)?;
                ctx.this.chunk.emit_bytecode(SpriteAnimeCreate, src_info);
            }
            b"SPRITEANIMEADDFRAME" => {
                ctx.result()?;
                let [name, gid, x, y, width, height, offset_x, offset_y, delay] =
                    ctx.unpack_some_args(args)?;
                ctx.this.expr_str(name)?;
                ctx.this.expr_int(gid)?;
                ctx.this.expr_int(x)?;
                ctx.this.expr_int(y)?;
                ctx.this.expr_int(width)?;
                ctx.this.expr_int(height)?;
                ctx.this.expr_int(offset_x)?;
                ctx.this.expr_int(offset_y)?;
                ctx.this.expr_int(delay)?;
                ctx.this.chunk.emit_bytecode(SpriteAnimeAddFrame, src_info);
            }
            b"GETBIT" => {
                ctx.result()?;
                let [val, bit] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(val)?;
                ctx.this.expr_int(bit)?;
                ctx.this.chunk.emit_bytecode(GetBit, src_info);
            }
            b"GETSTYLE" => {
                ctx.result()?;
                ctx.this.arr_get("@STYLE", vec![], src_info)?;
            }
            // b"CHKFONT" => todo!(),
            // b"GETFONT" => todo!(),
            // b"REPLACE" => todo!(),
            // b"SUBSTRING" => todo!(),
            // b"SUBSTRINGU" => todo!(),
            // b"STRFIND" => todo!(),
            // b"STRFINDU" => todo!(),
            // b"STRLENS" => todo!(),
            // b"STRLENSU" => todo!(),
            // b"STRCOUNT" => todo!(),
            // b"CUREENTREDRAW" => todo!(),
            // b"CURRENTALIGN" => todo!(),
            // b"CHKCHARADATA" => todo!(),
            // b"SAVETEXT" => todo!(),
            b"MAX" => {
                ctx.result()?;
                let [a1, a2] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(a1)?;
                ctx.this.expr_int(a2)?;
                ctx.this.chunk.emit_bytecode(MaximumInt, src_info);
            }
            b"MIN" => {
                ctx.result()?;
                let [a1, a2] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(a1)?;
                ctx.this.expr_int(a2)?;
                ctx.this.chunk.emit_bytecode(MinimumInt, src_info);
            }
            b"LIMIT" => {
                ctx.result()?;
                let [a, amax, amin] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(a)?;
                ctx.this.expr_int(amax)?;
                ctx.this.expr_int(amin)?;
                ctx.this.chunk.emit_bytecode(ClampInt, src_info);
            }
            b"INRANGE" => {
                ctx.result()?;
                let [a, amax, amin] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(a)?;
                ctx.this.expr_int(amax)?;
                ctx.this.expr_int(amin)?;
                ctx.this.chunk.emit_bytecode(InRangeInt, src_info);
            }
            // b"FINDCHARA" => todo!(),
            // b"FINDLASTCHARA" => todo!(),
            _ => bail_opt!(
                ctx.this,
                src_info,
                format!("function `{target}` is undefined or has no matching overloads")
            ),
        }

        Some(ctx.ret_kind)
    }
    #[must_use]
    fn statement(&mut self, func_kind: EraFunKind, stmt: EraStmt) -> Option<()> {
        use EraBytecodePrimaryType::*;
        use EraCommandStmt as Cmd;
        use EraExpressionValueKind::*;

        let stmt = match stmt {
            EraStmt::Expr(x) => {
                // Evaluate and discard values
                let src_info = x.source_pos_info();
                match self.expression(x)? {
                    TInteger | TString => {
                        self.chunk.emit_bytecode(Pop, src_info);
                    }
                    TVoid => (),
                }
                return Some(());
            }
            EraStmt::Command(x) => x,
            EraStmt::Label(x) => {
                let label = EraGotoLabelInfo {
                    stack_balance: self.stack_balance,
                    pos: self.chunk.cur_bytes_cnt(),
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
                self.chunk.emit_load_const(
                    self.p.new_value_str("invalid statement".to_owned()),
                    src_info,
                );
                self.chunk.emit_bytecode(InvalidWithMessage, src_info);
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
                    match self.expression(v)? {
                        TInteger => self.chunk.emit_bytecode(ConvertToString, src_info),
                        TString => (),
                        TVoid => bail_opt!(self, src_info, "cannot print void"),
                    }
                }
                self.chunk.emit_bytecode(BuildString, src_info);
                // TODO: BuildString check overflow
                self.chunk.append_u8(args_cnt as _, src_info);

                match u8::from(x.flags) {
                    x if x == PrintExtendedFlags::new().into() => {
                        self.chunk.emit_bytecode(Print, src_info);
                    }
                    x if x == PrintExtendedFlags::new().with_is_line(true).into() => {
                        self.chunk.emit_bytecode(PrintLine, src_info);
                    }
                    x => {
                        self.chunk.emit_bytecode(PrintExtended, src_info);
                        self.chunk.append_u8(x, src_info);
                    }
                }
            }
            Cmd::PrintData(x) => {
                todo!();
            }
            Cmd::Wait(x) => {
                self.chunk.emit_bytecode(Wait, x.src_info);
            }
            Cmd::If(x) => {
                // TODO: Optimize when there is no else body
                self.expression(x.cond)?;
                let backtrack_then = self.chunk.emit_jump_cond_hold(x.src_info);
                for stmt in x.else_body {
                    self.statement(func_kind, stmt)?;
                }
                // HACK: Steal source pos info from last statement
                let last_src_info = self
                    .chunk
                    .source_info_at(self.chunk.cur_bytes_cnt() - 1)
                    .unwrap();
                let backtrack_done = self.chunk.emit_jump_hold(last_src_info);
                if backtrack_then.complete(self.chunk).is_none() {
                    self.report_err(x.src_info, true, "jump too far to be encoded in bytecode");
                    return None;
                }
                for stmt in x.body {
                    self.statement(func_kind, stmt)?;
                }
                if backtrack_done.complete(self.chunk).is_none() {
                    self.report_err(x.src_info, true, "jump too far to be encoded in bytecode");
                    return None;
                }
            }
            Cmd::Quit(x) => {
                self.chunk.emit_bytecode(Quit, x.src_info);
            }
            Cmd::SelectCase(x) => {
                self.stack_balance += 1;

                let cond_k = self.expression(x.cond)?;
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
                        bt.complete(self.chunk)?;
                    }
                    let mut bt_bodies = Vec::new();
                    for case_cond in case_conds {
                        use crate::parser::EraSelectCaseStmtCondition::*;
                        self.chunk.emit_bytecode(Duplicate, x.src_info);
                        match case_cond {
                            Single(x) => {
                                let x_si = x.source_pos_info();
                                if cond_k != self.expression(x)? {
                                    bail_opt!(self, x_si, "case expression type mismatch");
                                }
                                self.chunk.emit_bytecode(CompareEq, x_si);
                            }
                            Range(lhs, rhs) => {
                                // No short-circuit within single range?
                                let lhs_si = lhs.source_pos_info();
                                if cond_k != self.expression(lhs)? {
                                    bail_opt!(self, lhs_si, "case expression type mismatch");
                                }
                                self.chunk.emit_bytecode(CompareL, lhs_si);
                                self.chunk.emit_bytecode(LogicalNot, lhs_si);
                                self.chunk.emit_bytecode(DuplicateN, lhs_si);
                                self.chunk.append_u8(2, lhs_si);
                                self.chunk.emit_bytecode(Pop, lhs_si);
                                let rhs_si = rhs.source_pos_info();
                                if cond_k != self.expression(rhs)? {
                                    bail_opt!(self, rhs_si, "case expression type mismatch");
                                }
                                self.chunk.emit_bytecode(CompareLEq, rhs_si);
                                self.chunk.emit_bytecode(BitAnd, rhs_si);
                            }
                            Condition(op, x) => {
                                let x_si = x.source_pos_info();
                                if cond_k != self.expression(x)? {
                                    bail_opt!(self, x_si, "case expression type mismatch");
                                }
                                let op_si = op.src_info;
                                match op.kind {
                                    EraTokenKind::CmpL => self.chunk.emit_bytecode(CompareL, op_si),
                                    EraTokenKind::CmpLEq => {
                                        self.chunk.emit_bytecode(CompareLEq, op_si)
                                    }
                                    EraTokenKind::CmpEq => {
                                        self.chunk.emit_bytecode(CompareEq, op_si)
                                    }
                                    EraTokenKind::CmpNEq => {
                                        self.chunk.emit_bytecode(CompareEq, op_si);
                                        self.chunk.emit_bytecode(LogicalNot, op_si);
                                    }
                                    EraTokenKind::CmpG => {
                                        self.chunk.emit_bytecode(CompareLEq, op_si);
                                        self.chunk.emit_bytecode(LogicalNot, op_si);
                                    }
                                    EraTokenKind::CmpGEq => {
                                        self.chunk.emit_bytecode(CompareL, op_si);
                                        self.chunk.emit_bytecode(LogicalNot, op_si);
                                    }
                                    _ => {
                                        bail_opt!(self, op_si, "unsupported operator as case")
                                    }
                                }
                            }
                        }
                        bt_bodies.push(self.chunk.emit_jump_cond_hold(x.src_info));
                    }
                    bt_else = Some(self.chunk.emit_jump_hold(x.src_info));
                    for bt in bt_bodies {
                        bt.complete(self.chunk)?;
                    }
                    for stmt in case_body {
                        self.statement(func_kind, stmt)?;
                    }
                    bt_dones.push(self.chunk.emit_jump_hold(x.src_info));
                }
                if let Some(bt) = bt_else {
                    bt.complete(self.chunk)?;
                }
                for stmt in x.case_else {
                    self.statement(func_kind, stmt)?;
                }
                for bt in bt_dones {
                    bt.complete(self.chunk)?;
                }
                self.chunk.emit_bytecode(Pop, x.src_info);

                self.stack_balance -= 1;
            }
            Cmd::While(x) => {
                let start_pos = self.chunk.cur_bytes_cnt();
                self.loop_structs.push(EraLoopStructCodeMetadata::new());
                self.expression(x.cond)?;
                self.chunk.emit_bytecode(LogicalNot, x.src_info);
                let backtrack_done = self.chunk.emit_jump_cond_hold(x.src_info);
                for stmt in x.body {
                    self.statement(func_kind, stmt)?;
                }
                let done_index = self.chunk.cur_bytes_cnt();
                // HACK: Steal source pos info from last statement
                let last_src_info = self
                    .chunk
                    .source_info_at(self.chunk.cur_bytes_cnt() - 1)
                    .unwrap();
                self.chunk
                    .emit_jump(-((done_index - start_pos) as isize), last_src_info);
                if backtrack_done.complete(self.chunk).is_none() {
                    bail_opt!(self, x.src_info, "jump too far to be encoded in bytecode");
                }
                let loop_struct = self.loop_structs.pop().unwrap();
                for bt in loop_struct.continue_queue {
                    bt.complete_at(self.chunk, start_pos)?;
                }
                for bt in loop_struct.done_queue {
                    bt.complete(self.chunk)?;
                }
            }
            Cmd::Call(x) => match x.func {
                // Try to optimize into a static call
                EraExpr::Term(EraTermExpr::Literal(EraLiteral::String(func, si))) => {
                    match self.static_fun_call(&func, x.args, si)? {
                        TInteger | TString => self.chunk.emit_bytecode(Pop, x.src_info),
                        TVoid => (),
                    }
                }
                _ => {
                    self.dynamic_fun_call(x.func, x.args)?;
                    self.chunk.emit_bytecode(Pop, x.src_info);
                }
            },
            Cmd::TryCall(x) => {
                // Enforce dynamic call
                self.dynamic_fun_call(x.func, x.args)?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::TryCCall(x) => {
                // Enforce dynamic call
                self.dynamic_fun_call(x.func, x.args)?;
                let bt_then = self.chunk.emit_jump_cond_hold(x.src_info);
                for stmt in x.catch_body {
                    self.statement(func_kind, stmt)?;
                }
                // HACK: Steal source pos info from last statement
                let last_src_info = self
                    .chunk
                    .source_info_at(self.chunk.cur_bytes_cnt() - 1)
                    .unwrap();
                let bt_done = self.chunk.emit_jump_hold(last_src_info);
                if bt_then.complete(self.chunk).is_none() {
                    bail_opt!(self, x.src_info, "jump too far to be encoded in bytecode");
                }
                for stmt in x.then_body {
                    self.statement(func_kind, stmt)?;
                }
                if bt_done.complete(self.chunk).is_none() {
                    bail_opt!(self, x.src_info, "jump too far to be encoded in bytecode");
                }
            }
            Cmd::Jump(x) => {
                let si = x.src_info;
                self.statement(func_kind, EraStmt::Command(Cmd::Call(x)))?;
                self.final_return_backtracks
                    .push(self.chunk.emit_jump_hold(si));
            }
            Cmd::TryJump(x) => {
                let si = x.src_info;
                self.statement(func_kind, EraStmt::Command(Cmd::TryCall(x)))?;
                self.final_return_backtracks
                    .push(self.chunk.emit_jump_hold(si));
            }
            Cmd::TryCJump(x) => {
                let si = x.src_info;
                self.statement(func_kind, EraStmt::Command(Cmd::TryCCall(x)))?;
                self.final_return_backtracks
                    .push(self.chunk.emit_jump_hold(si));
            }
            Cmd::Return(x) => {
                // NOTE: EraBasic only assigns values to RESULT:*
                if let EraFunKind::Procedure = func_kind {
                    // TODO: Optimize RETURN
                    for (val_idx, val) in x.vals.into_iter().enumerate() {
                        let val_si = val.source_pos_info();
                        let val_idx = EraExpr::Term(EraTermExpr::Literal(EraLiteral::Integer(
                            val_idx as _,
                            val_si,
                        )));
                        self.arr_set("RESULT", vec![val_idx], val_si, |this| {
                            let mut k = this.expression(val)?;
                            if let TString = k {
                                this.chunk.emit_bytecode(ConvertToInteger, val_si);
                                k = TInteger;
                            }
                            Some(k)
                        })?;
                    }
                    self.chunk.emit_bytecode(ReturnVoid, x.src_info);
                } else {
                    let src_info = x.src_info;
                    let Ok([val]) = TryInto::<[_; 1]>::try_into(x.vals) else {
                        bail_opt!(self, src_info, "invalid return type for current function");
                    };
                    let val_kind = self.expression(val)?;
                    match (func_kind, val_kind) {
                        (EraFunKind::Function, TInteger) => {
                            self.chunk.emit_bytecode(ReturnInteger, src_info);
                        }
                        (EraFunKind::FunctionS, TString) => {
                            self.chunk.emit_bytecode(ReturnString, src_info);
                        }
                        _ => bail_opt!(self, src_info, "invalid return type for current function"),
                    }
                }
            }
            Cmd::Continue(x) => {
                let Some(loop_struct) = self.loop_structs.last_mut() else {
                    bail_opt!(self, x.src_info, "loop controlling statements are valid only in the context of a loop structure");
                };
                // let pos_delta = (self.chunk.cur_bytes_cnt() - loop_struct.continue_pos) as isize;
                // self.chunk.emit_jump(-pos_delta, x.src_info);
                loop_struct
                    .continue_queue
                    .push(self.chunk.emit_jump_hold(x.src_info));
            }
            Cmd::Break(x) => {
                let Some(loop_struct) = self.loop_structs.last_mut() else {
                    bail_opt!(self, x.src_info, "loop controlling statements are valid only in the context of a loop structure");
                };
                loop_struct
                    .done_queue
                    .push(self.chunk.emit_jump_hold(x.src_info));
            }
            Cmd::Throw(x) => {
                self.expression(x.val)?;
                self.chunk.emit_bytecode(Throw, x.src_info);
            }
            Cmd::Repeat(x) => {
                // for (loopCount = ?, COUNT = 0; COUNT < loopCount; COUNT++) {}
                // Use stack to store temporary loopCount
                self.stack_balance += 1;

                match self.expression(x.loop_cnt)? {
                    TInteger => (),
                    TString | TVoid => {
                        bail_opt!(self, x.src_info, "invalid expression for REPEAT condition")
                    }
                }
                self.arr_set("COUNT", vec![], x.src_info, |this| {
                    this.chunk
                        .emit_load_const(this.p.new_value_int(0), x.src_info);
                    Some(TInteger)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
                let bt_body = self.chunk.emit_jump_hold(x.src_info);
                let start_pos = self.chunk.cur_bytes_cnt();
                self.loop_structs.push(EraLoopStructCodeMetadata::new());
                let step_fn = |this: &mut Self| {
                    this.arr_set("COUNT", vec![], x.src_info, |this| {
                        this.arr_get("COUNT", vec![], x.src_info)?;
                        this.chunk.emit_load_const(this.p.new_value_int(1), x.src_info);
                        this.chunk.emit_bytecode(Add, x.src_info);
                        Some(TInteger)
                    })?;
                    this.chunk.emit_bytecode(Pop, x.src_info);
                    Some(())
                };
                step_fn(self)?;
                bt_body.complete(self.chunk)?;
                self.chunk.emit_bytecode(Duplicate, x.src_info);
                self.arr_get("COUNT", vec![], x.src_info)?;
                self.chunk.emit_bytecode(CompareLEq, x.src_info);
                let bt_done = self.chunk.emit_jump_cond_hold(x.src_info);
                for stmt in x.body {
                    self.statement(func_kind, stmt)?;
                }
                {
                    let offset = -((self.chunk.cur_bytes_cnt() - start_pos) as isize);
                    self.chunk.emit_jump(offset, x.src_info);
                }
                let loop_struct = self.loop_structs.pop().unwrap();
                for bt in loop_struct.continue_queue {
                    bt.complete_at(self.chunk, start_pos)?;
                }
                for bt in loop_struct.done_queue {
                    bt.complete(self.chunk)?;
                }
                // NOTE: Emulates Eramaker behavior (inc COUNT even when break'ing)
                step_fn(self)?;
                bt_done.complete(self.chunk)?;
                self.chunk.emit_bytecode(Pop, x.src_info);

                self.stack_balance -= 1;
            }
            Cmd::Goto(x) => {
                let backtrack = self.chunk.emit_jump_hold(x.src_info);
                self.goto_backtracks.push(EraGotoJumpInfo {
                    backtrack,
                    stack_balance: self.stack_balance,
                    target: x.target.into(),
                });
            }
            Cmd::For(x) => {
                // var(2) + end(1) + step(1)
                self.stack_balance += 4;

                match self.arr_idx(&x.var.name, x.var.idxs, x.var.src_info)? {
                    TInteger => (),
                    TString | TVoid => {
                        bail_opt!(self, x.src_info, "invalid expression for FOR statement")
                    }
                }
                self.chunk.emit_duplicate_n(2, x.src_info);
                match self.expression(x.start)? {
                    TInteger => (),
                    TString | TVoid => {
                        bail_opt!(self, x.src_info, "invalid expression for FOR statement")
                    }
                }
                self.chunk.emit_bytecode(SetArrayVal, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);
                match self.expression(x.end)? {
                    TInteger => (),
                    TString | TVoid => {
                        bail_opt!(self, x.src_info, "invalid expression for FOR statement")
                    }
                }
                match self.expression(x.step)? {
                    TInteger => (),
                    TString | TVoid => {
                        bail_opt!(self, x.src_info, "invalid expression for FOR statement")
                    }
                }
                let emit_ternary_fn =
                    |this: &mut Self,
                     src_info,
                     then_fn: &mut dyn Fn(&mut Self, SourcePosInfo),
                     else_fn: &mut dyn Fn(&mut Self, SourcePosInfo)| {
                        // Assuming stack has condition pushed
                        let bt_then = this.chunk.emit_jump_cond_hold(src_info);
                        else_fn(this, src_info);
                        let bt_else = this.chunk.emit_jump_hold(src_info);
                        bt_then.complete(this.chunk);
                        then_fn(this, src_info);
                        bt_else.complete(this.chunk);
                    };
                let zero_val = self.p.new_value_int(0);
                let emit_cond_fn = |this: &mut Self, src_info| {
                    this.chunk.emit_duplicate_n(2, src_info);
                    this.chunk.emit_load_const(zero_val.clone(), src_info);
                    this.chunk.emit_bytecode(CompareL, src_info);
                    emit_ternary_fn(
                        this,
                        x.src_info,
                        &mut |this, src_info| {
                            this.chunk.emit_duplicate_one_n(5, src_info);
                            this.chunk.emit_duplicate_one_n(5, src_info);
                            this.chunk.emit_bytecode(GetArrayVal, src_info);
                            this.chunk.emit_bytecode(CompareL, src_info);
                            this.chunk.emit_bytecode(LogicalNot, src_info);
                        },
                        &mut |this, src_info| {
                            this.chunk.emit_duplicate_one_n(5, src_info);
                            this.chunk.emit_duplicate_one_n(5, src_info);
                            this.chunk.emit_bytecode(GetArrayVal, src_info);
                            this.chunk.emit_bytecode(CompareLEq, src_info);
                        },
                    );
                };
                let emit_step_fn = |this: &mut Self, src_info| {
                    this.chunk.emit_duplicate_one_n(4, src_info);
                    this.chunk.emit_duplicate_one_n(4, src_info);
                    this.chunk.emit_duplicate_n(2, src_info);
                    this.chunk.emit_bytecode(GetArrayVal, src_info);
                    this.chunk.emit_duplicate_one_n(4, src_info);
                    this.chunk.emit_bytecode(Add, src_info);
                    this.chunk.emit_bytecode(SetArrayVal, src_info);
                    this.chunk.emit_bytecode(Pop, src_info);
                };
                let bt_body = self.chunk.emit_jump_hold(x.src_info);
                let start_pos = self.chunk.cur_bytes_cnt();
                self.loop_structs.push(EraLoopStructCodeMetadata::new());
                emit_step_fn(self, x.src_info);
                bt_body.complete(self.chunk)?;
                emit_cond_fn(self, x.src_info);
                let bt_done = self.chunk.emit_jump_cond_hold(x.src_info);
                for stmt in x.body {
                    self.statement(func_kind, stmt)?;
                }
                {
                    let offset = -((self.chunk.cur_bytes_cnt() - start_pos) as isize);
                    self.chunk.emit_jump(offset, x.src_info);
                }
                let loop_struct = self.loop_structs.pop().unwrap();
                for bt in loop_struct.continue_queue {
                    bt.complete_at(self.chunk, start_pos)?;
                }
                for bt in loop_struct.done_queue {
                    bt.complete(self.chunk)?;
                }
                // NOTE: Emulates Eramaker behavior (inc COUNT even when break'ing)
                emit_step_fn(self, x.src_info);
                bt_done.complete(self.chunk)?;
                self.chunk.emit_bytecode(Pop, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);

                self.stack_balance -= 4;
            }
            Cmd::DoLoop(x) => {
                let si = x.src_info;
                let start_pos = self.chunk.cur_bytes_cnt();
                self.loop_structs.push(EraLoopStructCodeMetadata::new());
                for stmt in x.body {
                    self.statement(func_kind, stmt)?;
                }
                let continue_pos = self.chunk.cur_bytes_cnt();
                self.expression(x.cond)?;
                self.chunk
                    .emit_jump_cond_hold(si)
                    .complete_at(self.chunk, start_pos);
                let loop_struct = self.loop_structs.pop().unwrap();
                for bt in loop_struct.continue_queue {
                    bt.complete_at(self.chunk, continue_pos)?;
                }
                for bt in loop_struct.done_queue {
                    bt.complete(self.chunk)?;
                }
            }
            Cmd::Split(x) => {
                self.expr_str(x.input)?;
                self.expr_str(x.separator)?;
                self.arr_idx_str(&x.dest.name, x.dest.idxs, x.dest.src_info)?;
                self.arr_idx_int(&x.dest_count.name, x.dest_count.idxs, x.dest_count.src_info)?;
                self.chunk.emit_bytecode(SplitString, x.src_info);
            }
            Cmd::ResultCmdCall(x) => {
                self.builtin_fun_call(&x.name, x.args, x.src_info, true)?;
            }
            // TODO...
            Cmd::Swap(x) => {
                let k1 = self.arr_idx(&x.v1.name, x.v1.idxs, x.v1.src_info)?;
                let k2 = self.arr_idx(&x.v2.name, x.v2.idxs, x.v2.src_info)?;
                if k1 != k2 || matches!(k1, TVoid) {
                    bail_opt!(self, x.src_info, "invalid SWAP operands");
                }
                self.chunk.emit_duplicate_one_n(4, x.src_info);
                self.chunk.emit_duplicate_one_n(4, x.src_info);
                self.chunk.emit_bytecode(GetArrayVal, x.src_info);
                self.chunk.emit_duplicate_n(5, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);
                self.chunk.emit_bytecode(GetArrayVal, x.src_info);
                self.chunk.emit_bytecode(SetArrayVal, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);
                self.chunk.emit_bytecode(SetArrayVal, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);
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
    fn var_arr(&mut self, var_name: &str, src_info: SourcePosInfo) -> Option<EraVarArrCompileInfo> {
        use EraBytecodePrimaryType::*;
        use EraExpressionValueKind::*;
        use EraVarArrCompileInfo::*;

        let is_in_global_frame;
        let var_idx;
        let var_kind;
        let var_dims_cnt;
        if let Some(var) = self.func_info.local_vars.get(CaselessStr::new(&var_name)) {
            (var_kind, var_dims_cnt) = match var.init_val.clone().into_unpacked() {
                FlatValue::ArrInt(x) => (TInteger, x.borrow().dims.len()),
                FlatValue::ArrStr(x) => (TString, x.borrow().dims.len()),
                _ => unreachable!(),
            };
            is_in_global_frame = var.is_in_global_frame;
            var_idx = var.idx_in_frame;
        } else if let Some(var) = self.p.vars.get_var_idx(&var_name).or_else(|| {
            self.p
                .vars
                .get_var_idx(&format!("{}@{}", &var_name, self.func_info.name.as_str()))
        }) {
            (var_kind, var_dims_cnt) = match self
                .p
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
            if var_name.eq_ignore_ascii_case("RAND") {
                return Some(Pseudo(EraPseudoVarKind::Rand));
            } else {
                bail_opt!(
                    self,
                    src_info,
                    true,
                    format!("undefined variable `{}`", var_name)
                );
            }
        }
        self.chunk
            .emit_load_const(self.p.new_value_int(var_idx as _), src_info);
        if is_in_global_frame {
            self.chunk.emit_bytecode(GetGlobal, src_info);
        } else {
            self.chunk.emit_bytecode(GetLocal, src_info);
        }

        Some(Normal(var_kind, var_dims_cnt.try_into().unwrap()))
    }
    #[must_use]
    fn var_arr_with_dims(
        &mut self,
        var_name: &str,
        src_info: SourcePosInfo,
    ) -> Option<EraVarArrCompileInfoWithDims> {
        use EraBytecodePrimaryType::*;
        use EraExpressionValueKind::*;
        use EraVarArrCompileInfoWithDims::*;

        let is_in_global_frame;
        let var_idx;
        let var_kind;
        let var_dims;
        if let Some(var) = self.func_info.local_vars.get(CaselessStr::new(&var_name)) {
            (var_kind, var_dims) = match var.init_val.clone().into_unpacked() {
                FlatValue::ArrInt(x) => (TInteger, SmallVec::from_slice(&x.borrow().dims)),
                FlatValue::ArrStr(x) => (TString, SmallVec::from_slice(&x.borrow().dims)),
                _ => unreachable!(),
            };
            is_in_global_frame = var.is_in_global_frame;
            var_idx = var.idx_in_frame;
        } else if let Some(var) = self.p.vars.get_var_idx(&var_name).or_else(|| {
            self.p
                .vars
                .get_var_idx(&format!("{}@{}", &var_name, self.func_info.name.as_str()))
        }) {
            (var_kind, var_dims) = match self
                .p
                .vars
                .get_var_info(var)
                .unwrap()
                .val
                .clone()
                .into_unpacked()
            {
                FlatValue::ArrInt(x) => (TInteger, SmallVec::from_slice(&x.borrow().dims)),
                FlatValue::ArrStr(x) => (TString, SmallVec::from_slice(&x.borrow().dims)),
                _ => unreachable!(),
            };
            is_in_global_frame = true;
            var_idx = var;
        } else {
            // HACK: For undefined identifiers, do special handling (such as RAND pesudo-array access)
            if var_name.eq_ignore_ascii_case("RAND") {
                return Some(Pseudo(EraPseudoVarKind::Rand));
            } else {
                bail_opt!(
                    self,
                    src_info,
                    true,
                    format!("undefined variable `{}`", var_name)
                );
            }
        }
        self.chunk
            .emit_load_const(self.p.new_value_int(var_idx as _), src_info);
        if is_in_global_frame {
            self.chunk.emit_bytecode(GetGlobal, src_info);
        } else {
            self.chunk.emit_bytecode(GetLocal, src_info);
        }

        Some(Normal(var_kind, var_dims))
    }
    fn arr_mdidx(
        &mut self,
        var_name: &str,
        idxs: Vec<EraExpr>,
        src_info: SourcePosInfo,
    ) -> Option<(EraExpressionValueKind, u8)> {
        use EraExpressionValueKind::*;
        use EraVarArrCompileInfo::*;
        let Normal(var_kind, var_dims_cnt) = self.var_arr(var_name, src_info)? else {
            bail_opt!(self, src_info, true, "invalid access to pesudo variable");
        };
        // TODO: Check array size & type
        if var_dims_cnt > 1 && idxs.len() < var_dims_cnt as usize {
            self.report_err(src_info, false, "non-compliant use of array variable");
        }
        let idx_len = idxs.len();
        for idx in idxs {
            let src_info = idx.source_pos_info();
            let idx_kind = match idx {
                // HACK: Contextual indices replacing
                EraExpr::Term(EraTermExpr::Var(x))
                    if x.idxs.is_empty()
                        && routine::is_csv_var(&var_name)
                        && self
                            .p
                            .contextual_indices
                            .contains_key(Ascii::new_str(&x.name)) =>
                {
                    let idx = self
                        .p
                        .contextual_indices
                        .get(Ascii::new_str(&x.name))
                        .copied()
                        .unwrap();
                    self.chunk
                        .emit_load_const(self.p.new_value_int(idx as _), x.src_info);
                    TInteger
                }
                _ => self.expression(idx)?,
            };
            match idx_kind {
                TInteger => (),
                _ => bail_opt!(self, src_info, true, "array indices must be integers"),
            }
        }
        Some((var_kind, idx_len.try_into().unwrap()))
    }
    #[must_use]
    fn arr_mdget(
        &mut self,
        var_name: &str,
        idxs: Vec<EraExpr>,
        src_info: SourcePosInfo,
    ) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;
        let (var_kind, idx_len) = self.arr_mdidx(var_name, idxs, src_info)?;
        self.chunk.emit_bytecode(GetMDArrayVal, src_info);
        self.chunk.append_u8(idx_len, src_info);
        Some(var_kind)
    }
    #[must_use]
    fn arr_mdset(
        &mut self,
        var_name: &str,
        idxs: Vec<EraExpr>,
        src_info: SourcePosInfo,
        rhs: impl FnOnce(&mut Self) -> Option<EraExpressionValueKind>,
    ) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;
        let (var_kind, idx_len) = self.arr_mdidx(var_name, idxs, src_info)?;
        if var_kind != rhs(self)? {
            bail_opt!(
                self,
                src_info,
                true,
                "right-hand expression type is incompatible with left-hand"
            )
        }
        self.chunk.emit_bytecode(SetMDArrayVal, src_info);
        self.chunk.append_u8(idx_len, src_info);
        Some(var_kind)
    }
    #[must_use]
    fn arr_idx_or_pseudo(
        &mut self,
        var_name: &str,
        idxs: Vec<EraExpr>,
        src_info: SourcePosInfo,
    ) -> Option<EraVarArrCompileInfo> {
        use EraBytecodePrimaryType::*;
        use EraExpressionValueKind::*;
        use EraVarArrCompileInfoWithDims::*;
        let info = self.var_arr_with_dims(var_name, src_info)?;
        let slim_info = EraVarArrCompileInfo::from_fat(&info);
        match info {
            Pseudo(kind) => match kind {
                EraPseudoVarKind::Rand => {
                    if idxs.len() != 1 {
                        bail_opt!(
                            self,
                            src_info,
                            true,
                            "`RAND` requires exactly one parameter"
                        );
                    }
                    let rand_max = idxs.into_iter().next().unwrap();
                    let src_info = rand_max.source_pos_info();
                    match self.expression(rand_max)? {
                        TInteger => (),
                        _ => bail_opt!(self, src_info, true, "array indices must be integers"),
                    }
                }
            },
            Normal(var_kind, var_dims) => {
                // TODO: Check array size & type
                if var_dims.len() > 1 && idxs.len() < var_dims.len() {
                    self.report_err(src_info, false, "non-compliant use of array variable");
                }
                // FIXME: Insert checks for indices overflow for MD arrays
                // Decay array values to primitive values
                let idx_len = idxs.len();
                if idx_len > var_dims.len() {
                    bail_opt!(self, src_info, "too many indices into array");
                }
                self.chunk
                    .emit_load_const(self.p.new_value_int(0), src_info);
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
                for (idx, stride) in idxs.into_iter().zip(strides.into_iter().rev()) {
                    let src_info = idx.source_pos_info();

                    let idx_kind = match idx {
                        // HACK: Contextual indices replacing
                        EraExpr::Term(EraTermExpr::Var(x))
                            if x.idxs.is_empty()
                                && routine::is_csv_var(&var_name)
                                && self
                                    .p
                                    .contextual_indices
                                    .contains_key(Ascii::new_str(&x.name)) =>
                        {
                            let idx = self
                                .p
                                .contextual_indices
                                .get(Ascii::new_str(&x.name))
                                .copied()
                                .unwrap();
                            self.chunk
                                .emit_load_const(self.p.new_value_int(idx as _), x.src_info);
                            TInteger
                        }
                        _ => self.expression(idx)?,
                    };
                    match idx_kind {
                        TInteger => (),
                        _ => bail_opt!(self, src_info, true, "array indices must be integers"),
                    }

                    self.chunk
                        .emit_load_const(self.p.new_value_int(stride as _), src_info);
                    self.chunk.emit_bytecode(Multiply, src_info);
                    self.chunk.emit_bytecode(Add, src_info);
                }
            }
        }
        Some(slim_info)
    }
    #[must_use]
    fn arr_idx(
        &mut self,
        var_name: &str,
        idxs: Vec<EraExpr>,
        src_info: SourcePosInfo,
    ) -> Option<EraExpressionValueKind> {
        use EraVarArrCompileInfo::*;
        let Normal(var_kind, var_dims_cnt) = self.arr_idx_or_pseudo(var_name, idxs, src_info)?
        else {
            bail_opt!(self, src_info, true, "invalid access to pesudo variable");
        };
        Some(var_kind)
    }
    #[must_use]
    fn arr_get(
        &mut self,
        var_name: &str,
        idxs: Vec<EraExpr>,
        src_info: SourcePosInfo,
    ) -> Option<EraExpressionValueKind> {
        // NOTE: This method handles pseudo variables (like RAND:N)
        use EraBytecodePrimaryType::*;
        use EraExpressionValueKind::*;
        use EraVarArrCompileInfo::*;
        let idxs_len = idxs.len();
        let info = self.arr_idx_or_pseudo(var_name, idxs, src_info)?;
        let var_kind = match info {
            Pseudo(kind) => match kind {
                EraPseudoVarKind::Rand => {
                    self.chunk.emit_bytecode(GetRandomMax, src_info);
                    TInteger
                }
            },
            Normal(var_kind, idxs_len) => {
                self.chunk.emit_bytecode(GetArrayVal, src_info);
                var_kind
            }
        };
        Some(var_kind)
    }
    #[must_use]
    fn arr_set(
        &mut self,
        var_name: &str,
        idxs: Vec<EraExpr>,
        src_info: SourcePosInfo,
        rhs: impl FnOnce(&mut Self) -> Option<EraExpressionValueKind>,
    ) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;
        let var_kind = self.arr_idx(var_name, idxs, src_info)?;
        if var_kind != rhs(self)? {
            bail_opt!(
                self,
                src_info,
                true,
                "right-hand expression type is incompatible with left-hand"
            )
        }
        self.chunk.emit_bytecode(SetArrayVal, src_info);
        Some(var_kind)
    }
    #[must_use]
    fn var_arr_no_pseudo(
        &mut self,
        var_name: &str,
        src_info: SourcePosInfo,
    ) -> Option<EraExpressionValueKind> {
        use EraVarArrCompileInfo::*;
        let Normal(kind, dims_cnt) = self.var_arr(var_name, src_info)? else {
            bail_opt!(self, src_info, true, "invalid access to pesudo variable");
        };
        Some(kind)
    }
    #[must_use]
    fn expr_int(&mut self, expr: EraExpr) -> Option<()> {
        use EraExpressionValueKind::*;
        let si = expr.source_pos_info();
        let TInteger = self.expression(expr)? else {
            bail_opt!(self, si, "expected an integer expression");
        };
        Some(())
    }
    #[must_use]
    fn expr_str(&mut self, expr: EraExpr) -> Option<()> {
        use EraExpressionValueKind::*;
        let si = expr.source_pos_info();
        let TString = self.expression(expr)? else {
            bail_opt!(self, si, "expected a string expression");
        };
        Some(())
    }
    #[must_use]
    fn var_arr_int(&mut self, var_name: &str, src_info: SourcePosInfo) -> Option<()> {
        use EraExpressionValueKind::*;
        let TInteger = self.var_arr_no_pseudo(var_name, src_info)? else {
            bail_opt!(self, src_info, "expected an integer variable");
        };
        Some(())
    }
    #[must_use]
    fn var_arr_str(&mut self, var_name: &str, src_info: SourcePosInfo) -> Option<()> {
        use EraExpressionValueKind::*;
        let TString = self.var_arr_no_pseudo(var_name, src_info)? else {
            bail_opt!(self, src_info, "expected a string variable");
        };
        Some(())
    }
    #[must_use]
    fn arr_idx_int(
        &mut self,
        var_name: &str,
        idxs: Vec<EraExpr>,
        src_info: SourcePosInfo,
    ) -> Option<()> {
        use EraExpressionValueKind::*;
        let TInteger = self.arr_idx(var_name, idxs, src_info)? else {
            bail_opt!(self, src_info, "expected an integer array");
        };
        Some(())
    }
    #[must_use]
    fn arr_idx_str(
        &mut self,
        var_name: &str,
        idxs: Vec<EraExpr>,
        src_info: SourcePosInfo,
    ) -> Option<()> {
        use EraExpressionValueKind::*;
        let TString = self.arr_idx(var_name, idxs, src_info)? else {
            bail_opt!(self, src_info, "expected a string array");
        };
        Some(())
    }
    fn var_result(&mut self, src_info: SourcePosInfo) -> Option<()> {
        let idx = self.p.vars.get_var_idx("RESULT").unwrap();
        self.chunk
            .emit_load_const(self.p.new_value_int(idx as _), src_info);
        self.chunk
            .emit_bytecode(EraBytecodePrimaryType::GetGlobal, src_info);
        Some(())
    }
    fn var_results(&mut self, src_info: SourcePosInfo) -> Option<()> {
        let idx = self.p.vars.get_var_idx("RESULTS").unwrap();
        self.chunk
            .emit_load_const(self.p.new_value_int(idx as _), src_info);
        self.chunk
            .emit_bytecode(EraBytecodePrimaryType::GetGlobal, src_info);
        Some(())
    }
    fn report_err<V: Into<String>>(&mut self, src_info: SourcePosInfo, is_error: bool, msg: V) {
        self.p.report_err(src_info, is_error, msg)
    }
    fn report_far_err<V: Into<String>>(
        &mut self,
        file_name: &str,
        src_info: SourcePosInfo,
        is_error: bool,
        msg: V,
    ) {
        self.p.report_far_err(file_name, src_info, is_error, msg)
    }
}
