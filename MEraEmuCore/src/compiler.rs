use std::{
    any::Any,
    collections::{HashMap, HashSet},
    ops::Deref,
    rc::Rc,
};

use indoc::{concatdoc, indoc};
use smallvec::SmallVec;

use crate::{
    bytecode::{
        EraBytecodePrimaryType, EraInputSubBytecodeType, FlatValue, PrintExtendedFlags, ValueKind,
    },
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
macro_rules! err_opt {
    ($self:expr, $src_info:expr, $is_error:expr, $msg:expr) => {{
        $self.report_err($src_info, $is_error, $msg);
    }};
    ($self:expr, $src_info:expr, $msg:expr) => {{
        $self.report_err($src_info, true, $msg);
    }};
    ($self:expr, [$(($src_info:expr, $is_error:expr, $msg:expr)),+]) => {{
        $($self.report_err($src_info, $is_error, $msg);)+
    }};
    ($self:expr, [$(($src_info:expr, $msg:expr)),+]) => {{
        $($self.report_err($src_info, true, $msg);)+
    }};
    ($self:expr, [($src_info0:expr, $msg0:expr), $(($file_name:expr, $src_info:expr, $msg:expr)),+]) => {{
        $self.report_err($src_info0, true, $msg0);
        $($self.report_far_err($file_name, $src_info, true, $msg);)+
    }};
}

#[derive(Default)]
pub struct EraConstantPool {
    pub vals: Vec<Value>,
}
#[derive(Default)]
struct EraConstantPoolBuilder {
    intern_map: HashMap<either::Either<i64, *mut ()>, usize>,
    vals: Vec<Value>,
}
impl EraConstantPoolBuilder {
    fn build(self) -> EraConstantPool {
        EraConstantPool { vals: self.vals }
    }
    fn add_val(&mut self, val: Value) -> usize {
        // TODO: Should we intern simple strings?
        let key = match val.clone().into_unpacked() {
            FlatValue::Int(x) => either::Either::Left(x.val),
            FlatValue::Str(x) => either::Either::Right(Rc::as_ptr(&x) as _),
            FlatValue::ArrInt(x) => either::Either::Right(Rc::as_ptr(&x) as _),
            FlatValue::ArrStr(x) => either::Either::Right(Rc::as_ptr(&x) as _),
        };
        *self.intern_map.entry(key).or_insert_with(|| {
            let index = self.vals.len();
            self.vals.push(val);
            index
        })
    }
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

#[derive(Debug)]
struct EraFuncInfo {
    name: Rc<CaselessStr>,
    kind: EraFunKind,
    params: Vec<EraFuncArgInfo>,
    local_var_idxs: HashMap<Rc<CaselessStr>, usize>,
    local_vars: Vec<EraFuncLocalVarInfo>,
    local_frame_size: usize,
    local_size: Option<u32>,
    file_name: Rc<str>,
    src_info: SourcePosInfo,
}
#[derive(Debug)]
struct EraFuncArgInfo {
    // Default ArrInt / ArrStr value indicates ref argument
    target_var: (Rc<CaselessStr>, Vec<u32>),
    default_val: Value, // @FUN(value = <default_val>)
    src_info: SourcePosInfo,
}

#[derive(Debug)]
struct EraFuncLocalVarInfo {
    name: Rc<CaselessStr>,
    init_val: Value, // #DIM value = <init_val>
    has_init: bool,  // Is <init_val> present?
    idx_in_frame: usize,
    is_in_global_frame: bool,
    is_const: bool,
    is_charadata: bool,
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
struct EraBytecodeChunkBuilder {
    // TODO: Optimize debugging information presentation
    bytecode: Vec<u8>,
    // src_infos[bytecode_offset] => src_info
    src_infos: Vec<SlimSourcePosInfo>,
    name: Rc<str>,
    constants: EraConstantPoolBuilder,
}
struct EraBytecodeChunkBacktrackHolder {
    base_idx: usize,
    write_fn: &'static dyn Fn(&mut EraBytecodeChunkBuilder, usize, isize) -> Option<()>,
}
struct EraBytecodeChunkSnapshot {
    len: usize,
}
impl EraBytecodeChunk {
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
    // WARN: Do NOT modify the returned value
    pub fn get_constant(&self, idx: usize) -> Option<&Value> {
        self.constants.vals.get(idx)
    }
}
impl EraBytecodeChunkBuilder {
    fn build(self) -> EraBytecodeChunk {
        EraBytecodeChunk {
            bytecode: self.bytecode,
            src_infos: self.src_infos,
            name: self.name,
            constants: self.constants.build(),
        }
    }
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
    // WARN: Do NOT modify the returned value
    pub fn get_constant(&self, idx: usize) -> Option<&Value> {
        self.constants.vals.get(idx)
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
    fn add_constant(&mut self, value: Value) -> usize {
        self.constants.add_val(value)
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
        self.emit_bytecode(EraBytecodePrimaryType::JumpWW, src_info);
        self.append_u32(0xffffffff, src_info);
        EraBytecodeChunkBacktrackHolder {
            base_idx,
            write_fn: &|this, idx, offset| {
                this.overwrite_u32(i32::try_from(offset).ok()? as _, idx)
            },
        }
    }
    fn emit_jump_cond_hold(&mut self, src_info: SourcePosInfo) -> EraBytecodeChunkBacktrackHolder {
        let base_idx = self.cur_bytes_cnt();
        self.emit_bytecode(EraBytecodePrimaryType::JumpCondWW, src_info);
        self.append_u32(0xffffffff, src_info);
        EraBytecodeChunkBacktrackHolder {
            base_idx,
            write_fn: &|this, idx, offset| {
                this.overwrite_u32(i32::try_from(offset).ok()? as _, idx)
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
        } else if let Ok(offset) = i32::try_from(offset) {
            self.emit_bytecode(EraBytecodePrimaryType::JumpW, src_info);
            self.append_u32(offset as _, src_info);
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
        } else if let Ok(offset) = i32::try_from(offset) {
            self.emit_bytecode(EraBytecodePrimaryType::JumpCondW, src_info);
            self.append_u32(offset as _, src_info);
        } else {
            panic!("jump too far to be encoded in bytecode");
        }
    }
    fn emit_duplicate_n(&mut self, count: u8, src_info: SourcePosInfo) {
        if count == 1 {
            self.emit_bytecode(EraBytecodePrimaryType::Duplicate, src_info);
        } else {
            self.emit_bytecode(EraBytecodePrimaryType::DuplicateN, src_info);
            self.append_u8(count, src_info);
        }
    }
    fn emit_duplicate_one_n(&mut self, count: u8, src_info: SourcePosInfo) {
        self.emit_bytecode(EraBytecodePrimaryType::DuplicateOneN, src_info);
        self.append_u8(count, src_info);
    }
    fn emit_pop(&mut self, src_info: SourcePosInfo) {
        self.emit_bytecode(EraBytecodePrimaryType::Pop, src_info)
    }
    fn emit_pop_n(&mut self, cnt: usize, src_info: SourcePosInfo) {
        if cnt == 0 {
            return;
        }
        for _ in 0..cnt {
            self.emit_pop(src_info);
        }
    }
    fn take_snapshot(&self) -> EraBytecodeChunkSnapshot {
        EraBytecodeChunkSnapshot {
            len: self.bytecode.len(),
        }
    }
}
impl EraBytecodeChunkBacktrackHolder {
    fn source_pos_info(&self, chunk: &EraBytecodeChunkBuilder) -> Option<SourcePosInfo> {
        chunk.source_info_at(self.base_idx)
    }
    fn complete(self, chunk: &mut EraBytecodeChunkBuilder) -> Option<()> {
        let pos = chunk.cur_bytes_cnt();
        self.complete_at(chunk, pos)
    }
    fn complete_at(self, chunk: &mut EraBytecodeChunkBuilder, pos: usize) -> Option<()> {
        let offset = isize::try_from(pos)
            .ok()?
            .checked_sub_unsigned(self.base_idx)?;
        // 1 is bytecode size
        (self.write_fn)(chunk, self.base_idx + 1, offset)
    }
}
impl EraBytecodeChunkSnapshot {
    fn regret(self, chunk: &mut EraBytecodeChunkBuilder) {
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
    stack_balance: usize,
}
impl EraLoopStructCodeMetadata {
    fn new(stack_balance: usize) -> Self {
        EraLoopStructCodeMetadata {
            continue_queue: Vec::new(),
            done_queue: Vec::new(),
            stack_balance,
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
    // intern_vals: HashMap<either::Either<IntValue, Rc<StrValue>>, ()>,
    contextual_indices: HashMap<Ascii<String>, u32>,
}

pub struct EraCompilerImplFunctionSite<'p, 'a, ErrReportFn> {
    p: &'p mut EraCompilerImpl<'a, ErrReportFn>,
    funcs: &'p EraFuncPool,
    func_info: &'p EraFuncInfo,
    chunk: &'p mut EraBytecodeChunkBuilder,
    loop_structs: Vec<EraLoopStructCodeMetadata>,
    stack_balance: usize,
    goto_labels: HashMap<CaselessString, EraGotoLabelInfo>,
    goto_backtracks: Vec<EraGotoJumpInfo>,
    final_return_backtracks: Vec<EraBytecodeChunkBacktrackHolder>,
    body_start_pos: usize,
    fatal_stop: bool,
}

#[derive(Debug, Clone, Copy)]
enum EraPseudoVarKind {
    Rand,
    CharaNum,
}

#[derive(Debug, Clone, Copy)]
enum EraVarArrCompileInfo {
    Pseudo(EraPseudoVarKind),
    Normal(EraExpressionValueKind, u8),
}
#[derive(Debug, Clone)]
enum EraVarArrCompileInfoWithDims {
    Pseudo(EraPseudoVarKind),
    // (kind, dims, is_charadata)
    Normal(EraExpressionValueKind, smallvec::SmallVec<[u32; 3]>, bool),
}
impl EraVarArrCompileInfo {
    fn from_fat(value: &EraVarArrCompileInfoWithDims) -> Self {
        use EraVarArrCompileInfoWithDims::*;
        match value {
            Pseudo(kind) => Self::Pseudo(*kind),
            Normal(kind, dims, _) => Self::Normal(*kind, dims.len().try_into().unwrap()),
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
            // intern_vals: HashMap::new(),
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
                            if self
                                .contextual_indices
                                .contains_key(Ascii::new_str(&x.name))
                            {
                                self.report_err(
                                    x.src_info,
                                    false,
                                    "this declaration might be shadowed by a CSV index name",
                                );
                            }
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
            // std::mem::swap(&mut self.file_name, file_name);
            self.file_name = file_name.clone();

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
            let mut local_var_idxs = HashMap::new();
            let mut local_vars = Vec::new();
            let mut params = Vec::new();
            match global_funcs.func_names.entry(Rc::clone(&func_name)) {
                std::collections::hash_map::Entry::Occupied(e) => {
                    let old_func = *e.get();
                    let old_func = &global_funcs.funcs[old_func];
                    // HACK: Ignore error and skip this function
                    // TODO: Refactor error reporting mechanism
                    err_opt!(
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
                    // Mark function as invalid by resetting function name
                    fun.name.clear();
                    continue;
                }
                std::collections::hash_map::Entry::Vacant(e) => {
                    e.insert(fun_idx);
                }
            }
            self.vars.add_var(
                &format!("LOCAL@{}", func_name),
                Value::new_int_arr(smallvec::smallvec![16], Vec::new()),
            );
            self.vars.add_var(
                &format!("LOCALS@{}", func_name),
                Value::new_str_arr(smallvec::smallvec![16], Vec::new()),
            );
            self.vars.add_var(
                &format!("ARG@{}", func_name),
                Value::new_int_arr(smallvec::smallvec![16], Vec::new()),
            );
            self.vars.add_var(
                &format!("ARGS@{}", func_name),
                Value::new_str_arr(smallvec::smallvec![16], Vec::new()),
            );

            // Local variables
            let mut local_frame_var_counter = fun.params.len();
            for decl in std::mem::take(&mut fun.decls) {
                let src_info;
                let mut local_decl = match decl {
                    EraSharpDecl::VarDecl(mut x) => {
                        src_info = x.src_info;
                        let has_init;
                        let init_val = if x.is_ref {
                            if x.dims.is_empty() {
                                x.dims.push(0);
                            }
                            if x.dims.iter().any(|x| *x != 0) {
                                self.report_err(
                                    src_info,
                                    true,
                                    "ref-qualified function parameter must not have non-zero dimensions",
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
                                Value::new_str_arr(x.dims, Vec::new())
                            } else {
                                Value::new_int_arr(x.dims, Vec::new())
                            }
                        } else {
                            has_init = !x.inits.is_empty();
                            if x.dims.is_empty() {
                                x.dims = smallvec::smallvec![(x.inits.len() as u32).max(1)];
                            }
                            let arr_len = x.dims.iter().fold(1, |acc, &x| acc * x);
                            // NOTE: We need to reject zero-sized arrays (they can only be REF arrays)
                            if (arr_len as usize) < x.inits.len().max(1) {
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
                        EraFuncLocalVarInfo {
                            name: CaselessStr::new(x.name.as_str()).into(),
                            init_val,
                            has_init,
                            idx_in_frame: usize::MAX, //Stub
                            is_in_global_frame: false,
                            is_const: x.is_const,
                            is_charadata: x.is_charadata,
                            is_dynamic: x.is_dynamic,
                            is_ref: x.is_ref,
                            src_info,
                        }
                    }
                    _ => continue,
                };
                // Insert into global / local var pool
                if self.vars.get_var(local_decl.name.as_str()).is_some() {
                    self.report_err(
                        local_decl.src_info,
                        false,
                        "this declaration shadows a global variable with the same name",
                    );
                }
                if self
                    .contextual_indices
                    .contains_key(Ascii::new_str(local_decl.name.as_str()))
                {
                    self.report_err(
                        local_decl.src_info,
                        false,
                        "this declaration might be shadowed by a CSV index name",
                    );
                }
                if !local_decl.is_dynamic && !local_decl.is_ref {
                    let var_name = Self::decorate_as_func_local_name(
                        local_decl.name.as_str(),
                        func_name.as_str(),
                    );
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
                match local_var_idxs.entry(local_decl.name.clone()) {
                    std::collections::hash_map::Entry::Occupied(e) => {
                        self.report_err(
                            src_info,
                            true,
                            format!("redefinition of variable `{}`", local_decl.name),
                        );
                        return None;
                    }
                    std::collections::hash_map::Entry::Vacant(e) => {
                        let local_var_idx = local_vars.len();
                        local_vars.push(local_decl);
                        e.insert(local_var_idx);
                    }
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
                            let kind = local_var_idxs
                                .get(lhs_name)
                                .map(|&x| &local_vars[x])
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
                        let kind = local_var_idxs
                            .get(CaselessStr::new(&lhs.name))
                            .map(|&x| &mut local_vars[x])
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
                            ValueKind::ArrInt => {
                                Value::new_int_arr(smallvec::smallvec![0], Vec::new())
                            }
                            ValueKind::ArrStr => {
                                Value::new_str_arr(smallvec::smallvec![0], Vec::new())
                            }
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
                local_var_idxs,
                local_vars,
                local_frame_size: local_frame_var_counter,
                local_size: None,
                file_name: self.file_name.clone(),
                src_info: fun.src_info,
            });

            // std::mem::swap(&mut self.file_name, file_name);
        }

        let mut funcs = Vec::new();
        let mut chunks_idxs: HashMap<Rc<str>, usize> = HashMap::new();
        let mut chunks = Vec::new();

        // Compile function bodies
        for (file_name, func_decl) in fun_decls {
            self.file_name = file_name;

            // If function is marked as invalid (duplicate), skip compilation
            if func_decl.name.is_empty() {
                continue;
            }

            let chunk_idx = *chunks_idxs
                .entry(self.file_name.clone())
                .or_insert_with(|| {
                    let idx = chunks.len();
                    chunks.push(EraBytecodeChunkBuilder {
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

            assert_eq!(global_funcs.funcs[func_idx].name, func_info.name);

            // TODO: Check overflow
            func_info.chunk_idx = chunk_idx as _;
            funcs.push(func_info);
        }

        // let global_funcs = std::mem::take(&mut self.funcs);
        let global_vars = std::mem::take(&mut self.vars);

        Some(EraBytecodeCompilation {
            func_names: global_funcs.func_names,
            funcs,
            chunks: chunks.into_iter().map(|x| x.build()).collect(),
            global_vars,
        })
    }
    #[must_use]
    fn compile_function(
        &mut self,
        funcs: &EraFuncPool,
        func_info: &EraFuncInfo,
        func_decl: EraFunDecl,
        chunk: &mut EraBytecodeChunkBuilder,
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
        // use std::collections::hash_map::Entry::*;
        // let value = IntValue { val: value };
        // match self.intern_vals.entry(either::Either::Left(value)) {
        //     Occupied(x) => Value::new_int(x.key().as_ref().unwrap_left().val),
        //     Vacant(x) => {
        //         let r = Value::new_int(x.key().as_ref().unwrap_left().val);
        //         x.insert(());
        //         r
        //     }
        // }
        Value::new_int(value)
    }
    fn new_value_str(&mut self, value: String) -> Value {
        // use std::collections::hash_map::Entry::*;
        // let value = StrValue { val: value };
        // match self
        //     .intern_vals
        //     .entry(either::Either::Right(Rc::new(value)))
        // {
        //     Occupied(x) => Value::new_str_rc(x.key().clone().unwrap_right()),
        //     Vacant(x) => {
        //         let r = Value::new_str_rc(x.key().clone().unwrap_right());
        //         x.insert(());
        //         r
        //     }
        // }
        Value::new_str(value)
    }
}

impl<'p, 'a, T: FnMut(&EraCompileErrorInfo)> EraCompilerImplFunctionSite<'p, 'a, T> {
    fn new(
        parent: &'p mut EraCompilerImpl<'a, T>,
        funcs: &'p EraFuncPool,
        func_info: &'p EraFuncInfo,
        chunk: &'p mut EraBytecodeChunkBuilder,
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
            body_start_pos: 0,
            fatal_stop: false,
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
        for var_info in self.func_info.local_vars.iter() {
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

        // Function body start
        self.body_start_pos = self.chunk.cur_bytes_cnt();

        // Compile function body
        for stmt in func_decl.body {
            self.statement(func_decl.kind, stmt)?;
        }

        // Return
        for bt in std::mem::take(&mut self.final_return_backtracks) {
            bt.complete(self.chunk).unwrap();
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
            let src_si = backtrack.backtrack.source_pos_info(self.chunk).unwrap();
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
            if backtrack.stack_balance < target.stack_balance {
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
            } else if backtrack.stack_balance > target.stack_balance {
                // HACK: Jump to a dedicated subchunk to balance stack
                backtrack.backtrack.complete(self.chunk).unwrap();
                self.chunk
                    .emit_pop_n(backtrack.stack_balance - target.stack_balance, src_si);
                self.chunk
                    .emit_jump_hold(src_si)
                    .complete_at(self.chunk, target.pos)
                    .unwrap();
            } else {
                backtrack
                    .backtrack
                    .complete_at(self.chunk, target.pos)
                    .unwrap();
            }
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
                | EraTokenKind::ModuloAssign
                | EraTokenKind::BitAndAssign
                | EraTokenKind::BitOrAssign
                | EraTokenKind::BitXorAssign = op.kind
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
                                EraTokenKind::BitAndAssign => match (lhs_k, rhs_k) {
                                    (TInteger, TInteger) => {
                                        self.chunk.emit_bytecode(BitAnd, op.src_info);
                                        lhs_k
                                    }
                                    _ => bail_opt!(
                                        self,
                                        op.src_info,
                                        true,
                                        "operands must be integers"
                                    ),
                                },
                                EraTokenKind::BitOrAssign => match (lhs_k, rhs_k) {
                                    (TInteger, TInteger) => {
                                        self.chunk.emit_bytecode(BitOr, op.src_info);
                                        lhs_k
                                    }
                                    _ => bail_opt!(
                                        self,
                                        op.src_info,
                                        true,
                                        "operands must be integers"
                                    ),
                                },
                                EraTokenKind::BitXorAssign => match (lhs_k, rhs_k) {
                                    (TInteger, TInteger) => {
                                        self.chunk.emit_bytecode(BitXor, op.src_info);
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
                            (TInteger, TString) | (TString, TInteger) => {
                                self.chunk.emit_bytecode(RepeatString, op.src_info);
                                TString
                            }
                            _ => bail_opt!(
                                self,
                                op.src_info,
                                true,
                                "operands must be integers or (int, str)"
                            ),
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
                        EraTokenKind::BitShiftL => match (lhs, rhs) {
                            (TInteger, TInteger) => {
                                self.chunk.emit_bytecode(BitShiftL, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(self, op.src_info, true, "operands must be integers"),
                        },
                        EraTokenKind::BitShiftR => match (lhs, rhs) {
                            (TInteger, TInteger) => {
                                self.chunk.emit_bytecode(BitShiftR, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(self, op.src_info, true, "operands must be integers"),
                        },
                        EraTokenKind::BitAnd => match (lhs, rhs) {
                            (TInteger, TInteger) => {
                                self.chunk.emit_bytecode(BitAnd, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(self, op.src_info, true, "operands must be integers"),
                        },
                        EraTokenKind::BitOr => match (lhs, rhs) {
                            (TInteger, TInteger) => {
                                self.chunk.emit_bytecode(BitOr, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(self, op.src_info, true, "operands must be integers"),
                        },
                        EraTokenKind::BitXor => match (lhs, rhs) {
                            (TInteger, TInteger) => {
                                self.chunk.emit_bytecode(BitXor, op.src_info);
                                lhs
                            }
                            _ => bail_opt!(self, op.src_info, true, "operands must be integers"),
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
                EraTokenKind::BitNot => {
                    self.expr_int(*rhs)?;
                    self.chunk.emit_bytecode(BitNot, op.src_info);
                    TInteger
                }
                EraTokenKind::LogicalNot => {
                    self.expr_int(*rhs)?;
                    self.chunk.emit_bytecode(LogicalNot, op.src_info);
                    TInteger
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
                bt_then.complete(self.chunk).unwrap();
                let mhs_k = self.expression(*mhs)?;
                bt_done.complete(self.chunk).unwrap();
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

        // TODO: Use a better way to convey SourcePosInfo
        let mut emitted_bc = false;

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
                        "note: see signature of callee"
                    )
                ]
            );
        }
        for (arg, param) in args.into_iter().zip(target.params.iter()) {
            let Some(arg) = arg else {
                // Omitted argument
                // HACK: Steal source info from last line
                let src_info = if emitted_bc {
                    self.chunk
                        .source_info_at(self.chunk.cur_bytes_cnt() - 1)
                        .unwrap()
                } else {
                    target_src_info
                };
                if param.default_val.kind().is_arr() {
                    bail_opt!(
                        self,
                        [
                            (src_info, "reference parameter cannot be omitted"),
                            (
                                &target.file_name,
                                target.src_info,
                                "note: see signature of callee"
                            )
                        ]
                    );
                }
                self.chunk
                    .emit_load_const(param.default_val.clone(), src_info);
                emitted_bc = true;
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
                                    "note: see signature of callee"
                                )
                            ]
                        );
                    };
                    let arg_kind = self.var_arr_no_pseudo(&var_expr.name, var_expr.src_info)?.0;
                    emitted_bc = true;
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
                                    "note: see signature of callee"
                                )
                            ]
                        ),
                    }
                }
                ValueKind::Int | ValueKind::Str => {
                    let arg_kind = self.expression(arg)?;
                    emitted_bc = true;
                    match (arg_kind, param_kind) {
                        (EraExpressionValueKind::TInteger, ValueKind::Int)
                        | (EraExpressionValueKind::TString, ValueKind::Str) => (),
                        (EraExpressionValueKind::TInteger, ValueKind::Str) => {
                            err_opt!(
                                self,
                                [
                                    (arg_si, "incompatible argument type; implicit conversion from integer to string is disallowed"),
                                    (
                                        &target.file_name,
                                        target.src_info,
                                        "note: see signature of callee"
                                    )
                                ]
                            );
                            self.chunk.emit_bytecode(ConvertToString, arg_si);
                        }
                        _ => bail_opt!(
                            self,
                            [
                                (arg_si, "incompatible argument type"),
                                (
                                    &target.file_name,
                                    target.src_info,
                                    "note: see signature of callee"
                                )
                            ]
                        ),
                    }
                }
            }
        }
        for param in target.params.iter().skip(args_len) {
            // HACK: Steal source info from last line
            let src_info = if emitted_bc {
                self.chunk
                    .source_info_at(self.chunk.cur_bytes_cnt() - 1)
                    .unwrap()
            } else {
                target_src_info
            };
            if param.default_val.kind().is_arr() {
                bail_opt!(
                    self,
                    [
                        (src_info, "REF parameter cannot be omitted"),
                        (
                            &target.file_name,
                            target.src_info,
                            "note: see signature of callee"
                        )
                    ]
                );
            }
            self.chunk
                .emit_load_const(param.default_val.clone(), src_info);
            emitted_bc = true;
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
        force: bool,
    ) -> Option<EraExpressionValueKind> {
        use EraBytecodePrimaryType::*;
        use EraExpressionValueKind::*;

        // NOTE: We know nothing about the target, so we must emit args pack which contains
        //       enough information to carry out args resolution at run-time.
        let bytecode = if force { TryFunCallForce } else { TryFunCall };
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
        self.chunk.emit_bytecode(bytecode, target_si);
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
                let args_cnt = args.len();
                let Ok(args) = TryInto::try_into(args) else {
                    bail_opt!(
                        self.this,
                        self.src_info,
                        format!(
                            "function `{}` expects {} parameters, but {} were given",
                            self.target, N, args_cnt
                        )
                    );
                };
                Some(args)
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
            fn unpack_some_arg(&mut self, arg: Option<EraExpr>, idx: usize) -> Option<EraExpr> {
                if let Some(e) = arg {
                    Some(e)
                } else {
                    bail_opt!(
                        self.this,
                        self.src_info,
                        format!("argument {} cannot be omitted", idx + 1)
                    );
                }
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

        let upper_target = target.to_ascii_uppercase();

        match upper_target.as_bytes() {
            // TODO: Add foundation intrinsics
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
                    //ctx.this.chunk.emit_bytecode(Pop, src_info);
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
            b"SPRITEWIDTH" => {
                ctx.result()?;
                let [name] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(name)?;
                ctx.this.chunk.emit_bytecode(SpriteWidth, src_info);
            }
            b"SPRITEHEIGHT" => {
                ctx.result()?;
                let [name] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(name)?;
                ctx.this.chunk.emit_bytecode(SpriteHeight, src_info);
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
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.arr_get_int("@STYLE", vec![], src_info)?;
            }
            b"CHKFONT" => {
                ctx.result()?;
                let [font_name] = ctx.unpack_some_args(args)?;
                ctx.this.chunk.emit_bytecode(CheckFont, src_info);
            }
            b"GETFONT" => {
                ctx.results()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.arr_get_str("@FONT", vec![], src_info)?;
            }
            b"REPLACE" => {
                ctx.results()?;
                let [haystack, needle, replace_with] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(haystack)?;
                ctx.this.expr_str(needle)?;
                ctx.this.expr_str(replace_with)?;
                ctx.this.chunk.emit_bytecode(ReplaceString, src_info);
            }
            b"SUBSTRING" | b"SUBSTRINGU" => {
                ctx.results()?;
                let bytecode = match upper_target.as_bytes() {
                    b"SUBSTRING" => SubString,
                    b"SUBSTRINGU" => SubStringU,
                    _ => unreachable!(),
                };
                let [haystack, start_pos, length];
                match args.len() {
                    2 => {
                        [haystack, start_pos] = ctx.unpack_some_args(args)?;
                        length = EraExpr::new_int(-1, src_info);
                    }
                    3 => {
                        [haystack, start_pos, length] = ctx.unpack_some_args(args)?;
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
                ctx.this.expr_str(haystack)?;
                ctx.this.expr_int(start_pos)?;
                ctx.this.expr_int(length)?;
                ctx.this.chunk.emit_bytecode(bytecode, src_info);
            }
            b"STRFIND" | b"STRFINDU" => {
                ctx.result()?;
                let bytecode = match upper_target.as_bytes() {
                    b"STRFIND" => StrFind,
                    b"STRFINDU" => StrFindU,
                    _ => unreachable!(),
                };
                let [haystack, needle, start_pos];
                match args.len() {
                    2 => {
                        [haystack, needle] = ctx.unpack_some_args(args)?;
                        start_pos = EraExpr::new_int(0, src_info);
                    }
                    3 => {
                        [haystack, needle, start_pos] = ctx.unpack_some_args(args)?;
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
                ctx.this.expr_str(haystack)?;
                ctx.this.expr_str(needle)?;
                ctx.this.expr_int(start_pos)?;
                ctx.this.chunk.emit_bytecode(bytecode, src_info);
            }
            b"STRLENS" | b"STRLENSU" => {
                ctx.result()?;
                let bytecode = match upper_target.as_bytes() {
                    b"STRLENS" => StrLen,
                    b"STRLENSU" => StrLenU,
                    _ => unreachable!(),
                };
                let [haystack] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(haystack)?;
                ctx.this.chunk.emit_bytecode(bytecode, src_info);
            }
            b"STRCOUNT" => {
                ctx.result()?;
                let [haystack, needle] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(haystack)?;
                ctx.this.expr_str(needle)?;
                ctx.this.chunk.emit_bytecode(CountSubString, src_info);
            }
            b"CHARATU" => {
                ctx.results()?;
                let [haystack, pos] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(haystack)?;
                ctx.this.expr_int(pos)?;
                ctx.this.chunk.emit_bytecode(StrCharAtU, src_info);
            }
            b"CURRENTREDRAW" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.arr_get_int("@REDRAW", vec![], src_info)?;
            }
            b"CURRENTALIGN" => {
                ctx.results()?;
                let [] = ctx.unpack_some_args(args)?;
                //ctx.this.arr_get_int("@ALIGN", vec![], src_info)?;
                let args = vec![Some(EraExpr::new_var("@ALIGN".to_owned(), vec![], src_info))];
                // TODO: Assert return type of SYSFUNC_ALIGN_INT_TO_STR
                ctx.this.static_fun_call("SYSFUNC_ALIGN_INT_TO_STR", args, src_info)?;
            }
            b"MAX" => {
                ctx.result()?;
                if args.len() < 1 {
                    bail_opt!(ctx.this, src_info, "too few arguments");
                }
                if args.len() == 1 {
                    ctx.this.report_err(
                        src_info,
                        false,
                        format!("unnecessary call to `{target}` which has only one argument"),
                    );
                }
                let mut it = args.into_iter().enumerate();
                let first_arg = ctx.unpack_some_arg(it.next().unwrap().1, 0)?;
                ctx.this.expr_int(first_arg)?;
                for (idx, arg) in it {
                    let arg = ctx.unpack_some_arg(arg, idx)?;
                    ctx.this.expr_int(arg)?;
                    ctx.this.chunk.emit_bytecode(MaximumInt, src_info);
                }
            }
            b"MIN" => {
                ctx.result()?;
                if args.len() < 1 {
                    bail_opt!(ctx.this, src_info, "too few arguments");
                }
                if args.len() == 1 {
                    ctx.this.report_err(
                        src_info,
                        false,
                        format!("unnecessary call to `{target}` which has only one argument"),
                    );
                }
                let mut it = args.into_iter().enumerate();
                let first_arg = ctx.unpack_some_arg(it.next().unwrap().1, 0)?;
                ctx.this.expr_int(first_arg)?;
                for (idx, arg) in it {
                    let arg = ctx.unpack_some_arg(arg, idx)?;
                    ctx.this.expr_int(arg)?;
                    ctx.this.chunk.emit_bytecode(MinimumInt, src_info);
                }
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
            b"CHKCHARADATA" => {
                ctx.result()?;
                // TODO: CHKCHARADATA
                ctx.this
                    .chunk
                    .emit_load_const(ctx.this.p.new_value_int(0), src_info);
            }
            b"SAVETEXT" => {
                ctx.result()?;
                let [text, file_no, force_save_dir, force_utf8];
                match args.len() {
                    2 => {
                        [text, file_no] = ctx.unpack_some_args(args)?;
                        force_save_dir = EraExpr::new_int(0, src_info);
                        force_utf8 = EraExpr::new_int(0, src_info);
                    }
                    4 => {
                        [text, file_no, force_save_dir, force_utf8] = ctx.unpack_some_args(args)?;
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
                ctx.this.expr_str(text)?;
                ctx.this.expr_int(file_no)?;
                ctx.this.expr_int(force_save_dir)?;
                ctx.this.expr_int(force_utf8)?;
                ctx.this.chunk.emit_bytecode(SaveText, src_info);
            }
            b"LOADTEXT" => {
                ctx.results()?;
                let [file_no, force_save_dir, force_utf8];
                match args.len() {
                    1 => {
                        [file_no] = ctx.unpack_some_args(args)?;
                        force_save_dir = EraExpr::new_int(0, src_info);
                        force_utf8 = EraExpr::new_int(0, src_info);
                    }
                    3 => {
                        [file_no, force_save_dir, force_utf8] = ctx.unpack_some_args(args)?;
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
                ctx.this.expr_int(file_no)?;
                ctx.this.expr_int(force_save_dir)?;
                ctx.this.expr_int(force_utf8)?;
                ctx.this.chunk.emit_bytecode(LoadText, src_info);
            }
            b"FINDELEMENT" | b"FINDLASTELEMENT" => {
                ctx.result()?;
                let bytecode = if target.eq_ignore_ascii_case("FINDLASTELEMENT") {
                    FindLastElement
                } else {
                    FindElement
                };
                let (var, value, start_idx, end_idx, complete_match);
                match args.len() {
                    2 => {
                        let avar;
                        [avar, value] = ctx.unpack_some_args(args)?;
                        var = ctx.this.unpack_var_expr_from_expr(avar)?;
                        start_idx = EraExpr::new_int(0, src_info);
                        end_idx = EraExpr::new_int(-1, src_info);
                        complete_match = EraExpr::new_int(0, src_info);
                    }
                    3 => {
                        let [avar, avalue, astart_idx] = ctx.unpack_args(args)?;
                        let avar = ctx.unpack_some_arg(avar, 0)?;
                        var = ctx.this.unpack_var_expr_from_expr(avar)?;
                        value = ctx.unpack_some_arg(avalue, 1)?;
                        start_idx = astart_idx.unwrap_or_else(|| EraExpr::new_int(0, src_info));
                        end_idx = EraExpr::new_int(-1, src_info);
                        complete_match = EraExpr::new_int(0, src_info);
                    }
                    4 => {
                        let [avar, avalue, astart_idx, aend_idx] = ctx.unpack_args(args)?;
                        let avar = ctx.unpack_some_arg(avar, 0)?;
                        var = ctx.this.unpack_var_expr_from_expr(avar)?;
                        value = ctx.unpack_some_arg(avalue, 1)?;
                        start_idx = astart_idx.unwrap_or_else(|| EraExpr::new_int(0, src_info));
                        end_idx = aend_idx.unwrap_or_else(|| EraExpr::new_int(-1, src_info));
                        complete_match = EraExpr::new_int(0, src_info);
                    }
                    5 => {
                        let [avar, avalue, astart_idx, aend_idx, acomplete_match] =
                            ctx.unpack_args(args)?;
                        let avar = ctx.unpack_some_arg(avar, 0)?;
                        var = ctx.this.unpack_var_expr_from_expr(avar)?;
                        value = ctx.unpack_some_arg(avalue, 1)?;
                        start_idx = astart_idx.unwrap_or_else(|| EraExpr::new_int(0, src_info));
                        end_idx = aend_idx.unwrap_or_else(|| EraExpr::new_int(-1, src_info));
                        complete_match =
                            acomplete_match.unwrap_or_else(|| EraExpr::new_int(0, src_info));
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
                let arr_kind = ctx.this.var_arr_no_pseudo(&var.name, var.src_info)?.0;
                let val_kind = ctx.this.expression(value)?;
                if arr_kind != val_kind {
                    bail_opt!(
                        ctx.this,
                        src_info,
                        "array element type mismatches value type"
                    );
                }
                ctx.this.expr_int(start_idx)?;
                ctx.this.expr_int(end_idx)?;
                ctx.this.expr_int(complete_match)?;
                ctx.this.chunk.emit_bytecode(bytecode, src_info);
            }
            b"FINDCHARA" | b"FINDLASTCHARA" => {
                ctx.result()?;
                let bytecode = if target.eq_ignore_ascii_case("FINDLASTCHARA") {
                    FindLastChara
                } else {
                    FindChara
                };
                let (chara_var, value, start_id, end_id);
                match args.len() {
                    2 => {
                        let a_chara_var;
                        [a_chara_var, value] = ctx.unpack_some_args(args)?;
                        chara_var = ctx.this.unpack_var_expr_from_expr(a_chara_var)?;
                        start_id = EraExpr::new_int(0, src_info);
                        end_id = EraExpr::new_int(-1, src_info);
                    }
                    3 => {
                        let a_chara_var;
                        [a_chara_var, value, start_id] = ctx.unpack_some_args(args)?;
                        chara_var = ctx.this.unpack_var_expr_from_expr(a_chara_var)?;
                        end_id = EraExpr::new_int(-1, src_info);
                    }
                    4 => {
                        let a_chara_var;
                        [a_chara_var, value, start_id, end_id] = ctx.unpack_some_args(args)?;
                        chara_var = ctx.this.unpack_var_expr_from_expr(a_chara_var)?;
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
                let arr_kind = ctx.this.arr_idx_no_chara_fix(
                    &chara_var.name,
                    chara_var.idxs,
                    chara_var.src_info,
                )?;
                let val_kind = ctx.this.expression(value)?;
                if arr_kind != val_kind {
                    bail_opt!(
                        ctx.this,
                        src_info,
                        "array element type mismatches value type"
                    );
                }
                ctx.this.expr_int(start_id)?;
                ctx.this.expr_int(end_id)?;
                ctx.this.chunk.emit_bytecode(bytecode, src_info);
            }
            b"GETCOLOR" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.arr_get_int("@COLOR", vec![], src_info)?;
            }
            b"GETBGCOLOR" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.arr_get_int("@BGCOLOR", vec![], src_info)?;
            }
            b"GETDEFCOLOR" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.arr_get_int("@DEFCOLOR", vec![], src_info)?;
            }
            b"GETDEFBGCOLOR" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.arr_get_int("@DEFBGCOLOR", vec![], src_info)?;
            }
            b"GETFOCUSCOLOR" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.arr_get_int("@FOCUSCOLOR", vec![], src_info)?;
            }
            b"MATCH" | b"CMATCH" => {
                ctx.result()?;
                let bytecode = if upper_target == "CMATCH" {
                    CArrayCountMatches
                } else {
                    ArrayCountMatches
                };
                let (array, value, start_idx, end_idx);
                match args.len() {
                    2 => {
                        let aarray;
                        [aarray, value] = ctx.unpack_some_args(args)?;
                        array = ctx.this.unpack_var_expr_from_expr(aarray)?;
                        start_idx = EraExpr::new_int(0, src_info);
                        end_idx = EraExpr::new_int(-1, src_info);
                    }
                    3 => {
                        let aarray;
                        [aarray, value, start_idx] = ctx.unpack_some_args(args)?;
                        array = ctx.this.unpack_var_expr_from_expr(aarray)?;
                        end_idx = EraExpr::new_int(-1, src_info);
                    }
                    4 => {
                        let aarray;
                        [aarray, value, start_idx, end_idx] = ctx.unpack_some_args(args)?;
                        array = ctx.this.unpack_var_expr_from_expr(aarray)?;
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
                let arr_kind =
                    ctx.this
                        .arr_idx_no_chara_fix(&array.name, array.idxs, array.src_info)?;
                let val_kind = ctx.this.expression(value)?;
                if arr_kind != val_kind {
                    bail_opt!(
                        ctx.this,
                        src_info,
                        "array element type mismatches value type"
                    );
                }
                ctx.this.expr_int(start_idx)?;
                ctx.this.expr_int(end_idx)?;
                ctx.this.chunk.emit_bytecode(bytecode, src_info);
            }
            b"SUMARRAY" | b"SUMCARRAY" | b"MAXARRAY" | b"MAXCARRAY" | b"MINARRAY"
            | b"MINCARRAY" => {
                ctx.result()?;
                let bytecode = match upper_target.as_bytes() {
                    b"SUMARRAY" => SumArray,
                    b"SUMCARRAY" => SumCArray,
                    b"MAXARRAY" => MaxArray,
                    b"MAXCARRAY" => MaxCArray,
                    b"MINARRAY" => MinArray,
                    b"MINCARRAY" => MinCArray,
                    _ => unreachable!(),
                };
                let (array, start_idx, end_idx);
                match args.len() {
                    1 => {
                        let aarray;
                        [aarray] = ctx.unpack_some_args(args)?;
                        array = ctx.this.unpack_var_expr_from_expr(aarray)?;
                        start_idx = EraExpr::new_int(0, src_info);
                        end_idx = EraExpr::new_int(-1, src_info);
                    }
                    2 => {
                        let aarray;
                        [aarray, start_idx] = ctx.unpack_some_args(args)?;
                        array = ctx.this.unpack_var_expr_from_expr(aarray)?;
                        end_idx = EraExpr::new_int(-1, src_info);
                    }
                    3 => {
                        let aarray;
                        [aarray, start_idx, end_idx] = ctx.unpack_some_args(args)?;
                        array = ctx.this.unpack_var_expr_from_expr(aarray)?;
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
                let arr_kind =
                    ctx.this
                        .arr_idx_no_chara_fix(&array.name, array.idxs, array.src_info)?;
                // TODO: Ensure TInteger
                ctx.this.expr_int(start_idx)?;
                ctx.this.expr_int(end_idx)?;
                ctx.this.chunk.emit_bytecode(bytecode, src_info);
            }
            b"INRANGEARRAY" | b"INRANGECARRAY" => {
                ctx.result()?;
                let bytecode = match upper_target.as_bytes() {
                    b"INRANGEARRAY" => InRangeArray,
                    b"INRANGECARRAY" => InRangeCArray,
                    _ => unreachable!(),
                };
                let (array, lower, upper, start_idx, end_idx);
                match args.len() {
                    3 => {
                        let aarray;
                        [aarray, lower, upper] = ctx.unpack_some_args(args)?;
                        array = ctx.this.unpack_var_expr_from_expr(aarray)?;
                        start_idx = EraExpr::new_int(0, src_info);
                        end_idx = EraExpr::new_int(-1, src_info);
                    }
                    4 => {
                        let aarray;
                        [aarray, lower, upper, start_idx] = ctx.unpack_some_args(args)?;
                        array = ctx.this.unpack_var_expr_from_expr(aarray)?;
                        end_idx = EraExpr::new_int(-1, src_info);
                    }
                    5 => {
                        let aarray;
                        [aarray, lower, upper, start_idx, end_idx] = ctx.unpack_some_args(args)?;
                        array = ctx.this.unpack_var_expr_from_expr(aarray)?;
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
                let arr_kind =
                    ctx.this
                        .arr_idx_no_chara_fix(&array.name, array.idxs, array.src_info)?;
                // TODO: Ensure TInteger
                ctx.this.expr_int(lower)?;
                ctx.this.expr_int(upper)?;
                ctx.this.expr_int(start_idx)?;
                ctx.this.expr_int(end_idx)?;
                ctx.this.chunk.emit_bytecode(bytecode, src_info);
            }
            b"GETNUM" => {
                ctx.result()?;
                // HACK: Process constant expression only for now
                // let [target, index] = ctx.unpack_some_args(args)?;
                // let target = ctx.this.unpack_var_expr_from_expr(target)?;
                // if routine::is_csv_var(&target.name) {
                //     let index = ctx.this.p.evaluate_constant(index)?;
                //     let index = ctx.this.p.unwrap_str_constant(index)?;
                //     if let Some(&idx) = ctx.this.p.contextual_indices.get(&Ascii::new(index)) {
                //         ctx.this
                //             .chunk
                //             .emit_load_const(ctx.this.p.new_value_int(idx.into()), src_info);
                //     } else {
                //         ctx.this
                //             .chunk
                //             .emit_load_const(ctx.this.p.new_value_int(-1), src_info);
                //     }
                // } else {
                //     ctx.this
                //         .chunk
                //         .emit_load_const(ctx.this.p.new_value_int(-1), src_info);
                // }
                let [target, index] = ctx.unpack_some_args(args)?;
                let target = ctx.this.unpack_var_expr_from_expr(target)?;
                ctx.this.expr_str(index)?;
                ctx.this.chunk.emit_bytecode(CsvGetNum, src_info);
            }
            b"GROUPMATCH" | b"NOSAMES" | b"ALLSAMES" => {
                ctx.result()?;
                if args.is_empty() {
                    bail_opt!(
                        ctx.this,
                        src_info,
                        format!("`{target}` requires at least 1 argument")
                    );
                }
                let mut args_cnt = 0;
                let mut it = args.into_iter();
                let first_k = if let Some(first) = it.next().unwrap() {
                    ctx.this.expression(first)?
                } else {
                    bail_opt!(
                        ctx.this,
                        src_info,
                        format!("first argument of `{target}` cannot be omitted")
                    );
                };
                if !matches!(first_k, TInteger | TString) {
                    bail_opt!(
                        ctx.this,
                        src_info,
                        format!("first argument of `{target}` cannot be void")
                    );
                }
                for arg in it {
                    let Some(arg) = arg else {
                        continue;
                    };
                    match first_k {
                        TInteger => ctx.this.expr_int(arg)?,
                        TString => ctx.this.expr_str(arg)?,
                        TVoid => unreachable!(),
                    }
                    args_cnt += 1;
                }
                ctx.this.chunk.emit_bytecode(GroupMatch, src_info);
                ctx.this.chunk.append_u8(args_cnt, src_info);
                match upper_target.as_bytes() {
                    b"GROUPMATCH" => (),
                    b"NOSAMES" => {
                        ctx.this
                            .chunk
                            .emit_load_const(ctx.this.p.new_value_int(0), src_info);
                        ctx.this.chunk.emit_bytecode(CompareEq, src_info);
                    }
                    b"ALLSAMES" => {
                        ctx.this
                            .chunk
                            .emit_load_const(ctx.this.p.new_value_int(args_cnt as _), src_info);
                        ctx.this.chunk.emit_bytecode(CompareEq, src_info);
                    }
                    _ => unreachable!(),
                }
            }
            b"GETTIME" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.chunk.emit_bytecode(GetHostTime, src_info);
            }
            b"GETTIMES" => {
                ctx.results()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.chunk.emit_bytecode(GetHostTimeS, src_info);
            }
            b"UNICODE" => {
                // TODO: Support non-constexpr UNICODE
                ctx.results()?;
                let [arg] = ctx.unpack_some_args(args)?;
                // let arg = ctx.this.p.evaluate_constant(arg)?;
                // let arg = ctx.this.p.unwrap_int_constant(arg)?;
                // let ch = char::from_u32(arg as _).unwrap_or(char::REPLACEMENT_CHARACTER);
                // ctx.this
                //     .chunk
                //     .emit_load_const(ctx.this.p.new_value_str(ch.to_string()), src_info);
                ctx.this.expr_int(arg)?;
                ctx.this.chunk.emit_bytecode(UnicodeToStr, src_info);
            }
            b"GETMILLISECOND" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.chunk.emit_bytecode(GetHostTimeRaw, src_info);
            }
            b"GETSECOND" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.chunk.emit_bytecode(GetHostTimeRaw, src_info);
                ctx.this
                    .chunk
                    .emit_load_const(ctx.this.p.new_value_int(1000), src_info);
                ctx.this.chunk.emit_bytecode(Divide, src_info);
            }
            b"CSVNAME" | b"CSVCALLNAME" | b"CSVNICKNAME" | b"CSVMASTERNAME" | b"CSVBASE"
            | b"CSVCSTR" | b"CSVABL" | b"CSVTALENT" | b"CSVMARK" | b"CSVEXP" | b"CSVRELATION"
            | b"CSVJUEL" | b"CSVEQUIP" | b"CSVCFLAG" => {
                use crate::bytecode::EraCsvGetProp2SubBytecodeType::*;
                let (is_string, sub_bytecode) = match upper_target.as_bytes() {
                    b"CSVNAME" => (true, CsvName),
                    b"CSVCALLNAME" => (true, CsvCallName),
                    b"CSVNICKNAME" => (true, CsvNickName),
                    b"CSVMASTERNAME" => (true, CsvMasterName),
                    b"CSVBASE" => (false, CsvBase),
                    b"CSVCSTR" => (true, CsvCStr),
                    b"CSVABL" => (false, CsvAbl),
                    b"CSVTALENT" => (false, CsvTalent),
                    b"CSVMARK" => (false, CsvMark),
                    b"CSVEXP" => (false, CsvExp),
                    b"CSVRELATION" => (false, CsvRelation),
                    b"CSVJUEL" => (false, CsvJuel),
                    b"CSVEQUIP" => (false, CsvEquip),
                    b"CSVCFLAG" => (false, CsvCFlag),
                    _ => unreachable!(),
                };
                if is_string {
                    ctx.results()?;
                } else {
                    ctx.result()?;
                }
                let [chara_no, index];
                match upper_target.as_bytes() {
                    b"CSVNAME" | b"CSVCALLNAME" | b"CSVNICKNAME" | b"CSVMASTERNAME" => {
                        [chara_no] = ctx.unpack_some_args(args)?;
                        index = EraExpr::new_int(0, src_info);
                    }
                    _ => [chara_no, index] = ctx.unpack_some_args(args)?,
                }
                ctx.this.expr_int(chara_no)?;
                ctx.this.expr_int(index)?;
                ctx.this.chunk.emit_bytecode(CsvGetProp2, src_info);
                ctx.this.chunk.append_u8(sub_bytecode.to_i(), src_info);
            }
            b"POWER" => {
                ctx.result()?;
                let [base, expo] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(base)?;
                ctx.this.expr_int(expo)?;
                ctx.this.chunk.emit_bytecode(PowerInt, src_info);
            }
            b"RAND" => {
                ctx.result()?;
                match args.len() {
                    1 => {
                        let [upper] = ctx.unpack_some_args(args)?;
                        ctx.this.expr_int(upper)?;
                        ctx.this.chunk.emit_bytecode(GetRandomMax, src_info);
                    }
                    2 => {
                        let [lower, upper] = ctx.unpack_some_args(args)?;
                        ctx.this.expr_int(lower)?;
                        ctx.this.expr_int(upper)?;
                        ctx.this.chunk.emit_bytecode(GetRandomRange, src_info);
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
            }
            b"SQRT" | b"CBRT" | b"LOG" | b"LOG10" | b"EXPONENT" => {
                ctx.result()?;
                let bytecode = match upper_target.as_bytes() {
                    b"SQRT" => SqrtInt,
                    b"CBRT" => CbrtInt,
                    b"LOG" => LogInt,
                    b"LOG10" => Log10Int,
                    b"EXPONENT" => ExponentInt,
                    _ => unreachable!(),
                };
                let [value] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(value)?;
                ctx.this.chunk.emit_bytecode(bytecode, src_info);
            }
            b"ABS" => {
                ctx.result()?;
                let [value] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(value)?;
                ctx.this.chunk.emit_bytecode(AbsInt, src_info);
            }
            b"SIGN" => {
                ctx.result()?;
                let [value] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(value)?;
                ctx.this.chunk.emit_duplicate_n(1, src_info);
                ctx.this
                    .chunk
                    .emit_load_const(ctx.this.p.new_value_int(0), src_info);
                ctx.this.chunk.emit_bytecode(CompareEq, src_info);
                let bt_done = ctx.this.chunk.emit_jump_cond_hold(src_info);
                ctx.this.chunk.emit_duplicate_n(1, src_info);
                ctx.this.chunk.emit_bytecode(AbsInt, src_info);
                ctx.this.chunk.emit_bytecode(Divide, src_info);
                bt_done.complete(ctx.this.chunk).unwrap();
            }
            b"TOSTR" | b"MONEYSTR" => {
                let is_money = upper_target == "MONEYSTR";
                // TODO: Support https://learn.microsoft.com/ja-jp/dotnet/api/system.int64.tostring
                ctx.results()?;
                if is_money {
                    ctx.this
                        .chunk
                        .emit_load_const(ctx.this.p.new_value_str("$".to_owned()), src_info);
                }
                match args.len() {
                    1 => {
                        let [value] = ctx.unpack_some_args(args)?;
                        ctx.this.expr_int(value)?;
                        ctx.this.chunk.emit_bytecode(ConvertToString, src_info);
                    }
                    2 => {
                        let [value, format] = ctx.unpack_some_args(args)?;
                        ctx.this.expr_int(value)?;
                        ctx.this.expr_str(format)?;
                        ctx.this.chunk.emit_bytecode(FormatIntToStr, src_info);
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
                if is_money {
                    ctx.this.chunk.emit_bytecode(Add, src_info)
                }
            }
            b"TOINT" => {
                ctx.result()?;
                let [value] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(value)?;
                ctx.this.chunk.emit_bytecode(ConvertToInteger, src_info);
            }
            b"ISNUMERIC" => {
                ctx.result()?;
                let [value] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(value)?;
                ctx.this.chunk.emit_bytecode(StringIsValidInteger, src_info);
            }
            b"VARSIZE" => {
                // HACK: Support non-constexpr variable names
                ctx.result()?;
                let [var, dimension];
                match args.len() {
                    1 => {
                        [var] = ctx.unpack_some_args(args)?;
                        dimension = EraExpr::new_int(-1, src_info);
                    }
                    2 => {
                        [var, dimension] = ctx.unpack_some_args(args)?;
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
                let var = ctx.this.p.evaluate_constant(var)?;
                let var = ctx.this.p.unwrap_str_constant(var)?;
                let dimension = ctx.this.p.evaluate_constant(dimension)?;
                let dimension = ctx.this.p.unwrap_int_constant(dimension)?;
                let info = ctx.this.var_arr_with_dims(&var, src_info)?;
                ctx.this.chunk.emit_bytecode(Pop, src_info);
                let EraVarArrCompileInfoWithDims::Normal(kind, mut dims, is_charadata) = info
                else {
                    bail_opt!(
                        ctx.this,
                        src_info,
                        "`VARSIZE` cannot be used on pseudo variables"
                    );
                };
                // HACK: VARSIZE ignores chara_no dimension
                if is_charadata {
                    dims.remove(0);
                }
                let dimension = if dimension < 0 {
                    dims.last().copied()
                } else {
                    dims.get(dimension as usize).copied()
                };
                let Some(result) = dimension else {
                    bail_opt!(ctx.this, src_info, "invalid VARSIZE dimension");
                };
                ctx.this
                    .chunk
                    .emit_load_const(ctx.this.p.new_value_int(result.into()), src_info);
            }
            b"TOUPPER" | b"TOLOWER" | b"TOHALF" | b"TOFULL" => {
                ctx.results()?;
                let bytecode = match upper_target.as_bytes() {
                    b"TOUPPER" => StringToUpper,
                    b"TOLOWER" => StringToLower,
                    b"TOHALF" => StringToHalf,
                    b"TOFULL" => StringToFull,
                    _ => unreachable!(),
                };
                let [value] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(value)?;
                ctx.this.chunk.emit_bytecode(bytecode, src_info);
            }
            b"EXISTCSV" => {
                ctx.result()?;
                let [chara_no] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(chara_no)?;
                ctx.this.chunk.emit_bytecode(CharaCsvExists, src_info);
            }
            b"GETPALAMLV" | b"GETEXPLV" => {
                ctx.result()?;
                let bytecode = match upper_target.as_bytes() {
                    b"GETPALAMLV" => GetPalamLv,
                    b"GETEXPLV" => GetExpLv,
                    _ => unreachable!(),
                };
                let [value, max_lv] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(value)?;
                ctx.this.expr_int(max_lv)?;
                ctx.this.chunk.emit_bytecode(bytecode, src_info);
            }
            b"GETCHARA" => {
                ctx.result()?;
                let [chara_no] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(chara_no)?;
                ctx.this.chunk.emit_bytecode(GetCharaRegNum, src_info);
            }
            b"ESCAPE" => {
                ctx.results()?;
                let [haystack] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(haystack)?;
                ctx.this.chunk.emit_bytecode(EscapeRegexStr, src_info);
            }
            b"LINEISEMPTY" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.arr_get_int("@LINEISEMPTY", vec![], src_info)?;
            }
            b"ENCODETOUNI" => {
                ctx.result()?;
                let [haystack, pos];
                match args.len() {
                    1 => {
                        [haystack] = ctx.unpack_some_args(args)?;
                        pos = EraExpr::new_int(0, src_info);
                    }
                    2 => {
                        [haystack, pos] = ctx.unpack_some_args(args)?;
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
                ctx.this.expr_str(haystack)?;
                ctx.this.expr_int(pos)?;
                ctx.this.chunk.emit_bytecode(EncodeToUnicode, src_info);
            }
            b"GETCONFIG" => {
                ctx.result()?;
                let [name] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(name)?;
                ctx.this.chunk.emit_bytecode(GetConfig, src_info);
            }
            b"GETCONFIGS" => {
                ctx.results()?;
                let [name] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(name)?;
                ctx.this.chunk.emit_bytecode(GetConfigS, src_info);
            }
            b"ISSKIP" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.arr_get_int("@SKIPDISP", vec![], src_info)?;
            }
            b"MOUSESKIP" | b"MESSKIP" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.arr_get_int("@MESSKIP", vec![], src_info)?;
            }
            b"CONVERT" => {
                ctx.results()?;
                let [value, base] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(value)?;
                ctx.this.expr_int(base)?;
                ctx.this.chunk.emit_bytecode(IntToStrWithBase, src_info);
            }
            b"PRINTCPERLINE" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.arr_get_int("@PRINTCPERLINE", vec![], src_info)?;
            }
            b"PRINTCLENGTH" => {
                ctx.result()?;
                let [] = ctx.unpack_some_args(args)?;
                ctx.this.arr_get_int("@PRINTCLENGTH", vec![], src_info)?;
            }
            // b"COLOR_FROMNAME" => {
            //     ctx.result()?;
            //     // TODO...
            // }
            b"COLOR_FROMRGB" => {
                ctx.result()?;
                let [r, g, b] = ctx.unpack_some_args(args)?;
                // TODO: Check argument range [0, 255]
                ctx.this.expr_int(r)?;
                ctx.this
                    .chunk
                    .emit_load_const(ctx.this.p.new_value_int(16), src_info);
                ctx.this.chunk.emit_bytecode(BitShiftL, src_info);
                ctx.this.expr_int(g)?;
                ctx.this
                    .chunk
                    .emit_load_const(ctx.this.p.new_value_int(8), src_info);
                ctx.this.chunk.emit_bytecode(BitShiftL, src_info);
                ctx.this.chunk.emit_bytecode(Add, src_info);
                ctx.this.expr_int(b)?;
                ctx.this.chunk.emit_bytecode(Add, src_info);
            }
            b"HTML_TOPLAINTEXT" => {
                ctx.results()?;
                let [html] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(html)?;
                ctx.this.chunk.emit_bytecode(HtmlToPlainText, src_info);
            }
            b"GETKEY" => {
                ctx.result()?;
                let [keycode] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(keycode)?;
                ctx.this.chunk.emit_bytecode(KbGetKeyState, src_info);
                ctx.this
                    .chunk
                    .emit_load_const(ctx.this.p.new_value_int(15), src_info);
                ctx.this.chunk.emit_bytecode(GetBit, src_info);
            }
            b"GETKEYTRIGGERED" => {
                ctx.result()?;
                let [keycode] = ctx.unpack_some_args(args)?;
                ctx.this.expr_int(keycode)?;
                ctx.this.chunk.emit_bytecode(KbGetKeyState, src_info);
                ctx.this
                    .chunk
                    .emit_load_const(ctx.this.p.new_value_int(1), src_info);
                ctx.this.chunk.emit_bytecode(BitAnd, src_info);
            }
            b"FIND_CHARADATA" => {
                ctx.result()?;
                let [filename] = ctx.unpack_some_args(args)?;
                ctx.this.expr_str(filename)?;
                ctx.this.chunk.emit_bytecode(FindCharaDataFile, src_info);
            }
            _ => bail_opt!(
                ctx.this,
                src_info,
                format!("function `{target}` is undefined or has no matching overloads")
            ),
        }

        Some(ctx.ret_kind)
    }
    #[must_use]
    fn statement_raw(&mut self, func_kind: EraFunKind, stmt: EraStmt) -> Option<()> {
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
                let dest_k = self.arr_idx(&x.dest.name, x.dest.idxs, x.dest.src_info)?;
                match dest_k {
                    TInteger => {
                        for src in x.srcs {
                            self.chunk.emit_duplicate_n(2, x.src_info);
                            self.expr_int(src)?;
                            self.chunk.emit_bytecode(SetArrayVal, x.src_info);
                            self.chunk.emit_pop(x.src_info);
                            self.chunk
                                .emit_load_const(self.p.new_value_int(1), x.src_info);
                            self.chunk.emit_bytecode(Add, x.src_info);
                        }
                    }
                    TString => {
                        for src in x.srcs {
                            self.chunk.emit_duplicate_n(2, x.src_info);
                            self.expr_str(src)?;
                            self.chunk.emit_bytecode(SetArrayVal, x.src_info);
                            self.chunk.emit_pop(x.src_info);
                            self.chunk
                                .emit_load_const(self.p.new_value_int(1), x.src_info);
                            self.chunk.emit_bytecode(Add, x.src_info);
                        }
                    }
                    TVoid => unreachable!(),
                }
                self.chunk.emit_pop_n(2, x.src_info);
                return Some(());
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

                self.cmd_print_core(x.flags, src_info)?;
            }
            Cmd::PrintData(x) => {
                if x.data.is_empty() {
                    bail_opt!(
                        self,
                        x.src_info,
                        "cannot use PRINTDATA with empty data list"
                    );
                }
                if let Some(dest) = x.dest {
                    self.arr_set(&dest.name, dest.idxs, dest.src_info, |this| {
                        this.chunk
                            .emit_load_const(this.p.new_value_int(x.data.len() as _), x.src_info);
                        this.chunk.emit_bytecode(GetRandomMax, x.src_info);
                        Some(TInteger)
                    })?;
                } else {
                    self.chunk
                        .emit_load_const(self.p.new_value_int(x.data.len() as _), x.src_info);
                    self.chunk.emit_bytecode(GetRandomMax, x.src_info);
                }

                self.cmd_printdata_core(x.data, x.src_info)?;

                self.cmd_print_core(x.flags, x.src_info)?;
            }
            Cmd::Wait(x) => {
                self.chunk
                    .emit_load_const(self.p.new_value_int(x.any_key.into()), x.src_info);
                self.chunk
                    .emit_load_const(self.p.new_value_int(x.is_force.into()), x.src_info);
                self.chunk.emit_bytecode(Wait, x.src_info);
            }
            Cmd::If(x) => {
                self.cmd_if_stmt(func_kind, x)?;
            }
            Cmd::Quit(x) => {
                self.chunk.emit_bytecode(Quit, x.src_info);
            }
            Cmd::SelectCase(x) => {
                // self.stack_balance += 1;

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
                        bt.complete(self.chunk).unwrap();
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
                                    EraTokenKind::BitAnd => {
                                        self.chunk.emit_bytecode(BitAnd, op_si);
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
                        bt.complete(self.chunk).unwrap();
                    }
                    // HACK: Pop stack early to prevent unbalanced stack
                    self.chunk.emit_bytecode(Pop, x.src_info);
                    for stmt in case_body {
                        self.statement(func_kind, stmt)?;
                    }
                    bt_dones.push(self.chunk.emit_jump_hold(x.src_info));
                }
                if let Some(bt) = bt_else {
                    bt.complete(self.chunk).unwrap();
                }
                for stmt in x.case_else {
                    self.statement(func_kind, stmt)?;
                }
                for bt in bt_dones {
                    if bt.complete(self.chunk).is_none() {
                        bail_opt!(self, x.src_info, "jump too far to be encoded in bytecode");
                    }
                }
                // self.chunk.emit_bytecode(Pop, x.src_info);

                // self.stack_balance -= 1;
            }
            Cmd::While(x) => {
                let start_pos = self.chunk.cur_bytes_cnt();
                self.loop_structs
                    .push(EraLoopStructCodeMetadata::new(self.stack_balance));
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
                    bt.complete_at(self.chunk, start_pos).unwrap();
                }
                for bt in loop_struct.done_queue {
                    bt.complete(self.chunk).unwrap();
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
                    self.dynamic_fun_call(x.func, x.args, true)?;
                    // self.chunk.emit_bytecode(Pop, x.src_info);
                }
            },
            Cmd::TryCall(x) => {
                // Enforce dynamic call
                self.dynamic_fun_call(x.func, x.args, false)?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::TryCCall(x) => {
                // Enforce dynamic call
                self.dynamic_fun_call(x.func, x.args, false)?;
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
                        self.chunk.emit_bytecode(Pop, x.src_info);
                    }
                    self.chunk.emit_bytecode(ReturnVoid, x.src_info);
                } else {
                    let src_info = x.src_info;
                    let val = match TryInto::<[_; 1]>::try_into(x.vals) {
                        Ok([val]) => val,
                        Err(vals) => {
                            if vals.len() == 0 {
                                self.report_err(
                                    src_info,
                                    false,
                                    concatdoc! {"
                                        RETURN without values will implicitly return nulls for this function. ",
                                        "Did you forget to add the return value?"
                                    }
                                );
                                match func_kind {
                                    EraFunKind::Function => EraExpr::new_int(0, src_info),
                                    EraFunKind::FunctionS => {
                                        EraExpr::new_str(String::new(), src_info)
                                    }
                                    _ => unreachable!(),
                                }
                            } else {
                                bail_opt!(
                                    self,
                                    src_info,
                                    "too many return values for current function"
                                );
                            }
                        }
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
                // self.chunk.emit_pop_n(self.stack_balance - loop_struct.stack_balance, x.src_info);
                if self.stack_balance > loop_struct.stack_balance {
                    todo!("fix stack balance")
                }
                // self.stack_balance = loop_struct.stack_balance;
                loop_struct
                    .continue_queue
                    .push(self.chunk.emit_jump_hold(x.src_info));
            }
            Cmd::Break(x) => {
                let Some(loop_struct) = self.loop_structs.last_mut() else {
                    bail_opt!(self, x.src_info, "loop controlling statements are valid only in the context of a loop structure");
                };
                // self.chunk.emit_pop_n(self.stack_balance - loop_struct.stack_balance, x.src_info);
                if self.stack_balance > loop_struct.stack_balance {
                    todo!("fix stack balance")
                }
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
                self.loop_structs
                    .push(EraLoopStructCodeMetadata::new(self.stack_balance));
                let step_fn = |this: &mut Self| {
                    this.arr_set("COUNT", vec![], x.src_info, |this| {
                        this.arr_get_int("COUNT", vec![], x.src_info)?;
                        this.chunk
                            .emit_load_const(this.p.new_value_int(1), x.src_info);
                        this.chunk.emit_bytecode(Add, x.src_info);
                        Some(TInteger)
                    })?;
                    this.chunk.emit_bytecode(Pop, x.src_info);
                    Some(())
                };
                step_fn(self)?;
                bt_body.complete(self.chunk).unwrap();
                self.chunk.emit_bytecode(Duplicate, x.src_info);
                self.arr_get_int("COUNT", vec![], x.src_info)?;
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
                    bt.complete_at(self.chunk, start_pos).unwrap();
                }
                for bt in loop_struct.done_queue {
                    bt.complete(self.chunk).unwrap();
                }
                // NOTE: Emulates Eramaker behavior (inc COUNT even when break'ing)
                step_fn(self)?;
                bt_done.complete(self.chunk).unwrap();
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
                self.loop_structs
                    .push(EraLoopStructCodeMetadata::new(self.stack_balance));
                emit_step_fn(self, x.src_info);
                bt_body.complete(self.chunk).unwrap();
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
                    bt.complete_at(self.chunk, start_pos).unwrap();
                }
                for bt in loop_struct.done_queue {
                    bt.complete(self.chunk).unwrap();
                }
                // NOTE: Emulates Eramaker behavior (inc COUNT even when break'ing)
                emit_step_fn(self, x.src_info);
                bt_done.complete(self.chunk).unwrap();
                self.chunk.emit_bytecode(Pop, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);

                self.stack_balance -= 4;
            }
            Cmd::DoLoop(x) => {
                let si = x.src_info;
                let start_pos = self.chunk.cur_bytes_cnt();
                self.loop_structs
                    .push(EraLoopStructCodeMetadata::new(self.stack_balance));
                for stmt in x.body {
                    self.statement(func_kind, stmt)?;
                }
                let continue_pos = self.chunk.cur_bytes_cnt();
                self.expression(x.cond)?;
                self.chunk
                    .emit_jump_cond_hold(si)
                    .complete_at(self.chunk, start_pos)
                    .unwrap();
                let loop_struct = self.loop_structs.pop().unwrap();
                for bt in loop_struct.continue_queue {
                    bt.complete_at(self.chunk, continue_pos).unwrap();
                }
                for bt in loop_struct.done_queue {
                    bt.complete(self.chunk).unwrap();
                }
            }
            Cmd::Split(x) => {
                self.expr_str(x.input)?;
                self.expr_str(x.separator)?;
                self.arr_idx_str_ve(x.dest)?;
                self.arr_idx_int_ve(x.dest_count)?;
                self.chunk.emit_bytecode(SplitString, x.src_info);
            }
            Cmd::Times(x) => {
                self.arr_idx_int_ve(x.target)?;
                self.chunk
                    .emit_load_const(self.p.new_value_int(x.factor.to_bits() as _), x.src_info);
                self.chunk.emit_bytecode(TimesFloat, x.src_info);
            }
            Cmd::SetBit(x) => {
                // NOTE: Semantics of command SETBIT(, ...) and bytecode SetBit differs slightly
                //       in that SETBIT affects a variable, while SetBit operates on an
                //       intermediate integer.
                self.arr_idx_int_ve(x.target)?;
                self.chunk.emit_duplicate_n(2, x.src_info);
                self.chunk.emit_bytecode(GetArrayVal, x.src_info);
                for bit in x.bits {
                    self.expr_int(bit)?;
                    self.chunk.emit_bytecode(SetBit, x.src_info);
                }
                self.chunk.emit_bytecode(SetArrayVal, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::ClearBit(x) => {
                self.arr_idx_int_ve(x.target)?;
                self.chunk.emit_duplicate_n(2, x.src_info);
                self.chunk.emit_bytecode(GetArrayVal, x.src_info);
                for bit in x.bits {
                    self.expr_int(bit)?;
                    self.chunk.emit_bytecode(ClearBit, x.src_info);
                }
                self.chunk.emit_bytecode(SetArrayVal, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::InvertBit(x) => {
                self.arr_idx_int_ve(x.target)?;
                self.chunk.emit_duplicate_n(2, x.src_info);
                self.chunk.emit_bytecode(GetArrayVal, x.src_info);
                for bit in x.bits {
                    self.expr_int(bit)?;
                    self.chunk.emit_bytecode(InvertBit, x.src_info);
                }
                self.chunk.emit_bytecode(SetArrayVal, x.src_info);
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::SetColor(x) => {
                self.arr_set("@COLOR", vec![], x.src_info, |this| {
                    this.expression(x.color)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::ResetColor(x) => {
                self.arr_set("@COLOR", vec![], x.src_info, |this| {
                    this.arr_get_int("@DEFCOLOR", vec![], x.src_info)?;
                    Some(TInteger)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::SetBgColor(x) => {
                self.arr_set("@BGCOLOR", vec![], x.src_info, |this| {
                    this.expression(x.color)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::ResetBgColor(x) => {
                self.arr_set("@BGCOLOR", vec![], x.src_info, |this| {
                    this.arr_get_int("@DEFBGCOLOR", vec![], x.src_info)?;
                    Some(TInteger)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::VarSet(x) => {
                let (target_k, target_dims_cnt) =
                    self.var_arr_no_pseudo(&x.target.name, x.target.src_info)?;
                // TODO: Reject when is mdarray && specifies index range
                let value = x
                    .value
                    .unwrap_or_else(|| default_expr(target_k, x.src_info));
                let start_index = x
                    .start_index
                    .unwrap_or_else(|| EraExpr::new_int(0, x.src_info));
                let end_index = x
                    .end_index
                    .unwrap_or_else(|| EraExpr::new_int(-1, x.src_info));
                if target_k != self.expression(value)? {
                    bail_opt!(self, x.src_info, "array element type mismatches value type");
                }
                self.expr_int(start_index)?;
                self.expr_int(end_index)?;
                self.chunk.emit_bytecode(VarSet, x.src_info);
            }
            Cmd::CVarSet(x) => {
                let (target_k, target_dims_cnt) =
                    self.var_arr_no_pseudo(&x.target.name, x.target.src_info)?;
                self.expr_int(x.index)?;
                let value = x
                    .value
                    .unwrap_or_else(|| default_expr(target_k, x.src_info));
                let start_id = x
                    .start_id
                    .unwrap_or_else(|| EraExpr::new_int(0, x.src_info));
                let end_id = x.end_id.unwrap_or_else(|| EraExpr::new_int(-1, x.src_info));
                if target_k != self.expression(value)? {
                    bail_opt!(self, x.src_info, "array element type mismatches value type");
                }
                self.expr_int(start_id)?;
                self.expr_int(end_id)?;
                self.chunk.emit_bytecode(CVarSet, x.src_info);
            }
            Cmd::VarSize(x) => {
                // TODO: Optimize command VARSIZE performance
                let info = self.var_arr_with_dims(&x.var.name, x.var.src_info)?;
                self.chunk.emit_bytecode(Pop, x.src_info);
                let EraVarArrCompileInfoWithDims::Normal(kind, mut dims, is_charadata) = info
                else {
                    bail_opt!(
                        self,
                        x.src_info,
                        "`VARSIZE` cannot be used on pseudo variables"
                    );
                };
                // HACK: VARSIZE ignores chara_no dimension
                if is_charadata {
                    dims.remove(0);
                }
                for (idx, dim) in dims.into_iter().enumerate() {
                    let idx = EraExpr::new_int(idx as _, x.src_info);
                    self.arr_set("RESULT", vec![idx], x.src_info, |this| {
                        this.chunk
                            .emit_load_const(this.p.new_value_int(dim as _), x.var.src_info);
                        Some(TInteger)
                    })?;
                    self.chunk.emit_bytecode(Pop, x.src_info);
                }
            }
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
            Cmd::HtmlPrint(x) => {
                self.expr_str(x.expr)?;
                self.chunk.emit_bytecode(HtmlPrint, x.src_info);
            }
            Cmd::PrintButton(x) => {
                self.expr_str(x.content)?;
                match self.expression(x.value)? {
                    TInteger => self.chunk.emit_bytecode(ConvertToString, x.src_info),
                    TString => (),
                    TVoid => bail_opt!(self, x.src_info, "value cannot be void"),
                }
                self.chunk.emit_bytecode(PrintButton, x.src_info);
                self.chunk.append_u8(x.flags.into(), x.src_info);
            }
            Cmd::ArrayRemove(x) => {
                let info = self.var_arr_no_pseudo(&x.target.name, x.target.src_info)?;
                self.expr_int(x.start_index)?;
                self.expr_int(x.count)?;
                self.chunk.emit_bytecode(ArrayRemove, x.src_info);
            }
            Cmd::ArraySort(x) => {
                let bytecode = if x.is_ascending {
                    ArraySortAsc
                } else {
                    ArraySortDesc
                };
                let info = self.var_arr_no_pseudo(&x.target.name, x.target.src_info)?;
                let start_index = x
                    .start_index
                    .unwrap_or_else(|| EraExpr::new_int(0, x.src_info));
                let count = x.count.unwrap_or_else(|| EraExpr::new_int(-1, x.src_info));
                self.expr_int(start_index)?;
                self.expr_int(count)?;
                self.chunk.emit_bytecode(bytecode, x.src_info);
            }
            Cmd::ArrayMSort(x) => {
                // TODO: Do deeper dimensions checking
                self.var_arr_no_pseudo(&x.primary.name, x.primary.src_info)?;
                let subs_cnt = x.subs.len();
                for sub in x.subs {
                    self.var_arr_no_pseudo(&sub.name, sub.src_info)?;
                }
                self.chunk.emit_bytecode(ArrayMSort, x.src_info);
                self.chunk.append_u8(subs_cnt as _, x.src_info);
            }
            Cmd::ArrayCopy(x) => {
                // TODO: Support dynamic array names
                let from = self.p.evaluate_constant(x.from_name)?;
                let from_si = from.source_pos_info();
                let from = self.p.unwrap_str_constant(from)?;
                let to = self.p.evaluate_constant(x.to_name)?;
                let to_si = to.source_pos_info();
                let to = self.p.unwrap_str_constant(to)?;
                let (from_k, _) = self.var_arr_no_pseudo(&from, from_si)?;
                let (to_k, _) = self.var_arr_no_pseudo(&to, to_si)?;
                if from_k != to_k {
                    bail_opt!(self, x.src_info, "array elements type mismatch");
                }
                self.chunk.emit_bytecode(ArrayCopy, x.src_info);
            }
            Cmd::ArrayShift(x) => {
                let (target_k, target_dims_cnt) =
                    self.var_arr_no_pseudo(&x.target.name, x.target.src_info)?;
                self.expr_int(x.shift_count)?;
                if target_k != self.expression(x.value)? {
                    bail_opt!(self, x.src_info, "array element type mismatches value type");
                }
                self.expr_int(x.start_index)?;
                self.expr_int(x.target_count)?;
                self.chunk.emit_bytecode(ArrayShift, x.src_info);
            }
            Cmd::Input(_) | Cmd::InputS(_) => {
                let (x, is_string) = match stmt {
                    Cmd::Input(x) => (x, false),
                    Cmd::InputS(x) => (x, true),
                    _ => unreachable!(),
                };
                let sub_bc = EraInputSubBytecodeType::new()
                    .with_is_string(is_string)
                    .with_is_one(false)
                    .with_is_timed(false)
                    .with_has_default_value(x.default_value.is_some());
                if let Some(default_value) = x.default_value {
                    if is_string {
                        self.expr_str(default_value)?;
                    } else {
                        self.expr_int(default_value)?;
                    }
                }
                self.expr_int(x.can_click)?;
                self.expr_int(x.allow_skip)?;
                self.chunk.emit_bytecode(Input, x.src_info);
                self.chunk.append_u8(sub_bc.into(), x.src_info);
            }
            Cmd::TInput(_) | Cmd::TInputS(_) => {
                let (x, is_string) = match stmt {
                    Cmd::TInput(x) => (x, false),
                    Cmd::TInputS(x) => (x, true),
                    _ => unreachable!(),
                };
                let sub_bc = EraInputSubBytecodeType::new()
                    .with_is_string(is_string)
                    .with_is_one(false)
                    .with_is_timed(true)
                    .with_has_default_value(true);
                self.expr_int(x.time_limit)?;
                if is_string {
                    self.expr_str(x.default_value)?;
                } else {
                    self.expr_int(x.default_value)?;
                }
                self.expr_int(x.show_prompt)?;
                self.expr_str(x.expiry_msg)?;
                self.expr_int(x.can_click)?;
                self.chunk.emit_bytecode(Input, x.src_info);
                self.chunk.append_u8(sub_bc.into(), x.src_info);
            }
            Cmd::OneInput(_) | Cmd::OneInputS(_) => {
                let (x, is_string) = match stmt {
                    Cmd::OneInput(x) => (x, false),
                    Cmd::OneInputS(x) => (x, true),
                    _ => unreachable!(),
                };
                let sub_bc = EraInputSubBytecodeType::new()
                    .with_is_string(is_string)
                    .with_is_one(true)
                    .with_is_timed(false)
                    .with_has_default_value(x.default_value.is_some());
                if let Some(default_value) = x.default_value {
                    if is_string {
                        self.expr_str(default_value)?;
                    } else {
                        self.expr_int(default_value)?;
                    }
                }
                self.chunk.emit_bytecode(Input, x.src_info);
                self.chunk.append_u8(sub_bc.into(), x.src_info);
            }
            Cmd::TOneInput(_) | Cmd::TOneInputS(_) => {
                let (x, is_string) = match stmt {
                    Cmd::TOneInput(x) => (x, false),
                    Cmd::TOneInputS(x) => (x, true),
                    _ => unreachable!(),
                };
                let sub_bc = EraInputSubBytecodeType::new()
                    .with_is_string(is_string)
                    .with_is_one(true)
                    .with_is_timed(true)
                    .with_has_default_value(true);
                self.expr_int(x.time_limit)?;
                if is_string {
                    self.expr_str(x.default_value)?;
                } else {
                    self.expr_int(x.default_value)?;
                }
                self.expr_int(x.show_prompt)?;
                self.expr_str(x.expiry_msg)?;
                self.expr_int(x.can_click)?;
                self.chunk.emit_bytecode(Input, x.src_info);
                self.chunk.append_u8(sub_bc.into(), x.src_info);
            }
            Cmd::ReuseLastLine(x) => {
                self.expr_str(x.content)?;
                self.chunk.emit_bytecode(ReuseLastLine, x.src_info);
            }
            Cmd::ClearLine(x) => {
                self.expr_int(x.count)?;
                self.chunk.emit_bytecode(ClearLine, x.src_info);
            }
            Cmd::DrawLine(x) => {
                // self.arr_get_int("SCREENWIDTH", vec![], x.src_info)?;
                // self.chunk
                //     .emit_load_const(self.p.new_value_str("-".to_owned()), x.src_info);
                // self.chunk.emit_bytecode(RepeatString, x.src_info);
                // self.chunk.emit_bytecode(PrintLine, x.src_info);
                self.arr_get_str("DRAWLINESTR", vec![], x.src_info)?;
                self.chunk.emit_bytecode(PrintLine, x.src_info);
            }
            Cmd::CustomDrawLine(x) => {
                // TODO: Optimize command CUSTOMDRAWLINE performance
                // FIXME: If content is too long, output will be empty
                self.expr_str(x.content)?;
                self.arr_get_int("SCREENWIDTH", vec![], x.src_info)?;
                self.chunk.emit_duplicate_one_n(2, x.src_info);
                self.chunk.emit_bytecode(StrLen, x.src_info);
                self.chunk.emit_bytecode(Divide, x.src_info);
                self.chunk.emit_bytecode(RepeatString, x.src_info);
                self.chunk.emit_duplicate_n(1, x.src_info);
                self.chunk
                    .emit_load_const(self.p.new_value_int(0), x.src_info);
                self.arr_get_int("SCREENWIDTH", vec![], x.src_info)?;
                self.chunk.emit_duplicate_one_n(3, x.src_info);
                self.chunk.emit_bytecode(StrLen, x.src_info);
                self.chunk.emit_bytecode(Subtract, x.src_info);
                self.chunk.emit_bytecode(SubString, x.src_info);
                self.chunk.emit_bytecode(Add, x.src_info);
                self.chunk.emit_bytecode(PrintLine, x.src_info);
            }
            Cmd::TWait(x) => {
                self.expr_int(x.duration)?;
                self.expr_int(x.force_wait)?;
                self.chunk.emit_bytecode(TWait, x.src_info);
            }
            Cmd::FontStyle(x) => {
                self.arr_set("@STYLE", vec![], x.src_info, |this| {
                    this.expr_int(x.style)?;
                    Some(TInteger)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::FontBold(x) => {
                self.arr_set("@STYLE", vec![], x.src_info, |this| {
                    this.arr_get_int("@STYLE", vec![], x.src_info)?;
                    this.chunk
                        .emit_load_const(this.p.new_value_int(1), x.src_info);
                    this.chunk.emit_bytecode(BitOr, x.src_info);
                    Some(TInteger)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::FontItalic(x) => {
                self.arr_set("@STYLE", vec![], x.src_info, |this| {
                    this.arr_get_int("@STYLE", vec![], x.src_info)?;
                    this.chunk
                        .emit_load_const(this.p.new_value_int(1), x.src_info);
                    this.chunk.emit_bytecode(BitOr, x.src_info);
                    Some(TInteger)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::FontRegular(x) => {
                self.arr_set("@STYLE", vec![], x.src_info, |this| {
                    this.chunk
                        .emit_load_const(this.p.new_value_int(0), x.src_info);
                    Some(TInteger)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::SetFont(x) => {
                self.arr_set("@FONT", vec![], x.src_info, |this| {
                    this.expr_str(x.font_name)?;
                    Some(TString)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::StrData(x) => {
                if x.data.is_empty() {
                    bail_opt!(self, x.src_info, "cannot use STRDATA with empty data list");
                }
                self.arr_set(&x.target.name, x.target.idxs, x.target.src_info, |this| {
                    this.chunk
                        .emit_load_const(this.p.new_value_int(x.data.len() as _), x.src_info);
                    this.chunk.emit_bytecode(GetRandomMax, x.src_info);
                    this.cmd_printdata_core(x.data, x.src_info)?;
                    Some(TString)
                })?;
                self.chunk.emit_pop(x.src_info);
            }
            Cmd::PutForm(x) => {
                self.arr_set("SAVEDATA_TEXT", vec![], x.src_info, |this| {
                    this.arr_get_str("SAVEDATA_TEXT", vec![], x.src_info)?;
                    this.expr_str(x.cont)?;
                    this.chunk.emit_bytecode(Add, x.src_info);
                    Some(TString)
                })?;
                self.chunk.emit_pop(x.src_info);
            }
            Cmd::SkipDisp(x) => {
                self.arr_set("@SKIPDISP", vec![], x.src_info, |this| {
                    this.expr_int(x.is_skip)?;
                    Some(TInteger)
                })?;
                self.chunk.emit_pop(x.src_info);
            }
            Cmd::Begin(x) => {
                use crate::bytecode::EraBeginSystemProcedureKind::*;
                //self.chunk.emit_bytecode(BeginSystemProcedure, x.src_info);
                //self.chunk.append_u8(x.proc as _, x.src_info);
                let (func_name, reset_exec) = match x.proc {
                    First => ("SYSPROC_BEGIN_FIRST", false),
                    Title => ("SYSPROC_BEGIN_TITLE", false),
                    Train => ("SYSPROC_BEGIN_TRAIN", true),
                    AfterTrain => ("SYSPROC_BEGIN_AFTERTRAIN", false),
                    AblUp => ("SYSPROC_BEGIN_ABLUP", false),
                    TurnEnd => ("SYSPROC_BEGIN_TURNEND", false),
                    Shop => ("SYSPROC_BEGIN_SHOP", false),
                };
                if reset_exec {
                    let Some(&func) = self.funcs.func_names.get(CaselessStr::new(&func_name))
                    else {
                        bail_opt!(
                            self,
                            x.src_info,
                            format!("internal function `{func_name}` not found")
                        );
                    };
                    let func_info = &self.funcs.funcs[func];
                    if !func_info.params.is_empty() {
                        bail_opt!(
                            self,
                            x.src_info,
                            format!("internal function `{func_name}` must not accept arguments")
                        );
                    }
                    self.chunk
                        .emit_load_const(self.p.new_value_int(func as _), x.src_info);
                    self.chunk.emit_bytecode(RestartExecAtFun, x.src_info);
                } else {
                    self.static_fun_call(func_name, vec![], x.src_info)?;
                }
            }
            Cmd::DoTrain(x) => {
                //self.expr_int(x.number)?;
                //self.chunk.emit_bytecode(DoTrain, x.src_info);
                self.static_fun_call("SYSPROC_DOTRAIN", vec![Some(x.number)], x.src_info)?;
            }
            Cmd::Redraw(x) => {
                self.arr_set("@REDRAW", vec![], x.src_info, |this| {
                    this.expr_int(x.arg)?;
                    Some(TInteger)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::StrLen(x) => {
                self.arr_set("RESULT", vec![], x.src_info, |this| {
                    this.expr_str(x.cont)?;
                    this.chunk.emit_bytecode(StrLen, x.src_info);
                    Some(TInteger)
                })?;
                self.chunk.emit_pop(x.src_info);
            }
            Cmd::StrLenU(x) => {
                self.arr_set("RESULT", vec![], x.src_info, |this| {
                    this.expr_str(x.cont)?;
                    this.chunk.emit_bytecode(StrLenU, x.src_info);
                    Some(TInteger)
                })?;
                self.chunk.emit_pop(x.src_info);
            }
            Cmd::Alignment(x) => {
                self.arr_set("@ALIGN", vec![], x.src_info, |this| {
                    this.chunk
                        .emit_load_const(this.p.new_value_int(x.alignment as _), x.src_info);
                    Some(TInteger)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::ToolTipSetDelay(x) => {
                self.arr_set("@TOOLTIP_DELAY", vec![], x.src_info, |this| {
                    this.expr_int(x.duration)?;
                    Some(TInteger)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::ToolTipSetDuration(x) => {
                self.arr_set("@TOOLTIP_DURATION", vec![], x.src_info, |this| {
                    this.expr_int(x.duration)?;
                    Some(TInteger)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::Randomize(x) => {
                self.report_err(
                    x.src_info,
                    true,
                    "RANDOMIZE is not supported and will be ignored",
                );
            }
            Cmd::DumpRand(x) => {
                self.report_err(
                    x.src_info,
                    true,
                    "DUMPRAND is not supported and will be ignored",
                );
            }
            Cmd::InitRand(x) => {
                self.report_err(
                    x.src_info,
                    true,
                    "INITRAND is not supported and will be ignored",
                );
            }
            Cmd::Bar(x) => {
                self.expr_int(x.value)?;
                self.expr_int(x.max_value)?;
                self.expr_int(x.length)?;
                self.chunk.emit_bytecode(BuildBarString, x.src_info);
                self.cmd_print_core(
                    PrintExtendedFlags::new().with_is_line(x.new_line),
                    x.src_info,
                )?;
            }
            Cmd::AddChara(x) => {
                for chara in x.charas {
                    self.expr_int(chara)?;
                    self.chunk.emit_bytecode(AddChara, x.src_info);
                }
            }
            Cmd::PickUpChara(x) => {
                let charas_cnt = x.charas.len();
                for chara in x.charas {
                    self.expr_int(chara)?;
                }
                self.chunk.emit_bytecode(PickUpChara, x.src_info);
                self.chunk.append_u8(charas_cnt as _, x.src_info);
            }
            Cmd::DelChara(x) => {
                let charas_cnt = x.charas.len();
                for chara in x.charas {
                    self.expr_int(chara)?;
                }
                self.chunk.emit_bytecode(DeleteChara, x.src_info);
                self.chunk.append_u8(charas_cnt as _, x.src_info);
            }
            Cmd::SwapChara(x) => {
                self.expr_int(x.chara1)?;
                self.expr_int(x.chara2)?;
                self.chunk.emit_bytecode(SwapChara, x.src_info);
            }
            Cmd::AddCopyChara(x) => {
                self.expr_int(x.chara)?;
                self.chunk.emit_bytecode(AddCopyChara, x.src_info);
            }
            Cmd::ResetStain(x) => {
                self.expr_int(x.chara)?;
                self.chunk.emit_bytecode(ResetCharaStain, x.src_info);
            }
            Cmd::SaveChara(x) => {
                let charas_cnt = x.charas.len();
                self.expr_str(x.filename)?;
                self.expr_str(x.memo)?;
                for chara in x.charas {
                    self.expr_int(chara)?;
                }
                self.chunk.emit_bytecode(SaveChara, x.src_info);
                self.chunk.append_u8(charas_cnt as _, x.src_info);
            }
            Cmd::LoadChara(x) => {
                self.arr_set("RESULT", vec![], x.src_info, |this| {
                    this.expr_str(x.filename)?;
                    this.chunk.emit_bytecode(LoadChara, x.src_info);
                    Some(TInteger)
                })?;
                self.chunk.emit_bytecode(Pop, x.src_info);
            }
            Cmd::SetAnimeTimer(x) => {
                self.arr_set("@ANIMETIMER", vec![], x.src_info, |this| {
                    this.expr_int(x.duration)?;
                    Some(TInteger)
                })?;
                self.chunk.emit_pop(x.src_info);
            }
            Cmd::HtmlTagSplit(x) => {
                self.expr_str(x.html)?;
                self.var_arr_str(&x.var_tags.name, x.var_tags.src_info)?;
                self.var_arr_int(&x.var_count.name, x.var_count.src_info)?;
                self.chunk.emit_bytecode(HtmlTagSplit, x.src_info);
            }
            Cmd::Power(x) => {
                self.arr_set(&x.target.name, x.target.idxs, x.target.src_info, |this| {
                    this.expr_int(x.base)?;
                    this.expr_int(x.exponent)?;
                    this.chunk.emit_bytecode(PowerInt, x.src_info);
                    Some(TInteger)
                })?;
                self.chunk.emit_pop(x.src_info);
            }
            Cmd::SaveData(x) => {
                self.expr_int(x.save_id)?;
                self.expr_str(x.save_info)?;
                self.chunk.emit_bytecode(SaveData, x.src_info);
            }
            Cmd::Restart(x) => {
                // HACK: RESTART is unconditional GOTO start of function body
                self.chunk
                    .emit_jump_hold(x.src_info)
                    .complete_at(self.chunk, self.body_start_pos)
                    .unwrap();
            }
            Cmd::GetTime(x) => {
                let src_info = x.src_info;
                self.arr_set("RESULT", vec![], src_info, |this| {
                    this.chunk.emit_bytecode(GetHostTime, src_info);
                    Some(TInteger)
                })?;
                self.chunk.emit_pop(x.src_info);
                self.arr_set("RESULTS", vec![], src_info, |this| {
                    this.chunk.emit_bytecode(GetHostTimeS, src_info);
                    Some(TString)
                })?;
                self.chunk.emit_pop(x.src_info);
            }
            Cmd::LoadGlobal(x) => {
                self.arr_set("RESULT", vec![], x.src_info, |this| {
                    this.chunk.emit_bytecode(LoadGlobal, x.src_info);
                    Some(TInteger)
                })?;
                self.chunk.emit_pop(x.src_info);
            }
            Cmd::SaveGlobal(x) => {
                // NOTE: Incompatible with Emuera: returns a value to indicate success or failure
                self.arr_set("RESULT", vec![], x.src_info, |this| {
                    this.chunk.emit_bytecode(SaveGlobal, x.src_info);
                    Some(TInteger)
                })?;
                self.chunk.emit_pop(x.src_info);
            }
            Cmd::LoadGame(x) => {
                // self.chunk.emit_bytecode(LoadGame, x.src_info);
                self.static_fun_call("SYSPROC_LOADGAME", vec![], x.src_info)?;
            }
            Cmd::SaveGame(x) => {
                // self.chunk.emit_bytecode(SaveGame, x.src_info);
                self.static_fun_call("SYSPROC_SAVEGAME", vec![], x.src_info)?;
            }
            Cmd::DebugClear(x) => {
                // TODO: Implement Cmd::DebugClear
                //self.chunk.emit_bytecode(SaveGame, x.src_info);
            }
            Cmd::ResetData(x) => {
                self.chunk.emit_bytecode(ResetData, x.src_info);
            }
            Cmd::ResultCmdCall(x) => {
                self.builtin_fun_call(&x.name, x.args, x.src_info, true)?;
            }
            _ => bail_opt!(
                self,
                stmt.source_pos_info(),
                format!("unsupported statement: {stmt:?}")
            ),
        }

        Some(())
    }
    #[must_use]
    fn statement(&mut self, func_kind: EraFunKind, stmt: EraStmt) -> Option<()> {
        // HACK: Recover even when there are compilation errors
        let si: SourcePosInfo = stmt.source_pos_info();
        let failed = self.statement_raw(func_kind, stmt).is_none();
        if self.fatal_stop {
            None
        } else {
            // Fill invalid bytecode
            if failed {
                self.chunk
                    .emit_bytecode(EraBytecodePrimaryType::Invalid, si);
            }
            Some(())
        }
    }
    // Precondition: string on stack
    // Postcondition: empty stack
    fn cmd_print_core(&mut self, flags: PrintExtendedFlags, src_info: SourcePosInfo) -> Option<()> {
        use EraBytecodePrimaryType::*;
        match u8::from(flags) {
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
        Some(())
    }
    // Precondition: selection on stack
    // Postcondition: composed string on stack
    fn cmd_printdata_core(
        &mut self,
        data: Vec<Vec<EraExpr>>,
        src_info: SourcePosInfo,
    ) -> Option<()> {
        use EraBytecodePrimaryType::*;
        let mut bt_data = Vec::new();
        // Jump fields
        for i in 0..data.len() {
            self.chunk.emit_bytecode(Duplicate, src_info);
            self.chunk
                .emit_load_const(self.p.new_value_int(i as _), src_info);
            self.chunk.emit_bytecode(CompareEq, src_info);
            bt_data.push(self.chunk.emit_jump_cond_hold(src_info));
        }
        // Data fields
        let mut bt_done = Vec::new();
        for (data_vec, bt) in data.into_iter().zip(bt_data.into_iter()) {
            bt.complete(self.chunk).unwrap();
            // HACK: Pop early
            self.chunk.emit_bytecode(Pop, src_info);
            let mut strings_cnt = 1;
            let mut data_it = data_vec.into_iter();
            let first_data = if let Some(data) = data_it.next() {
                data
            } else {
                EraExpr::new_str(String::new(), src_info)
            };
            self.expr_str(first_data)?;
            let newline_value = self.p.new_value_str("\n".to_owned());
            for rest_data in data_it {
                self.chunk.emit_load_const(newline_value.clone(), src_info);
                self.expr_str(rest_data)?;
                strings_cnt += 2;
            }
            self.chunk.emit_bytecode(BuildString, src_info);
            self.chunk.append_u8(strings_cnt, src_info);
            bt_done.push(self.chunk.emit_jump_hold(src_info));
        }
        for bt in bt_done {
            bt.complete(self.chunk).unwrap();
        }
        Some(())
    }
    // NOTE: This function is strengthened to prevent stack overflow when encountered
    //       too deep nesting of IF-ELSEIF's.
    fn cmd_if_stmt(&mut self, func_kind: EraFunKind, x: crate::parser::EraIfStmt) -> Option<()> {
        // TODO: Optimize when there is no else body
        self.expression(x.cond)?;
        let backtrack_then = self.chunk.emit_jump_cond_hold(x.src_info);
        match TryInto::<[_; 1]>::try_into(x.else_body) {
            Ok([stmt]) => {
                if let EraStmt::Command(EraCommandStmt::If(x)) = stmt {
                    // STRENGTHENED: Prevent stack overflow in debug builds.
                    self.cmd_if_stmt(func_kind, x)?;
                } else {
                    self.statement(func_kind, stmt)?;
                }
            }
            Err(else_body) => {
                for stmt in else_body {
                    self.statement(func_kind, stmt)?;
                }
            }
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
        if let Some(var) = self
            .func_info
            .local_var_idxs
            .get(CaselessStr::new(&var_name))
            .map(|&x| &self.func_info.local_vars[x])
        {
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
            } else if var_name.eq_ignore_ascii_case("CHARANUM") {
                return Some(Pseudo(EraPseudoVarKind::CharaNum));
            } else {
                let vars: Vec<_> = self
                    .func_info
                    .local_vars
                    .iter()
                    .map(|x| x.name.as_str())
                    .collect();
                let mut vars = vars.join(", ");
                if vars.is_empty() {
                    vars.push_str("<none>");
                }
                bail_opt!(
                    self,
                    [
                        (src_info, format!("undefined variable `{}`", var_name)),
                        (
                            self.func_info.src_info,
                            format!("note: available local variables are: {vars}")
                        )
                    ]
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
        let is_charadata;
        if let Some(var) = self
            .func_info
            .local_var_idxs
            .get(CaselessStr::new(&var_name))
            .map(|&x| &self.func_info.local_vars[x])
        {
            (var_kind, var_dims) = match var.init_val.clone().into_unpacked() {
                FlatValue::ArrInt(x) => (TInteger, SmallVec::from_slice(&x.borrow().dims)),
                FlatValue::ArrStr(x) => (TString, SmallVec::from_slice(&x.borrow().dims)),
                _ => unreachable!(),
            };
            is_in_global_frame = var.is_in_global_frame;
            var_idx = var.idx_in_frame;
            is_charadata = var.is_charadata;
        } else if let Some(glob_var_idx) = self.p.vars.get_var_idx(&var_name).or_else(|| {
            self.p
                .vars
                .get_var_idx(&format!("{}@{}", &var_name, self.func_info.name.as_str()))
        }) {
            let var_info = self.p.vars.get_var_info(glob_var_idx).unwrap();
            (var_kind, var_dims) = match var_info.val.clone().into_unpacked() {
                FlatValue::ArrInt(x) => (TInteger, SmallVec::from_slice(&x.borrow().dims)),
                FlatValue::ArrStr(x) => (TString, SmallVec::from_slice(&x.borrow().dims)),
                _ => unreachable!(),
            };
            is_in_global_frame = true;
            var_idx = glob_var_idx;
            is_charadata = var_info.is_charadata;
        } else {
            // HACK: For undefined identifiers, do special handling (such as RAND pesudo-array access)
            if var_name.eq_ignore_ascii_case("RAND") {
                return Some(Pseudo(EraPseudoVarKind::Rand));
            } else if var_name.eq_ignore_ascii_case("CHARANUM") {
                return Some(Pseudo(EraPseudoVarKind::CharaNum));
            } else {
                let vars: Vec<_> = self
                    .func_info
                    .local_vars
                    .iter()
                    .map(|x| x.name.as_str())
                    .collect();
                let mut vars = vars.join(", ");
                if vars.is_empty() {
                    vars.push_str("<none>");
                }
                bail_opt!(
                    self,
                    [
                        (src_info, format!("undefined variable `{}`", var_name)),
                        (
                            self.func_info.src_info,
                            format!("note: available local variables are: {vars}")
                        )
                    ]
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

        Some(Normal(var_kind, var_dims, is_charadata))
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
        mut idxs: Vec<EraExpr>,
        src_info: SourcePosInfo,
        fix_chara_idxs: bool,
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
                EraPseudoVarKind::CharaNum => {
                    if idxs.len() != 0 {
                        bail_opt!(self, src_info, true, "`CHARANUM` cannot be indexed");
                    }
                }
            },
            Normal(var_kind, var_dims, is_charadata) => {
                // TODO: Check array size & type
                if routine::is_chara_nodim(var_name) {
                    // HACK: Push implicit 0 as last index
                    idxs.push(EraExpr::new_int(0, src_info));
                }
                let mut idx_len = idxs.len();
                if idx_len > var_dims.len() {
                    bail_opt!(self, src_info, "too many indices into array");
                }
                if var_dims.len() > 1 && idx_len < var_dims.len() {
                    if is_charadata {
                        if fix_chara_idxs {
                            // Push implicit TARGET as first index, then try again
                            let target_var = EraVarExpr {
                                name: "TARGET".to_owned(),
                                idxs: vec![],
                                src_info,
                            };
                            idxs.insert(0, EraExpr::Term(EraTermExpr::Var(target_var)));
                        } else {
                            idxs.insert(0, EraExpr::new_int(0, src_info));
                        }
                        idx_len = idxs.len();
                        if idx_len != var_dims.len() {
                            //bail_opt!(self, src_info, "too few indices into CHARADATA variable");
                            self.report_err(
                                src_info,
                                true,
                                "too few indices into CHARADATA variable",
                            );
                            while idx_len != var_dims.len() {
                                // HACK: Push implicit 0 as last index to workaround bugs in game code
                                idxs.push(EraExpr::new_int(0, src_info));
                                idx_len = idxs.len();
                            }
                        } else {
                            if fix_chara_idxs {
                                // Maybe we should not report the omission of TARGET in CHARADATA variable?
                                // self.report_err(
                                //     src_info,
                                //     false,
                                //     "non-compliant use of CHARADATA variable",
                                // );
                            }
                        }
                    } else {
                        self.report_err(src_info, false, "non-compliant use of array variable");
                    }
                }
                let idx_len = idx_len;
                assert_eq!(idx_len, idxs.len());
                let is_csv_var = routine::is_csv_var(&var_name);

                // TODO: Optimize access for 0d / 1d arrays

                for idx in idxs {
                    let idx_kind = match idx {
                        // HACK: Contextual indices replacing
                        EraExpr::Term(EraTermExpr::Var(x))
                            if x.idxs.is_empty()
                                && is_csv_var
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
                        TString if is_csv_var => {
                            // HACK: Dynamic CSV string to index translation
                            self.report_err(
                                src_info,
                                false,
                                "using strings as array indices is discouraged",
                            );
                            self.chunk.emit_bytecode(CsvGetNum, src_info);
                        }
                        _ => bail_opt!(self, src_info, true, "array indices must be integers"),
                    }
                }

                self.chunk.emit_bytecode(BuildArrayIndexFromMD, src_info);
                self.chunk.append_u8(idx_len as _, src_info);
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
        let Normal(var_kind, var_dims_cnt) =
            self.arr_idx_or_pseudo(var_name, idxs, src_info, true)?
        else {
            bail_opt!(self, src_info, true, "invalid access to pesudo variable");
        };
        Some(var_kind)
    }
    #[must_use]
    fn arr_idx_no_chara_fix(
        &mut self,
        var_name: &str,
        idxs: Vec<EraExpr>,
        src_info: SourcePosInfo,
    ) -> Option<EraExpressionValueKind> {
        use EraVarArrCompileInfo::*;
        let Normal(var_kind, var_dims_cnt) =
            self.arr_idx_or_pseudo(var_name, idxs, src_info, false)?
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
        let info = self.arr_idx_or_pseudo(var_name, idxs, src_info, true)?;
        let var_kind = match info {
            Pseudo(kind) => match kind {
                EraPseudoVarKind::Rand => {
                    self.chunk.emit_bytecode(GetRandomMax, src_info);
                    TInteger
                }
                EraPseudoVarKind::CharaNum => {
                    self.chunk.emit_bytecode(GetCharaNum, src_info);
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
    ) -> Option<(EraExpressionValueKind, u8)> {
        use EraVarArrCompileInfo::*;
        let Normal(kind, dims_cnt) = self.var_arr(var_name, src_info)? else {
            bail_opt!(self, src_info, true, "invalid access to pesudo variable");
        };
        Some((kind, dims_cnt))
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
    fn var_arr_int(&mut self, var_name: &str, src_info: SourcePosInfo) -> Option<u8> {
        use EraExpressionValueKind::*;
        let (TInteger, dims_cnt) = self.var_arr_no_pseudo(var_name, src_info)? else {
            bail_opt!(self, src_info, "expected an integer variable");
        };
        Some(dims_cnt)
    }
    #[must_use]
    fn var_arr_str(&mut self, var_name: &str, src_info: SourcePosInfo) -> Option<u8> {
        use EraExpressionValueKind::*;
        let (TString, dims_cnt) = self.var_arr_no_pseudo(var_name, src_info)? else {
            bail_opt!(self, src_info, "expected a string variable");
        };
        Some(dims_cnt)
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
    #[must_use]
    fn arr_idx_int_ve(&mut self, var_expr: EraVarExpr) -> Option<()> {
        self.arr_idx_int(&var_expr.name, var_expr.idxs, var_expr.src_info)
    }
    #[must_use]
    fn arr_idx_str_ve(&mut self, var_expr: EraVarExpr) -> Option<()> {
        self.arr_idx_str(&var_expr.name, var_expr.idxs, var_expr.src_info)
    }
    #[must_use]
    fn arr_get_int(
        &mut self,
        var_name: &str,
        idxs: Vec<EraExpr>,
        src_info: SourcePosInfo,
    ) -> Option<()> {
        use EraExpressionValueKind::*;
        let TInteger = self.arr_get(var_name, idxs, src_info)? else {
            bail_opt!(self, src_info, "expected an integer variable");
        };
        Some(())
    }
    #[must_use]
    fn arr_get_str(
        &mut self,
        var_name: &str,
        idxs: Vec<EraExpr>,
        src_info: SourcePosInfo,
    ) -> Option<()> {
        use EraExpressionValueKind::*;
        let TString = self.arr_get(var_name, idxs, src_info)? else {
            bail_opt!(self, src_info, "expected a string variable");
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
    fn unpack_var_expr_from_expr(&mut self, expr: EraExpr) -> Option<EraVarExpr> {
        let EraExpr::Term(EraTermExpr::Var(var_expr)) = expr else {
            bail_opt!(
                self,
                expr.source_pos_info(),
                true,
                "expected a variable expression"
            );
        };
        Some(var_expr)
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

fn default_expr(expr_kind: EraExpressionValueKind, src_info: SourcePosInfo) -> EraExpr {
    use EraExpressionValueKind::*;
    match expr_kind {
        TString => EraExpr::new_str(String::new(), src_info),
        TInteger => EraExpr::new_int(0, src_info),
        TVoid => panic!("no default EraExpr for void type"),
    }
}
