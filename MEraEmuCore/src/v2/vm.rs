use std::{
    cell::RefCell,
    num::NonZeroUsize,
    ops::{ControlFlow, Deref, DerefMut},
    sync::atomic::AtomicBool,
};

use anyhow::Context;
use cstree::interning::{InternKey, Resolver, TokenKey};
use hashbrown::HashMap;
use rclite::Rc;
use rustc_hash::FxBuildHasher;
use serde::{Deserialize, Serialize};

use crate::{
    types::*,
    util::{
        number::formatting::csharp_format_i64,
        random::SimpleUniformGenerator,
        rcstr::{self, ArcStr},
        Ascii,
    },
    v2::{
        intrinsics::{self, *},
        routines,
    },
};

type FxHashMap<K, V> = HashMap<K, V, FxBuildHasher>;

#[derive(Debug)]
pub struct TailVecRef<'a, T> {
    vec: &'a mut Vec<T>,
    start: usize,
}

impl<'a, T> TailVecRef<'a, T> {
    pub fn new(vec: &'a mut Vec<T>, start: usize) -> Self {
        assert!(start <= vec.len(), "start out of bounds");
        Self { vec, start }
    }
}

impl<'a, T> std::ops::Deref for TailVecRef<'a, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.vec[self.start..]
    }
}

impl<'a, T> std::ops::DerefMut for TailVecRef<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vec[self.start..]
    }
}

impl<T> TailVecRef<'_, T> {
    pub fn as_slice(&self) -> &[T] {
        &self.vec[self.start..]
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        &mut self.vec[self.start..]
    }

    pub fn push(&mut self, value: T) {
        self.vec.push(value);
    }

    pub fn len(&self) -> usize {
        self.vec.len() - self.start
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.vec.len() > self.start {
            self.vec.pop()
        } else {
            None
        }
    }

    pub fn drain<R>(&mut self, range: R) -> std::vec::Drain<'_, T>
    where
        R: std::ops::RangeBounds<usize>,
    {
        let range = self.normalize_range(range);
        let offset = self.start;
        self.vec.drain(range.start + offset..range.end + offset)
    }

    pub fn splice<R, I>(&mut self, range: R, replace_with: I) -> std::vec::Splice<'_, I::IntoIter>
    where
        R: std::ops::RangeBounds<usize>,
        I: IntoIterator<Item = T>,
    {
        let range = self.normalize_range(range);
        let offset = self.start;
        self.vec
            .splice(range.start + offset..range.end + offset, replace_with)
    }

    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        self.vec.extend(iter);
    }

    pub fn get_view_mut<R>(&mut self, range: R) -> VecViewMut<T>
    where
        R: std::ops::RangeBounds<usize>,
    {
        let range = self.normalize_range(range);
        let offset = self.start;
        VecViewMut::new(self.vec, range.start + offset, range.end + offset)
    }

    pub fn get_sized_view_mut<const LEN: usize>(
        &mut self,
        start: usize,
    ) -> VecSizedViewMut<T, LEN> {
        let offset = self.start;
        VecSizedViewMut::new(self.vec, start + offset)
    }

    pub fn get_tail_sized_view_mut<const LEN: usize>(&mut self) -> VecSizedViewMut<T, LEN> {
        if LEN > self.len() {
            panic!("tail view too short");
        }
        VecSizedViewMut::new(self.vec, self.vec.len() - LEN)
    }

    pub fn get_tail_sized_view_mut_checked<const LEN: usize>(
        &mut self,
    ) -> Option<VecSizedViewMut<T, LEN>> {
        if LEN > self.len() {
            None
        } else {
            Some(VecSizedViewMut::new(self.vec, self.vec.len() - LEN))
        }
    }

    pub fn inner(&self) -> &Vec<T> {
        self.vec
    }

    pub fn inner_mut(&mut self) -> &mut Vec<T> {
        self.vec
    }

    fn normalize_range<R>(&self, range: R) -> std::ops::Range<usize>
    where
        R: std::ops::RangeBounds<usize>,
    {
        let len = self.len();
        let start = match range.start_bound() {
            std::ops::Bound::Included(&x) => x,
            std::ops::Bound::Excluded(&x) => x + 1,
            std::ops::Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            std::ops::Bound::Included(&x) => x + 1,
            std::ops::Bound::Excluded(&x) => x,
            std::ops::Bound::Unbounded => len,
        };
        start..end
    }
}

impl<T: Clone> TailVecRef<'_, T> {
    pub fn extend_from_within<R>(&mut self, src: R)
    where
        R: std::ops::RangeBounds<usize>,
    {
        let src = self.normalize_range(src);
        let offset = self.start;
        self.vec
            .extend_from_within(src.start + offset..src.end + offset);
    }
}

pub struct VecViewMut<'a, T> {
    vec: &'a mut Vec<T>,
    start: usize,
    end: usize,
}

impl<'a, T> VecViewMut<'a, T> {
    pub fn new(vec: &'a mut Vec<T>, start: usize, end: usize) -> Self {
        assert!(start <= end, "start > end");
        assert!(end <= vec.len(), "end out of bounds");
        Self { vec, start, end }
    }

    pub fn as_slice(&self) -> &[T] {
        &self.vec[self.start..self.end]
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        &mut self.vec[self.start..self.end]
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn replace(self, replace_with: impl IntoIterator<Item = T>) {
        self.vec.splice(self.start..self.end, replace_with);
    }
}

impl<T> std::ops::Deref for VecViewMut<'_, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.vec[self.start..self.end]
    }
}

impl<T> std::ops::DerefMut for VecViewMut<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vec[self.start..self.end]
    }
}

struct VecSizedViewMut<'a, T, const LEN: usize> {
    vec: &'a mut Vec<T>,
    start: usize,
}

impl<'a, T, const LEN: usize> VecSizedViewMut<'a, T, LEN> {
    pub fn new(vec: &'a mut Vec<T>, start: usize) -> Self {
        assert!(start + LEN <= vec.len(), "end out of bounds");
        Self { vec, start }
    }

    pub fn as_slice(&self) -> &[T] {
        &self.vec[self.start..self.start + LEN]
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        &mut self.vec[self.start..self.start + LEN]
    }

    pub fn as_ref(&self) -> &[T; LEN] {
        self
    }

    pub fn as_mut(&mut self) -> &mut [T; LEN] {
        self
    }

    pub fn len(&self) -> usize {
        LEN
    }

    pub const fn is_empty(&self) -> bool {
        LEN == 0
    }

    pub fn replace(self, replace_with: impl IntoIterator<Item = T>) {
        self.vec.splice(self.start..self.start + LEN, replace_with);
    }
}

impl<T, const LEN: usize> std::ops::Deref for VecSizedViewMut<'_, T, LEN> {
    type Target = [T; LEN];

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self.vec.as_ptr().add(self.start) as *const [T; LEN]) }
    }
}

impl<T, const LEN: usize> std::ops::DerefMut for VecSizedViewMut<'_, T, LEN> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self.vec.as_mut_ptr().add(self.start) as *mut [T; LEN]) }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub struct EraExecIp {
    pub chunk: u32,
    pub offset: u32,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct EraFuncExecFrame {
    pub stack_start: u32,
    pub ip: EraExecIp,
    pub ret_ip: EraExecIp,
    pub ignore_return_value: bool,
    pub is_transient: bool,
}

#[derive(Debug)]
pub struct EraVirtualMachineState {
    stack: Vec<Value>,
    frames: Vec<EraFuncExecFrame>,
    inner: EraVirtualMachineStateInner,
}

impl EraVirtualMachineState {
    /// Resets the execution state and instruction pointer. Global variables remain unchanged.
    pub fn reset_exec_to_ip(&mut self, ip: EraExecIp) {
        self.stack.clear();
        self.stack.push(Value::new_int(0)); // Stub value
        self.frames.clear();
        self.frames.push(EraFuncExecFrame {
            stack_start: self.stack.len() as _,
            ip,
            ret_ip: EraExecIp {
                chunk: 0,
                offset: 0,
            },
            ignore_return_value: true,
            is_transient: false,
        });
        self.inner.rand_gen = SimpleUniformGenerator::new();
    }

    pub fn get_exec_frames(&self) -> &[EraFuncExecFrame] {
        &self.frames[..]
    }

    pub fn stack_len(&self) -> usize {
        self.stack.len()
    }

    pub fn get_stack_value(&self, idx: usize) -> Option<&Value> {
        self.stack.get(idx)
    }
}

#[derive(Debug)]
pub struct EraVirtualMachineStateInner {
    rand_gen: SimpleUniformGenerator,
    regex_cache: lru::LruCache<ArcStr, regex::Regex>,
    // var addr -> var index
    trap_vars: FxHashMap<*const (), u32>,
    charas_count: u32,
}

// TODO: Custom serde support for EraVirtualMachineState which involves
//       the compiler context.

// TODO: To support moving pc after hot reloading, we need to stringify
//       the old and new function bytecodes, and text diff them to find
//       the new pc. This is a bit hacky, but should be better than diffing
//       the binary bytecode directly.

impl Default for EraVirtualMachineState {
    fn default() -> Self {
        Self::new()
    }
}

impl EraVirtualMachineState {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            frames: Vec::new(),
            inner: EraVirtualMachineStateInner {
                rand_gen: SimpleUniformGenerator::new(),
                regex_cache: lru::LruCache::new(NonZeroUsize::new(15).unwrap()),
                trap_vars: FxHashMap::default(),
                charas_count: 0,
            },
        }
    }
}

pub struct EraVirtualMachine<'ctx, 'i, 's, Callback> {
    ctx: &'ctx mut EraCompilerCtx<'i, Callback>,
    state: &'s mut EraVirtualMachineState,
}

impl<'ctx, 'i, 's, Callback: EraCompilerCallback> EraVirtualMachine<'ctx, 'i, 's, Callback> {
    pub fn new(
        ctx: &'ctx mut EraCompilerCtx<'i, Callback>,
        state: &'s mut EraVirtualMachineState,
    ) -> Self {
        Self { ctx, state }
    }

    /// Returns Some(()) if the trap variable was successfully added.
    pub fn add_trap_var(&mut self, name: &str) -> Option<()> {
        let Some(var_idx) = self.ctx.variables.get_var_idx(name) else {
            return None;
        };
        let var = self.ctx.variables.get_var_by_idx(var_idx).unwrap();
        let addr = match var.as_unpacked() {
            RefFlatValue::Int(_) | RefFlatValue::Str(_) => return None,
            RefFlatValue::ArrInt(x) => {
                x.borrow_mut().flags.set_is_trap(true);
                Rc::as_ptr(x) as *const ()
            }
            RefFlatValue::ArrStr(x) => {
                x.borrow_mut().flags.set_is_trap(true);
                Rc::as_ptr(x) as *const ()
            }
        };
        self.state.inner.trap_vars.insert(addr, var_idx as u32);
        Some(())
    }

    pub fn state_mut(&mut self) -> &mut EraVirtualMachineState {
        self.state
    }

    pub fn execute(&mut self, run_flag: &AtomicBool, max_inst_cnt: u64) -> EraExecutionBreakReason {
        match self.execute_inner(run_flag, max_inst_cnt) {
            Ok(reason) => reason,
            Err(err) => {
                let mut diag = Diagnostic::new();
                if let Some(ip) = self.state.frames.last().map(|f| f.ip) {
                    let chunk = ip.chunk;
                    let chunk = self.ctx.bc_chunks.get(chunk as usize).unwrap();
                    diag.span_err(
                        chunk.name.clone(),
                        chunk.lookup_src(ip.offset as usize).unwrap_or_default(),
                        err.to_string(),
                    );
                } else {
                    diag.span_err(
                        rcstr::literal!("<builtin>"),
                        SrcSpan::new(SrcPos(0), 0),
                        err.to_string(),
                    );
                }
                self.ctx.emit_diag(diag);
                EraExecutionBreakReason::InternalError
            }
        }
    }
}

pub struct EraVmExecSite<'ctx, 'i, 's, Callback> {
    ctx: &'ctx mut EraCompilerCtx<'i, Callback>,
    i: &'s mut EraVirtualMachineStateInner,
    stack: TailVecRef<'s, Value>,
    cur_chunk: &'ctx EraBcChunk,
    cur_frame: &'s mut EraFuncExecFrame,
    prev_frames: &'s [EraFuncExecFrame],
}

impl<Callback> Deref for EraVmExecSite<'_, '_, '_, Callback> {
    type Target = EraVirtualMachineStateInner;

    fn deref(&self) -> &Self::Target {
        self.i
    }
}

impl<Callback> DerefMut for EraVmExecSite<'_, '_, '_, Callback> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.i
    }
}

impl<Callback: EraCompilerCallback> EraVmExecSite<'_, '_, '_, Callback> {
    pub fn cur_filename(&self) -> ArcStr {
        self.cur_chunk.name.clone()
    }

    pub fn cur_bc_span(&self) -> SrcSpan {
        self.cur_chunk
            .lookup_src(self.cur_frame.ip.offset as usize)
            .unwrap_or_default()
    }

    pub fn span_msg_here(
        &self,
        diag: &mut Diagnostic,
        level: DiagnosticLevel,
        message: impl Into<String>,
    ) {
        let filename = self.cur_filename();
        let span = self.cur_bc_span();
        diag.span_msg(filename, span, level, message);
    }

    pub fn span_err_here(&self, diag: &mut Diagnostic, message: impl Into<String>) {
        self.span_msg_here(diag, DiagnosticLevel::Error, message);
    }

    pub fn span_warn_here(&self, diag: &mut Diagnostic, message: impl Into<String>) {
        self.span_msg_here(diag, DiagnosticLevel::Warning, message);
    }

    pub fn span_note_here(&self, diag: &mut Diagnostic, message: impl Into<String>) {
        self.span_msg_here(diag, DiagnosticLevel::Note, message);
    }
}

impl<'i, Callback: EraCompilerCallback> EraVmExecSite<'_, 'i, '_, Callback> {
    /// Retrieves the current function being executed.
    pub fn get_current_function(&self) -> Option<&EraFuncInfo<'i>> {
        let cur_ip = self.cur_frame.ip;
        self.ctx
            .func_info_from_chunk_pos(cur_ip.chunk as _, cur_ip.offset as _)
    }

    /// Retrieves a variable in the current execution scope.
    pub fn get_var_by_name(&self, name: &str) -> Option<&Value> {
        let cur_func = self.get_current_function()?;
        if let Some(var) = cur_func.frame_info.vars.get(Ascii::new_str(name)) {
            let idx = var.var_idx as usize;
            if var.in_local_frame {
                self.stack.get(idx)
            } else {
                self.ctx.variables.get_var_by_idx(idx)
            }
        } else if let Some(var) = self.ctx.variables.get_var(name) {
            Some(var)
        } else {
            None
        }
    }

    pub fn get_global_var_int(&self, name: &str) -> anyhow::Result<&Rc<RefCell<ArrIntValue>>> {
        let var = self
            .ctx
            .variables
            .get_var(name)
            .with_context(|| format!("variable `{}` not found", name))?;
        var.as_arrint()
            .with_context(|| format!("variable `{}` is not an array of integers", name))
    }

    pub fn get_global_var_str(&self, name: &str) -> anyhow::Result<&Rc<RefCell<ArrStrValue>>> {
        let var = self
            .ctx
            .variables
            .get_var(name)
            .with_context(|| format!("variable `{}` not found", name))?;
        var.as_arrstr()
            .with_context(|| format!("variable `{}` is not an array of strings", name))
    }

    pub fn get_variable_name_from_ptr(&self, value: *const ()) -> Option<&str> {
        self.ctx.variables.iter().find_map(|v| {
            let addr = match v.val.as_unpacked() {
                RefFlatValue::Int(_) | RefFlatValue::Str(_) => return None,
                RefFlatValue::ArrInt(x) => Rc::as_ptr(x) as *const (),
                RefFlatValue::ArrStr(x) => Rc::as_ptr(x) as *const (),
            };
            (addr == value).then(|| v.name.as_ref())
        })
    }

    pub fn get_variable_name_from_value(&self, value: &Value) -> Option<&str> {
        let var_addr = match value.as_unpacked() {
            RefFlatValue::Int(_) | RefFlatValue::Str(_) => return None,
            RefFlatValue::ArrInt(x) => Rc::as_ptr(x) as *const (),
            RefFlatValue::ArrStr(x) => Rc::as_ptr(x) as *const (),
        };
        self.get_variable_name_from_ptr(var_addr)
        // let var_addr = match value.as_unpacked() {
        //     RefFlatValue::Int(_) | RefFlatValue::Str(_) => return None,
        //     RefFlatValue::ArrInt(x) => Rc::as_ptr(x) as *const (),
        //     RefFlatValue::ArrStr(x) => Rc::as_ptr(x) as *const (),
        // };
        // self.ctx.variables.iter().find_map(|v| {
        //     let addr = match v.val.as_unpacked() {
        //         RefFlatValue::Int(_) | RefFlatValue::Str(_) => return None,
        //         RefFlatValue::ArrInt(x) => Rc::as_ptr(x) as *const (),
        //         RefFlatValue::ArrStr(x) => Rc::as_ptr(x) as *const (),
        //     };
        //     (addr == var_addr).then(|| v.name.as_ref())
        // })
    }
}

const MAX_CALL_DEPTH: usize = 1024;

impl<'ctx, 'i, 's, Callback: EraCompilerCallback> EraVirtualMachine<'ctx, 'i, 's, Callback> {
    fn execute_inner(
        &mut self,
        run_flag: &AtomicBool,
        max_inst_cnt: u64,
    ) -> anyhow::Result<EraExecutionBreakReason> {
        use EraBytecodeKind as Bc;

        let mut bc_chunks;
        let mut s;

        macro_rules! make_site {
            () => {{
                let (cur_frame, prev_frames) = self
                    .state
                    .frames
                    .split_last_mut()
                    .context("no function to execute")?;
                let chunk = cur_frame.ip.chunk;
                bc_chunks = Rc::clone(&self.ctx.bc_chunks);
                let cur_chunk = bc_chunks.get(chunk as usize).unwrap();
                s = EraVmExecSite {
                    ctx: self.ctx,
                    i: &mut self.state.inner,
                    stack: TailVecRef::new(&mut self.state.stack, cur_frame.stack_start as _),
                    cur_chunk,
                    cur_frame,
                    prev_frames,
                };
            }};
        }
        macro_rules! drop_site {
            () => {
                drop(s);
                drop(bc_chunks);
            };
        }
        macro_rules! view_stack {
            () => {
                s.stack.get_tail_sized_view_mut_checked()
            };
        }
        macro_rules! view_stack_inline {
            () => {
                s.stack.get_tail_sized_view_mut_checked().as_deref_mut()
            };
        }
        macro_rules! view_stack_or_bail {
            () => {
                s.stack
                    .get_tail_sized_view_mut_checked()
                    .context("function stack underflow")?
            };
        }
        macro_rules! ensure_stack_len {
            ($len:expr) => {
                if s.stack.len() < $len {
                    let mut diag = Diagnostic::new();
                    diag.span_err(
                        s.cur_filename(),
                        s.cur_bc_span(),
                        "function stack underflow",
                    );
                    s.ctx.emit_diag(diag);
                    break EraExecutionBreakReason::InternalError;
                }
            };
        }
        macro_rules! do_intrinsic {
            ($intrin_kind:expr) => {
                let intrinsic = get_global_intrinsic($intrin_kind).unwrap();
                if let Err(e) = intrinsic.invoke_and_consume(&mut s.stack) {
                    match e {
                        EraIntrinsicError::IllegalArguments(e) => {
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), e.to_string());
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                        EraIntrinsicError::GeneralError(e) => return Err(e),
                    }
                }
            };
        }

        let Some(FlatValue::ArrInt(var_result)) = self
            .ctx
            .variables
            .get_var("RESULT")
            .map(|v| v.clone().into_unpacked())
        else {
            anyhow::bail!("variable `RESULT` not found or not an array of integers");
        };
        let Some(FlatValue::ArrStr(var_results)) = self
            .ctx
            .variables
            .get_var("RESULTS")
            .map(|v| v.clone().into_unpacked())
        else {
            anyhow::bail!("variable `RESULTS` not found or not an array of strings");
        };

        make_site!();

        let mut i = 0;
        let reason = 'vm: loop {
            if !run_flag.load(std::sync::atomic::Ordering::Relaxed) {
                break EraExecutionBreakReason::StopFlag;
            }
            if i >= max_inst_cnt {
                break EraExecutionBreakReason::ReachedMaxInstructions;
            }
            i += 1;

            let Some(bc_area) = s.cur_chunk.get_bc().get(s.cur_frame.ip.offset as usize..) else {
                let mut diag = Diagnostic::new();
                diag.span_err(
                    s.cur_chunk.name.clone(),
                    s.cur_chunk
                        .lookup_src(s.cur_chunk.len() - 1)
                        .unwrap_or_default(),
                    "unexpected end of bytecode",
                );
                s.ctx.emit_diag(diag);
                break EraExecutionBreakReason::IllegalInstruction;
            };
            // let bc_span = s
            //     .cur_chunk
            //     .lookup_src(s.cur_frame.ip.offset as usize)
            //     .unwrap_or_default();
            let Some((inst, inst_len)) = EraBytecodeKind::with_len_from_bytes(bc_area) else {
                let mut diag = Diagnostic::new();
                diag.span_err(
                    s.cur_filename(),
                    s.cur_bc_span(),
                    "unexpected end of bytecode",
                );
                s.ctx.emit_diag(diag);
                break EraExecutionBreakReason::IllegalInstruction;
            };
            let mut ip_delta = inst_len as i32;

            // dbg!(&s.stack);
            // dbg!((s.cur_frame.ip.offset, inst));

            macro_rules! unpack_one_arg {
                ($var:ident:i) => {
                    let $var = match $var.as_unpacked() {
                        RefFlatValue::Int(x) => x.val,
                        v => {
                            let msg = format!("expected an integer, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                };
                ($var:ident:s) => {
                    let $var = match $var.as_unpacked() {
                        RefFlatValue::Str(x) => &x.val,
                        v => {
                            let msg = format!("expected a string, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                };
                ($var:ident:vi) => {
                    let $var = match $var.as_unpacked() {
                        RefFlatValue::ArrInt(x) => x,
                        v => {
                            let msg = format!("expected an integer variable, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                };
                ($var:ident:vs) => {
                    let $var = match $var.as_unpacked() {
                        RefFlatValue::ArrStr(x) => x,
                        v => {
                            let msg = format!("expected a string variable, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                };
                ($var:ident:b) => {
                    let $var = match $var.as_unpacked() {
                        RefFlatValue::Int(x) => x.val != 0,
                        v => {
                            let msg = format!("expected an boolean, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                };
            }
            macro_rules! unpack_args {
                ($($var:ident:$kind:ident),* $(,)?) => {
                    $(unpack_one_arg!($var:$kind);)*
                };
            }
            macro_rules! unpack_one_arg_mut {
                ($var:ident:i) => {
                    let $var = match $var.as_unpacked_mut() {
                        RefFlatValueMut::Int(x) => x,
                        v => {
                            let msg = format!("expected an integer, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                };
                ($var:ident:s) => {
                    let $var = match $var.as_unpacked_mut() {
                        RefFlatValueMut::Str(x) => x,
                        v => {
                            let msg = format!("expected a string, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                };
                ($var:ident:vi) => {
                    let $var = match $var.as_unpacked_mut() {
                        RefFlatValueMut::ArrInt(x) => x,
                        v => {
                            let msg = format!("expected an integer variable, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                };
                ($var:ident:vs) => {
                    let $var = match $var.as_unpacked_mut() {
                        RefFlatValueMut::ArrStr(x) => x,
                        v => {
                            let msg = format!("expected a string variable, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                };
            }
            macro_rules! unpack_args_mut {
                ($($var:ident:$kind:ident),* $(,)?) => {
                    $(unpack_one_arg_mut!($var:$kind);)*
                };
            }
            macro_rules! value_into {
                ($($var:ident),* $(,)?) => {
                    $(let Some($var) = $var.value_into() else {
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), "ValueInto::value_into failed");
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };)*
                }
            }

            // Execute instruction
            // NOTE: Execution of instructions is atomic: they are either fully executed or not at all.
            //       This is important for the stack to remain consistent.
            match inst {
                Bc::FailWithMsg => {
                    let msg = if let Some([msg]) = view_stack_inline!() {
                        match msg.as_unpacked() {
                            RefFlatValue::Int(x) => x.val.to_string(),
                            RefFlatValue::Str(x) => x.val.as_str().to_string(),
                            RefFlatValue::ArrInt(_) => "array of integers".to_string(),
                            RefFlatValue::ArrStr(_) => "array of strings".to_string(),
                        }
                    } else {
                        "<invalid>".to_string()
                    };
                    let mut diag = Diagnostic::new();
                    diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                    s.ctx.emit_diag(diag);
                    break EraExecutionBreakReason::FailInstruction;
                }
                Bc::DebugBreak => {
                    break EraExecutionBreakReason::DebugBreakInstruction;
                }
                Bc::Quit => {
                    break EraExecutionBreakReason::CodeQuit;
                }
                Bc::Throw => {
                    let err = if let Some([err]) = view_stack_inline!() {
                        match err.as_unpacked() {
                            RefFlatValue::Int(x) => x.val.to_string(),
                            RefFlatValue::Str(x) => x.val.to_string(),
                            RefFlatValue::ArrInt(_) => "array of integers".to_string(),
                            RefFlatValue::ArrStr(_) => "array of strings".to_string(),
                        }
                    } else {
                        "<invalid>".to_string()
                    };
                    return Err(anyhow::anyhow!("throw: {}", err));
                }
                Bc::Nop => {}
                Bc::ReturnVoid => {
                    drop_site!();
                    let frame = self.state.frames.pop().unwrap();
                    if !frame.is_transient {
                        self.state.stack.truncate(frame.stack_start as usize);
                    }
                    make_site!();
                    s.cur_frame.ip = frame.ret_ip;
                    ip_delta = 0;
                }
                Bc::ReturnInt | Bc::ReturnStr => {
                    let sview = view_stack_or_bail!();
                    let [val] = sview.as_ref();
                    let val = val.clone();
                    drop_site!();
                    let frame = self.state.frames.pop().unwrap();
                    if !frame.is_transient {
                        self.state.stack.truncate(frame.stack_start as usize);
                    }
                    make_site!();
                    s.cur_frame.ip = frame.ret_ip;
                    ip_delta = 0;
                    if !frame.ignore_return_value && !frame.is_transient {
                        s.stack.push(val);
                    }
                }
                Bc::CallFun { args_cnt } => {
                    // NOTE: Function index must be Int, not Str (i.e. no dynamic function calls).
                    let args_cnt: usize = args_cnt.into();
                    let slen = s.stack.len();
                    ensure_stack_len!(args_cnt + 1);
                    let sview = s.stack.get_view_mut(slen - args_cnt - 1..);
                    let func_idx = match sview.last().unwrap().as_unpacked() {
                        RefFlatValue::Int(x) => x.val as usize,
                        v => {
                            let msg = format!("expected an integer function index, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                    let Some((func_name, func_info)) = s
                        .ctx
                        .func_entries
                        .get_index(func_idx)
                        .and_then(|(&k, v)| v.as_ref().map(|v| (k, v)))
                    else {
                        let msg = format!("function {} not found", func_idx);
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    let new_ip = EraExecIp {
                        chunk: func_info.chunk_idx,
                        offset: func_info.bc_offset,
                    };
                    let ret_ip = EraExecIp {
                        chunk: s.cur_frame.ip.chunk,
                        offset: s.cur_frame.ip.offset + ip_delta as u32,
                    };
                    // Verify that the function has the correct number of arguments.
                    // TODO: Perform full type checking for arguments.
                    let params_cnt = func_info.frame_info.args.len();
                    if args_cnt != params_cnt {
                        let msg = format!(
                            "function `{}` expects {} arguments, but got {}. Broken codegen?",
                            func_name, params_cnt, args_cnt
                        );
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    }

                    // Check call stack depth
                    if s.prev_frames.len() + 1 >= MAX_CALL_DEPTH {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            "maximum call depth exceeded",
                        );
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::InternalError;
                    }

                    // Now prepare arguments for the function
                    // WARN: The following procedure must not fail, or stack will be corrupted.
                    s.stack.pop();

                    // Create a new execution frame for the function
                    let is_transient = func_info.is_transient;
                    let stack_start = if is_transient {
                        s.prev_frames.last().map_or(0, |f| f.stack_start as usize)
                    } else {
                        s.stack.inner().len() - args_cnt
                    };
                    drop_site!();
                    self.state.frames.push(EraFuncExecFrame {
                        stack_start: stack_start as _,
                        ip: new_ip,
                        ret_ip,
                        ignore_return_value: false,
                        is_transient,
                    });
                    ip_delta = 0;
                    make_site!();
                }
                Bc::TryCallFun { args_cnt } | Bc::TryCallFunForce { args_cnt } => {
                    let is_force = matches!(inst, Bc::TryCallFunForce { .. });

                    let args_cnt: usize = args_cnt.into();
                    let slen = s.stack.len();
                    ensure_stack_len!(args_cnt * 2 + 1);
                    let mut processed_args = Vec::with_capacity(args_cnt);
                    let sview = s.stack.get_view_mut(slen - args_cnt * 2 - 1..);
                    let lookup_result = match sview.last().unwrap().as_unpacked() {
                        RefFlatValue::Int(x) => {
                            // Find function by index
                            let idx = x.val as usize;
                            s.ctx
                                .func_entries
                                .get_index(idx)
                                .and_then(|(&k, v)| v.as_ref().map(|v| (k, v)))
                        }
                        RefFlatValue::Str(x) => {
                            // Find function by name
                            let idx = x.val.as_str();
                            s.ctx
                                .func_entries
                                .get_full(Ascii::new_str(idx))
                                .and_then(|(_, &k, v)| v.as_ref().map(|v| (k, v)))
                        }
                        v => {
                            let msg = format!("expected a function index, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                    if let Some((func_name, func_info)) = lookup_result {
                        // Function exists, check function argument pack
                        #[derive(Debug)]
                        enum ArgPackKind<'a> {
                            Empty,
                            Scalar(&'a Value),
                            ArrIdx(&'a Value, usize),
                        }

                        fn parse_arg_pack<'a>(
                            sview: &'a [Value],
                            idx: usize,
                        ) -> anyhow::Result<ArgPackKind<'a>> {
                            let Some(arg_pack) = sview.chunks_exact(2).nth(idx) else {
                                return Ok(ArgPackKind::Empty);
                            };
                            match (arg_pack[0].as_unpacked(), arg_pack[1].as_unpacked()) {
                                (
                                    RefFlatValue::ArrInt(_) | RefFlatValue::ArrStr(_),
                                    RefFlatValue::Int(idx),
                                ) => {
                                    if idx.val < 0 {
                                        return Err(anyhow::anyhow!(
                                            "array index {} is negative",
                                            idx.val
                                        ));
                                    }
                                    Ok(ArgPackKind::ArrIdx(&arg_pack[0], idx.val as _))
                                }
                                (
                                    RefFlatValue::Int(_) | RefFlatValue::Str(_),
                                    RefFlatValue::Int(tag),
                                ) => {
                                    if tag.val == 1 {
                                        Ok(ArgPackKind::Empty)
                                    } else {
                                        Ok(ArgPackKind::Scalar(&arg_pack[0]))
                                    }
                                }
                                _ => Err(anyhow::anyhow!(
                                    "invalid argument pack (got {:?}, {:?})",
                                    arg_pack[0],
                                    arg_pack[1]
                                )),
                            }
                        }

                        // Process arguments
                        for (i, param) in func_info.frame_info.args.iter().enumerate() {
                            // TODO: Check dimensionality of arrays

                            let arg_pack = parse_arg_pack(&sview, i)?;
                            // For exposition (error reporting)
                            let i = i + 1;
                            match param.var_kind {
                                ValueKind::ArrInt => {
                                    let ArgPackKind::ArrIdx(arr, idx) = arg_pack else {
                                        let msg = format!(
                                            "expected an array argument for parameter {}, got {:?}",
                                            i, arg_pack
                                        );
                                        let mut diag = Diagnostic::new();
                                        diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                                        s.ctx.emit_diag(diag);
                                        break 'vm EraExecutionBreakReason::IllegalArguments;
                                    };
                                    if arr.as_arrint().is_none() {
                                        let msg = format!(
                                            "expected an array of integers for parameter {}, got {:?}",
                                            i, arr
                                        );
                                        let mut diag = Diagnostic::new();
                                        diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                                        s.ctx.emit_diag(diag);
                                        break 'vm EraExecutionBreakReason::IllegalArguments;
                                    }
                                    processed_args.push(arr.clone());
                                }
                                ValueKind::ArrStr => {
                                    let ArgPackKind::ArrIdx(arr, idx) = arg_pack else {
                                        let msg = format!(
                                            "expected an array argument for parameter {}, got {:?}",
                                            i, arg_pack
                                        );
                                        let mut diag = Diagnostic::new();
                                        diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                                        s.ctx.emit_diag(diag);
                                        break 'vm EraExecutionBreakReason::IllegalArguments;
                                    };
                                    if arr.as_arrstr().is_none() {
                                        let msg = format!(
                                            "expected an array of strings for parameter {}, got {:?}",
                                            i, arr
                                        );
                                        let mut diag = Diagnostic::new();
                                        diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                                        s.ctx.emit_diag(diag);
                                        break 'vm EraExecutionBreakReason::IllegalArguments;
                                    }
                                    processed_args.push(arr.clone());
                                }
                                ValueKind::Int => match arg_pack {
                                    ArgPackKind::Scalar(val) => {
                                        if val.as_int().is_none() {
                                            let msg = format!(
                                                "expected an integer for parameter {}, got {:?}",
                                                i, val
                                            );
                                            let mut diag = Diagnostic::new();
                                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                                            s.ctx.emit_diag(diag);
                                            break 'vm EraExecutionBreakReason::IllegalArguments;
                                        }
                                        processed_args.push(val.clone());
                                    }
                                    ArgPackKind::Empty => {
                                        let val = param.default_value.coerce_as_int().unwrap();
                                        processed_args.push(Value::new_int(val));
                                    }
                                    ArgPackKind::ArrIdx(arr, idx) => {
                                        let Some(arr) = arr.as_arrint() else {
                                            let msg = format!("expected an array of integers for parameter {i}, got ArrStr");
                                            let mut diag = Diagnostic::new();
                                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                                            s.ctx.emit_diag(diag);
                                            break 'vm EraExecutionBreakReason::IllegalArguments;
                                        };
                                        let Some(val) = arr.borrow().flat_get(idx).map(|x| x.val)
                                        else {
                                            let mut diag = Diagnostic::new();
                                            diag.span_err(
                                                s.cur_filename(),
                                                s.cur_bc_span(),
                                                format!("array index {idx} out of bounds"),
                                            );
                                            s.ctx.emit_diag(diag);
                                            break 'vm EraExecutionBreakReason::IllegalArguments;
                                        };
                                        processed_args.push(Value::new_int(val));
                                    }
                                },
                                ValueKind::Str => match arg_pack {
                                    ArgPackKind::Scalar(val) => {
                                        if val.as_str().is_none() {
                                            let msg = format!(
                                                "expected a string for parameter {}, got {:?}",
                                                i, val
                                            );
                                            let mut diag = Diagnostic::new();
                                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                                            s.ctx.emit_diag(diag);
                                            break 'vm EraExecutionBreakReason::IllegalArguments;
                                        }
                                        processed_args.push(val.clone());
                                    }
                                    ArgPackKind::Empty => {
                                        let val = param.default_value.coerce_as_str().unwrap();
                                        processed_args.push(Value::new_str(val.into()));
                                    }
                                    ArgPackKind::ArrIdx(arr, idx) => {
                                        let Some(arr) = arr.as_arrstr() else {
                                            let mut diag = Diagnostic::new();
                                            diag.span_err(
                                                s.cur_filename(),
                                                s.cur_bc_span(),
                                                format!("expected an array of strings, got ArrInt"),
                                            );
                                            s.ctx.emit_diag(diag);
                                            break 'vm EraExecutionBreakReason::IllegalArguments;
                                        };
                                        let Some(val) =
                                            arr.borrow().flat_get(idx).map(|x| x.val.clone())
                                        else {
                                            let mut diag = Diagnostic::new();
                                            diag.span_err(
                                                s.cur_filename(),
                                                s.cur_bc_span(),
                                                format!("array index {idx} out of bounds"),
                                            );
                                            s.ctx.emit_diag(diag);
                                            break 'vm EraExecutionBreakReason::IllegalArguments;
                                        };
                                        processed_args.push(Value::new_str(val));
                                    }
                                },
                            }
                        }

                        // Check call stack depth
                        if s.prev_frames.len() + 1 >= MAX_CALL_DEPTH {
                            let mut diag = Diagnostic::new();
                            diag.span_err(
                                s.cur_filename(),
                                s.cur_bc_span(),
                                "maximum call depth exceeded",
                            );
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::InternalError;
                        }

                        // Now apply the arguments and call the function
                        // WARN: The following procedure must not fail, or stack will be corrupted.
                        let args_cnt = processed_args.len();
                        if is_force {
                            sview.replace(processed_args);
                        } else {
                            // Push 1 to indicate that the function exists
                            sview.replace([Value::new_int(1)]);
                            s.stack.extend(processed_args);
                        }

                        let new_ip = EraExecIp {
                            chunk: func_info.chunk_idx,
                            offset: func_info.bc_offset,
                        };
                        let ret_ip = EraExecIp {
                            chunk: s.cur_frame.ip.chunk,
                            offset: s.cur_frame.ip.offset + ip_delta as u32,
                        };

                        // Create a new execution frame for the function
                        let is_transient = func_info.is_transient;
                        let stack_start = if is_transient {
                            s.prev_frames.last().map_or(0, |f| f.stack_start as usize)
                        } else {
                            s.stack.inner().len() - args_cnt
                        };
                        drop_site!();
                        self.state.frames.push(EraFuncExecFrame {
                            stack_start: stack_start as _,
                            ip: new_ip,
                            ret_ip,
                            ignore_return_value: false,
                            is_transient,
                        });
                        ip_delta = 0;
                        make_site!();
                    } else {
                        // Function does not exist
                        if is_force {
                            let msg = format!("function `{:?}` not found", sview.last().unwrap());
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        } else {
                            // Push 0 to indicate that the function does not exist
                            sview.replace([Value::new_int(0)]);
                        }
                    }
                }
                Bc::RestartExecAtFun => {
                    let sview = view_stack_or_bail!();
                    let [func_idx] = sview.as_ref();
                    let func_idx = match func_idx.as_unpacked() {
                        RefFlatValue::Int(x) => x.val as usize,
                        v => {
                            let msg = format!("expected an integer function index, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                    let Some((func_name, func_info)) = s
                        .ctx
                        .func_entries
                        .get_index(func_idx)
                        .and_then(|(&k, v)| v.as_ref().map(|v| (k, v)))
                    else {
                        let msg = format!("function {} not found", func_idx);
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    let new_ip = EraExecIp {
                        chunk: func_info.chunk_idx,
                        offset: func_info.bc_offset,
                    };
                    let ret_ip = EraExecIp {
                        chunk: s.cur_frame.ip.chunk,
                        offset: s.cur_frame.ip.offset + ip_delta as u32,
                    };
                    // Verify that the function has no arguments.
                    let params_cnt = func_info.frame_info.args.len();
                    if params_cnt != 0 {
                        let msg = format!(
                            "function `{}` must take no arguments, but actually takes {}",
                            func_name, params_cnt
                        );
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    }

                    // Create a new execution frame for the function
                    let stack_start = 0;
                    drop_site!();
                    self.state.stack.clear();
                    self.state.frames.clear();
                    self.state.frames.push(EraFuncExecFrame {
                        stack_start: stack_start as _,
                        ip: new_ip,
                        ret_ip,
                        ignore_return_value: false,
                        is_transient: false,
                    });
                    ip_delta = 0;
                    make_site!();
                }
                Bc::JumpWW { offset } => {
                    ip_delta = offset;
                }
                Bc::JumpIfWW { offset } => {
                    let sview = view_stack_or_bail!();
                    let [cond] = sview.as_ref();
                    let cond = !cond.is_default();
                    if cond {
                        ip_delta = offset;
                    }
                    sview.replace([]);
                }
                Bc::JumpIfNotWW { offset } => {
                    let sview = view_stack_or_bail!();
                    let [cond] = sview.as_ref();
                    let cond = !cond.is_default();
                    if !cond {
                        ip_delta = offset;
                    }
                    sview.replace([]);
                }
                Bc::LoadConstStr { idx } => {
                    let str = s
                        .ctx
                        .interner()
                        .resolve(TokenKey::try_from_u32(idx).unwrap());
                    s.stack.push(Value::new_str(ArcStr::from(str)));
                }
                Bc::LoadImm8 { imm } => {
                    s.stack.push(Value::new_int(imm.into()));
                }
                Bc::LoadImm16 { imm } => {
                    s.stack.push(Value::new_int(imm.into()));
                }
                Bc::LoadImm32 { imm } => {
                    s.stack.push(Value::new_int(imm.into()));
                }
                Bc::LoadImm64 { imm } => {
                    s.stack.push(Value::new_int(imm));
                }
                Bc::LoadVarWW { idx } => {
                    let var_idx = idx as usize;
                    let Some(var) = s.ctx.variables.get_var_by_idx(var_idx) else {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            format!("variable {} not found", var_idx),
                        );
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    s.stack.push(var.clone());
                }
                Bc::LoadConstVarWW { idx } => {
                    let var_idx = idx as usize;
                    let Some(var) = s.ctx.variables.get_var_by_idx(var_idx) else {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            format!("constant variable {} not found", var_idx),
                        );
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    s.stack.push(var.deep_clone());
                }
                Bc::LoadLocalVar { idx } => {
                    let Some(var) = s.stack.get(idx as usize) else {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            format!("local variable {} not found", idx),
                        );
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    let var = var.clone();
                    var.ensure_alloc();
                    s.stack.push(var);
                }
                Bc::Pop => {
                    s.stack.pop();
                }
                Bc::PopAllN { count } => {
                    let count = count as usize;
                    ensure_stack_len!(count);
                    s.stack.drain(s.stack.len() - count..);
                }
                Bc::PopOneN { idx } => {
                    let idx = idx as usize;
                    ensure_stack_len!(idx);
                    if idx > 0 {
                        let start = s.stack.len() - idx;
                        let end = start + 1;
                        s.stack.drain(start..end);
                    }
                }
                Bc::Swap2 => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    std::mem::swap(a, b);
                }
                Bc::Duplicate => {
                    let sview = view_stack_or_bail!();
                    let [val] = sview.as_ref();
                    let val = val.clone();
                    s.stack.push(val);
                }
                Bc::DuplicateAllN { count } => {
                    let count = count as usize;
                    ensure_stack_len!(count);
                    s.stack.extend_from_within(s.stack.len() - count..);
                }
                Bc::DuplicateOneN { idx } => {
                    let idx = idx as usize;
                    ensure_stack_len!(idx);
                    if idx > 0 {
                        let start = s.stack.len() - idx;
                        let val = s.stack[start].clone();
                        s.stack.push(val);
                    }
                }
                Bc::AddInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    let Some(a) = a.as_int_mut() else {
                        let msg = format!("expected an integer, got {:?}", a);
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    let Some(b) = b.as_int_mut() else {
                        let msg = format!("expected an integer, got {:?}", b);
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    a.val = a.val.wrapping_add(b.val);
                    s.stack.pop();
                }
                Bc::SubInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val = a.val.wrapping_sub(b.val);
                    s.stack.pop();
                }
                Bc::MulInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val = a.val.wrapping_mul(b.val);
                    s.stack.pop();
                }
                Bc::DivInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    if b.val == 0 {
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), "division by zero");
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    }
                    a.val = a.val.wrapping_div(b.val);
                    s.stack.pop();
                }
                Bc::ModInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    if b.val == 0 {
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), "division by zero");
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    }
                    a.val = a.val.wrapping_rem(b.val);
                    s.stack.pop();
                }
                Bc::NegInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a] = sview.as_mut();
                    unpack_args_mut!(a:i);
                    a.val = a.val.wrapping_neg();
                }
                Bc::BitAndInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val &= b.val;
                    s.stack.pop();
                }
                Bc::BitOrInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val |= b.val;
                    s.stack.pop();
                }
                Bc::BitXorInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val ^= b.val;
                    s.stack.pop();
                }
                Bc::BitNotInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a] = sview.as_mut();
                    unpack_args_mut!(a:i);
                    a.val = !a.val;
                }
                Bc::ShlInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val = a.val.wrapping_shl(b.val as u32);
                    s.stack.pop();
                }
                Bc::ShrInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val = a.val.wrapping_shr(b.val as u32);
                    s.stack.pop();
                }
                Bc::CmpIntLT => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val = (a.val < b.val) as i64;
                    s.stack.pop();
                }
                Bc::CmpIntLEq => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val = (a.val <= b.val) as i64;
                    s.stack.pop();
                }
                Bc::CmpIntGT => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val = (a.val > b.val) as i64;
                    s.stack.pop();
                }
                Bc::CmpIntGEq => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val = (a.val >= b.val) as i64;
                    s.stack.pop();
                }
                Bc::CmpIntEq => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val = (a.val == b.val) as i64;
                    s.stack.pop();
                }
                Bc::CmpIntNEq => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val = (a.val != b.val) as i64;
                    s.stack.pop();
                }
                Bc::CmpStrLT => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    *a = {
                        unpack_args_mut!(a:s, b:s);
                        Value::new_int((a.val < b.val) as i64)
                    };
                    s.stack.pop();
                }
                Bc::CmpStrLEq => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    *a = {
                        unpack_args_mut!(a:s, b:s);
                        Value::new_int((a.val <= b.val) as i64)
                    };
                    s.stack.pop();
                }
                Bc::CmpStrGT => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    *a = {
                        unpack_args_mut!(a:s, b:s);
                        Value::new_int((a.val > b.val) as i64)
                    };
                    s.stack.pop();
                }
                Bc::CmpStrGEq => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    *a = {
                        unpack_args_mut!(a:s, b:s);
                        Value::new_int((a.val >= b.val) as i64)
                    };
                    s.stack.pop();
                }
                Bc::CmpStrEq => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    *a = {
                        unpack_args_mut!(a:s, b:s);
                        Value::new_int((a.val == b.val) as i64)
                    };
                    s.stack.pop();
                }
                Bc::CmpStrNEq => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    *a = {
                        unpack_args_mut!(a:s, b:s);
                        Value::new_int((a.val != b.val) as i64)
                    };
                    s.stack.pop();
                }
                Bc::LogicalNot => {
                    let mut sview = view_stack_or_bail!();
                    let [a] = sview.as_mut();
                    unpack_args_mut!(a:i);
                    a.val = (a.val == 0) as i64;
                }
                Bc::MaxInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val = a.val.max(b.val);
                    s.stack.pop();
                }
                Bc::MinInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    a.val = a.val.min(b.val);
                    s.stack.pop();
                }
                Bc::ClampInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b, c] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i, c:i);
                    a.val = a.val.max(b.val).min(c.val);
                    s.stack.drain(s.stack.len() - 2..);
                }
                Bc::InRangeInt => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b, c] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i, c:i);
                    a.val = (a.val >= b.val && a.val <= c.val) as i64;
                    s.stack.drain(s.stack.len() - 2..);
                }
                Bc::InRangeStr => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b, c] = sview.as_mut();
                    *a = {
                        unpack_args_mut!(a:s, b:s, c:s);
                        Value::new_int((a.val >= b.val && a.val <= c.val) as i64)
                    };
                    s.stack.drain(s.stack.len() - 2..);
                }
                Bc::GetBit | Bc::SetBit | Bc::ClearBit | Bc::InvertBit => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    let bit = b.val as usize;
                    if bit >= 64 {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            format!("bit index {} out of range", bit),
                        );
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    }
                    a.val = match inst {
                        Bc::GetBit => (a.val >> bit) & 1,
                        Bc::SetBit => a.val | (1 << bit),
                        Bc::ClearBit => a.val & !(1 << bit),
                        Bc::InvertBit => a.val ^ (1 << bit),
                        _ => unreachable!(),
                    };
                    s.stack.pop();
                }
                Bc::BuildString { count } => {
                    let count = count as usize;
                    ensure_stack_len!(count);
                    let sview = s.stack.get_view_mut(s.stack.len() - count..);
                    let mut buf = String::with_capacity(count * 4);
                    for val in sview.iter() {
                        let Some(val) = val.as_str() else {
                            let msg = format!("expected a string, got {:?}", val);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break 'vm EraExecutionBreakReason::IllegalArguments;
                        };
                        buf.push_str(val.val.as_str());
                    }
                    sview.replace([Value::new_str(ArcStr::from(buf))]);
                }
                Bc::PadString { flags } => {
                    use unicode_width::UnicodeWidthStr;

                    let mut sview = view_stack_or_bail!();
                    let [a, width] = sview.as_mut();
                    unpack_args_mut!(a:s, width:i);
                    let val_width = a.val.width();
                    if val_width < width.val as usize {
                        let spaces = " ".repeat(width.val as usize - val_width);
                        // NOTE: left_pad means: pad to left (spaces on the right side),
                        //       which is somewhat unintuitive.
                        let result = match (flags.left_pad(), flags.right_pad()) {
                            (true, false) => String::from(a.val.as_str()) + &spaces,
                            (false, true) | _ => spaces + &a.val,
                        };
                        a.val = ArcStr::from(result);
                    }
                    s.stack.pop();
                }
                Bc::RepeatStr => {
                    let mut sview = view_stack_or_bail!();
                    let [a, count] = sview.as_mut();
                    unpack_args_mut!(a:s, count:i);
                    let Some(result) = ArcStr::try_repeat(&a.val, count.val as usize) else {
                        let msg = format!("string repetition failed: {} * {}", a.val, count.val);
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    a.val = result;
                    s.stack.pop();
                }
                Bc::BuildArrIdxFromMD { count } => {
                    let count = count as usize;
                    ensure_stack_len!(count + 1);
                    let sview = s.stack.get_view_mut(s.stack.len() - count - 1..);
                    // Fetch indices
                    let mut dims = EraVarDims::new();
                    for val in sview.iter().skip(1) {
                        let Some(val) = val.as_int() else {
                            let msg = format!("expected an integer, got {:?}", val);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break 'vm EraExecutionBreakReason::IllegalArguments;
                        };
                        let Ok(len) = val.val.try_into() else {
                            let mut diag = Diagnostic::new();
                            diag.span_err(
                                s.cur_filename(),
                                s.cur_bc_span(),
                                "array index out of bounds",
                            );
                            s.ctx.emit_diag(diag);
                            break 'vm EraExecutionBreakReason::IllegalArguments;
                        };
                        dims.push(len);
                    }
                    // Calculate flat index
                    let flat_idx = match sview[0].as_unpacked() {
                        RefFlatValue::ArrInt(arr) => {
                            let Some(idx) = arr.borrow().calc_idx(&dims) else {
                                let mut diag = Diagnostic::new();
                                diag.span_err(
                                    s.cur_filename(),
                                    s.cur_bc_span(),
                                    "array index out of bounds",
                                );
                                s.ctx.emit_diag(diag);
                                break 'vm EraExecutionBreakReason::IllegalArguments;
                            };
                            idx
                        }
                        RefFlatValue::ArrStr(arr) => {
                            let Some(idx) = arr.borrow().calc_idx(&dims) else {
                                let mut diag = Diagnostic::new();
                                diag.span_err(
                                    s.cur_filename(),
                                    s.cur_bc_span(),
                                    "array index out of bounds",
                                );
                                s.ctx.emit_diag(diag);
                                break 'vm EraExecutionBreakReason::IllegalArguments;
                            };
                            idx
                        }
                        v => {
                            let msg = format!("expected an array, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break 'vm EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                    let sview = s.stack.get_view_mut(s.stack.len() - count..);
                    sview.replace([Value::new_int(flat_idx as _)]);
                }
                Bc::GetArrValFlat => {
                    let mut sview = view_stack_or_bail!();
                    let [arr, idx] = sview.as_mut();
                    let idx = match idx.as_int() {
                        Some(idx) => idx.val as usize,
                        None => {
                            let msg = format!("expected an integer, got {:?}", idx);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                    let val = match arr.as_unpacked() {
                        RefFlatValue::ArrInt(arr) => {
                            let arr_ptr = Rc::as_ptr(arr) as *const ();
                            let mut arr = arr.borrow_mut();
                            let flags = arr.flags;
                            let Some(val) = arr.flat_get_mut(idx) else {
                                drop(arr);
                                let mut diag = Diagnostic::new();
                                diag.span_err(
                                    s.cur_filename(),
                                    s.cur_bc_span(),
                                    format!("array index `{idx}` out of bounds"),
                                );
                                if let Some(arr) = s.get_variable_name_from_ptr(arr_ptr) {
                                    diag.span_note(
                                        s.cur_filename(),
                                        s.cur_bc_span(),
                                        format!("while reading from array `{}`", arr),
                                    );
                                }
                                s.ctx.emit_diag(diag);
                                break EraExecutionBreakReason::IllegalArguments;
                            };

                            // Check trap
                            if flags.is_trap() {
                                let Some(&trap_var_info) = s.i.trap_vars.get(&arr_ptr) else {
                                    drop(arr);
                                    let mut diag = Diagnostic::new();
                                    diag.span_err(
                                        s.cur_filename(),
                                        s.cur_bc_span(),
                                        "array trap variable not registered",
                                    );
                                    s.ctx.emit_diag(diag);
                                    break EraExecutionBreakReason::IllegalArguments;
                                };
                                let Some(trap_var_info) =
                                    s.ctx.variables.get_var_info(trap_var_info as _)
                                else {
                                    drop(arr);
                                    let mut diag = Diagnostic::new();
                                    diag.span_err(
                                        s.cur_filename(),
                                        s.cur_bc_span(),
                                        "array trap variable not found in global variables",
                                    );
                                    s.ctx.emit_diag(diag);
                                    break EraExecutionBreakReason::IllegalArguments;
                                };
                                val.val = match s
                                    .ctx
                                    .callback
                                    .on_var_get_int(trap_var_info.name.as_ref(), idx)
                                {
                                    Ok(val) => val.into(),
                                    Err(e) => {
                                        drop(arr);
                                        let mut diag = Diagnostic::new();
                                        diag.span_err(
                                            s.cur_filename(),
                                            s.cur_bc_span(),
                                            format!("trap handler failed: {e}"),
                                        );
                                        s.ctx.emit_diag(diag);
                                        break EraExecutionBreakReason::InternalError;
                                    }
                                };
                            }

                            // Return value
                            Value::new_int(val.val)
                        }
                        RefFlatValue::ArrStr(arr) => {
                            let arr_ptr = Rc::as_ptr(arr) as *const ();
                            let mut arr = arr.borrow_mut();
                            let flags = arr.flags;
                            let Some(val) = arr.flat_get_mut(idx) else {
                                drop(arr);
                                let mut diag = Diagnostic::new();
                                diag.span_err(
                                    s.cur_filename(),
                                    s.cur_bc_span(),
                                    format!("array index `{idx}` out of bounds"),
                                );
                                if let Some(arr) = s.get_variable_name_from_ptr(arr_ptr) {
                                    diag.span_note(
                                        s.cur_filename(),
                                        s.cur_bc_span(),
                                        format!("while reading from array `{}`", arr),
                                    );
                                }
                                s.ctx.emit_diag(diag);
                                break EraExecutionBreakReason::IllegalArguments;
                            };

                            // Check trap
                            if flags.is_trap() {
                                let Some(&trap_var_info) = s.i.trap_vars.get(&arr_ptr) else {
                                    drop(arr);
                                    let mut diag = Diagnostic::new();
                                    diag.span_err(
                                        s.cur_filename(),
                                        s.cur_bc_span(),
                                        "array trap variable not registered",
                                    );
                                    s.ctx.emit_diag(diag);
                                    break EraExecutionBreakReason::IllegalArguments;
                                };
                                let Some(trap_var_info) =
                                    s.ctx.variables.get_var_info(trap_var_info as _)
                                else {
                                    drop(arr);
                                    let mut diag = Diagnostic::new();
                                    diag.span_err(
                                        s.cur_filename(),
                                        s.cur_bc_span(),
                                        "array trap variable not found in global variables",
                                    );
                                    s.ctx.emit_diag(diag);
                                    break EraExecutionBreakReason::IllegalArguments;
                                };
                                val.val = match s
                                    .ctx
                                    .callback
                                    .on_var_get_str(trap_var_info.name.as_ref(), idx)
                                {
                                    Ok(val) => val.into(),
                                    Err(e) => {
                                        drop(arr);
                                        let mut diag = Diagnostic::new();
                                        diag.span_err(
                                            s.cur_filename(),
                                            s.cur_bc_span(),
                                            format!("trap handler failed: {e}"),
                                        );
                                        s.ctx.emit_diag(diag);
                                        break EraExecutionBreakReason::InternalError;
                                    }
                                };
                            }

                            // Return value
                            Value::new_str(val.val.clone())
                        }
                        v => {
                            let msg = format!("expected an array, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                    sview.replace([val]);
                }
                Bc::SetArrValFlat => {
                    let slen = s.stack.len();
                    let mut sview = view_stack_or_bail!();
                    let [arr, idx, val] = sview.as_mut();
                    unpack_args!(idx:i);
                    let idx = idx as usize;
                    match arr.as_unpacked() {
                        RefFlatValue::ArrInt(arr) => {
                            let arr_ptr = Rc::as_ptr(arr) as *const ();
                            unpack_args!(val:i);
                            let mut arr = arr.borrow_mut();
                            let Some(cell) = arr.flat_get_mut(idx) else {
                                drop(arr);
                                let mut diag = Diagnostic::new();
                                diag.span_err(
                                    s.cur_filename(),
                                    s.cur_bc_span(),
                                    format!("array index `{idx}` out of bounds"),
                                );
                                if let Some(arr) = s.get_variable_name_from_ptr(arr_ptr) {
                                    diag.span_note(
                                        s.cur_filename(),
                                        s.cur_bc_span(),
                                        format!("while writing to array `{}`", arr),
                                    );
                                }
                                s.ctx.emit_diag(diag);
                                break EraExecutionBreakReason::IllegalArguments;
                            };
                            cell.val = val;

                            // Check trap
                            let flags = arr.flags;
                            drop(arr);
                            if flags.is_trap() {
                                let Some(&trap_var_info) = s.i.trap_vars.get(&arr_ptr) else {
                                    let mut diag = Diagnostic::new();
                                    diag.span_err(
                                        s.cur_filename(),
                                        s.cur_bc_span(),
                                        "array trap variable not registered",
                                    );
                                    s.ctx.emit_diag(diag);
                                    break EraExecutionBreakReason::IllegalArguments;
                                };
                                let Some(trap_var_info) =
                                    s.ctx.variables.get_var_info(trap_var_info as _)
                                else {
                                    let mut diag = Diagnostic::new();
                                    diag.span_err(
                                        s.cur_filename(),
                                        s.cur_bc_span(),
                                        "array trap variable not found in global variables",
                                    );
                                    s.ctx.emit_diag(diag);
                                    break EraExecutionBreakReason::IllegalArguments;
                                };
                                match s.ctx.callback.on_var_set_int(
                                    trap_var_info.name.as_ref(),
                                    idx,
                                    val,
                                ) {
                                    Ok(_) => (),
                                    Err(e) => {
                                        let mut diag = Diagnostic::new();
                                        diag.span_err(
                                            s.cur_filename(),
                                            s.cur_bc_span(),
                                            format!("trap handler failed: {e}"),
                                        );
                                        s.ctx.emit_diag(diag);
                                        break EraExecutionBreakReason::InternalError;
                                    }
                                }
                            }
                        }
                        RefFlatValue::ArrStr(arr) => {
                            let arr_ptr = Rc::as_ptr(arr) as *const ();
                            unpack_args!(val:s);
                            let val = val.clone();
                            let mut arr = arr.borrow_mut();
                            let flags = arr.flags;
                            let Some(cell) = arr.flat_get_mut(idx) else {
                                drop(arr);
                                let mut diag = Diagnostic::new();
                                diag.span_err(
                                    s.cur_filename(),
                                    s.cur_bc_span(),
                                    format!("array index `{idx}` out of bounds"),
                                );
                                if let Some(arr) = s.get_variable_name_from_ptr(arr_ptr) {
                                    diag.span_note(
                                        s.cur_filename(),
                                        s.cur_bc_span(),
                                        format!("while writing to array `{}`", arr),
                                    );
                                }
                                s.ctx.emit_diag(diag);
                                break EraExecutionBreakReason::IllegalArguments;
                            };
                            cell.val = val;

                            // Check trap
                            if flags.is_trap() {
                                let Some(&trap_var_info) = s.i.trap_vars.get(&arr_ptr) else {
                                    drop(arr);
                                    let mut diag = Diagnostic::new();
                                    diag.span_err(
                                        s.cur_filename(),
                                        s.cur_bc_span(),
                                        "array trap variable not registered",
                                    );
                                    s.ctx.emit_diag(diag);
                                    break EraExecutionBreakReason::IllegalArguments;
                                };
                                let Some(trap_var_info) =
                                    s.ctx.variables.get_var_info(trap_var_info as _)
                                else {
                                    drop(arr);
                                    let mut diag = Diagnostic::new();
                                    diag.span_err(
                                        s.cur_filename(),
                                        s.cur_bc_span(),
                                        "array trap variable not found in global variables",
                                    );
                                    s.ctx.emit_diag(diag);
                                    break EraExecutionBreakReason::IllegalArguments;
                                };
                                match s.ctx.callback.on_var_set_str(
                                    trap_var_info.name.as_ref(),
                                    idx,
                                    &cell.val,
                                ) {
                                    Ok(_) => (),
                                    Err(e) => {
                                        drop(arr);
                                        let mut diag = Diagnostic::new();
                                        diag.span_err(
                                            s.cur_filename(),
                                            s.cur_bc_span(),
                                            format!("trap handler failed: {e}"),
                                        );
                                        s.ctx.emit_diag(diag);
                                        break EraExecutionBreakReason::InternalError;
                                    }
                                }
                            }
                        }
                        v => {
                            let msg = format!("expected an array, got {:?}", v);
                            let mut diag = Diagnostic::new();
                            diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    }
                    // Reserve the right-hand side
                    s.stack.drain(slen - 3..slen - 1);
                }
                Bc::TimesFloat => {
                    let mut sview = view_stack_or_bail!();
                    let [a, b] = sview.as_mut();
                    unpack_args_mut!(a:i, b:i);
                    let factor = f64::from_bits(b.val as u64);
                    a.val = (a.val as f64 * factor) as i64;
                    s.stack.pop();
                }
                Bc::FunExists => {
                    let sview = view_stack_or_bail!();
                    let [func_name] = sview.as_ref();
                    unpack_args!(func_name:s);
                    let exists = s
                        .ctx
                        .func_entries
                        .get(Ascii::new_str(func_name.as_str()))
                        .map_or(false, |v| v.is_some());
                    sview.replace([Value::new_int(exists as i64)]);
                }
                Bc::ReplaceStr => {
                    let sview = view_stack_or_bail!();
                    let [haystack, needle, replace_with] = sview.as_ref();
                    unpack_args!(haystack:s, needle:s, replace_with:s);
                    let re = match s
                        .i
                        .regex_cache
                        .try_get_or_insert(needle.clone(), || regex::Regex::new(needle))
                    {
                        Ok(re) => re,
                        Err(e) => {
                            let mut diag = Diagnostic::new();
                            diag.span_err(
                                s.cur_filename(),
                                s.cur_bc_span(),
                                format!("failed to compile regex: {e}"),
                            );
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                    let result = re.replace_all(&haystack, replace_with.as_str()).into();
                    sview.replace([Value::new_str(result)]);
                }
                Bc::SubStr | Bc::SubStrU => {
                    let sview = view_stack_or_bail!();
                    let [haystack, start_pos, length] = sview.as_ref();
                    unpack_args!(haystack:s, start_pos:i, length:i);
                    let haystack = haystack.as_str();
                    let start_pos = start_pos.max(0) as usize;
                    let length = if length < 0 {
                        usize::MAX
                    } else {
                        length as usize
                    };
                    let result = if length <= 0 {
                        ArcStr::default()
                    } else {
                        let haystack_len = haystack.len();
                        let idx_fn = &|(index, _)| index;
                        let mut it = haystack.char_indices();
                        let start_byte_pos = it.nth(start_pos).map_or(haystack_len, idx_fn);
                        let end_byte_pos = it.nth(length - 1).map_or(haystack_len, idx_fn);
                        // SAFETY: the indices are guaranteed to be valid
                        unsafe { haystack.get_unchecked(start_byte_pos..end_byte_pos).into() }
                    };
                    sview.replace([Value::new_str(result)]);
                }
                Bc::StrFind | Bc::StrFindU => {
                    let sview = view_stack_or_bail!();
                    let [haystack, needle, start_pos] = sview.as_ref();
                    unpack_args!(haystack:s, needle:s, start_pos:i);
                    let haystack = haystack.as_str();
                    let needle = needle.as_str();
                    let start_pos = start_pos.max(0) as usize;
                    let haystack_len = haystack.len();
                    let start_byte_pos = haystack
                        .char_indices()
                        .nth(start_pos)
                        .map_or(haystack_len, |(index, _)| index);
                    let result = haystack[start_byte_pos..].find(needle).map_or(-1, |pos| {
                        haystack[..start_byte_pos + pos].chars().count() as i64
                    });
                    sview.replace([Value::new_int(result)]);
                }
                Bc::StrLen | Bc::StrLenU => {
                    let sview = view_stack_or_bail!();
                    let [haystack] = sview.as_ref();
                    unpack_args!(haystack:s);
                    let result = haystack.as_str().chars().count() as i64;
                    sview.replace([Value::new_int(result)]);
                }
                Bc::CountSubStr => {
                    let sview = view_stack_or_bail!();
                    let [haystack, needle] = sview.as_ref();
                    unpack_args!(haystack:s, needle:s);
                    let result = haystack.as_str().matches(needle.as_str()).count() as i64;
                    sview.replace([Value::new_int(result)]);
                }
                Bc::StrCharAtU => {
                    let sview = view_stack_or_bail!();
                    let [haystack, pos] = sview.as_ref();
                    unpack_args!(haystack:s, pos:i);
                    let haystack = haystack.as_str();
                    let result = if pos < 0 {
                        ArcStr::default()
                    } else {
                        let pos = pos as usize;
                        haystack
                            .chars()
                            .nth(pos)
                            .map_or(ArcStr::default(), |x| ArcStr::from(x.to_string()))
                    };
                    sview.replace([Value::new_str(result)]);
                }
                Bc::IntToStr => {
                    let sview = view_stack_or_bail!();
                    let [val] = sview.as_ref();
                    unpack_args!(val:i);
                    let result = itoa::Buffer::new().format(val).into();
                    sview.replace([Value::new_str(result)]);
                }
                Bc::StrToInt => {
                    let sview = view_stack_or_bail!();
                    let [val] = sview.as_ref();
                    unpack_args!(val:s);
                    let Some(result) = routines::parse_int_literal_with_sign(val.as_bytes()) else {
                        let msg = format!("failed to parse integer from string: {}", val);
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), msg);
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    sview.replace([Value::new_int(result)]);
                }
                Bc::FormatIntToStr => {
                    let sview = view_stack_or_bail!();
                    let [val, fmt] = sview.as_ref();
                    unpack_args!(val:i, fmt:s);
                    let result = match csharp_format_i64(val, fmt) {
                        Ok(x) => x,
                        Err(e) => {
                            let mut diag = Diagnostic::new();
                            diag.span_err(
                                s.cur_filename(),
                                s.cur_bc_span(),
                                format!("failed to format integer: {e}"),
                            );
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                    sview.replace([Value::new_str(result.into())]);
                }
                Bc::StrIsValidInt => {
                    let sview = view_stack_or_bail!();
                    let [val] = sview.as_ref();
                    unpack_args!(val:s);
                    let result = routines::parse_int_literal(val.as_bytes()).is_some() as i64;
                    sview.replace([Value::new_int(result)]);
                }
                Bc::StrToUpper | Bc::StrToLower | Bc::StrToHalf | Bc::StrToFull => {
                    use full2half::CharacterWidth;

                    let sview = view_stack_or_bail!();
                    let [value] = sview.as_ref();
                    unpack_args!(value:s);
                    let result = match inst {
                        Bc::StrToUpper => value.as_str().to_ascii_uppercase(),
                        Bc::StrToLower => value.as_str().to_ascii_lowercase(),
                        Bc::StrToHalf => value.as_str().to_half_width(),
                        Bc::StrToFull => value.as_str().to_full_width(),
                        _ => unreachable!(),
                    };
                    sview.replace([Value::new_str(result.into())]);
                }
                Bc::BuildBarStr => {
                    use muldiv::MulDiv;

                    let sview = view_stack_or_bail!();
                    let [value, max_value, length] = sview.as_ref();
                    unpack_args!(value:i, max_value:i, length:i);
                    if length < 0 {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            "bar string length cannot be negative",
                        );
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    }
                    if length > 1024 {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            "bar string length too long",
                        );
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    }
                    let length = length as u32;
                    let max_value = max_value.max(0) as u32;
                    let value = (value.max(0) as u32).min(max_value);
                    let Some(fill_cnt) = value.mul_div_floor(length, max_value) else {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            "failed to calculate bar string fill count",
                        );
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    let rest_cnt = length - fill_cnt;
                    let mut result = String::with_capacity(length as usize + 2);
                    result.push('[');
                    result += &"*".repeat(fill_cnt as _);
                    result += &".".repeat(rest_cnt as _);
                    result.push(']');
                    sview.replace([Value::new_str(result.into())]);
                }
                Bc::EscapeRegexStr => {
                    let sview = view_stack_or_bail!();
                    let [value] = sview.as_ref();
                    unpack_args!(value:s);
                    let result = regex::escape(value.as_str()).into();
                    sview.replace([Value::new_str(result)]);
                }
                Bc::EncodeToUnicode => {
                    let sview = view_stack_or_bail!();
                    let [haystack, pos] = sview.as_ref();
                    unpack_args!(haystack:s, pos:i);
                    let Ok(pos) = pos.try_into() else {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            "invalid index into string",
                        );
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    let Some(result) = haystack.chars().nth(pos) else {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            "invalid index into string",
                        );
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    sview.replace([Value::new_int(result as _)]);
                }
                Bc::UnicodeToStr => {
                    let sview = view_stack_or_bail!();
                    let [value] = sview.as_ref();
                    unpack_args!(value:i);
                    let Some(result) = value.try_into().map(char::from_u32).ok().flatten() else {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            "value is not a valid Unicode scalar value",
                        );
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    sview.replace([Value::new_str(result.to_string().into())]);
                }
                Bc::IntToStrWithBase => {
                    let sview = view_stack_or_bail!();
                    let [val, base] = sview.as_ref();
                    unpack_args!(val:i, base:i);
                    let base = base as u32;
                    if base < 2 || base > 36 {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            "base must be in the range 2..=36",
                        );
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    }
                    let result = routines::format_radix(val, base).unwrap();
                    sview.replace([Value::new_str(result.into())]);
                }
                Bc::HtmlTagSplit => {
                    // TODO: Honor trapped variables
                    do_intrinsic!(IntrinsicKind::HtmlTagSplit);
                    // let intrinsic = get_global_intrinsic(IntrinsicKind::HtmlTagSplit).unwrap();
                    // if let Err(e) = intrinsic.invoke_and_consume(&mut s.stack) {
                    //     match e {
                    //         EraIntrinsicError::IllegalArguments(e) => {
                    //             let mut diag = Diagnostic::new();
                    //             diag.span_err(s.cur_filename(), s.cur_bc_span(), e.to_string());
                    //             s.ctx.emit_diag(diag);
                    //             break EraExecutionBreakReason::IllegalArguments;
                    //         }
                    //         EraIntrinsicError::GeneralError(e) => return Err(e),
                    //     }
                    // }
                    // ----- Old code -----
                    // let sview = view_stack_or_bail!();
                    // let [html, tags, tags_idx, count, count_idx] = sview.as_ref();
                    // unpack_args!(html:s, tags:vs, tags_idx:i, count:vi, count_idx:i);
                    // // TODO: Honor trapped variables
                    // {
                    //     let Some(mut tags) =
                    //         MaskedArr::try_new(tags.borrow_mut(), tags_idx as _, -1)
                    //     else {
                    //         let mut diag = Diagnostic::new();
                    //         s.span_err_here(&mut diag, "invalid indices into array");
                    //         s.ctx.emit_diag(diag);
                    //         break EraExecutionBreakReason::IllegalArguments;
                    //     };
                    //     let mut count = count.borrow_mut();
                    //     let count_idx = count_idx as usize;
                    //     let mut parts_count: usize = 0;
                    //     for part in crate::util::html::split_html_tags(&html) {
                    //         let Ok(part) = part else {
                    //             drop(tags);
                    //             drop(count);
                    //             let mut diag = Diagnostic::new();
                    //             s.span_err_here(&mut diag, "found invalid html tag while parsing");
                    //             s.ctx.emit_diag(diag);
                    //             break 'vm EraExecutionBreakReason::IllegalArguments;
                    //         };
                    //         let Some(tags) = tags.get_mut(parts_count) else {
                    //             drop(tags);
                    //             drop(count);
                    //             let mut diag = Diagnostic::new();
                    //             s.span_err_here(&mut diag, "invalid indices into array");
                    //             s.ctx.emit_diag(diag);
                    //             break 'vm EraExecutionBreakReason::IllegalArguments;
                    //         };
                    //         tags.val = part.into();
                    //         parts_count += 1;
                    //     }
                    //     let Some(count) = count.flat_get_mut(count_idx) else {
                    //         drop(tags);
                    //         drop(count);
                    //         let mut diag = Diagnostic::new();
                    //         s.span_err_here(&mut diag, "invalid indices into array");
                    //         s.ctx.emit_diag(diag);
                    //         break EraExecutionBreakReason::IllegalArguments;
                    //     };
                    //     count.val = parts_count as _;
                    // }
                    // sview.replace([]);
                }
                Bc::HtmlToPlainText => {
                    let sview = view_stack_or_bail!();
                    let [html] = sview.as_ref();
                    unpack_args!(html:s);
                    let result = nanohtml2text::html2text(html);
                    sview.replace([Value::new_str(result.into())]);
                }
                Bc::HtmlEscape => {
                    let sview = view_stack_or_bail!();
                    let [html] = sview.as_ref();
                    unpack_args!(html:s);
                    let result = htmlize::escape_all_quotes(html).into();
                    sview.replace([Value::new_str(result)]);
                }
                Bc::PowerInt => {
                    let mut sview = view_stack_or_bail!();
                    let [base, exp] = sview.as_mut();
                    unpack_args_mut!(base:i, exp:i);
                    let result = base.val.wrapping_pow(exp.val as _);
                    base.val = result;
                    s.stack.pop();
                }
                Bc::SqrtInt
                | Bc::CbrtInt
                | Bc::LogInt
                | Bc::Log10Int
                | Bc::ExponentInt
                | Bc::AbsInt
                | Bc::SignInt => {
                    use num_integer::Roots;

                    let mut sview = view_stack_or_bail!();
                    let [value] = sview.as_mut();
                    unpack_args_mut!(value:i);
                    let v = value.val;
                    let result = match inst {
                        Bc::SqrtInt => {
                            if v < 0 {
                                let mut diag = Diagnostic::new();
                                diag.span_err(
                                    s.cur_filename(),
                                    s.cur_bc_span(),
                                    "cannot take square root of a negative number",
                                );
                                s.ctx.emit_diag(diag);
                                break EraExecutionBreakReason::IllegalArguments;
                            }
                            v.sqrt()
                        }
                        Bc::CbrtInt => v.cbrt(),
                        // FIXME: Use f128 when https://github.com/rust-lang/rust/issues/116909 lands.
                        Bc::LogInt => (v as f64).ln() as _,
                        Bc::Log10Int => v.ilog10().into(),
                        Bc::ExponentInt => (v as f64).exp() as _,
                        Bc::AbsInt => v.wrapping_abs(),
                        Bc::SignInt => v.signum(),
                        _ => unreachable!(),
                    };
                    value.val = result;
                }
                Bc::GroupMatch { count } => {
                    let count = count as usize;
                    ensure_stack_len!(count + 1);
                    let sview = s.stack.get_view_mut(s.stack.len() - count - 1..);
                    let value = &sview[0];
                    if value.kind().is_arr() {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            "expected a scalar, got an array",
                        );
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    }
                    let count = sview.iter().skip(1).filter(|x| value == *x).count() as i64;
                    sview.replace([Value::new_int(count)]);
                }
                Bc::ArrayCountMatches => {
                    do_intrinsic!(IntrinsicKind::ArrayCountMatches);
                }
                Bc::CArrayCountMatches => {
                    do_intrinsic!(IntrinsicKind::CArrayCountMatches);
                }
                Bc::SumArray => {
                    do_intrinsic!(IntrinsicKind::SumArray);
                }
                Bc::SumCArray => {
                    do_intrinsic!(IntrinsicKind::SumCArray);
                }
                Bc::MaxArray => {
                    do_intrinsic!(IntrinsicKind::MaxArray);
                }
                Bc::MaxCArray => {
                    do_intrinsic!(IntrinsicKind::MaxCArray);
                }
                Bc::MinArray => {
                    do_intrinsic!(IntrinsicKind::MinArray);
                }
                Bc::MinCArray => {
                    do_intrinsic!(IntrinsicKind::MinCArray);
                }
                Bc::InRangeArray => {
                    do_intrinsic!(IntrinsicKind::InRangeArray);
                }
                Bc::InRangeCArray => {
                    do_intrinsic!(IntrinsicKind::InRangeCArray);
                }
                Bc::ArrayRemove => {
                    do_intrinsic!(IntrinsicKind::ArrayRemove);
                }
                Bc::ArraySortAsc => {
                    do_intrinsic!(IntrinsicKind::ArraySortAsc);
                }
                Bc::ArraySortDesc => {
                    do_intrinsic!(IntrinsicKind::ArraySortDesc);
                }
                Bc::ArrayMSort { subs_cnt } => {
                    let subs_cnt = subs_cnt as usize;
                    let sview = s.stack.get_view_mut(s.stack.len() - subs_cnt..);
                    let result = match crate::v2::intrinsics::array_multi_sort(sview.as_ref()) {
                        Ok(x) => x,
                        Err(e) => match e {
                            EraIntrinsicError::IllegalArguments(e) => {
                                let mut diag = Diagnostic::new();
                                diag.span_err(
                                    s.cur_filename(),
                                    s.cur_bc_span(),
                                    format!("failed to sort array: {e}"),
                                );
                                s.ctx.emit_diag(diag);
                                break EraExecutionBreakReason::IllegalArguments;
                            }
                            EraIntrinsicError::GeneralError(e) => {
                                return Err(e);
                            }
                        },
                    };
                    sview.replace(result.result);
                }
                Bc::ArrayCopy => {
                    do_intrinsic!(IntrinsicKind::ArrayCopy);
                }
                Bc::ArrayShift => {
                    do_intrinsic!(IntrinsicKind::ArrayShift);
                }
                Bc::Print | Bc::PrintLine | Bc::PrintExtended { .. } => {
                    let sview = view_stack_or_bail!();
                    let [value] = sview.as_ref();
                    let flags = match inst {
                        Bc::Print => EraPrintExtendedFlags::new(),
                        Bc::PrintLine => EraPrintExtendedFlags::new().with_is_line(true),
                        Bc::PrintExtended { flags } => flags,
                        _ => unreachable!(),
                    };
                    match value.as_unpacked() {
                        RefFlatValue::Int(x) => s
                            .ctx
                            .callback
                            .on_print(itoa::Buffer::new().format(x.val), flags),
                        RefFlatValue::Str(x) => s.ctx.callback.on_print(&x.val, flags),
                        _ => {
                            let mut diag = Diagnostic::new();
                            diag.span_err(
                                s.cur_filename(),
                                s.cur_bc_span(),
                                "expected primitive values as operands",
                            );
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    }
                    sview.replace([]);
                }
                Bc::ReuseLastLine => {
                    let sview = view_stack_or_bail!();
                    let [content] = sview.as_ref();
                    unpack_args!(content:s);
                    s.ctx.callback.on_reuselastline(content);
                    sview.replace([]);
                }
                Bc::ClearLine => {
                    let sview = view_stack_or_bail!();
                    let [count] = sview.as_ref();
                    unpack_args!(count:i);
                    s.ctx.callback.on_clearline(count);
                    sview.replace([]);
                }
                Bc::Wait { flags } => {
                    let any_key = flags.any_key();
                    let is_force = flags.is_force();
                    s.ctx.callback.on_wait(any_key, is_force);
                }
                Bc::TWait => {
                    let sview = view_stack_or_bail!();
                    let [duration, is_force] = sview.as_ref();
                    unpack_args!(duration:i, is_force:i);
                    let is_force = is_force != 0;
                    s.ctx.callback.on_twait(duration, is_force);
                    sview.replace([]);
                }
                Bc::Input { flags } => {
                    macro_rules! handle_result {
                        ($v:ident:i) => {
                            match $v {
                                ControlFlow::Continue(v) => {
                                    if let Some(v) = v {
                                        if let Some(x) = var_result.borrow_mut().flat_get_mut(0) {
                                            x.val = v.into();
                                        } else {
                                            anyhow::bail!("invalid indices into array");
                                        }
                                    }
                                }
                                ControlFlow::Break(()) => {
                                    break EraExecutionBreakReason::CallbackBreak;
                                }
                            }
                        };
                        ($v:ident:s) => {
                            match $v {
                                ControlFlow::Continue(v) => {
                                    if let Some(v) = v {
                                        if let Some(x) = var_results.borrow_mut().flat_get_mut(0) {
                                            x.val = v.into();
                                        } else {
                                            anyhow::bail!("invalid indices into array");
                                        }
                                    }
                                }
                                ControlFlow::Break(()) => {
                                    break EraExecutionBreakReason::CallbackBreak;
                                }
                            }
                        };
                    }

                    match (flags.is_string(), flags.is_one(), flags.is_timed()) {
                        (false, false, false) => {
                            if flags.has_default_value() {
                                let sview = view_stack_or_bail!();
                                let [default_value, can_click, allow_skip] = sview.as_ref();
                                unpack_args!(default_value:i, can_click:b, allow_skip:b);
                                let r = s.ctx.callback.on_input_int(
                                    Some(default_value),
                                    can_click,
                                    allow_skip,
                                );
                                handle_result!(r:i);
                            } else {
                                let sview = view_stack_or_bail!();
                                let [can_click, allow_skip] = sview.as_ref();
                                unpack_args!(can_click:b, allow_skip:b);
                                let r = s.ctx.callback.on_input_int(None, can_click, allow_skip);
                                handle_result!(r:i);
                            }
                        }
                        (true, false, false) => {
                            if flags.has_default_value() {
                                let sview = view_stack_or_bail!();
                                let [default_value, can_click, allow_skip] = sview.as_ref();
                                unpack_args!(default_value:s, can_click:b, allow_skip:b);
                                let r = s.ctx.callback.on_input_str(
                                    Some(default_value),
                                    can_click,
                                    allow_skip,
                                );
                                handle_result!(r:s);
                            } else {
                                let sview = view_stack_or_bail!();
                                let [can_click, allow_skip] = sview.as_ref();
                                unpack_args!(can_click:b, allow_skip:b);
                                let r = s.ctx.callback.on_input_str(None, can_click, allow_skip);
                                handle_result!(r:s);
                            }
                        }
                        (false, false, true) => {
                            let sview = view_stack_or_bail!();
                            let [time_limit, default_value, show_prompt, expiry_msg, can_click] =
                                sview.as_ref();
                            unpack_args!(
                                time_limit:i,
                                default_value:i,
                                show_prompt:b,
                                expiry_msg:s,
                                can_click:b
                            );
                            let r = s.ctx.callback.on_tinput_int(
                                time_limit,
                                default_value,
                                show_prompt,
                                expiry_msg,
                                can_click,
                            );
                            handle_result!(r:i);
                        }
                        (true, false, true) => {
                            let sview = view_stack_or_bail!();
                            let [time_limit, default_value, show_prompt, expiry_msg, can_click] =
                                sview.as_ref();
                            unpack_args!(
                                time_limit:i,
                                default_value:s,
                                show_prompt:b,
                                expiry_msg:s,
                                can_click:b
                            );
                            let r = s.ctx.callback.on_tinput_str(
                                time_limit,
                                default_value,
                                show_prompt,
                                expiry_msg,
                                can_click,
                            );
                            handle_result!(r:s);
                        }
                        (false, true, false) => {
                            if flags.has_default_value() {
                                let sview = view_stack_or_bail!();
                                let [default_value] = sview.as_ref();
                                unpack_args!(default_value:i);
                                let r = s.ctx.callback.on_oneinput_int(Some(default_value));
                                handle_result!(r:i);
                            } else {
                                let sview = view_stack_or_bail!();
                                let [] = sview.as_ref();
                                let r = s.ctx.callback.on_oneinput_int(None);
                                handle_result!(r:i);
                            }
                        }
                        (true, true, false) => {
                            if flags.has_default_value() {
                                let sview = view_stack_or_bail!();
                                let [default_value] = sview.as_ref();
                                unpack_args!(default_value:s);
                                let r = s.ctx.callback.on_oneinput_str(Some(default_value));
                                handle_result!(r:s);
                            } else {
                                let sview = view_stack_or_bail!();
                                let [] = sview.as_ref();
                                let r = s.ctx.callback.on_oneinput_str(None);
                                handle_result!(r:s);
                            }
                        }
                        (false, true, true) => {
                            let sview = view_stack_or_bail!();
                            let [time_limit, default_value, show_prompt, expiry_msg, can_click] =
                                sview.as_ref();
                            unpack_args!(
                                time_limit:i,
                                default_value:i,
                                show_prompt:b,
                                expiry_msg:s,
                                can_click:b
                            );
                            let r = s.ctx.callback.on_toneinput_int(
                                time_limit,
                                default_value,
                                show_prompt,
                                expiry_msg,
                                can_click,
                            );
                            handle_result!(r:i);
                        }
                        (true, true, true) => {
                            let sview = view_stack_or_bail!();
                            let [time_limit, default_value, show_prompt, expiry_msg, can_click] =
                                sview.as_ref();
                            unpack_args!(
                                time_limit:i,
                                default_value:s,
                                show_prompt:b,
                                expiry_msg:s,
                                can_click:b
                            );
                            let r = s.ctx.callback.on_toneinput_str(
                                time_limit,
                                default_value,
                                show_prompt,
                                expiry_msg,
                                can_click,
                            );
                            handle_result!(r:s);
                        }
                    }
                }
                Bc::KbGetKeyState => {
                    let sview = view_stack_or_bail!();
                    let [keycode] = sview.as_ref();
                    unpack_args!(keycode:i);
                    let result = s.ctx.callback.on_get_key_state(keycode);
                    sview.replace([Value::new_int(result)]);
                }
                Bc::GetCallerFuncName => {
                    let name = s.prev_frames.last().map_or("", |x| {
                        let EraExecIp { chunk, offset } = x.ip;
                        s.ctx
                            .func_info_from_chunk_pos(chunk as _, offset as _)
                            .map_or("", |x| s.ctx.resolve_str(x.name))
                    });
                    s.stack.push(Value::new_str(name.into()));
                }
                Bc::GetCharaNum => {
                    let chara_num = s.charas_count;
                    s.stack.push(Value::new_int(chara_num as _));
                }
                Bc::CsvGetNum { kind } => {
                    let sview = view_stack_or_bail!();
                    let [name] = sview.as_ref();
                    unpack_args!(name:s);
                    let result = s.ctx.get_csv_num(kind, name).map_or(-1, |x| x as _);
                    sview.replace([Value::new_int(result)]);
                }
                Bc::GetRandomRange => {
                    let sview = view_stack_or_bail!();
                    let [min, max] = sview.as_ref();
                    unpack_args!(min:i, max:i);
                    if min >= max {
                        anyhow::bail!("invalid range: min >= max ({}) >= ({})", min, max);
                    }
                    let range_len = (max - min) as u64;
                    let rand_num =
                        s.i.rand_gen
                            .gen_range(0..range_len, || s.ctx.callback.on_get_rand());
                    let result = min.wrapping_add_unsigned(rand_num);
                    sview.replace([Value::new_int(result)]);
                }
                Bc::GetRandomMax => {
                    let sview = view_stack_or_bail!();
                    let [max] = sview.as_ref();
                    unpack_args!(max:i);
                    if max <= 0 {
                        anyhow::bail!("invalid max value: {}", max);
                    }
                    let result =
                        s.i.rand_gen
                            .gen_range(0..max as u64, || s.ctx.callback.on_get_rand());
                    sview.replace([Value::new_int(result as _)]);
                }
                Bc::RowAssign { vals_cnt } => {
                    let vals_cnt = vals_cnt as usize;
                    let sview = s.stack.get_view_mut(s.stack.len() - vals_cnt - 2..);
                    let var = &sview[0];
                    let idx = &sview[1];
                    let vals = &sview[2..];
                    value_into!(var, idx);
                    intrinsics::row_assign(var, idx, vals)?;
                    sview.replace([]);
                }
                Bc::ForLoopStep => {
                    do_intrinsic!(IntrinsicKind::ForLoopStep);
                }
                Bc::ExtendStrToWidth => {
                    do_intrinsic!(IntrinsicKind::ExtendStrToWidth);
                }
                Bc::HtmlPrint => {
                    let sview = view_stack_or_bail!();
                    let [html] = sview.as_ref();
                    unpack_args!(html:s);
                    s.ctx.callback.on_html_print(html);
                    sview.replace([]);
                }
                Bc::PrintButton { flags } => {
                    let sview = view_stack_or_bail!();
                    let [content, value] = sview.as_ref();
                    unpack_args!(content:s, value:s);
                    s.ctx.callback.on_print_button(content, value, flags);
                    sview.replace([]);
                }
                Bc::PrintImg | Bc::PrintImg4 => {
                    anyhow::bail!("PrintImg is not yet implemented");
                }
                Bc::SplitString => {
                    do_intrinsic!(IntrinsicKind::SplitString);
                }
                Bc::GCreate => {
                    let sview = view_stack_or_bail!();
                    let [gid, width, height] = sview.as_ref();
                    unpack_args!(gid:i, width:i, height:i);
                    let ret = s.ctx.callback.on_gcreate(gid, width, height);
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::GCreateFromFile => {
                    let sview = view_stack_or_bail!();
                    let [gid, file_path] = sview.as_ref();
                    unpack_args!(gid:i, file_path:s);
                    let ret = s.ctx.callback.on_gcreatefromfile(gid, file_path);
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::GDispose => {
                    let sview = view_stack_or_bail!();
                    let [gid] = sview.as_ref();
                    unpack_args!(gid:i);
                    let ret = s.ctx.callback.on_gdispose(gid);
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::GCreated => {
                    let sview = view_stack_or_bail!();
                    let [gid] = sview.as_ref();
                    unpack_args!(gid:i);
                    let ret = s.ctx.callback.on_gcreated(gid);
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::GDrawSprite => {
                    let sview = view_stack_or_bail!();
                    let [gid, sprite_name, dest_x, dest_y, dest_width, dest_height] =
                        sview.as_ref();
                    unpack_args!(gid:i, sprite_name:s, dest_x:i, dest_y:i, dest_width:i, dest_height:i);
                    let ret = s.ctx.callback.on_gdrawsprite(
                        gid,
                        sprite_name,
                        dest_x,
                        dest_y,
                        dest_width,
                        dest_height,
                        None,
                    );
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::GDrawSpriteWithColorMatrix => {
                    let sview = view_stack_or_bail!();
                    let [gid, sprite_name, dest_x, dest_y, dest_width, dest_height, color_matrix] =
                        sview.as_ref();
                    unpack_args!(gid:i, sprite_name:s, dest_x:i, dest_y:i, dest_width:i, dest_height:i);
                    let color_matrix = {
                        // Parse ColorMatrix
                        // TODO: EmuEra uses VariableTerm to convey indices info,
                        //       support this by adding full indices as a single
                        //       VM stack value.
                        unpack_args!(color_matrix:vi);
                        let color_matrix = color_matrix.borrow();
                        if color_matrix.dims.len() < 2 {
                            anyhow::bail!("ColorMatrix array has too few dimensions");
                        }
                        let mut clr_mat = [[0.0; 5]; 5];
                        for i in 0..5 {
                            for j in 0..5 {
                                let idxs = &[i as _, j as _];
                                let Some(val) = color_matrix.get(idxs) else {
                                    anyhow::bail!("invalid indices into array");
                                };
                                clr_mat[i][j] = val.val as f32 / 256.0;
                            }
                        }
                        EraColorMatrix::from(clr_mat)
                    };
                    let ret = s.ctx.callback.on_gdrawsprite(
                        gid,
                        sprite_name,
                        dest_x,
                        dest_y,
                        dest_width,
                        dest_height,
                        Some(&color_matrix),
                    );
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::GClear => {
                    let sview = view_stack_or_bail!();
                    let [gid, color] = sview.as_ref();
                    unpack_args!(gid:i, color:i);
                    let ret = s.ctx.callback.on_gclear(gid, color);
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::SpriteCreate => {
                    let sview = view_stack_or_bail!();
                    let [sprite_name, gid, x, y, width, height] = sview.as_ref();
                    unpack_args!(sprite_name:s, gid:i, x:i, y:i, width:i, height:i);
                    let ret = s
                        .ctx
                        .callback
                        .on_spritecreate(sprite_name, gid, x, y, width, height);
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::SpriteDispose => {
                    let sview = view_stack_or_bail!();
                    let [name] = sview.as_ref();
                    unpack_args!(name:s);
                    let ret = s.ctx.callback.on_spritedispose(name);
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::SpriteCreated => {
                    let sview = view_stack_or_bail!();
                    let [name] = sview.as_ref();
                    unpack_args!(name:s);
                    let ret = s.ctx.callback.on_spritecreated(name);
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::SpriteAnimeCreate => {
                    let sview = view_stack_or_bail!();
                    let [name, width, height] = sview.as_ref();
                    unpack_args!(name:s, width:i, height:i);
                    let ret = s.ctx.callback.on_spriteanimecreate(name, width, height);
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::SpriteAnimeAddFrame => {
                    let sview = view_stack_or_bail!();
                    let [name, gid, x, y, width, height, offset_x, offset_y, delay] =
                        sview.as_ref();
                    unpack_args!(name:s, gid:i, x:i, y:i, width:i, height:i, offset_x:i, offset_y:i, delay:i);
                    let ret = s.ctx.callback.on_spriteanimeaddframe(
                        name, gid, x, y, width, height, offset_x, offset_y, delay,
                    );
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::SpriteWidth => {
                    let sview = view_stack_or_bail!();
                    let [name] = sview.as_ref();
                    unpack_args!(name:s);
                    let ret = s.ctx.callback.on_spritewidth(name);
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::SpriteHeight => {
                    let sview = view_stack_or_bail!();
                    let [name] = sview.as_ref();
                    unpack_args!(name:s);
                    let ret = s.ctx.callback.on_spriteheight(name);
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::CheckFont => {
                    let sview = view_stack_or_bail!();
                    let [font_name] = sview.as_ref();
                    unpack_args!(font_name:s);
                    let ret = s.ctx.callback.on_check_font(font_name);
                    sview.replace([Value::new_int(ret)]);
                }
                Bc::SaveText => {
                    anyhow::bail!("SaveText not yet implemented");
                }
                Bc::LoadText => {
                    anyhow::bail!("LoadText not yet implemented");
                }
                Bc::FindElement | Bc::FindLastElement => {
                    use crate::v2::intrinsics::find_element_with_match;

                    let is_first = matches!(inst, Bc::FindElement);

                    let sview = view_stack_or_bail!();
                    let [var, var_idx, value, start_idx, end_idx, complete_match] = sview.as_ref();
                    let Some(var) = var.value_into() else {
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), "value must be an array");
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    unpack_args!(var_idx:i);
                    let Some(value) = value.value_into() else {
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), "value must be an scalar");
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    unpack_args!(start_idx:i, end_idx:i, complete_match:i);
                    let result = find_element_with_match(
                        is_first,
                        var,
                        var_idx,
                        value,
                        start_idx,
                        end_idx,
                        complete_match != 0,
                        &mut s.i.regex_cache,
                    )?;
                    sview.replace([Value::new_int(result)]);
                }
                Bc::FindChara => {
                    do_intrinsic!(IntrinsicKind::FindChara);
                }
                Bc::FindLastChara => {
                    do_intrinsic!(IntrinsicKind::FindLastChara);
                }
                Bc::VarSet => {
                    do_intrinsic!(IntrinsicKind::VarSet);
                }
                Bc::CVarSet => {
                    do_intrinsic!(IntrinsicKind::CVarSet);
                }
                Bc::GetVarSizeByName => {
                    let sview = view_stack_or_bail!();
                    let [name, dim_pos] = sview.as_ref();
                    unpack_args!(name:s, dim_pos:i);
                    let name = &name.clone();
                    drop(sview);
                    let Some(var) = s.get_var_by_name(name) else {
                        anyhow::bail!("variable `{}` not found", name);
                    };
                    let dims = var.dims().unwrap();
                    let result = if dim_pos < 0 {
                        let dim_pos = dim_pos.wrapping_add_unsigned(dims.len() as _) as usize;
                        dims.get(dim_pos).copied()
                    } else {
                        dims.get(dim_pos as usize).copied()
                    };
                    let Some(result) = result else {
                        anyhow::bail!("dimension index `{dim_pos}` out of bounds");
                    };
                    s.stack
                        .splice(s.stack.len() - 2.., [Value::new_int(result as _)]);
                }
                Bc::GetVarAllSize => {
                    let sview = view_stack_or_bail!();
                    let [var] = sview.as_ref();
                    let Some(dims) = var.dims() else {
                        let mut diag = Diagnostic::new();
                        diag.span_err(s.cur_filename(), s.cur_bc_span(), "value must be an array");
                        s.ctx.emit_diag(diag);
                        break EraExecutionBreakReason::IllegalArguments;
                    };
                    let mut var_result = var_result.borrow_mut();
                    if var_result.vals.len() < dims.len() {
                        anyhow::bail!("output array too small");
                    }
                    for (i, dim) in dims.iter().enumerate() {
                        var_result.vals[i].val = *dim as _;
                    }
                    sview.replace([]);
                }
                Bc::GetHostTimeRaw => {
                    let result = s.ctx.callback.on_get_host_time();
                    s.stack.push(Value::new_int(result as _));
                }
                Bc::GetHostTime => {
                    use chrono::*;
                    // NOTE: Native time zone info is used
                    let result = s.ctx.callback.on_get_host_time();
                    let t = DateTime::from_timestamp_millis(result as _).unwrap();
                    let t: DateTime<Local> = t.into();
                    let mut result = t.year() as i64;
                    result = result * 100 + t.month() as i64;
                    result = result * 100 + t.day() as i64;
                    result = result * 100 + t.hour() as i64;
                    result = result * 100 + t.minute() as i64;
                    result = result * 100 + t.second() as i64;
                    result = result * 1000 + (t.nanosecond() as i64) / 1_000_000;
                    s.stack.push(Value::new_int(result));
                }
                Bc::GetHostTimeS => {
                    use chrono::*;
                    // NOTE: Native time zone info is used
                    let result = s.ctx.callback.on_get_host_time();
                    let t = DateTime::from_timestamp_millis(result as _).unwrap();
                    let t: DateTime<Local> = t.into();
                    let result = t.format("%Y/%m/%d %H:%M:%S");
                    s.stack.push(Value::new_str(result.to_string().into()));
                }
                Bc::CsvGetProp2 { csv_kind } => {
                    use EraCharaCsvPropType::*;

                    let sview = view_stack_or_bail!();
                    let [chara_no, index] = sview.as_ref();
                    unpack_args!(chara_no:i, index:i);
                    let chara_no = chara_no as u32;
                    let index = index as u32;
                    let Some(chara_tmpl) = s.ctx.chara_templates.get(&chara_no) else {
                        anyhow::bail!("chara template not found");
                    };
                    match csv_kind {
                        CsvName | CsvCallName | CsvNickName | CsvMasterName | CsvCStr => {
                            // Return string
                            let r = match csv_kind {
                                CsvName => Some(&chara_tmpl.name),
                                CsvCallName => Some(&chara_tmpl.callname),
                                CsvNickName => Some(&chara_tmpl.nickname),
                                CsvMasterName => Some(&chara_tmpl.mastername),
                                CsvCStr => chara_tmpl.cstr.get(&index),
                                _ => unreachable!(),
                            };
                            let r = r.map(|x| x.to_owned()).unwrap_or_default();
                            sview.replace([Value::new_str(r)]);
                        }
                        _ => {
                            // Return integer
                            let r = match csv_kind {
                                CsvBase => chara_tmpl.maxbase.get(&index),
                                CsvAbl => chara_tmpl.abl.get(&index),
                                CsvTalent => chara_tmpl.talent.get(&index),
                                CsvMark => chara_tmpl.mark.get(&index),
                                CsvExp => chara_tmpl.exp.get(&index),
                                CsvRelation => chara_tmpl.relation.get(&index),
                                CsvJuel => chara_tmpl.juel.get(&index),
                                CsvEquip => chara_tmpl.equip.get(&index),
                                CsvCFlag => chara_tmpl.cflag.get(&index),
                                _ => unreachable!(),
                            };
                            let r = r.map(|x| x.to_owned()).unwrap_or_default();
                            sview.replace([Value::new_int(r)]);
                        }
                    }
                }
                Bc::CharaCsvExists => {
                    let sv = view_stack_or_bail!();
                    let [chara_no] = sv.as_ref();
                    unpack_args!(chara_no:i);
                    let exists = s.ctx.chara_templates.contains_key(&(chara_no as u32));
                    sv.replace([Value::new_int(exists as _)]);
                }
                /*
                GetPalamLv | GetExpLv => {
                    let [value, max_lv] = ctx.pop_stack()?;
                    let value = ctx.unpack_int(value.into())?;
                    let max_lv = ctx.unpack_int(max_lv.into())?;
                    let target_arr = match primary_bytecode {
                        GetPalamLv => "PALAMLV",
                        GetExpLv => "EXPLV",
                        _ => unreachable!(),
                    };
                    let target_arr = ctx.get_var_arrint(target_arr)?;
                    let target_arr = target_arr.borrow();
                    let mut result = 0;
                    while result < max_lv.val {
                        let Some(limit) = target_arr.vals.get((result + 1) as usize) else {
                            break;
                        };
                        if value.val < limit.val {
                            break;
                        }
                        result += 1;
                    }
                    ctx.stack.push(Value::new_int(result).into());
                } */
                Bc::GetPalamLv | Bc::GetExpLv => {
                    let sview = view_stack_or_bail!();
                    let [value, max_lv] = sview.as_ref();
                    unpack_args!(value:i, max_lv:i);
                    let target_arr = match inst {
                        Bc::GetPalamLv => "PALAMLV",
                        Bc::GetExpLv => "EXPLV",
                        _ => unreachable!(),
                    };
                    let Some(target_arr) = s
                        .ctx
                        .variables
                        .get_var(target_arr)
                        .and_then(|x| x.as_arrint())
                    else {
                        anyhow::bail!("variable `{}` not found or not an ArrInt", target_arr);
                    };
                    let target_arr = target_arr.borrow();
                    let mut result = 0;
                    while result < max_lv.min(target_arr.vals.len() as i64) {
                        let limit = target_arr.vals[(result + 1) as usize].val;
                        if value < limit {
                            break;
                        }
                        result += 1;
                    }
                    sview.replace([Value::new_int(result)]);
                }
                Bc::AddChara => {
                    let sview = view_stack_or_bail!();
                    let [chara_tmpl_no] = sview.as_ref();
                    unpack_args!(chara_tmpl_no:i);
                    let chara_reg_slot = s.i.charas_count;
                    if chara_reg_slot >= crate::v2::engine::MAX_CHARA_COUNT {
                        anyhow::bail!("too many charas");
                    }
                    intrinsics::add_chara(s.ctx, chara_reg_slot, chara_tmpl_no)?;
                    s.i.charas_count += 1;
                    sview.replace([]);
                }
                Bc::AddVoidChara => {
                    anyhow::bail!("AddVoidChara not yet implemented");
                }
                Bc::PickUpChara { charas_cnt } => {
                    let charas_cnt = charas_cnt as usize;
                    let sview = s.stack.get_view_mut(s.stack.len() - charas_cnt..);
                    let chara_nos = dedup_chara_numbers(sview.as_ref(), s.i.charas_count)?;
                    intrinsics::pick_up_chara(&chara_nos, s.ctx)?;
                    s.i.charas_count = chara_nos.len() as u32;
                    sview.replace([]);
                }
                Bc::DeleteChara { charas_cnt } => {
                    let charas_cnt = charas_cnt as usize;
                    let sview = s.stack.get_view_mut(s.stack.len() - charas_cnt..);
                    let chara_nos = sort_dedup_chara_numbers(sview.as_ref(), s.i.charas_count)?;
                    intrinsics::delete_chara(&chara_nos, s.i.charas_count as _, s.ctx)?;
                    s.i.charas_count -= chara_nos.len() as u32;
                    sview.replace([]);
                }
                Bc::SwapChara => {
                    let sview = view_stack_or_bail!();
                    let [chara_no1, chara_no2] = sview.as_ref();
                    unpack_args!(chara_no1:i, chara_no2:i);
                    let chara_no1 = sanitize_chara_no(chara_no1, s.i.charas_count)?;
                    let chara_no2 = sanitize_chara_no(chara_no2, s.i.charas_count)?;
                    intrinsics::swap_chara(chara_no1 as _, chara_no2 as _, s.ctx)?;
                }
                Bc::AddCopyChara => {
                    // TODO: Bc::AddCopyChara
                    anyhow::bail!("AddCopyChara not yet implemented");
                }
                Bc::LoadData => {
                    let sv = view_stack_or_bail!();
                    let [save_id] = sv.as_ref();
                    unpack_args!(save_id:i);
                    let file = format!(".\\sav\\save{save_id:02}.sav");
                    match intrinsics::load_data(&file, s.ctx) {
                        Ok(r) => {
                            s.i.charas_count = r.charas_count;
                            let r = r.file_exists.into();
                            sv.replace([Value::new_int(r)]);
                        }
                        Err(e) => {
                            sv.replace([Value::new_int(-1)]);

                            let mut diag = Diagnostic::new();
                            diag.span_err(
                                s.cur_filename(),
                                s.cur_bc_span(),
                                format!("failed to load data: {}", e),
                            );
                            s.ctx.emit_diag(diag);
                        }
                    }
                }
                Bc::SaveData => {
                    // TODO: Bc::SaveData
                    let mut diag = Diagnostic::new();
                    diag.span_err(
                        s.cur_filename(),
                        s.cur_bc_span(),
                        "SaveData not yet implemented",
                    );
                    s.ctx.emit_diag(diag);
                }
                Bc::CheckData => {
                    let sv = view_stack_or_bail!();
                    let [save_id] = sv.as_ref();
                    unpack_args!(save_id:i);
                    let file = format!(".\\sav\\save{save_id:02}.sav");
                    match intrinsics::check_data(&file, s.ctx)
                        .map(|x| (x.status, x.timestamp, x.save_info))
                        .unwrap_or_else(|e| (4, 0, e.to_string()))
                    {
                        (0, timestamp, save_info) => {
                            let mut vresult = var_result.borrow_mut();
                            vresult.vals[0].val = timestamp as _;
                            let mut vresults = var_results.borrow_mut();
                            vresults.vals[0].val = save_info.into();
                            sv.replace([Value::new_int(0)]);
                        }
                        (status, _, error) => {
                            let mut vresults = var_results.borrow_mut();
                            vresults.vals[0].val = error.into();
                            sv.replace([Value::new_int(status)]);
                        }
                    }
                }
                Bc::GetCharaRegNum => {
                    let var_no = s.get_global_var_int("NO")?.clone();
                    let var_no = MaskedArr::try_new(var_no.borrow(), 0, -1).unwrap();
                    let sv = view_stack_or_bail!();
                    let [chara_tmpl_no] = sv.as_ref();
                    unpack_args!(chara_tmpl_no:i);
                    let r = var_no
                        .iter()
                        .position(|x| x.val == chara_tmpl_no)
                        .map(|x| x as i64)
                        .unwrap_or(-1);
                    sv.replace([Value::new_int(r)]);
                }
                Bc::LoadGlobal => {
                    // TODO: Bc::LoadGlobal
                    {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            "LoadGlobal not yet implemented",
                        );
                        s.ctx.emit_diag(diag);
                    }
                    let sv = view_stack_or_bail!();
                    let [] = sv.as_ref();
                    sv.replace([Value::new_int(0)]);
                }
                Bc::SaveGlobal => {
                    // TODO: Bc::SaveGlobal
                    {
                        let mut diag = Diagnostic::new();
                        diag.span_err(
                            s.cur_filename(),
                            s.cur_bc_span(),
                            "SaveGlobal not yet implemented",
                        );
                        s.ctx.emit_diag(diag);
                    }
                    let sv = view_stack_or_bail!();
                    let [] = sv.as_ref();
                    sv.replace([Value::new_int(0)]);
                }
                Bc::ResetData => {
                    intrinsics::reset_data(s.ctx);
                    s.charas_count = 0;
                }
                Bc::ResetCharaStain => {
                    let var_stain = s.get_global_var_int("STAIN")?.clone();
                    let var_default_stain = s.get_global_var_int("DEFAULT_STAIN")?.clone();
                    let sview = view_stack_or_bail!();
                    let [chara_no] = sview.as_ref();
                    unpack_args!(chara_no:i);
                    let var_stain = var_stain.borrow_mut();
                    let stride: usize = var_stain.dims[1..].iter().map(|&x| x as usize).product();
                    let Some(mut var_stain) =
                        MaskedArr::try_new(var_stain, (chara_no as usize * stride) as _, -1)
                    else {
                        anyhow::bail!("invalid indices into array");
                    };
                    let var_default_stain =
                        MaskedArr::try_new(var_default_stain.borrow(), 0, -1).unwrap();
                    let default_stain = var_default_stain
                        .iter()
                        .map(|x| x.val)
                        .chain(std::iter::repeat(0));
                    for (stain, default_stain) in var_stain.iter_mut().zip(default_stain) {
                        stain.val = default_stain;
                    }
                    sview.replace([]);
                }
                Bc::SaveChara { charas_cnt } => {
                    let charas_cnt = charas_cnt as usize;
                    let sv = s.stack.get_view_mut(s.stack.len() - charas_cnt - 2..);
                    let chara_nos = dedup_chara_numbers(&sv.as_ref()[2..], s.i.charas_count)?;
                    let filename = &sv[0];
                    let memo = &sv[1];
                    unpack_args!(filename:s, memo:s);
                    // TODO: Bc::SaveChara
                    anyhow::bail!("SaveChara not yet implemented");
                    sv.replace([]);
                }
                Bc::LoadChara => {
                    let sv = view_stack_or_bail!();
                    let [filename] = sv.as_ref();
                    // TODO: Bc::LoadChara
                    anyhow::bail!("LoadChara not yet implemented");
                    sv.replace([]);
                }
                Bc::GetConfig => {
                    let sv = view_stack_or_bail!();
                    let [key] = sv.as_ref();
                    unpack_args!(key:s);
                    let result = s.ctx.callback.on_get_config_int(key)?;
                    sv.replace([Value::new_int(result)]);
                }
                Bc::GetConfigS => {
                    let sv = view_stack_or_bail!();
                    let [key] = sv.as_ref();
                    unpack_args!(key:s);
                    let result = s.ctx.callback.on_get_config_str(key)?;
                    sv.replace([Value::new_str(result.into())]);
                }
                Bc::FindCharaDataFile => {
                    let sv = view_stack_or_bail!();
                    let [filename] = sv.as_ref();
                    // TODO: Bc::FindCharaDataFile
                    anyhow::bail!("FindCharaDataFile not yet implemented");
                    sv.replace([]);
                }
                _ => {
                    let mut diag = Diagnostic::new();
                    diag.span_err(
                        s.cur_filename(),
                        s.cur_bc_span(),
                        format!("unimplemented bytecode {:?}", inst),
                    );
                    s.ctx.emit_diag(diag);
                    break EraExecutionBreakReason::IllegalInstruction;
                }
            }

            let Some(new_offset) = s.cur_frame.ip.offset.checked_add_signed(ip_delta) else {
                let mut diag = Diagnostic::new();
                diag.span_err(
                    s.cur_chunk.name.clone(),
                    s.cur_chunk
                        .lookup_src(s.cur_frame.ip.offset as usize)
                        .unwrap_or_default(),
                    "bad bytecode offset",
                );
                s.ctx.emit_diag(diag);
                break EraExecutionBreakReason::IllegalInstruction;
            };
            s.cur_frame.ip.offset = new_offset;
        };

        drop_site!();

        Ok(reason)
    }
}

fn sanitize_chara_no(chara_no: i64, charas_count: u32) -> anyhow::Result<u32> {
    if chara_no < 0 || chara_no >= charas_count as i64 {
        anyhow::bail!("chara number out of bounds: {}", chara_no);
    }
    Ok(chara_no as _)
}

/// Deduplicate character numbers, preserving order.
fn dedup_chara_numbers(chara_nos: &[Value], charas_count: u32) -> anyhow::Result<Vec<u32>> {
    use itertools::Itertools;

    let chara_nos = chara_nos
        .iter()
        .map(|x| {
            x.as_int()
                .context("expected integer")
                .and_then(|x| sanitize_chara_no(x.val, charas_count))
        })
        .process_results(|x| x.unique().collect_vec())?;
    Ok(chara_nos)
}

/// Deduplicate character numbers, in ascending order.
fn sort_dedup_chara_numbers(chara_nos: &[Value], charas_count: u32) -> anyhow::Result<Vec<u32>> {
    use itertools::Itertools;

    let chara_nos = chara_nos
        .iter()
        .map(|x| {
            x.as_int()
                .context("expected integer")
                .and_then(|x| sanitize_chara_no(x.val, charas_count))
        })
        .process_results(|x| x.sorted_unstable().dedup().collect_vec())?;
    Ok(chara_nos)
}
