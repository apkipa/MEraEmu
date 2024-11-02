use std::{
    num::NonZeroUsize,
    ops::{Deref, DerefMut},
    sync::atomic::AtomicBool,
};

use anyhow::Context;
use hashbrown::HashMap;
use rclite::Rc;
use rustc_hash::FxBuildHasher;
use serde::{Deserialize, Serialize};

use crate::{
    types::*,
    util::{
        random::SimpleUniformGenerator,
        rcstr::{self, ArcStr},
        Ascii,
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

struct VecViewMut<'a, T> {
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

#[derive(Serialize, Deserialize, Debug)]
struct EraFuncExecFrame {
    stack_start: u32,
    ip: EraExecIp,
    ret_ip: EraExecIp,
    ignore_return_value: bool,
    is_transient: bool,
}

#[derive(Debug)]
pub struct EraVirtualMachineState {
    stack: Vec<Value>,
    frames: Vec<EraFuncExecFrame>,
    inner: EraVirtualMachineStateInner,
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

    // Reset the execution state and instruction pointer. Global variables remain unchanged.
    pub fn reset_exec_and_ip(&mut self, ip: EraExecIp) {
        self.state.stack.clear();
        self.state.frames.clear();
        self.state.frames.push(EraFuncExecFrame {
            stack_start: self.state.stack.len() as u32,
            ip,
            ret_ip: EraExecIp {
                chunk: 0,
                offset: 0,
            },
            ignore_return_value: true,
            is_transient: false,
        });
        // self.state.rand_gen = SimpleUniformGenerator::new();
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

struct EraVmExecSite<'ctx, 'i, 's, Callback> {
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
                // let Some((cur_frame, prev_frames)) = self.state.frames.split_last_mut() else {
                //     let mut diag = self.ctx.make_diag();
                //     diag.span_err(
                //         rcstr::literal!("<builtin>"),
                //         SrcSpan::new(SrcPos(0), 0),
                //         "no function to execute",
                //     );
                //     self.ctx.emit_diag(diag);
                //     return Ok(EraExecutionBreakReason::InternalError);
                // };
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

        make_site!();

        let mut i = 0;
        let reason = loop {
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
            let Some(inst) = EraBytecodeKind::from_bytes(bc_area) else {
                let mut diag = Diagnostic::new();
                diag.span_err(
                    s.cur_filename(),
                    s.cur_bc_span(),
                    "unexpected end of bytecode",
                );
                s.ctx.emit_diag(diag);
                break EraExecutionBreakReason::IllegalInstruction;
            };
            let mut ip_delta = inst.bytes_len() as i32;

            macro_rules! unpack_one_arg {
                ($var:ident:i) => {
                    let $var = match $var.as_unpacked() {
                        RefFlatValue::Int(x) => x.val,
                        v => {
                            let mut diag = Diagnostic::new();
                            diag.span_err(
                                s.cur_filename(),
                                s.cur_bc_span(),
                                format!("expected an integer, got {:?}", v),
                            );
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                };
                ($var:ident:s) => {
                    let $var = match $var.as_unpacked() {
                        RefFlatValue::Str(x) => &x.val,
                        v => {
                            let mut diag = Diagnostic::new();
                            diag.span_err(
                                s.cur_filename(),
                                s.cur_bc_span(),
                                format!("expected a string, got {:?}", v),
                            );
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                };
                ($var:ident:vi) => {
                    let $var = match $var.as_unpacked() {
                        RefFlatValue::ArrInt(x) => x,
                        v => {
                            let mut diag = Diagnostic::new();
                            diag.span_err(
                                s.cur_filename(),
                                s.cur_bc_span(),
                                format!("expected an integer variable, got {:?}", v),
                            );
                            s.ctx.emit_diag(diag);
                            break EraExecutionBreakReason::IllegalArguments;
                        }
                    };
                };
                ($var:ident:vs) => {
                    let $var = match $var.as_unpacked() {
                        RefFlatValue::ArrStr(x) => x,
                        v => {
                            let mut diag = Diagnostic::new();
                            diag.span_err(
                                s.cur_filename(),
                                s.cur_bc_span(),
                                format!("expected a string variable, got {:?}", v),
                            );
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
                    let mut sview = view_stack_or_bail!();
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
                    if !frame.ignore_return_value {
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

                    // Create a new execution frame for the function
                    let is_transient = func_info.is_transient;
                    let stack_start = if is_transient {
                        s.prev_frames.last().map_or(0, |f| f.stack_start as usize)
                    } else {
                        slen - args_cnt - 1
                    };
                    drop_site!();
                    self.state.frames.push(EraFuncExecFrame {
                        stack_start: stack_start as _,
                        ip: new_ip,
                        ret_ip,
                        ignore_return_value: false,
                        is_transient,
                    });
                    self.state.stack.pop();
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
                    // let new_ip = EraExecIp {
                    //     chunk: func_info.chunk_idx,
                    //     offset: func_info.bc_offset,
                    // };
                    // let ret_ip = EraExecIp {
                    //     chunk: s.cur_frame.ip.chunk,
                    //     offset: s.cur_frame.ip.offset + ip_delta as u32,
                    // };
                    if let Some((func_name, func_info)) = lookup_result {
                        // Function exists, check function argument pack
                        processed_args.push(Value::new_int(0));
                    } else {
                        // Function does not exist
                    }
                }
                // TODO...
                _ => unimplemented!("bytecode {:?}", inst),
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
