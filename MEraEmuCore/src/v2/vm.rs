#[cfg(target_arch = "aarch64")]
mod aarch64;
#[cfg(target_arch = "x86_64")]
mod x86_64;

#[cfg(target_arch = "aarch64")]
use aarch64 as arch;
#[cfg(target_arch = "x86_64")]
use x86_64 as arch;

use std::{
    borrow::Cow,
    mem::ManuallyDrop,
    num::NonZeroUsize,
    ops::{ControlFlow, Deref, DerefMut},
    ptr::NonNull,
    sync::atomic::AtomicBool,
};

use anyhow::Context;
use cstree::interning::{InternKey, Interner, Resolver, TokenKey};
use dynasmrt::{dynasm, AssemblyOffset, DynasmApi, DynasmLabelApi};
use hashbrown::{HashMap, HashSet};
use itertools::Itertools;
use rclite::Arc;
use rustc_hash::FxBuildHasher;
use serde::{Deserialize, Serialize};

use crate::{
    types::*,
    util::{
        number::formatting::csharp_format_i64,
        random::SimpleUniformGenerator,
        rcstr::{self, ArcStr, RcStr},
        AnyhowExt, Ascii,
    },
    v2::{codegen::EraCodeGenerator, lexer::EraLexer, parser::EraParser, routines},
};
use EraBytecodeKind as Bc;

type FxHashMap<K, V> = HashMap<K, V, FxBuildHasher>;
type FxHashSet<T> = HashSet<T, FxBuildHasher>;

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

    pub fn renew_start(&mut self, start: usize) {
        assert!(start <= self.vec.len(), "start out of bounds");
        self.start = start;
    }
}

impl<'a, T> std::ops::Deref for TailVecRef<'a, T> {
    type Target = [T];

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.vec[self.start..]
    }
}

impl<'a, T> std::ops::DerefMut for TailVecRef<'a, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vec[self.start..]
    }
}

// NOTE: Implementation is optimized for VM scenarios.
impl<T> TailVecRef<'_, T> {
    #[inline(always)]
    pub fn as_slice(&self) -> &[T] {
        &self.vec[self.start..]
    }

    #[inline(always)]
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        &mut self.vec[self.start..]
    }

    #[inline(always)]
    pub fn push(&mut self, value: T) {
        // self.vec.push(value);

        let len = self.vec.len();
        if crate::util::unlikely(len == self.vec.capacity()) {
            // Fallback
            self.vec.push(value);
        } else {
            // Fast path
            unsafe {
                let end = self.vec.as_mut_ptr().add(len);
                std::ptr::write(end, value);
                self.vec.set_len(len + 1);
            }
        }
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.vec.len() - self.start
    }

    #[inline(always)]
    pub fn pop(&mut self) -> Option<T> {
        if crate::util::likely(self.vec.len() > self.start) {
            // Fast path
            unsafe { Some(self.vec.pop().unwrap_unchecked()) }
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn drain<R>(&mut self, range: R) -> std::vec::Drain<'_, T>
    where
        R: std::ops::RangeBounds<usize>,
    {
        let range = self.normalize_range(range);
        let offset = self.start;
        self.vec.drain(range.start + offset..range.end + offset)
    }

    #[inline(always)]
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

    #[inline(always)]
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

    #[inline(always)]
    pub fn inner(&self) -> &Vec<T> {
        self.vec
    }

    #[inline(always)]
    pub fn inner_mut(&mut self) -> &mut Vec<T> {
        self.vec
    }

    #[inline(always)]
    pub fn must_pop_many(&mut self, count: usize) {
        if crate::util::unlikely(self.len() < count) {
            panic!(
                "stack underflow, vec len = {}, count = {}",
                self.len(),
                count
            );
        }
        self.vec.truncate(self.vec.len() - count);
    }

    #[inline(always)]
    pub fn replace_tail(&mut self, count: usize, replace_with: impl IntoIterator<Item = T>) {
        if crate::util::unlikely(self.len() < count) {
            panic!(
                "stack underflow, vec len = {}, count = {}",
                self.len(),
                count
            );
        }
        let start = self.vec.len() - count;
        // self.vec.splice(start.., replace_with);
        // Spec splice (adapted from std)
        self.vec.drain(start..);
        let mut iterator = replace_with.into_iter();
        while let Some(element) = iterator.next() {
            let len = self.vec.len();
            // Mark vec grow as cold
            if crate::util::unlikely(len == self.vec.capacity()) {
                let (lower, _) = iterator.size_hint();
                self.vec.reserve(lower.saturating_add(1));
            }
            unsafe {
                core::ptr::write(self.vec.as_mut_ptr().add(len), element);
                // Since next() executes user code which can panic we have to bump the length
                // after each step.
                // NB can't overflow since we would have had to alloc the address space
                self.vec.set_len(len + 1);
            }
        }
    }

    /// # Safety
    ///
    /// No bounds checking is performed. Also the function will not call `drop` on the removed elements.
    #[inline(always)]
    pub unsafe fn replace_tail_no_drop_unchecked_spec<const N: usize>(
        &mut self,
        count: usize,
        replace_with: [T; N],
    ) {
        unsafe {
            let new_len = self.vec.len() - count;
            self.vec.set_len(new_len);
            if N <= count {
                // Spec extend; hopefully the compiler will optimize the checks away.
                let dst = self.vec.as_mut_ptr().add(new_len);
                let src = ManuallyDrop::new(replace_with);
                std::ptr::copy_nonoverlapping(src.as_ptr(), dst, N);
                self.vec.set_len(new_len + N);
            } else {
                self.vec.extend(replace_with);
            }
        }
    }

    /// # Safety
    ///
    /// No bounds checking is performed.
    #[inline(always)]
    pub unsafe fn replace_tail_unchecked_spec<const N: usize>(
        &mut self,
        count: usize,
        replace_with: [T; N],
    ) {
        unsafe {
            let new_len = self.vec.len() - count;
            self.vec.set_len(new_len);
            for i in 0..count {
                let ptr = self.vec.as_mut_ptr().add(new_len + i);
                std::ptr::drop_in_place(ptr);
            }
            if N <= count {
                // Spec extend; hopefully the compiler will optimize the checks away.
                let dst = self.vec.as_mut_ptr().add(new_len);
                let src = ManuallyDrop::new(replace_with);
                std::ptr::copy_nonoverlapping(src.as_ptr(), dst, N);
                self.vec.set_len(new_len + N);
            } else {
                self.vec.extend(replace_with);
            }
        }
    }

    /// # Safety
    ///
    /// No bounds checking is performed. Also the function will not call `drop` on the removed elements.
    #[inline(always)]
    pub unsafe fn drain_no_drop_unchecked_spec<R>(&mut self, range: R)
    where
        R: std::ops::RangeBounds<usize>,
    {
        unsafe {
            let range = self.normalize_range(range);
            let offset = self.start;
            let start = range.start + offset;
            let end = range.end + offset;
            let len = self.vec.len();
            let ptr = self.vec.as_mut_ptr();
            let new_len = len - (end - start);
            let tail_len = len - end;
            let tail_ptr = ptr.add(end);
            std::ptr::copy(tail_ptr, ptr.add(start), tail_len);
            self.vec.set_len(new_len);
        }
    }

    #[inline(always)]
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
    #[inline(always)]
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
    /// The index of the first (normal) stack value in this frame.
    pub stack_start: u32,
    /// The index of the first (normal) stack value in this frame, without
    /// considering transient properties.
    pub real_stack_start: u32,
    /// The index of the first (dynamic) variable storage in this frame.
    pub vars_stack_start: u32,
    pub ip: EraExecIp,
    pub ret_ip: EraExecIp,
    pub ignore_return_value: bool,
    pub is_transient: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EraVirtualMachineState {
    /// The stack of values. The first value is a dummy value.
    stack: Vec<StackValue>,
    /// The stack of variable storage arrays. Intended for DYNAMIC variables.
    var_stack: Vec<ArrayValue>,
    frames: Vec<EraFuncExecFrame>,
    inner: EraVirtualMachineStateInner,
}

impl EraVirtualMachineState {
    /// Resets the execution state and instruction pointer. Global variables remain unchanged.
    pub fn reset_exec_to_ip(&mut self, ip: EraExecIp) {
        self.stack.clear();
        self.stack.push(StackValue::new_int(0)); // Stub value
        self.frames.clear();
        self.frames.push(EraFuncExecFrame {
            stack_start: self.stack.len() as _,
            real_stack_start: self.stack.len() as _,
            vars_stack_start: self.var_stack.len() as _,
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

    /// Gets the current instruction pointer.
    pub fn get_cur_ip(&self) -> Option<EraExecIp> {
        self.frames.last().map(|f| f.ip)
    }

    /// Gets the current non-transient instruction pointer.
    pub fn get_cur_non_transient_ip(&self, end: Option<usize>) -> Option<EraExecIp> {
        let frames = if let Some(end) = end {
            self.frames.get(..end)?
        } else {
            &self.frames[..]
        };
        (frames.iter().rev())
            .find(|f| !f.is_transient)
            .map(|f| f.ip)
    }

    pub fn get_cur_non_transient_exec_frame_index(&self, end: Option<usize>) -> Option<usize> {
        let frames = if let Some(end) = end {
            self.frames.get(..end)?
        } else {
            &self.frames[..]
        };
        frames.iter().enumerate().rev().find_map(
            |(i, f)| {
                if !f.is_transient {
                    Some(i)
                } else {
                    None
                }
            },
        )
    }

    pub fn diverge_transient_exec_frame_from(&self, frame_idx: usize) -> Option<EraFuncExecFrame> {
        if frame_idx >= self.frames.len() {
            return None;
        }
        let exec_frame = self.frames[..=frame_idx]
            .iter()
            .rev()
            .find(|f| !f.is_transient)?;
        let mut new_frame = exec_frame.clone();
        new_frame.real_stack_start = self.stack_len() as _;
        new_frame.vars_stack_start = self.var_stack.len() as _;
        new_frame.is_transient = true;
        Some(new_frame)
    }

    /// Sets the current instruction pointer.
    pub fn set_cur_ip(&mut self, ip: EraExecIp) -> Result<(), ()> {
        if let Some(frame) = self.frames.last_mut() {
            frame.ip = ip;
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn get_exec_frames(&self) -> &[EraFuncExecFrame] {
        &self.frames[..]
    }

    pub fn get_stack(&self) -> &[StackValue] {
        &self.stack
    }

    pub fn get_var_stack(&self) -> &[ArrayValue] {
        &self.var_stack
    }

    pub fn stack_len(&self) -> usize {
        self.stack.len()
    }

    pub fn get_stack_value(&self, idx: usize) -> Option<&StackValue> {
        self.stack.get(idx)
    }

    pub fn get_var_stack_value(&self, idx: usize) -> Option<&ArrayValue> {
        self.var_stack.get(idx)
    }

    pub fn get_var_stack_value_mut(&mut self, idx: usize) -> Option<&mut ArrayValue> {
        self.var_stack.get_mut(idx)
    }

    pub fn truncate_stack(&mut self, new_len: usize) {
        self.stack.truncate(new_len);
    }

    pub fn truncate_exec_frames(&mut self, new_len: usize) {
        self.frames.truncate(new_len);
    }

    pub fn push_exec_frame(&mut self, new_frame: EraFuncExecFrame) {
        self.frames.push(new_frame);
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EraVirtualMachineStateInner {
    #[serde(skip)]
    rand_gen: SimpleUniformGenerator,
    #[serde(skip, default = "make_default_regex_cache")]
    regex_cache: lru::LruCache<ArcStr, regex::Regex>,
    // var index (global only, no DYNAMIC vars)
    // NOTE: When serializing, we need to convert the address into `VariablePlaceRef`.
    trap_vars: FxHashSet<u32>,
    charas_count: u32,
}

impl Clone for EraVirtualMachineStateInner {
    fn clone(&self) -> Self {
        // NB: Never clone regex_cache, it is meaningless to do so.
        Self {
            rand_gen: self.rand_gen.clone(),
            regex_cache: make_default_regex_cache(),
            trap_vars: self.trap_vars.clone(),
            charas_count: self.charas_count,
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.rand_gen.clone_from(&source.rand_gen);
        // self.regex_cache = make_default_regex_cache();
        self.trap_vars.clone_from(&source.trap_vars);
        self.charas_count.clone_from(&source.charas_count);
    }
}

fn make_default_regex_cache() -> lru::LruCache<ArcStr, regex::Regex> {
    // lru::LruCache::new(NonZeroUsize::new(1023).unwrap())
    // TODO: LruCache::unbounded().clone() panics, fix it when necessary.
    lru::LruCache::unbounded()
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
            var_stack: Vec::new(),
            inner: EraVirtualMachineStateInner {
                rand_gen: SimpleUniformGenerator::new(),
                regex_cache: make_default_regex_cache(),
                trap_vars: Default::default(),
                charas_count: 0,
            },
        }
    }
}

pub struct EraVirtualMachine<'ctx, 'i, 's, Callback> {
    ctx: &'ctx mut EraCompilerCtx<'i, Callback>,
    state: &'s mut EraVirtualMachineState,
    enable_jit: bool,
}

impl<'ctx, 'i, 's, Callback: EraCompilerCallback> EraVirtualMachine<'ctx, 'i, 's, Callback> {
    pub fn new(
        ctx: &'ctx mut EraCompilerCtx<'i, Callback>,
        state: &'s mut EraVirtualMachineState,
    ) -> Self {
        Self {
            ctx,
            state,
            enable_jit: false,
        }
    }

    pub fn set_enable_jit(&mut self, enable_jit: bool) {
        self.enable_jit = enable_jit;
    }

    /// Returns Some(()) if the trap variable was successfully added.
    pub fn add_trap_var(&mut self, name: &str) -> Option<usize> {
        let Some(var_idx) = self.ctx.variables.get_var_idx(name) else {
            return None;
        };
        let var = self.ctx.variables.get_var_by_idx_mut(var_idx).unwrap();
        match var.as_unpacked_mut() {
            FlatArrayValueRefMut::ArrInt(x) => {
                x.flags.set_is_trap(true);
            }
            FlatArrayValueRefMut::ArrStr(x) => {
                x.flags.set_is_trap(true);
            }
        };
        self.state.inner.trap_vars.insert(var_idx as u32);
        Some(var_idx)
    }

    pub fn state_mut(&mut self) -> &mut EraVirtualMachineState {
        self.state
    }

    pub fn execute(&mut self, run_flag: &AtomicBool, max_inst_cnt: u64) -> EraExecutionBreakReason {
        match self.execute_inner(run_flag, max_inst_cnt) {
            Ok(reason) => reason,
            Err(err) => {
                if let Some(err) = err.downcast_ref::<FireEscapeError>() {
                    return err.0;
                }

                let mut diag = Diagnostic::new();
                if let Some(ip) = self.state.frames.last().map(|f| f.ip) {
                    let chunk = ip.chunk;
                    let chunk = self.ctx.bc_chunks.get(chunk as usize).unwrap();
                    diag.span_err(
                        chunk.name.clone(),
                        chunk.lookup_src(ip.offset as usize).unwrap_or_default(),
                        format!("{err:?}"),
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

struct EraVmExecSiteOuter<'ctx, 'i, 's, Callback> {
    ctx: &'ctx mut EraCompilerCtx<'i, Callback>,
    stack: TailVecRef<'s, StackValue>,
    var_stack: &'s mut Vec<ArrayValue>,
    cur_chunk: &'ctx EraBcChunk,
    cur_frame: &'s mut EraFuncExecFrame,
}

struct EraJitCompiledFunction {
    index: usize,
    code: dynasmrt::ExecutableBuffer,
    jit_unwind_registry_guard: Option<arch::JitUnwindRegistryGuard>,
}

impl EraJitCompiledFunction {
    fn new(
        index: usize,
        code: dynasmrt::ExecutableBuffer,
        jit_unwind_registry_guard: Option<arch::JitUnwindRegistryGuard>,
    ) -> Self {
        Self {
            index,
            code,
            jit_unwind_registry_guard,
        }
    }

    fn new_invalid(index: usize) -> Self {
        Self {
            index,
            code: dynasmrt::ExecutableBuffer::new(0).unwrap(),
            jit_unwind_registry_guard: None,
        }
    }

    fn is_invalid(&self) -> bool {
        self.code.is_empty()
    }
}

#[derive(Debug, Clone)]
struct EraJitExecFrame {
    func_idx: u32,
    ip: usize,
}

struct EraVmExecSite<'ctx, 'i, 's, Callback> {
    optr: NonNull<EraVirtualMachineState>,
    o: EraVmExecSiteOuter<'ctx, 'i, 's, Callback>,
    i: &'s mut EraVirtualMachineStateInner,
    break_reason: EraExecutionBreakReason,
    var_result_place: VariablePlaceRef,
    var_results_place: VariablePlaceRef,
    // NOTE: Some of these fields are meant to be accessed by the JIT code / related functions.
    run_flag: *const AtomicBool,
    remaining_inst_cnt: u64,
    jit_compiled_functions: Vec<Option<EraJitCompiledFunction>>,
    jit_side_frame: Vec<EraJitExecFrame>,
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

impl<'ctx, 'i, 's, Callback: EraCompilerCallback> EraVmExecSite<'ctx, 'i, 's, Callback> {
    fn try_new(
        ctx: &'ctx mut EraCompilerCtx<'i, Callback>,
        state: &'s mut EraVirtualMachineState,
        run_flag: &AtomicBool,
        remaining_inst_cnt: u64,
    ) -> anyhow::Result<Self> {
        let var_result_place = VariablePlaceRef {
            is_dynamic: false,
            index: ctx
                .variables
                .get_var_idx("RESULT")
                .context_unlikely("variable `RESULT` not found")?,
        };
        ctx.variables
            .get_var_by_idx_mut(var_result_place.index as usize)
            .unwrap()
            .ensure_alloc();
        let var_results_place: VariablePlaceRef = VariablePlaceRef {
            is_dynamic: false,
            index: ctx
                .variables
                .get_var_idx("RESULTS")
                .context_unlikely("variable `RESULTS` not found")?,
        };
        ctx.variables
            .get_var_by_idx_mut(var_results_place.index as usize)
            .unwrap()
            .ensure_alloc();
        // Make placeholder with uninitialized fields
        let mut this = unsafe {
            Self {
                optr: state.into(),
                i: &mut state.inner,
                o: EraVmExecSiteOuter {
                    ctx,
                    stack: TailVecRef::new(&mut state.stack, 0),
                    var_stack: &mut state.var_stack,
                    cur_chunk: NonNull::dangling().as_ref(),
                    cur_frame: NonNull::dangling().as_mut(),
                },
                break_reason: EraExecutionBreakReason::InternalError,
                var_result_place,
                var_results_place,
                run_flag,
                remaining_inst_cnt,
                jit_compiled_functions: Vec::new(),
                jit_side_frame: Vec::new(),
            }
        };
        this.remake_site()?;
        Ok(this)
    }

    // TODO: Mark as unsafe
    /// Rebuilds the execution site to point to the current function being executed.
    fn remake_site(&mut self) -> anyhow::Result<()> {
        let o = unsafe { self.optr.as_mut() };
        let cur_frame = o
            .frames
            .last_mut()
            .context_unlikely("no function to execute")?;
        let cur_chunk = (self.o.ctx.bc_chunks)
            .get(cur_frame.ip.chunk as usize)
            .context_unlikely("chunk not found")?;
        self.i = &mut o.inner;
        self.o.stack = TailVecRef::new(&mut o.stack, cur_frame.stack_start as _);
        self.o.var_stack = &mut o.var_stack;
        self.o.cur_chunk = unsafe { std::mem::transmute(cur_chunk) };
        self.o.cur_frame = cur_frame;
        Ok(())
    }

    fn add_ip_offset(&mut self, offset: i32) {
        self.o.cur_frame.ip.offset = self.o.cur_frame.ip.offset.wrapping_add_signed(offset);
    }

    pub fn get_cur_ip(&self) -> EraExecIp {
        self.o.cur_frame.ip
    }

    pub fn ensure_call_stack_and_get_o(
        &self,
    ) -> anyhow::Result<&'static mut EraVirtualMachineState> {
        // TODO: UB?
        // Only ctx is a valid reference after the call.
        let o = unsafe { &mut *self.optr.as_ptr() };
        if o.frames.len() >= MAX_CALL_DEPTH {
            crate::util::cold();

            anyhow::bail!("maximum call depth exceeded");

            // self.remake_site().unwrap();

            // let mut diag = Diagnostic::new();
            // diag.span_err(
            //     self.o.cur_filename(),
            //     self.o.cur_bc_span(),
            //     "maximum call depth exceeded",
            // );
            // self.o.ctx.emit_diag(diag);
            // self.break_reason = EraExecutionBreakReason::InternalError;
            // return Err(FireEscapeError(self.break_reason).into());
        }
        Ok(o)
    }
}

impl<Callback: EraCompilerCallback> EraVmExecSiteOuter<'_, '_, '_, Callback> {
    fn cur_filename(&self) -> ArcStr {
        self.cur_chunk.name.clone()
    }

    fn cur_bc_span(&self) -> SrcSpan {
        self.cur_chunk
            .lookup_src(self.cur_frame.ip.offset as usize)
            .unwrap_or_default()
    }

    fn span_msg_here(
        &self,
        diag: &mut Diagnostic,
        level: DiagnosticLevel,
        message: impl Into<String>,
    ) {
        let filename = self.cur_filename();
        let span = self.cur_bc_span();
        diag.span_msg(filename, span, level, message);
    }

    fn span_err_here(&self, diag: &mut Diagnostic, message: impl Into<String>) {
        self.span_msg_here(diag, DiagnosticLevel::Error, message);
    }

    fn span_warn_here(&self, diag: &mut Diagnostic, message: impl Into<String>) {
        self.span_msg_here(diag, DiagnosticLevel::Warning, message);
    }

    fn span_note_here(&self, diag: &mut Diagnostic, message: impl Into<String>) {
        self.span_msg_here(diag, DiagnosticLevel::Note, message);
    }
}

impl<'i, Callback: EraCompilerCallback> EraVmExecSiteOuter<'_, 'i, '_, Callback> {
    /// Retrieves the current function being executed.
    pub fn get_current_function(&self) -> Option<&EraFuncInfo<'i>> {
        let cur_ip = self.cur_frame.ip;
        self.ctx
            .func_info_from_chunk_pos(cur_ip.chunk as _, cur_ip.offset as _)
    }

    pub fn resolve_variable_place(&self, place: VariablePlaceRef) -> Option<&ArrayValue> {
        let var_idx = place.index as usize;
        if place.is_dynamic {
            self.var_stack.get(var_idx)
        } else {
            self.ctx.variables.get_var_by_idx(var_idx)
        }
    }

    pub fn resolve_variable_place_anyhow(
        &self,
        place: VariablePlaceRef,
    ) -> anyhow::Result<&ArrayValue> {
        self.resolve_variable_place(place)
            .with_context_unlikely(|| format!("array {:?} not found", place))
    }

    pub fn resolve_variable_place_mut(
        &mut self,
        place: VariablePlaceRef,
    ) -> Option<&mut ArrayValue> {
        let var_idx = place.index as usize;
        if place.is_dynamic {
            self.var_stack.get_mut(var_idx)
        } else {
            self.ctx.variables.get_var_by_idx_mut(var_idx)
        }
    }

    pub fn resolve_variable_place_mut_anyhow(
        &mut self,
        place: VariablePlaceRef,
    ) -> anyhow::Result<&mut ArrayValue> {
        self.resolve_variable_place_mut(place)
            .with_context_unlikely(|| format!("array {:?} not found", place))
    }

    /// Retrieves a variable in the current execution scope.
    pub fn get_var_by_name(&self, name: &str) -> Option<&ArrayValue> {
        let cur_func = self.get_current_function()?;
        if let Some(var) = cur_func.frame_info.vars.get(Ascii::new_str(name)) {
            let idx = var.var_idx as usize;
            if var.in_local_frame {
                let RefFlatStackValue::ArrRef(&var_place) =
                    self.stack.get(idx).map(StackValue::as_unpacked)?
                else {
                    return None;
                };
                self.resolve_variable_place(var_place)
            } else {
                self.ctx.variables.get_var_by_idx(idx)
            }
        } else if let Some(var) = self.ctx.variables.get_var(name) {
            Some(var)
        } else {
            None
        }
    }

    pub fn get_global_var_int(&self, name: &str) -> anyhow::Result<&ArrIntValue> {
        let var = (self.ctx.variables)
            .get_var(name)
            .with_context_unlikely(|| format!("variable `{}` not found", name))?;
        var.as_arrint()
            .with_context_unlikely(|| format!("variable `{}` is not ArrInt", name))
    }

    pub fn get_global_var_str(&self, name: &str) -> anyhow::Result<&ArrStrValue> {
        let var = (self.ctx.variables)
            .get_var(name)
            .with_context_unlikely(|| format!("variable `{}` not found", name))?;
        var.as_arrstr()
            .with_context_unlikely(|| format!("variable `{}` is not ArrStr", name))
    }

    pub fn get_global_var_int_mut(&mut self, name: &str) -> anyhow::Result<&mut ArrIntValue> {
        let var = (self.ctx.variables)
            .get_var_mut(name)
            .with_context_unlikely(|| format!("variable `{}` not found", name))?;
        var.as_arrint_mut()
            .with_context_unlikely(|| format!("variable `{}` is not ArrInt", name))
    }

    pub fn get_global_var_str_mut(&mut self, name: &str) -> anyhow::Result<&mut ArrStrValue> {
        let var = (self.ctx.variables)
            .get_var_mut(name)
            .with_context_unlikely(|| format!("variable `{}` not found", name))?;
        var.as_arrstr_mut()
            .with_context_unlikely(|| format!("variable `{}` is not ArrStr", name))
    }

    pub unsafe fn get_global_var_int_mut_unchecked(
        &self,
        name: &str,
    ) -> anyhow::Result<&mut ArrIntValue> {
        let var = (self.ctx.variables)
            .get_var(name)
            .with_context_unlikely(|| format!("variable `{}` not found", name))?;
        var.as_arrint_mut_unchecked()
            .with_context_unlikely(|| format!("variable `{}` is not ArrInt", name))
    }

    pub unsafe fn get_global_var_str_mut_unchecked(
        &self,
        name: &str,
    ) -> anyhow::Result<&mut ArrStrValue> {
        let var = (self.ctx.variables)
            .get_var(name)
            .with_context_unlikely(|| format!("variable `{}` not found", name))?;
        var.as_arrstr_mut_unchecked()
            .with_context_unlikely(|| format!("variable `{}` is not ArrStr", name))
    }

    pub fn get_variable_name_from_ptr(&self, value: *const ()) -> Option<&str> {
        self.ctx.variables.iter().find_map(|v| {
            let addr = v.val.as_untagged_ptr().as_ptr() as *const ();
            (addr == value).then(|| v.name.as_ref())
        })
    }

    pub fn get_variable_name_from_value(&self, value: &ArrayValue) -> Option<&str> {
        let var_addr = value.as_untagged_ptr().as_ptr() as *const ();
        self.get_variable_name_from_ptr(var_addr)
    }
}

const MAX_CALL_DEPTH: usize = 1024;

/// A sentinel error type for when the VM needs to escape execution without emitting an error.
struct FireEscapeError(EraExecutionBreakReason);
impl std::fmt::Debug for FireEscapeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("fire escape")
    }
}
impl std::fmt::Display for FireEscapeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("fire escape")
    }
}
impl std::error::Error for FireEscapeError {}

// ----- Start of VM Instruction Implementations -----
macro_rules! latter_expr {
    ($a:expr, $b:expr) => {
        $b
    };
}

macro_rules! view_stack_inner_count {
    ($var:ident) => {
        1
    };
    ($var:ident:$count:expr) => {
        $count
    };
}

macro_rules! view_stack_inner_item {
    ($s:expr, $var:ident:any, $invals:expr) => {
        let $var = &$invals[0];
    };
    ($s:expr, $var:ident:any:$count:expr, $invals:expr) => {
        let $var = &$invals[..$count];
    };
    ($s:expr, $var:ident:i, $invals:expr) => {
        let $var = match $invals[0].as_unpacked() {
            RefFlatStackValue::Int(x) => x.val,
            v => {
                crate::util::cold();

                let msg = format!("expected an integer, got {:?}", v);
                let mut diag = Diagnostic::new();
                diag.span_err($s.o.cur_filename(), $s.o.cur_bc_span(), msg);
                $s.o.ctx.emit_diag(diag);
                $s.break_reason = EraExecutionBreakReason::IllegalArguments;
                return Err(FireEscapeError($s.break_reason).into());
            }
        };
    };
    ($s:expr, $var:ident:s, $invals:expr) => {
        let $var = match $invals[0].as_unpacked() {
            RefFlatStackValue::Str(x) => &x.val,
            v => {
                crate::util::cold();

                let msg = format!("expected a string, got {:?}", v);
                let mut diag = Diagnostic::new();
                diag.span_err($s.o.cur_filename(), $s.o.cur_bc_span(), msg);
                $s.o.ctx.emit_diag(diag);
                $s.break_reason = EraExecutionBreakReason::IllegalArguments;
                return Err(FireEscapeError($s.break_reason).into());
            }
        };
    };
    ($s:expr, $var:ident:a, $invals:expr) => {
        let $var = match $invals[0].as_unpacked() {
            RefFlatStackValue::ArrRef(x) => *x,
            v => {
                crate::util::cold();

                let msg = format!("expected an array reference, got {:?}", v);
                let mut diag = Diagnostic::new();
                diag.span_err($s.o.cur_filename(), $s.o.cur_bc_span(), msg);
                $s.o.ctx.emit_diag(diag);
                $s.break_reason = EraExecutionBreakReason::IllegalArguments;
                return Err(FireEscapeError($s.break_reason).into());
            }
        };
    };
    ($s:expr, $var:ident:b, $invals:expr) => {
        let $var = match $invals[0].as_unpacked() {
            RefFlatStackValue::Int(x) => x.val != 0,
            v => {
                crate::util::cold();

                let msg = format!("expected an integer, got {:?}", v);
                let mut diag = Diagnostic::new();
                diag.span_err($s.o.cur_filename(), $s.o.cur_bc_span(), msg);
                $s.o.ctx.emit_diag(diag);
                $s.break_reason = EraExecutionBreakReason::IllegalArguments;
                return Err(FireEscapeError($s.break_reason).into());
            }
        };
    };
}

macro_rules! view_stack {
    ($s:expr, $read_count:tt, $($var:ident:$kind:ident $(:$count:expr)?),* $(,)?) => {
        let items_counts = [$(view_stack_inner_count!($var $(:$count)?) as usize),*];
        let items_count: usize = items_counts.iter().sum();
        let $read_count = items_count;
        let stack_len = $s.o.stack.len();
        if crate::util::unlikely(stack_len < items_count) {
            return Err(anyhow::anyhow!("function stack underflow"));
        }
        let mut _offset = 0;
        let mut _i = 0;
        $(
            view_stack_inner_item!($s, $var:$kind $(:latter_expr!($count, items_counts[_i]))?, &$s.o.stack[stack_len - items_count + _offset..]);
            _offset += items_counts[_i];
            _i += 1;
        )*
    };
}

macro_rules! resolve_array_inner_item {
    ($s:expr, $var:ident) => {
        let $var = $s
            .resolve_variable_place($var)
            .with_context_unlikely(|| format!("array {:?} not found", $var))?;
    };
    ($s:expr, $var:ident:i) => {
        let $var =
            $s.o.resolve_variable_place($var)
                .with_context_unlikely(|| format!("array {:?} not found", $var))?;
        let $var = match $var.as_unpacked() {
            FlatArrayValueRef::ArrInt(x) => x,
            FlatArrayValueRef::ArrStr(_) => {
                crate::util::cold();

                anyhow::bail!("expected ArrInt, got ArrStr");
            }
        };
    };
    ($s:expr, $var:ident:s) => {
        let $var =
            $s.o.resolve_variable_place($var)
                .with_context_unlikely(|| format!("array {:?} not found", $var))?;
        let $var = match $var.as_unpacked() {
            FlatArrayValueRef::ArrStr(x) => x,
            FlatArrayValueRef::ArrInt(_) => {
                crate::util::cold();

                anyhow::bail!("expected ArrStr, got ArrInt");
            }
        };
    };
    ($s:expr, $var:ident:a) => {
        let $var = $s
            .resolve_variable_place($var)
            .with_context_unlikely(|| format!("array {:?} not found", $var))?;
    };
}

macro_rules! resolve_array_inner_post_item {
    ($s:expr, $var:ident;$varidx:expr) => {
        let $var = $var
            .flat_get($varidx as _)
            .context_unlikely("invalid indices into array")?;
    };
    ($s:expr, $var:ident;$varidx:expr;$dimpos:expr) => {
        let $var = MaskedArr::try_new($var, $varidx as _, $dimpos as _)
            .context_unlikely("invalid indices into array")?;
    };
}

macro_rules! resolve_array_mut_unsafe_inner_item {
    ($s:expr, $var:ident) => {
        let $var = $s
            .resolve_variable_place($var)
            .with_context_unlikely(|| format!("array {:?} not found", $var))?;
    };
    ($s:expr, $var:ident:i) => {
        let $var =
            $s.o.resolve_variable_place($var)
                .with_context_unlikely(|| format!("array {:?} not found", $var))?;
        let $var = match unsafe { $var.as_unpacked_mut_unchecked() } {
            FlatArrayValueRefMut::ArrInt(x) => x,
            FlatArrayValueRefMut::ArrStr(_) => {
                crate::util::cold();

                anyhow::bail!("expected ArrInt, got ArrStr");
            }
        };
    };
    ($s:expr, $var:ident:s) => {
        let $var =
            $s.o.resolve_variable_place($var)
                .with_context_unlikely(|| format!("array {:?} not found", $var))?;
        let $var = match unsafe { $var.as_unpacked_mut_unchecked() } {
            FlatArrayValueRefMut::ArrStr(x) => x,
            FlatArrayValueRefMut::ArrInt(_) => {
                crate::util::cold();

                anyhow::bail!("expected ArrStr, got ArrInt");
            }
        };
    };
    ($s:expr, $var:ident:a) => {
        let $var =
            $s.o.resolve_variable_place($var)
                .with_context_unlikely(|| format!("array {:?} not found", $var))?;
    };
}

macro_rules! resolve_array_mut_unsafe_inner_post_item {
    ($s:expr, $var:ident;$varidx:expr) => {
        let $var = $var
            .flat_get_mut($varidx as _)
            .context_unlikely("invalid indices into array")?;
    };
    ($s:expr, $var:ident;$varidx:expr;$dimpos:expr) => {
        let mut $var = MaskedArr::try_new($var, $varidx as _, $dimpos as _)
            .context_unlikely("invalid indices into array")?;
    };
}

/// # Safety
///
/// See `ArrayValue::as_unpacked_mut_unchecked`.
macro_rules! resolve_array_mut_unchecked {
    ($s:expr, $($var:ident $(:$kind:ident)?),* $(,)?) => {
        $(
            resolve_array_mut_unsafe_inner_item!($s, $var $(:$kind)?);
        )*
    };
}

/// # Safety
///
/// When arrays are only accessed through this macro (invoked exactly once), it is safe. Otherwise,
/// see `ArrayValue::as_unpacked_mut_unchecked`.
macro_rules! resolve_array_mut_unsafe {
    ($s:expr, $($var:ident $(:$kind:ident)? $(;$varidx:expr $(;$dimpos:expr)?)?),* $(,)?) => {
        {
            let _check_equality = [$(($var, stringify!($var))),*];
            for i in 1.._check_equality.len() {
                for j in 0..i {
                    if crate::util::unlikely(_check_equality[i].0 == _check_equality[j].0) {
                        anyhow::bail!("variable collision: {:?} versus {:?}", _check_equality[i].1, _check_equality[j].1);
                    }
                }
            }
        }
        $(
            resolve_array_mut_unsafe_inner_item!($s, $var $(:$kind)?);
            $(
                resolve_array_mut_unsafe_inner_post_item!($s, $var;$varidx $(;$dimpos)?);
            )?
        )*
    };
}

macro_rules! resolve_array {
    ($s:expr, $($var:ident $(:$kind:ident)? $(;$varidx:expr $(;$dimpos:expr)?)?),* $(,)?) => {
        // NOTE: No need to check collisions here, as we are only reading.
        $(
            resolve_array_inner_item!($s, $var $(:$kind)?);
            $(
                resolve_array_inner_post_item!($s, $var;$varidx $(;$dimpos)?);
            )?
        )*
    };
}

macro_rules! resolve_array_kind {
    ($s:expr, $var:ident) => {
        $s.o.resolve_variable_place($var)
            .with_context_unlikely(|| format!("array {:?} not found", $var))?
            .kind()
    };
}

macro_rules! resolve_any_scalar {
    ($s:expr, $($var:ident:$kind:ident),* $(,)?) => {
        $(
            view_stack_inner_item!($s, $var:$kind, [$var]);
        )*
    };
}

trait ControlFlowExt<B, C> {
    fn continue_anyhow(self) -> anyhow::Result<C>;
}

impl<C> ControlFlowExt<(), C> for ControlFlow<(), C> {
    fn continue_anyhow(self) -> anyhow::Result<C> {
        match self {
            ControlFlow::Continue(x) => Ok(x),
            ControlFlow::Break(_) => {
                crate::util::cold();

                Err(FireEscapeError(EraExecutionBreakReason::CallbackBreak).into())
            }
        }
    }
}

#[derive(Debug)]
pub struct CheckSaveHeaderResult {
    pub save_type: crate::v2::savefs::EraSaveFileType,
    pub status: i64,
    pub timestamp: u64,
    pub save_info: String,
}

#[derive(Debug)]
pub struct LoadDataResult {
    pub file_exists: bool,
    pub charas_count: Option<u32>,
}

#[derive(Debug)]
pub struct CheckDataResult {
    pub status: i64,
    pub timestamp: u64,
    pub save_info: String,
}

impl<'i, Callback: EraCompilerCallback> EraVmExecSiteOuter<'_, 'i, '_, Callback> {
    fn routine_check_save_header(
        &mut self,
        save_header: crate::v2::savefs::EraSaveFileHeader,
    ) -> anyhow::Result<CheckSaveHeaderResult> {
        use anyhow::bail;

        let version = save_header.version;
        if version != 1808 {
            bail!("unsupported version {version}");
        }
        let save_type = save_header.file_type;
        // if !matches!(save_type, EraSaveFileType::Normal) {
        //     bail!("invalid save file type {save_file_type:?}");
        // }

        let get_var_i32_0d = |name| {
            (self.ctx.variables.get_var(name)).and_then(|x| x.as_arrint().map(|x| x.vals[0].val))
        };
        let cur_game_code = get_var_i32_0d("GAMEBASE_GAMECODE").unwrap_or(0);
        let cur_game_ver = get_var_i32_0d("GAMEBASE_VERSION").unwrap_or(0);
        let cur_game_min_ver = get_var_i32_0d("GAMEBASE_ALLOWVERSION").unwrap_or(0);
        // Check game code
        let game_code = save_header.game_code;
        if !(game_code == 0 || game_code == cur_game_code) {
            return Ok(CheckSaveHeaderResult {
                save_type,
                status: 2,
                timestamp: 0,
                save_info: String::new(),
            });
        }
        // Check game version
        let game_ver = save_header.game_version;
        if !(game_ver >= cur_game_min_ver || game_ver == cur_game_ver) {
            return Ok(CheckSaveHeaderResult {
                save_type,
                status: 3,
                timestamp: 0,
                save_info: String::new(),
            });
        }

        let timestamp = 0;
        let save_info = save_header.save_info;
        Ok(CheckSaveHeaderResult {
            save_type,
            status: 0,
            timestamp,
            save_info,
        })
    }

    fn routine_load_data(
        &mut self,
        save_path: &str,
        reset: bool,
        expected_file_type: crate::v2::savefs::EraSaveFileType,
    ) -> anyhow::Result<LoadDataResult> {
        use crate::util::io::CSharpBinaryReader;
        use crate::v2::savefs::*;
        use anyhow::bail;
        use binrw::BinReaderExt;
        use num_traits::FromPrimitive;

        if !self.ctx.callback.on_check_host_file_exists(save_path)? {
            return Ok(LoadDataResult {
                file_exists: false,
                charas_count: None,
            });
        }
        let file = self.ctx.callback.on_open_host_file(save_path, false)?;
        let mut file = std::io::BufReader::new(file);

        let save_header: EraSaveFileHeader = file.read_le()?;
        let CheckSaveHeaderResult {
            save_type: file_type,
            ..
        } = self.routine_check_save_header(save_header)?;

        if reset {
            // Reset data before loading
            self.routine_reset_data()?;
        }

        // Check save file type
        if file_type != expected_file_type {
            anyhow::bail!("invalid save file type {file_type:?}, expected {expected_file_type:?}");
        }

        let charas_count = if file_type == EraSaveFileType::Normal {
            // Load character variables
            let charas_count = file
                .read_i64()?
                .try_into()
                .context_unlikely("invalid character count")?;
            let old_charas_cap = (self.ctx.variables)
                .charas_var_capacity()
                .context_unlikely("no chara variables")?;
            if charas_count > old_charas_cap {
                // Grow chara variables
                use crate::v2::engine::CHARA_CAP_GROWTH_STEP;
                let grow_count = (charas_count - old_charas_cap + CHARA_CAP_GROWTH_STEP - 1)
                    / CHARA_CAP_GROWTH_STEP
                    * CHARA_CAP_GROWTH_STEP;
                self.ctx.variables.grow_charas_var_capacity(grow_count);
            }
            for chara_i in 0..charas_count {
                loop {
                    let var_type = EraSaveDataType::from_u8(file.read_u8()?)
                        .context_unlikely("invalid save data type")?;
                    match var_type {
                        EraSaveDataType::Separator => continue,
                        EraSaveDataType::EOC | EraSaveDataType::EOF => break,
                        _ => {
                            let var_name = file.read_utf16_string()?;
                            let var = (self.ctx.variables)
                                .get_var_info_by_name_mut(&var_name)
                                .with_context_unlikely(|| {
                                    format!("variable `{}` does not exist", var_name)
                                })?;
                            if !var.is_charadata {
                                bail!("variable `{}` is not CHARADATA", var_name);
                            }
                            file.read_var(var_type, var, Some(chara_i as _))
                                .with_context_unlikely(|| {
                                    format!("read variable `{}` failed", var_name)
                                })?;
                        }
                    }
                }
            }
            Some(charas_count)
        } else {
            None
        };

        // Load normal variables
        loop {
            let var_type = EraSaveDataType::from_u8(file.read_u8()?)
                .context_unlikely("invalid save data type")?;
            match var_type {
                EraSaveDataType::EOF => break,
                _ => {
                    let var_name = file.read_utf16_string()?;
                    let var = (self.ctx.variables)
                        .get_var_info_by_name_mut(&var_name)
                        .with_context_unlikely(|| {
                            format!("variable `{}` does not exist", var_name)
                        })?;
                    if var.is_charadata {
                        bail!("variable `{}` is CHARADATA", var_name);
                    }
                    file.read_var(var_type, var, None)
                        .with_context_unlikely(|| format!("read variable `{}` failed", var_name))?;
                }
            }
        }

        Ok(LoadDataResult {
            file_exists: true,
            charas_count,
        })
    }

    fn routine_save_data(
        &mut self,
        save_path: &str,
        save_info: &str,
        charas_count: u32,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryWriter;
        use crate::v2::savefs::*;
        use binrw::BinWrite;

        let file = self.ctx.callback.on_open_host_file(save_path, true)?;
        let mut file = std::io::BufWriter::new(file);

        let game_code = (self.ctx.variables)
            .get_var_i_0("GAMEBASE_GAMECODE")
            .unwrap_or(0);
        let game_ver = (self.ctx.variables)
            .get_var_i_0("GAMEBASE_VERSION")
            .unwrap_or(0);

        // Write save file header
        let header = EraSaveFileHeader {
            version: 1808,
            data: Vec::new(),
            file_type: EraSaveFileType::Normal,
            game_code,
            game_version: game_ver,
            save_info: save_info.to_owned(),
        };
        header.write_le(&mut file)?;

        // Write charas variables
        file.write_i64(charas_count as _)?;
        for chara_i in 0..charas_count {
            let mut user_chara_vars = Vec::new();

            fn write_one_chara_var(
                file: &mut (impl std::io::Write + std::io::Seek),
                var: &EraVarInfo,
                chara_i: u32,
            ) -> anyhow::Result<()> {
                let is_str = var.val.is_arrstr();
                let is_nodim = routines::is_chara_nodim(var.name.as_ref());
                let var_dims_cnt = var.val.dims_cnt() - 1 - (is_nodim as usize);
                let var_type = EraSaveDataType::new_var(is_str, var_dims_cnt)
                    .with_context_unlikely(|| format!("cannot save variable `{}`", var.name))?;
                file.write_u8(var_type as _)?;
                file.write_utf16_string(var.name.as_ref())?;
                file.write_var(var_type, var, Some(chara_i as _))?;
                Ok(())
            }

            // First write built-in chara variables
            for var in self.ctx.variables.chara_vars_iter() {
                if !var.is_savedata || !var.is_charadata || var.is_global {
                    continue;
                }

                if routines::is_builtin_chara_var(var.name.as_ref()) {
                    write_one_chara_var(&mut file, var, chara_i)?;
                } else {
                    user_chara_vars.push(var);
                }
            }

            // Then write user-defined chara variables
            if !user_chara_vars.is_empty() {
                file.write_u8(EraSaveDataType::Separator as _)?;
                for var in user_chara_vars {
                    write_one_chara_var(&mut file, var, chara_i)?;
                }
            }

            // Write separator
            file.write_u8(EraSaveDataType::EOC as _)?;
        }

        // Write normal variables
        for var in self.ctx.variables.iter() {
            if !var.is_savedata || var.is_charadata || var.is_global {
                continue;
            }

            let is_str = var.val.is_arrstr();
            let var_dims_cnt = var.val.dims_cnt();
            let var_type = EraSaveDataType::new_var(is_str, var_dims_cnt)
                .with_context_unlikely(|| format!("cannot save variable `{}`", var.name))?;
            file.write_u8(var_type as _)?;
            file.write_utf16_string(var.name.as_ref())?;
            file.write_var(var_type, var, None)?;
        }

        // Write EOF
        file.write_u8(EraSaveDataType::EOF as _)?;

        Ok(())
    }

    fn routine_save_global_data(&mut self, save_path: &str) -> anyhow::Result<()> {
        // todo!()
        anyhow::bail!("routine_save_global_data not yet implemented")
    }

    fn routine_check_data(&mut self, save_path: &str) -> anyhow::Result<CheckDataResult> {
        use crate::v2::savefs::*;
        use binrw::BinReaderExt;

        if !self.ctx.callback.on_check_host_file_exists(save_path)? {
            return Ok(CheckDataResult {
                status: 1,
                timestamp: 0,
                save_info: String::new(),
            });
        }
        let file = self.ctx.callback.on_open_host_file(save_path, false)?;
        let mut file = std::io::BufReader::new(file);

        let save_header: EraSaveFileHeader = file.read_le()?;
        let result = self.routine_check_save_header(save_header)?;
        if result.save_type != EraSaveFileType::Normal {
            anyhow::bail!("invalid save file type {:?}", result.save_type);
        }

        Ok(CheckDataResult {
            status: result.status,
            timestamp: result.timestamp,
            save_info: result.save_info,
        })
    }

    fn routine_reset_data(&mut self) -> anyhow::Result<()> {
        // TODO: Fully reset data according to Emuera
        self.ctx.variables.reset_variables();
        self.ctx.variables.reinit_variables();
        Ok(())
    }
}

impl<'i, Callback: EraCompilerCallback> EraVmExecSite<'_, 'i, '_, Callback> {
    fn ensure_pre_step_instruction(&mut self) -> anyhow::Result<()> {
        let run_flag = unsafe { &*self.run_flag };
        if !run_flag.load(std::sync::atomic::Ordering::Relaxed) {
            self.break_reason = EraExecutionBreakReason::StopFlag;
            return Err(FireEscapeError(self.break_reason).into());
        }
        if self.remaining_inst_cnt == 0 {
            self.break_reason = EraExecutionBreakReason::ReachedMaxInstructions;
            return Err(FireEscapeError(self.break_reason).into());
        }
        self.remaining_inst_cnt -= 1;
        Ok(())
    }

    fn instr_fail_with_msg(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let msg = if self.o.stack.len() < 1 {
            "<invalid>".to_owned()
        } else {
            view_stack!(self, _, msg:any);
            match msg.as_unpacked() {
                RefFlatStackValue::Int(x) => x.val.to_string(),
                RefFlatStackValue::Str(x) => x.val.to_string(),
                RefFlatStackValue::ArrRef(x) => format!("{{array ref {:?}}}", x),
            }
        };
        let mut diag = Diagnostic::new();
        let msg = format!("failed: {}", msg);
        diag.span_err(self.o.cur_filename(), self.o.cur_bc_span(), msg);
        self.o.ctx.emit_diag(diag);
        self.break_reason = EraExecutionBreakReason::FailInstruction;
        Err(FireEscapeError(self.break_reason).into())
    }

    fn instr_debug_break(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        self.break_reason = EraExecutionBreakReason::DebugBreakInstruction;
        Err(FireEscapeError(self.break_reason).into())
    }

    fn instr_quit(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        self.break_reason = EraExecutionBreakReason::CodeQuit;
        Err(FireEscapeError(self.break_reason).into())
    }

    fn instr_throw(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let msg = if self.o.stack.len() < 1 {
            "<invalid>".to_owned()
        } else {
            view_stack!(self, _, msg:any);
            match msg.as_unpacked() {
                RefFlatStackValue::Int(x) => x.val.to_string(),
                RefFlatStackValue::Str(x) => x.val.to_string(),
                RefFlatStackValue::ArrRef(x) => format!("{{array ref {:?}}}", x),
            }
        };
        let mut diag = Diagnostic::new();
        let msg = format!("THROW: {}", msg);
        diag.span_err(self.o.cur_filename(), self.o.cur_bc_span(), msg);
        self.o.ctx.emit_diag(diag);
        self.break_reason = EraExecutionBreakReason::CodeThrows;
        Err(FireEscapeError(self.break_reason).into())
    }

    fn instr_nop(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        self.add_ip_offset(Bc::Nop.bytes_len() as _);
        Ok(())
    }

    fn instr_return_void(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        // DANGER: Touches the site execution state. Watch out for UB.
        let o = unsafe { self.optr.as_mut() };
        if o.frames.len() <= 1 {
            anyhow::bail!("no function to return to");
        }
        let cur_frame = o.frames.pop().unwrap();
        o.stack.truncate(cur_frame.real_stack_start as _);
        o.var_stack.truncate(cur_frame.vars_stack_start as _);
        if cur_frame.is_transient {
            // Remove transient source (should be the latest one)
            let source_map = Arc::get_mut(&mut self.o.ctx.i.source_map)
                .context_unlikely("source_map is shared")?;
            let func_entries = Arc::get_mut(&mut self.o.ctx.i.func_entries)
                .context_unlikely("function_entries is shared")?;
            let bc_chunks = Arc::get_mut(&mut self.o.ctx.i.bc_chunks)
                .context_unlikely("bc_chunks is shared")?;
            if let Some(chunk) = bc_chunks.last() {
                use indexmap::map::Entry;
                match source_map.entry(chunk.name.clone()) {
                    Entry::Occupied(e) if e.get().is_transient => {
                        e.swap_remove();
                        let func_name = chunk.name.clone();
                        func_entries.swap_remove(Ascii::new_str(&func_name));
                        bc_chunks.pop();
                    }
                    _ => (),
                }
            }
        }
        self.remake_site()?;
        self.o.cur_frame.ip = cur_frame.ret_ip;
        Ok(())
    }

    fn instr_return_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        // DANGER: Touches the site execution state. Watch out for UB.
        view_stack!(self, _, val:i);
        let val = val.clone();
        let o = unsafe { self.optr.as_mut() };
        if o.frames.len() <= 1 {
            anyhow::bail!("no function to return to");
        }
        let cur_frame = o.frames.pop().unwrap();
        o.stack.truncate(cur_frame.real_stack_start as _);
        o.var_stack.truncate(cur_frame.vars_stack_start as _);
        if cur_frame.is_transient {
            // Remove transient source (should be the latest one)
            let source_map = Arc::get_mut(&mut self.o.ctx.i.source_map)
                .context_unlikely("source_map is shared")?;
            let func_entries = Arc::get_mut(&mut self.o.ctx.i.func_entries)
                .context_unlikely("function_entries is shared")?;
            let bc_chunks = Arc::get_mut(&mut self.o.ctx.i.bc_chunks)
                .context_unlikely("bc_chunks is shared")?;
            if let Some(chunk) = bc_chunks.last() {
                use indexmap::map::Entry;
                match source_map.entry(chunk.name.clone()) {
                    Entry::Occupied(e) if e.get().is_transient => {
                        e.swap_remove();
                        let func_name = chunk.name.clone();
                        func_entries.swap_remove(Ascii::new_str(&func_name));
                        bc_chunks.pop();
                    }
                    _ => (),
                }
            }
        }
        self.remake_site()?;
        self.o.cur_frame.ip = cur_frame.ret_ip;
        if !cur_frame.ignore_return_value {
            self.o.stack.push(StackValue::new_int(val));
        }
        Ok(())
    }

    fn instr_return_str(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        // DANGER: Touches the site execution state. Watch out for UB.
        view_stack!(self, _, val:s);
        let val = val.clone();
        let o = unsafe { self.optr.as_mut() };
        if o.frames.len() <= 1 {
            anyhow::bail!("no function to return to");
        }
        let cur_frame = o.frames.pop().unwrap();
        o.stack.truncate(cur_frame.real_stack_start as _);
        o.var_stack.truncate(cur_frame.vars_stack_start as _);
        if cur_frame.is_transient {
            // Remove transient source (should be the latest one)
            let source_map = Arc::get_mut(&mut self.o.ctx.i.source_map)
                .context_unlikely("source_map is shared")?;
            let func_entries = Arc::get_mut(&mut self.o.ctx.i.func_entries)
                .context_unlikely("function_entries is shared")?;
            let bc_chunks = Arc::get_mut(&mut self.o.ctx.i.bc_chunks)
                .context_unlikely("bc_chunks is shared")?;
            if let Some(chunk) = bc_chunks.last() {
                use indexmap::map::Entry;
                match source_map.entry(chunk.name.clone()) {
                    Entry::Occupied(e) if e.get().is_transient => {
                        e.swap_remove();
                        let func_name = chunk.name.clone();
                        func_entries.swap_remove(Ascii::new_str(&func_name));
                        bc_chunks.pop();
                    }
                    _ => (),
                }
            }
        }
        self.remake_site()?;
        self.o.cur_frame.ip = cur_frame.ret_ip;
        if !cur_frame.ignore_return_value {
            self.o.stack.push(StackValue::new_str(val));
        }
        Ok(())
    }

    fn instr_call_fun(&mut self, args_cnt: u8, func_idx: u32) -> anyhow::Result<()> {
        self.instr_call_fun_with_return_value(args_cnt, func_idx)?;
        Ok(())
    }

    fn instr_call_fun_with_return_value(
        &mut self,
        args_cnt: u8,
        func_idx: u32,
    ) -> anyhow::Result<usize> {
        self.ensure_pre_step_instruction()?;

        // DANGER: Touches the site execution state. Watch out for UB.
        let inst_len = Bc::CallFun { args_cnt, func_idx }.bytes_len() as u32;
        view_stack!(self, _, _args:any:args_cnt);
        let func_idx = func_idx as usize;
        let Some((func_name, func_info)) = (self.o.ctx.func_entries)
            .get_index(func_idx)
            .and_then(|(k, v)| v.as_ref().map(|v| (k.clone(), v)))
        else {
            crate::util::cold();

            let msg = format!("function index {} not found", func_idx);
            let mut diag = Diagnostic::new();
            diag.span_err(self.o.cur_filename(), self.o.cur_bc_span(), msg);
            self.o.ctx.emit_diag(diag);
            self.break_reason = EraExecutionBreakReason::InternalError;
            return Err(FireEscapeError(self.break_reason).into());
        };
        let new_ip = EraExecIp {
            chunk: func_info.chunk_idx,
            offset: func_info.bc_offset,
        };
        let ret_ip = EraExecIp {
            chunk: self.o.cur_frame.ip.chunk,
            offset: self.o.cur_frame.ip.offset + inst_len,
        };
        // Verify that the function has the correct number of arguments.
        // TODO: Perform full type checking for arguments.
        let args_cnt = args_cnt as usize;
        let params_cnt = func_info.frame_info.args.len();
        if args_cnt != params_cnt {
            crate::util::cold();

            let msg = format!(
                "function `{}` expects {} arguments, but got {}. Broken codegen?",
                func_name, params_cnt, args_cnt
            );
            let mut diag = Diagnostic::new();
            diag.span_err(self.o.cur_filename(), self.o.cur_bc_span(), msg);
            self.o.ctx.emit_diag(diag);
            self.break_reason = EraExecutionBreakReason::InternalError;
            return Err(FireEscapeError(self.break_reason).into());
        }

        // Check call stack depth
        let o = self.ensure_call_stack_and_get_o()?;

        // Now prepare arguments for the function
        // WARN: The following procedure must not fail until remake, or stack will be corrupted.
        // // Pop function index
        // assert!(o.stack.pop().is_some(), "stack underflow");

        // Create a new execution frame for the function
        let is_transient = func_info.is_transient;
        let stack_start = if is_transient {
            o.frames.last().map_or(0, |f| f.stack_start as usize)
        } else {
            o.stack.len() - args_cnt
        };
        o.frames.push(EraFuncExecFrame {
            stack_start: stack_start as _,
            real_stack_start: stack_start as _,
            vars_stack_start: o.var_stack.len() as _,
            ip: new_ip,
            ret_ip,
            ignore_return_value: false,
            is_transient,
        });
        self.remake_site()?;

        Ok(func_idx)
    }

    fn instr_try_call_fun(&mut self, args_cnt: u8, is_force: bool) -> anyhow::Result<()> {
        self.instr_try_call_fun_with_return_value(args_cnt, is_force)?;
        Ok(())
    }

    fn instr_try_call_fun_with_return_value(
        &mut self,
        args_cnt: u8,
        is_force: bool,
    ) -> anyhow::Result<usize> {
        self.ensure_pre_step_instruction()?;

        // DANGER: Touches the site execution state. Watch out for UB.
        let instr_len = Bc::TryCallFun { args_cnt }.bytes_len() as u32;
        let args_cnt = args_cnt as usize;
        view_stack!(self, stack_count, args:any:(args_cnt * 2), func_idx:any);
        let mut processed_args = Vec::with_capacity(args_cnt);
        let mut num_func_idx = usize::MAX; // -1: Function does not exist
        let lookup_result = match func_idx.as_unpacked() {
            RefFlatStackValue::Int(x) => {
                // Find function by index
                let idx = x.val as usize;
                (self.o.ctx.i.func_entries)
                    .get_index(idx)
                    .and_then(|(k, v)| {
                        num_func_idx = idx;
                        v.as_ref().map(|v| (k.clone(), v))
                    })
            }
            RefFlatStackValue::Str(x) => {
                // Find function by name
                let idx = x.val.as_str();
                (self.o.ctx.i.func_entries)
                    .get_full(Ascii::new_str(idx))
                    .and_then(|(num_idx, k, v)| {
                        v.as_ref().map(|v| {
                            num_func_idx = num_idx;
                            (k.clone(), v)
                        })
                    })
            }
            v => {
                crate::util::cold();

                let msg = format!("expected a function index, got {:?}", v);
                let mut diag = Diagnostic::new();
                diag.span_err(self.o.cur_filename(), self.o.cur_bc_span(), msg);
                self.o.ctx.emit_diag(diag);
                self.break_reason = EraExecutionBreakReason::IllegalArguments;
                return Err(FireEscapeError(self.break_reason).into());
            }
        };
        if let Some((func_name, func_info)) = lookup_result {
            // Function exists, check function argument pack
            #[derive(Debug)]
            enum ArgPackKind<'a> {
                Empty,
                Scalar(&'a StackValue),
                ArrIdx(&'a StackValue, usize),
            }

            fn parse_arg_pack<'a>(
                sview: &'a [StackValue],
                idx: usize,
            ) -> anyhow::Result<ArgPackKind<'a>> {
                let Some(arg_pack) = sview.chunks_exact(2).nth(idx) else {
                    return Ok(ArgPackKind::Empty);
                };
                match (arg_pack[0].as_unpacked(), arg_pack[1].as_unpacked()) {
                    (RefFlatStackValue::ArrRef(_), RefFlatStackValue::Int(idx)) => {
                        if idx.val < 0 {
                            crate::util::cold();

                            return Err(anyhow::anyhow!("array index {} is negative", idx.val));
                        }
                        Ok(ArgPackKind::ArrIdx(&arg_pack[0], idx.val as _))
                    }
                    (
                        RefFlatStackValue::Int(_) | RefFlatStackValue::Str(_),
                        RefFlatStackValue::Int(tag),
                    ) => {
                        if tag.val == 1 {
                            Ok(ArgPackKind::Empty)
                        } else {
                            Ok(ArgPackKind::Scalar(&arg_pack[0]))
                        }
                    }
                    _ => {
                        crate::util::cold();

                        Err(anyhow::anyhow!(
                            "invalid argument pack (got `{:?}`, `{:?}`)",
                            arg_pack[0],
                            arg_pack[1]
                        ))
                    }
                }
            }

            // Process arguments
            for (i, param) in func_info.frame_info.args.iter().enumerate() {
                // TODO: Check dimensionality of arrays

                let arg_pack = parse_arg_pack(args, i)?;
                // For exposition (error reporting)
                let i = i + 1;
                match param.var_kind {
                    ValueKind::ArrInt | ValueKind::ArrStr => {
                        let ArgPackKind::ArrIdx(arr, _idx) = arg_pack else {
                            crate::util::cold();

                            let msg = format!(
                                "expected an array argument for parameter {}, got {:?}",
                                i, arg_pack
                            );
                            let mut diag = Diagnostic::new();
                            diag.span_err(self.o.cur_filename(), self.o.cur_bc_span(), msg);
                            self.o.ctx.emit_diag(diag);
                            self.break_reason = EraExecutionBreakReason::IllegalArguments;
                            return Err(FireEscapeError(self.break_reason).into());
                        };
                        let arr_var = (self.o)
                            .resolve_variable_place(*arr.as_arr_ref().unwrap())
                            .context_unlikely("array not found")?;
                        let type_matches = match param.var_kind {
                            ValueKind::ArrInt => arr_var.is_arrint(),
                            ValueKind::ArrStr => arr_var.is_arrstr(),
                            _ => unreachable!(),
                        };
                        if !type_matches {
                            crate::util::cold();

                            let msg = format!(
                                "expected {:?} for parameter {}, got {:?}",
                                param.var_kind, i, arr_var
                            );
                            let mut diag = Diagnostic::new();
                            diag.span_err(self.o.cur_filename(), self.o.cur_bc_span(), msg);
                            self.o.ctx.emit_diag(diag);
                            self.break_reason = EraExecutionBreakReason::IllegalArguments;
                            return Err(FireEscapeError(self.break_reason).into());
                        }
                        processed_args.push(arr.clone());
                    }
                    ValueKind::Int => match arg_pack {
                        ArgPackKind::Scalar(val) => {
                            if val.as_int().is_none() {
                                crate::util::cold();

                                let msg = format!(
                                    "expected an integer for parameter {}, got {:?}",
                                    i, val
                                );
                                let mut diag = Diagnostic::new();
                                diag.span_err(self.o.cur_filename(), self.o.cur_bc_span(), msg);
                                self.o.ctx.emit_diag(diag);
                                self.break_reason = EraExecutionBreakReason::IllegalArguments;
                                return Err(FireEscapeError(self.break_reason).into());
                            }
                            processed_args.push(val.clone());
                        }
                        ArgPackKind::Empty => {
                            let val = param.default_value.coerce_as_int().unwrap();
                            processed_args.push(StackValue::new_int(val));
                        }
                        ArgPackKind::ArrIdx(arr, idx) => {
                            let arr = (self.o)
                                .resolve_variable_place(*arr.as_arr_ref().unwrap())
                                .context_unlikely("array not found")?;
                            let val = match arr.as_unpacked() {
                                FlatArrayValueRef::ArrInt(arr) => {
                                    arr.flat_get(idx).map(|x| x.val).with_context_unlikely(
                                        || format!("array index {} out of bounds", idx),
                                    )?
                                }
                                FlatArrayValueRef::ArrStr(arr) => {
                                    let val =
                                        arr.flat_get(idx).map(|x| &x.val).with_context_unlikely(
                                            || format!("array index {} out of bounds", idx),
                                        )?;
                                    let val = routines::parse_int_literal_with_sign(val.as_bytes())
                                        .with_context_unlikely(|| {
                                            format!(
                                                "string {:?} is not a valid integer",
                                                val.as_str()
                                            )
                                        })?;

                                    let mut diag = Diagnostic::new();
                                    diag.span_warn(
                                        self.o.cur_filename(),
                                        self.o.cur_bc_span(),
                                        "Implicit cast of Str to Int is discouraged; consider wrapping in `TOINT`",
                                    );
                                    self.o.ctx.i.emit_diag_to(diag, &mut self.o.ctx.callback);

                                    val
                                }
                            };
                            processed_args.push(StackValue::new_int(val));
                        }
                    },
                    ValueKind::Str => match arg_pack {
                        ArgPackKind::Scalar(val) => {
                            if val.as_str().is_none() {
                                crate::util::cold();

                                let msg =
                                    format!("expected a string for parameter {}, got {:?}", i, val);
                                let mut diag = Diagnostic::new();
                                diag.span_err(self.o.cur_filename(), self.o.cur_bc_span(), msg);
                                self.o.ctx.emit_diag(diag);
                                self.break_reason = EraExecutionBreakReason::IllegalArguments;
                                return Err(FireEscapeError(self.break_reason).into());
                            }
                            processed_args.push(val.clone());
                        }
                        ArgPackKind::Empty => {
                            let val = param.default_value.coerce_as_str().unwrap();
                            processed_args.push(StackValue::new_str(val.into()));
                        }
                        ArgPackKind::ArrIdx(arr, idx) => {
                            let arr = (self.o)
                                .resolve_variable_place(*arr.as_arr_ref().unwrap())
                                .context_unlikely("array not found")?;
                            let val = match arr.as_unpacked() {
                                FlatArrayValueRef::ArrInt(arr) => {
                                    let val = arr
                                        .flat_get(idx)
                                        .map(|x| itoa::Buffer::new().format(x.val).into())
                                        .with_context_unlikely(|| {
                                            format!("array index {} out of bounds", idx)
                                        })?;

                                    let mut diag = Diagnostic::new();
                                    diag.span_warn(
                                        self.o.cur_filename(),
                                        self.o.cur_bc_span(),
                                        "Implicit cast of Int to Str is discouraged; consider wrapping in `TOSTR`",
                                    );
                                    self.o.ctx.i.emit_diag_to(diag, &mut self.o.ctx.callback);

                                    val
                                }
                                FlatArrayValueRef::ArrStr(arr) => {
                                    let val =
                                        arr.flat_get(idx).map(|x| &x.val).with_context_unlikely(
                                            || format!("array index {} out of bounds", idx),
                                        )?;
                                    val.clone()
                                }
                            };
                            processed_args.push(StackValue::new_str(val));
                        }
                    },
                }
            }

            // Check call stack depth
            let o = self.ensure_call_stack_and_get_o()?;

            // Now apply the arguments and call the function
            // WARN: The following procedure must not fail, or stack will be corrupted.
            let args_cnt = processed_args.len();
            if is_force {
                o.stack
                    .splice(o.stack.len() - stack_count.., processed_args);
            } else {
                // Push 1 to indicate that the function exists
                o.stack.splice(
                    o.stack.len() - stack_count..,
                    std::iter::once(StackValue::new_int(1)).chain(processed_args),
                );
            }

            let new_ip = EraExecIp {
                chunk: func_info.chunk_idx,
                offset: func_info.bc_offset,
            };
            let ret_ip = EraExecIp {
                chunk: self.o.cur_frame.ip.chunk,
                offset: self.o.cur_frame.ip.offset + instr_len,
            };

            // Create a new execution frame for the function
            let is_transient = func_info.is_transient;
            let stack_start = if is_transient {
                o.frames.last().map_or(0, |f| f.stack_start as usize)
            } else {
                o.stack.len() - args_cnt
            };
            o.frames.push(EraFuncExecFrame {
                stack_start: stack_start as _,
                real_stack_start: stack_start as _,
                vars_stack_start: o.var_stack.len() as _,
                ip: new_ip,
                ret_ip,
                ignore_return_value: false,
                is_transient,
            });
            self.remake_site()?;
        } else {
            // Function does not exist
            if is_force {
                crate::util::cold();

                let msg = format!(
                    "function `{}` not found",
                    match func_idx.as_unpacked() {
                        RefFlatStackValue::Int(x) => x.val.to_string(),
                        RefFlatStackValue::Str(x) => x.val.to_string(),
                        v => format!("{:?}", v),
                    }
                );
                let mut diag = Diagnostic::new();
                diag.span_err(self.o.cur_filename(), self.o.cur_bc_span(), msg);
                self.o.ctx.emit_diag(diag);
                self.break_reason = EraExecutionBreakReason::InternalError;
                return Err(FireEscapeError(self.break_reason).into());
            } else {
                // Push 0 to indicate that the function does not exist
                (self.o.stack).splice(self.o.stack.len() - stack_count.., [StackValue::new_int(0)]);
                self.add_ip_offset(instr_len as _);
            }
        }

        Ok(num_func_idx)
    }

    fn instr_restart_exec_at_fun(&mut self) -> anyhow::Result<()> {
        self.instr_restart_exec_at_fun_with_return_value()?;
        Ok(())
    }

    fn instr_restart_exec_at_fun_with_return_value(&mut self) -> anyhow::Result<usize> {
        self.ensure_pre_step_instruction()?;

        // DANGER: Touches the site execution state. Watch out for UB.
        view_stack!(self, _, func_idx:i);
        let func_idx = func_idx as usize;
        let Some((func_name, func_info)) = (self.o.ctx.func_entries)
            .get_index(func_idx)
            .and_then(|(k, v)| v.as_ref().map(|v| (k.clone(), v)))
        else {
            let msg = format!("function index {} not found", func_idx);
            let mut diag = Diagnostic::new();
            diag.span_err(self.o.cur_filename(), self.o.cur_bc_span(), msg);
            self.o.ctx.emit_diag(diag);
            self.break_reason = EraExecutionBreakReason::InternalError;
            return Err(FireEscapeError(self.break_reason).into());
        };
        let new_ip = EraExecIp {
            chunk: func_info.chunk_idx,
            offset: func_info.bc_offset,
        };
        let inst_len = Bc::RestartExecAtFun.bytes_len() as u32;
        let ret_ip = EraExecIp {
            chunk: self.o.cur_frame.ip.chunk,
            offset: self.o.cur_frame.ip.offset + inst_len,
        };
        // Verify that the function has no arguments.
        let params_cnt = func_info.frame_info.args.len();
        if params_cnt != 0 {
            let msg = format!(
                "function `{}` must take no arguments, but actually takes {}",
                func_name, params_cnt
            );
            let mut diag = Diagnostic::new();
            diag.span_err(self.o.cur_filename(), self.o.cur_bc_span(), msg);
            self.o.ctx.emit_diag(diag);
            self.break_reason = EraExecutionBreakReason::InternalError;
            return Err(FireEscapeError(self.break_reason).into());
        }

        // Create a new execution frame for the function
        let o = unsafe { self.optr.as_mut() };
        let stack_start = 0;
        o.stack.clear();
        o.frames.clear();
        o.frames.push(EraFuncExecFrame {
            stack_start: stack_start as _,
            real_stack_start: stack_start as _,
            vars_stack_start: o.var_stack.len() as _,
            ip: new_ip,
            ret_ip,
            ignore_return_value: false,
            is_transient: false,
        });
        {
            // Clean up all transient sources
            let source_map = Arc::get_mut(&mut self.o.ctx.i.source_map)
                .context_unlikely("source_map is shared")?;
            let func_entries = Arc::get_mut(&mut self.o.ctx.i.func_entries)
                .context_unlikely("function_entries is shared")?;
            let bc_chunks = Arc::get_mut(&mut self.o.ctx.i.bc_chunks)
                .context_unlikely("bc_chunks is shared")?;
            while let Some(chunk) = bc_chunks.last() {
                // We must stop as soon as we encounter a non-transient source
                use indexmap::map::Entry;
                _ = match source_map.entry(chunk.name.clone()) {
                    Entry::Occupied(e) if e.get().is_transient => e.swap_remove(),
                    _ => break,
                };
                let func_name = chunk.name.clone();
                func_entries.swap_remove(Ascii::new_str(&func_name));
                bc_chunks.pop();
            }
        }
        self.remake_site()?;

        Ok(func_idx)
    }

    fn instr_jump_ww(&mut self, offset: i32) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        self.add_ip_offset(offset);
        Ok(())
    }

    fn instr_jump_if_ww(&mut self, offset: i32) -> anyhow::Result<()> {
        self.instr_jump_if_ww_with_return_value(offset)?;
        Ok(())
    }

    fn instr_jump_if_ww_with_return_value(&mut self, offset: i32) -> anyhow::Result<bool> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, cond:i);
        let cond = cond != 0;
        if cond {
            self.add_ip_offset(offset);
        } else {
            self.add_ip_offset(Bc::JumpIfWW { offset }.bytes_len() as i32);
        }
        self.o.stack.must_pop_many(stack_count);
        Ok(cond)
    }

    fn instr_jump_if_not_ww(&mut self, offset: i32) -> anyhow::Result<()> {
        self.instr_jump_if_not_ww_with_return_value(offset)?;
        Ok(())
    }

    fn instr_jump_if_not_ww_with_return_value(&mut self, offset: i32) -> anyhow::Result<bool> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, cond:i);
        let cond = cond == 0;
        if cond {
            self.add_ip_offset(offset);
        } else {
            self.add_ip_offset(Bc::JumpIfNotWW { offset }.bytes_len() as i32);
        }
        self.o.stack.must_pop_many(stack_count);
        Ok(cond)
    }

    fn instr_load_const_str(&mut self, idx: u32) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let interner = self.o.ctx.interner();
        let val =
            interner.resolve(TokenKey::try_from_u32(idx).context_unlikely("invalid token key")?);
        self.o.stack.push(StackValue::new_str(val.into()));
        self.add_ip_offset(Bc::LoadConstStr { idx }.bytes_len() as i32);
        Ok(())
    }

    fn instr_load_imm8(&mut self, imm: i8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        self.o.stack.push(StackValue::new_int(imm.into()));
        self.add_ip_offset(Bc::LoadImm8 { imm }.bytes_len() as i32);
        Ok(())
    }

    fn instr_load_imm16(&mut self, imm: i16) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        self.o.stack.push(StackValue::new_int(imm.into()));
        self.add_ip_offset(Bc::LoadImm16 { imm }.bytes_len() as i32);
        Ok(())
    }

    fn instr_load_imm32(&mut self, imm: i32) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        self.o.stack.push(StackValue::new_int(imm.into()));
        self.add_ip_offset(Bc::LoadImm32 { imm }.bytes_len() as i32);
        Ok(())
    }

    fn instr_load_imm64(&mut self, imm: i64) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        self.o.stack.push(StackValue::new_int(imm));
        self.add_ip_offset(Bc::LoadImm64 { imm }.bytes_len() as i32);
        Ok(())
    }

    fn instr_load_var_ww(&mut self, idx: u32) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let var = (self.o.ctx.variables)
            .get_var_by_idx_mut(idx as _)
            .with_context_unlikely(|| format!("variable index {} not found", idx))?;
        var.ensure_alloc();
        self.o.stack.push(StackValue::new_arr_ref(VariablePlaceRef {
            is_dynamic: false,
            index: idx as _,
        }));
        self.add_ip_offset(Bc::LoadVarWW { idx }.bytes_len() as i32);
        Ok(())
    }

    fn instr_load_const_var_ww(&mut self, idx: u32) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let mut var = (self.o.ctx.variables)
            .get_var_by_idx(idx as _)
            .with_context_unlikely(|| format!("variable index {} not found", idx))?
            .clone();
        var.ensure_alloc();
        let dyn_var_idx = self.o.var_stack.len();
        self.o.var_stack.push(var);
        self.o.stack.push(StackValue::new_arr_ref(VariablePlaceRef {
            is_dynamic: true,
            index: dyn_var_idx as _,
        }));
        self.add_ip_offset(Bc::LoadConstVarWW { idx }.bytes_len() as i32);
        Ok(())
    }

    fn instr_load_local_var(&mut self, idx: u8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let var = (self.o.stack.get(idx as usize))
            .with_context_unlikely(|| format!("local variable index {} not found", idx))?;
        self.o.stack.push(var.clone());
        self.add_ip_offset(Bc::LoadLocalVar { idx }.bytes_len() as i32);
        Ok(())
    }

    fn instr_pop(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, _, _val:any);
        self.o.stack.must_pop_many(1);
        self.add_ip_offset(Bc::Pop.bytes_len() as i32);
        Ok(())
    }

    fn instr_pop_all_n(&mut self, count: u8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, _, _vals:any:count);
        self.o.stack.must_pop_many(count as _);
        self.add_ip_offset(Bc::PopAllN { count }.bytes_len() as i32);
        Ok(())
    }

    fn instr_pop_one_n(&mut self, idx: u8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        if idx > 0 {
            view_stack!(self, _, _vals:any:idx);
            let start = self.o.stack.len() - idx as usize;
            let end = start + 1;
            self.o.stack.drain(start..end);
        }
        self.add_ip_offset(Bc::PopOneN { idx }.bytes_len() as i32);
        Ok(())
    }

    fn instr_swap_2(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, _, _a:any, _b:any);
        let slen = self.o.stack.len();
        self.o.stack[slen - 2..].swap(0, 1);
        self.add_ip_offset(Bc::Swap2.bytes_len() as i32);
        Ok(())
    }

    fn instr_duplicate(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, _, val:any);
        let val = val.clone();
        self.o.stack.push(val);
        self.add_ip_offset(Bc::Duplicate.bytes_len() as i32);
        Ok(())
    }

    fn instr_duplicate_all_n(&mut self, count: u8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, _vals:any:count);
        let start = self.o.stack.len() - stack_count;
        self.o.stack.extend_from_within(start..);
        self.add_ip_offset(Bc::DuplicateAllN { count }.bytes_len() as i32);
        Ok(())
    }

    fn instr_duplicate_one_n(&mut self, idx: u8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        if idx > 0 {
            view_stack!(self, _, _vals:any:idx);
            let start = self.o.stack.len() - idx as usize;
            let end = start + 1;
            self.o.stack.extend_from_within(start..end);
        }
        self.add_ip_offset(Bc::DuplicateOneN { idx }.bytes_len() as i32);
        Ok(())
    }

    fn instr_add_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int(a.wrapping_add(b));
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::AddInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_sub_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int(a.wrapping_sub(b));
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::SubInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_mul_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int(a.wrapping_mul(b));
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::MulInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_div_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        if b == 0 {
            return Err(anyhow::anyhow!("division by zero"));
        }
        let r = StackValue::new_int(a.wrapping_div(b));
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::DivInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_mod_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        if b == 0 {
            return Err(anyhow::anyhow!("division by zero"));
        }
        let r = StackValue::new_int(a.wrapping_rem(b));
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::ModInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_neg_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:i);
        let r = StackValue::new_int(val.wrapping_neg());
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::NegInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_bit_and_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int(a & b);
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::BitAndInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_bit_or_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int(a | b);
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::BitOrInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_bit_xor_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int(a ^ b);
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::BitXorInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_bit_not_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:i);
        let r = StackValue::new_int(!val);
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::BitNotInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_shl_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int(a.wrapping_shl(b as u32));
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::ShlInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_shr_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int(a.wrapping_shr(b as u32));
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::ShrInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_cmp_int_lt(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int((a < b) as i64);
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::CmpIntLT.bytes_len() as i32);
        Ok(())
    }

    fn instr_cmp_int_leq(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int((a <= b) as i64);
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::CmpIntLEq.bytes_len() as i32);
        Ok(())
    }

    fn instr_cmp_int_gt(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int((a > b) as i64);
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::CmpIntGT.bytes_len() as i32);
        Ok(())
    }

    fn instr_cmp_int_geq(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int((a >= b) as i64);
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::CmpIntGEq.bytes_len() as i32);
        Ok(())
    }

    fn instr_cmp_int_eq(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int((a == b) as i64);
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::CmpIntEq.bytes_len() as i32);
        Ok(())
    }

    fn instr_cmp_int_neq(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int((a != b) as i64);
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::CmpIntNEq.bytes_len() as i32);
        Ok(())
    }

    fn instr_cmp_str_lt(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:s, b:s);
        let r = StackValue::new_int((a < b) as i64);
        self.o.stack.splice(self.o.stack.len() - stack_count.., [r]);
        self.add_ip_offset(Bc::CmpStrLT.bytes_len() as i32);
        Ok(())
    }

    fn instr_cmp_str_leq(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:s, b:s);
        let r = StackValue::new_int((a <= b) as i64);
        self.o.stack.splice(self.o.stack.len() - stack_count.., [r]);
        self.add_ip_offset(Bc::CmpStrLEq.bytes_len() as i32);
        Ok(())
    }

    fn instr_cmp_str_gt(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:s, b:s);
        let r = StackValue::new_int((a > b) as i64);
        self.o.stack.splice(self.o.stack.len() - stack_count.., [r]);
        self.add_ip_offset(Bc::CmpStrGT.bytes_len() as i32);
        Ok(())
    }

    fn instr_cmp_str_geq(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:s, b:s);
        let r = StackValue::new_int((a >= b) as i64);
        self.o.stack.splice(self.o.stack.len() - stack_count.., [r]);
        self.add_ip_offset(Bc::CmpStrGEq.bytes_len() as i32);
        Ok(())
    }

    fn instr_cmp_str_eq(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:s, b:s);
        let r = StackValue::new_int((a == b) as i64);
        self.o.stack.splice(self.o.stack.len() - stack_count.., [r]);
        self.add_ip_offset(Bc::CmpStrEq.bytes_len() as i32);
        Ok(())
    }

    fn instr_cmp_str_neq(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:s, b:s);
        let r = StackValue::new_int((a != b) as i64);
        self.o.stack.splice(self.o.stack.len() - stack_count.., [r]);
        self.add_ip_offset(Bc::CmpStrNEq.bytes_len() as i32);
        Ok(())
    }

    fn instr_logical_not(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:i);
        let r = StackValue::new_int((val == 0) as i64);
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::LogicalNot.bytes_len() as i32);
        Ok(())
    }

    fn instr_max_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int(a.max(b));
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::MaxInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_min_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let r = StackValue::new_int(a.min(b));
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::MinInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_clamp_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:i, min:i, max:i);
        let r = StackValue::new_int(val.max(min).min(max));
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::ClampInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_in_range_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:i, min:i, max:i);
        let r = StackValue::new_int((min <= val && val <= max) as i64);
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::InRangeInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_in_range_str(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:s, min:s, max:s);
        let r = StackValue::new_int((min <= val && val <= max) as i64);
        // TODO: Not using replace_tail_no_drop_unchecked_spec because ArcStr needs drop;
        //       Maybe use replace_tail_unchecked_spec?
        self.o.stack.splice(self.o.stack.len() - stack_count.., [r]);
        self.add_ip_offset(Bc::InRangeStr.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_bit(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        if crate::util::unlikely(b < 0 || b >= 64) {
            anyhow::bail!("bit index {} out of bounds", b);
        }
        let bit = b as usize;
        let r = StackValue::new_int((a >> bit) & 1);
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::GetBit.bytes_len() as i32);
        Ok(())
    }

    fn instr_set_bit(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        if crate::util::unlikely(b < 0 || b >= 64) {
            anyhow::bail!("bit index {} out of bounds", b);
        }
        let bit = b as usize;
        let r = StackValue::new_int(a | (1 << bit));
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::SetBit.bytes_len() as i32);
        Ok(())
    }

    fn instr_clear_bit(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        if crate::util::unlikely(b < 0 || b >= 64) {
            anyhow::bail!("bit index {} out of bounds", b);
        }
        let bit = b as usize;
        let r = StackValue::new_int(a & !(1 << bit));
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::ClearBit.bytes_len() as i32);
        Ok(())
    }

    fn instr_invert_bit(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        if crate::util::unlikely(b < 0 || b >= 64) {
            anyhow::bail!("bit index {} out of bounds", b);
        }
        let bit = b as usize;
        let r = StackValue::new_int(a ^ (1 << bit));
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::InvertBit.bytes_len() as i32);
        Ok(())
    }

    fn instr_build_string(&mut self, count: u8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, vals:any:count);
        let mut buf = String::new();
        for val in vals {
            let Some(val) = val.as_str() else {
                anyhow::bail!("expected a string, got {:?}", val);
            };
            buf.push_str(val.val.as_str());
        }
        let r = StackValue::new_str(buf.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::BuildString { count }.bytes_len() as i32);
        Ok(())
    }

    fn instr_pad_string(&mut self, flags: EraPadStringFlags) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use unicode_width::UnicodeWidthStr;
        view_stack!(self, stack_count, val:s, width:i);
        let val_width = val.width();
        let width = width as usize;
        if val_width < width {
            let spaces = " ".repeat(width - val_width);
            // NOTE: left_pad means: pad to left (spaces on the right side),
            //       which is somewhat unintuitive.
            let result = match (flags.left_pad(), flags.right_pad()) {
                (true, false) => String::from(val.as_str()) + &spaces,
                (false, true) | _ => spaces + &val,
            };
            let r = StackValue::new_str(result.into());
            self.o.stack.replace_tail(stack_count, [r]);
        } else {
            self.o.stack.must_pop_many(1);
        }
        self.add_ip_offset(Bc::PadString { flags }.bytes_len() as i32);
        Ok(())
    }

    fn instr_repeat_str(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:s, count:i);
        let r = ArcStr::try_repeat(val, count as usize).with_context_unlikely(|| {
            format!("failed to repeat string `{:?}` {} times", val, count)
        })?;
        let r = StackValue::new_str(r.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::RepeatStr.bytes_len() as i32);
        Ok(())
    }

    fn instr_build_arr_idx_from_md(&mut self, count: u8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arr:a, vals:any:count);
        // Fetch indices
        let mut dims = EraVarDims::new();
        for val in vals {
            let val = val.as_int().context_unlikely("expected an integer")?;
            dims.push(val.val.try_into()?);
        }
        // Calculate flat index
        let arr = (self.o.resolve_variable_place(arr))
            .with_context_unlikely(|| format!("array {:?} not found", arr))?;
        let flat_idx = arr.calc_idx(&dims).with_context_unlikely(|| {
            format!("array index {:?} out of bounds of {:?}", dims, arr.dims())
        })?;
        let r = StackValue::new_int(flat_idx as _);
        self.o.stack.replace_tail(stack_count - 1, [r]);
        self.add_ip_offset(Bc::BuildArrIdxFromMD { count }.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_arr_val_flat(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arr:a, idx:i);
        if idx < 0 || idx >= i32::MAX.into() {
            anyhow::bail!("array index {} out of bounds", idx);
        }
        let idx = idx as usize;
        let arr_place = arr;
        let arr = (self.o.resolve_variable_place(arr))
            .with_context_unlikely(|| format!("array {:?} not found", arr))?;
        let arr_ptr = arr.as_untagged_ptr();
        let r = match arr.as_unpacked() {
            FlatArrayValueRef::ArrInt(_) => {
                // SAFETY: We only access the array through the erased pointer in the current invocation,
                //         in the hope of not violating Rust's aliasing rules.
                let x = unsafe { arr_ptr.cast::<ArrIntValue>().as_mut() };
                let flags = x.flags;
                let Some(val) = x.flat_get_mut(idx) else {
                    anyhow::bail!("array index {} out of bounds of {:?}", idx, x.dims);
                };
                if flags.is_trap() {
                    // Trapped; redirect to global variable manipulation
                    let trap_var_info = (self.o.ctx.i.variables)
                        .get_var_info(arr_place.index)
                        .context_unlikely("bad trap variable info")?;
                    val.val = (self.o.ctx.callback)
                        .on_var_get_int(trap_var_info.name.as_ref(), idx)
                        .context_unlikely("trap handler failed")?;
                }
                StackValue::new_int(val.val)
            }
            FlatArrayValueRef::ArrStr(_) => {
                // SAFETY: We only access the array through the erased pointer in the current invocation,
                //         in the hope of not violating Rust's aliasing rules.
                let x = unsafe { arr_ptr.cast::<ArrStrValue>().as_mut() };
                let flags = x.flags;
                let Some(val) = x.flat_get_mut(idx) else {
                    anyhow::bail!("array index {} out of bounds of {:?}", idx, x.dims);
                };
                if flags.is_trap() {
                    // Trapped; redirect to global variable manipulation
                    let trap_var_info = (self.o.ctx.i.variables)
                        .get_var_info(arr_place.index)
                        .context_unlikely("bad trap variable info")?;
                    val.val = (self.o.ctx.callback)
                        .on_var_get_str(trap_var_info.name.as_ref(), idx)
                        .context_unlikely("trap handler failed")?
                        .into();
                }
                StackValue::new_str(val.val.clone())
            }
        };
        unsafe {
            (self.o.stack).replace_tail_no_drop_unchecked_spec(stack_count, [r]);
        }
        self.add_ip_offset(Bc::GetArrValFlat.bytes_len() as i32);
        Ok(())
    }

    fn instr_set_arr_val_flat(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, _, arr:a, idx:i, _val:any);
        if idx < 0 || idx >= i32::MAX.into() {
            anyhow::bail!("array index {} out of bounds", idx);
        }
        let idx = idx as usize;
        let arr_place = arr;
        let arr = (self.o.resolve_variable_place(arr))
            .with_context_unlikely(|| format!("array {:?} not found", arr))?;
        let arr_ptr = arr.as_untagged_ptr();
        match arr.as_unpacked() {
            FlatArrayValueRef::ArrInt(_) => {
                view_stack!(self, _, in_val:i);
                // SAFETY: We only access the array through the erased pointer in the current invocation,
                //         in the hope of not violating Rust's aliasing rules.
                let x = unsafe { arr_ptr.cast::<ArrIntValue>().as_mut() };
                let flags = x.flags;
                let Some(val) = x.flat_get_mut(idx) else {
                    anyhow::bail!("array index {} out of bounds of {:?}", idx, x.dims);
                };
                val.val = in_val;
                if flags.is_trap() {
                    // Trapped; redirect to global variable manipulation
                    let trap_var_info = (self.o.ctx.i.variables)
                        .get_var_info(arr_place.index)
                        .context_unlikely("bad trap variable info")?;
                    (self.o.ctx.callback)
                        .on_var_set_int(trap_var_info.name.as_ref(), idx, in_val)
                        .context_unlikely("trap handler failed")?;
                }
            }
            FlatArrayValueRef::ArrStr(_) => {
                view_stack!(self, _, in_val:s);
                // SAFETY: We only access the array through the erased pointer in the current invocation,
                //         in the hope of not violating Rust's aliasing rules.
                let x = unsafe { arr_ptr.cast::<ArrStrValue>().as_mut() };
                let flags = x.flags;
                let Some(val) = x.flat_get_mut(idx) else {
                    anyhow::bail!("array index {} out of bounds of {:?}", idx, x.dims);
                };
                val.val = in_val.clone();
                if flags.is_trap() {
                    // Trapped; redirect to global variable manipulation
                    let trap_var_info = (self.o.ctx.i.variables)
                        .get_var_info(arr_place.index)
                        .context_unlikely("bad trap variable info")?;
                    (self.o.ctx.callback)
                        .on_var_set_str(trap_var_info.name.as_ref(), idx, in_val.as_str())
                        .context_unlikely("trap handler failed")?;
                }
            }
        };
        let slen = self.o.stack.len();
        // Keep the value on the stack
        unsafe {
            (self.o.stack).drain_no_drop_unchecked_spec(slen - 3..slen - 1);
        }
        self.add_ip_offset(Bc::SetArrValFlat.bytes_len() as i32);
        Ok(())
    }

    fn instr_times_float(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, a:i, b:i);
        let factor = f64::from_bits(b as u64);
        let r = StackValue::new_int((a as f64 * factor) as i64);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::TimesFloat.bytes_len() as i32);
        Ok(())
    }

    fn instr_fun_exists(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, func_name:s);
        let exists = (self.o.ctx.func_entries)
            .get(Ascii::new_str(func_name))
            .map_or(false, Option::is_some);
        let r = StackValue::new_int(exists as i64);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::FunExists.bytes_len() as i32);
        Ok(())
    }

    fn instr_replace_str(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, haystack:s, needle:s, replace_with:s);
        let re = self
            .i
            .regex_cache
            .try_get_or_insert(needle.clone(), || regex::Regex::new(needle))
            .context_unlikely("failed to compile regex")?;
        let r = re.replace_all(&haystack, replace_with.as_str()).into();
        let r = StackValue::new_str(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::ReplaceStr.bytes_len() as i32);
        Ok(())
    }

    fn instr_sub_str(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, haystack:s, start_pos:i, length:i);
        let haystack = haystack.as_str();
        let start_pos = start_pos.max(0) as usize;
        let length = if length < 0 {
            usize::MAX
        } else {
            length as usize
        };
        let r = if length <= 0 {
            ArcStr::default()
        } else {
            let haystack_len = haystack.len();
            let idx_fn = &|(index, _)| index;
            let mut it = haystack.char_indices();
            let start_byte_pos = it.nth(start_pos).map_or(haystack_len, idx_fn);
            let end_byte_pos = it.nth(length - 1).map_or(haystack_len, idx_fn);
            // SAFETY: The indices are guaranteed to be valid.
            unsafe { haystack.get_unchecked(start_byte_pos..end_byte_pos).into() }
        };
        let r = StackValue::new_str(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::SubStr.bytes_len() as i32);
        Ok(())
    }

    fn instr_str_find(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, haystack:s, needle:s, start_pos:i);
        let needle = needle.as_str();
        let start_pos = start_pos.max(0) as usize;
        let haystack_len = haystack.len();
        let start_byte_pos = haystack
            .char_indices()
            .nth(start_pos)
            .map_or(haystack_len, |(index, _)| index);
        let r = haystack[start_byte_pos..].find(needle).map_or(-1, |pos| {
            haystack[..start_byte_pos + pos].chars().count() as i64
        });
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::StrFind.bytes_len() as i32);
        Ok(())
    }

    fn instr_str_len(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:s);
        let r = StackValue::new_int(val.chars().count() as i64);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::StrLen.bytes_len() as i32);
        Ok(())
    }

    fn instr_count_sub_str(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, haystack:s, needle:s);
        let count = haystack.matches(needle.as_str()).count() as i64;
        let r = StackValue::new_int(count);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::CountSubStr.bytes_len() as i32);
        Ok(())
    }

    fn instr_str_char_at(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, haystack:s, pos:i);
        let r = if pos < 0 {
            ArcStr::default()
        } else {
            haystack
                .chars()
                .nth(pos as usize)
                .map_or(ArcStr::default(), |x| ArcStr::from(x.to_string()))
        };
        let r = StackValue::new_str(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::StrCharAtU.bytes_len() as i32);
        Ok(())
    }

    fn instr_int_to_str(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:i);
        let r = itoa::Buffer::new().format(val).into();
        let r = StackValue::new_str(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::IntToStr.bytes_len() as i32);
        Ok(())
    }

    fn instr_str_to_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:s);
        // let r = routines::parse_int_literal_with_sign(val.as_bytes())
        //     .with_context_unlikely(|| format!("string {:?} is not a valid integer", val.as_str()))?;
        let r = if let Some(r) = routines::parse_int_literal_with_sign(val.as_bytes()) {
            r
        } else {
            // Don't fail, return 0 instead, just like Emuera
            let mut diag = Diagnostic::new();
            diag.span_err(
                self.o.cur_filename(),
                self.o.cur_bc_span(),
                format!("string {:?} is not a valid integer", val.as_str()),
            );
            self.o.ctx.emit_diag(diag);
            0
        };
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::StrToInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_format_int_to_str(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:i, fmt:s);
        let r = csharp_format_i64(val, fmt).context_unlikely("failed to format integer")?;
        let r = StackValue::new_str(r.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::FormatIntToStr.bytes_len() as i32);
        Ok(())
    }

    fn instr_str_is_valid_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:s);
        let r = routines::parse_int_literal_with_sign(val.as_bytes()).is_some() as i64;
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::StrIsValidInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_str_to_upper(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:s);
        let r = val.to_ascii_uppercase();
        let r = StackValue::new_str(r.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::StrToUpper.bytes_len() as i32);
        Ok(())
    }

    fn instr_str_to_lower(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:s);
        let r = val.to_ascii_lowercase();
        let r = StackValue::new_str(r.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::StrToLower.bytes_len() as i32);
        Ok(())
    }

    fn instr_str_to_half(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use full2half::CharacterWidth;

        view_stack!(self, stack_count, val:s);
        let r = val.to_half_width();
        let r = StackValue::new_str(r.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::StrToHalf.bytes_len() as i32);
        Ok(())
    }

    fn instr_str_to_full(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use full2half::CharacterWidth;

        view_stack!(self, stack_count, val:s);
        let r = val.to_full_width();
        let r = StackValue::new_str(r.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::StrToFull.bytes_len() as i32);
        Ok(())
    }

    fn instr_build_bar_str(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use muldiv::MulDiv;

        view_stack!(self, stack_count, val:i, max_val:i, length:i);
        if length < 0 || length > 1024 {
            anyhow::bail!("invalid bar string length {}", length);
        }
        let length = length as u32;
        let max_val = max_val.max(0) as u32;
        let val = (val.max(0) as u32).min(max_val);
        let fill_cnt = val
            .mul_div_floor(length, max_val)
            .with_context_unlikely(|| {
                format!(
                    "failed to calculate muldiv({}, {}, {})",
                    val, length, max_val
                )
            })?;
        let rest_cnt = length - fill_cnt;
        let mut r = String::with_capacity(length as usize + 2);
        r += "[";
        r += &"*".repeat(fill_cnt as _);
        r += &".".repeat(rest_cnt as _);
        r += "]";
        let r = StackValue::new_str(r.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::BuildBarStr.bytes_len() as i32);
        Ok(())
    }

    fn instr_escape_regex_str(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:s);
        let r = regex::escape(val).into();
        let r = StackValue::new_str(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::EscapeRegexStr.bytes_len() as i32);
        Ok(())
    }

    fn instr_encode_to_unicode(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, haystack:s, pos:i);
        let r = (pos.try_into().ok())
            .and_then(|pos| haystack.chars().nth(pos))
            .with_context_unlikely(|| format!("invalid index {} into string", pos))?;
        let r = StackValue::new_int(r as _);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::EncodeToUnicode.bytes_len() as i32);
        Ok(())
    }

    fn instr_unicode_to_str(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:i);
        let r = (val.try_into().ok().and_then(char::from_u32))
            .with_context_unlikely(|| format!("invalid unicode value {}", val))?;
        let r = StackValue::new_str(r.to_string().into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::UnicodeToStr.bytes_len() as i32);
        Ok(())
    }

    fn instr_int_to_str_with_base(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:i, base:i);
        if base < 2 || base > 36 {
            anyhow::bail!("invalid base {}", base);
        }
        let base = base as u32;
        let r = routines::format_radix(val, base).unwrap();
        let r = StackValue::new_str(r.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::IntToStrWithBase.bytes_len() as i32);
        Ok(())
    }

    fn instr_html_tag_split(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        // TODO: Honor trapped variables
        view_stack!(self, stack_count, html:s, tags:a, tags_idx:i, count:a, count_idx:i);
        resolve_array_mut_unsafe!(self, tags:s;tags_idx;-1, count:i;count_idx);
        let mut parts_count: usize = 0;
        for part in crate::util::html::split_html_tags(&html) {
            let part = part.context_unlikely("found invalid html tag while parsing")?;
            let tags = tags
                .get_mut(parts_count)
                .context_unlikely("invalid indices into array")?;
            tags.val = part.into();
            parts_count += 1;
        }
        count.val = parts_count as i64;
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::HtmlTagSplit.bytes_len() as i32);
        Ok(())
    }

    fn instr_html_to_plain_text(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, html:s);
        let r = nanohtml2text::html2text(html);
        let r = StackValue::new_str(r.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::HtmlToPlainText.bytes_len() as i32);
        Ok(())
    }

    fn instr_html_escape(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, html:s);
        let r = htmlize::escape_all_quotes(html.as_str());
        let r = StackValue::new_str(r.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::HtmlEscape.bytes_len() as i32);
        Ok(())
    }

    fn instr_power_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, base:i, exp:i);
        let r = base.wrapping_pow(exp as _);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::PowerInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_sqrt_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use num_integer::Roots;

        view_stack!(self, stack_count, val:i);
        if val < 0 {
            anyhow::bail!("cannot take square root of negative number {}", val);
        }
        let r = val.sqrt();
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::SqrtInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_cbrt_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use num_integer::Roots;

        view_stack!(self, stack_count, val:i);
        let r = val.cbrt();
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::CbrtInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_log_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        // FIXME: Use f128 when https://github.com/rust-lang/rust/issues/116909 lands.
        view_stack!(self, stack_count, val:i);
        let r = (val as f64).ln() as _;
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::LogInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_log_10_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:i);
        let r = if val <= 0 { 0 } else { val.ilog10().into() };
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::LogInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_exponent_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:i);
        let r = (val as f64).exp() as _;
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::ExponentInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_abs_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:i);
        let r = val.wrapping_abs();
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::AbsInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_sign_int(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:i);
        let r = val.signum();
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::SignInt.bytes_len() as i32);
        Ok(())
    }

    fn instr_group_match(&mut self, count: u8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:any, idx:any:count);
        if val.as_arr_ref().is_some() {
            anyhow::bail!("GROUPMATCH cannot be applied to arrays");
        }
        let r = idx.iter().filter(|&x| val == x).count() as i64;
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GroupMatch { count }.bytes_len() as i32);
        Ok(())
    }

    fn instr_array_count_matches(&mut self, dim_pos: i64) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arr:a, arr_idx:i, value:any, start_idx:i, end_idx:i);
        let start_idx = start_idx.max(0) as usize;
        let end_idx = if end_idx < 0 {
            usize::MAX
        } else {
            end_idx as usize
        };
        let count = match resolve_array_kind!(self, arr) {
            ArrayValueKind::ArrInt => {
                resolve_array!(self, arr:i;arr_idx;dim_pos);
                resolve_any_scalar!(self, value:i);
                let end_idx = end_idx.min(arr.len());
                let mut count = 0;
                for i in start_idx..end_idx {
                    let arr = arr.get(i).context_unlikely("invalid indices into array")?;
                    if arr.val == value {
                        count += 1;
                    }
                }
                count
            }
            ArrayValueKind::ArrStr => {
                resolve_array!(self, arr:s;arr_idx;dim_pos);
                resolve_any_scalar!(self, value:s);
                let end_idx = end_idx.min(arr.len());
                let mut count = 0;
                for i in start_idx..end_idx {
                    let arr = arr.get(i).context_unlikely("invalid indices into array")?;
                    if arr.val == *value {
                        count += 1;
                    }
                }
                count
            }
        };
        let r = StackValue::new_int(count);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::ArrayCountMatches.bytes_len() as i32);
        Ok(())
    }

    fn instr_array_aggregate<AGGREGATOR: crate::util::Aggregator<i64>, const INSTR_LEN: usize>(
        &mut self,
        dim_pos: i64,
    ) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arr:a, arr_idx:i, start_idx:i, end_idx:i);
        let start_idx = start_idx.max(0) as usize;
        let end_idx = if end_idx < 0 {
            usize::MAX
        } else {
            end_idx as usize
        };
        resolve_array!(self, arr:i;arr_idx;dim_pos);
        let end_idx = end_idx.min(arr.len());
        let mut count = AGGREGATOR::INIT;
        for i in start_idx..end_idx {
            let arr = arr.get(i).context_unlikely("invalid indices into array")?;
            count = AGGREGATOR::aggregate(count, arr.val);
        }
        let r = StackValue::new_int(count);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(INSTR_LEN as i32);
        Ok(())
    }

    fn instr_array_in_range(&mut self, dim_pos: i64) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arr:a, arr_idx:i, lower_bound:i, upper_bound:i, start_idx:i, end_idx:i);
        let start_idx = start_idx.max(0) as usize;
        let end_idx = if end_idx < 0 {
            usize::MAX
        } else {
            end_idx as usize
        };
        resolve_array!(self, arr:i;arr_idx;dim_pos);
        let end_idx = end_idx.min(arr.len());
        let mut count = 0;
        for i in start_idx..end_idx {
            let arr = arr.get(i).context_unlikely("invalid indices into array")?;
            if (lower_bound..upper_bound).contains(&arr.val) {
                count += 1;
            }
        }
        let r = StackValue::new_int(count);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::InRangeArray.bytes_len() as i32);
        Ok(())
    }

    fn instr_array_remove(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arr:a, arr_idx:i, start_idx:i, count:i);
        let start_idx = start_idx.max(0) as usize;
        let count = if count < 0 { usize::MAX } else { count as _ };
        match resolve_array_kind!(self, arr) {
            ArrayValueKind::ArrInt => {
                resolve_array_mut_unsafe!(self, arr:i;arr_idx;-1);
                let end_idx = start_idx.saturating_add(count);
                let end_idx = end_idx.min(arr.len());
                if start_idx < end_idx {
                    let arr = arr.as_slice_mut().unwrap();
                    arr[start_idx..end_idx].fill(Default::default());
                    arr[start_idx..].rotate_left(end_idx - start_idx);
                }
            }
            ArrayValueKind::ArrStr => {
                resolve_array_mut_unsafe!(self, arr:s;arr_idx;-1);
                let end_idx = start_idx.saturating_add(count);
                let end_idx = end_idx.min(arr.len());
                if start_idx < end_idx {
                    let arr = arr.as_slice_mut().unwrap();
                    arr[start_idx..end_idx].fill(Default::default());
                    arr[start_idx..].rotate_left(end_idx - start_idx);
                }
            }
        }
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::ArrayRemove.bytes_len() as i32);
        Ok(())
    }

    fn instr_array_sort(&mut self, is_asc: bool) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arr:a, arr_idx:i, start_idx:i, count:i);
        let start_idx = start_idx.max(0) as usize;
        let count = if count < 0 { usize::MAX } else { count as _ };
        match resolve_array_kind!(self, arr) {
            ArrayValueKind::ArrInt => {
                resolve_array_mut_unsafe!(self, arr:i;arr_idx;-1);
                let end_idx = start_idx.saturating_add(count);
                let end_idx = end_idx.min(arr.len());
                if start_idx < end_idx {
                    let arr = arr.as_slice_mut().unwrap();
                    if is_asc {
                        arr[start_idx..end_idx].sort_unstable();
                    } else {
                        arr[start_idx..end_idx].sort_unstable_by(|a, b| b.cmp(a));
                    }
                }
            }
            ArrayValueKind::ArrStr => {
                resolve_array_mut_unsafe!(self, arr:s;arr_idx;-1);
                let end_idx = start_idx.saturating_add(count);
                let end_idx = end_idx.min(arr.len());
                if start_idx < end_idx {
                    let arr = arr.as_slice_mut().unwrap();
                    if is_asc {
                        arr[start_idx..end_idx].sort_unstable();
                    } else {
                        arr[start_idx..end_idx].sort_unstable_by(|a, b| b.cmp(a));
                    }
                }
            }
        }
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::ArraySortAsc.bytes_len() as i32);
        Ok(())
    }

    fn instr_array_multi_sort(&mut self, subs_cnt: u8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arrs:any:(subs_cnt as usize + 1));
        let prim_arr = arrs[0]
            .as_arr_ref()
            .cloned()
            .context_unlikely("expected array to sort by")?;
        let indices = match resolve_array_kind!(self, prim_arr) {
            ArrayValueKind::ArrInt => {
                resolve_array!(self, prim_arr:i;0;-1);
                let x = prim_arr.as_slice().unwrap();
                let mut indices = x
                    .iter()
                    .take_while(|x| x.val != 0)
                    .enumerate()
                    .map(|(i, _)| i)
                    .collect_vec();
                indices.sort_unstable_by(|&a, &b| x[a].cmp(&x[b]));
                indices
            }
            ArrayValueKind::ArrStr => {
                resolve_array!(self, prim_arr:s;0;-1);
                let x = prim_arr.as_slice().unwrap();
                let mut indices = x
                    .iter()
                    .take_while(|x| !x.val.is_empty())
                    .enumerate()
                    .map(|(i, _)| i)
                    .collect_vec();
                indices.sort_unstable_by(|&a, &b| x[a].cmp(&x[b]));
                indices
            }
        };
        // Verify that all arrays are long enough
        for arr in arrs.iter() {
            resolve_any_scalar!(self, arr:a);
            let arr_dim = self.o.resolve_variable_place_anyhow(arr)?.dims()[0] as usize;
            if arr_dim < indices.len() {
                anyhow::bail!(
                    "array {:?} is too short ({} versus {})",
                    arr,
                    arr_dim,
                    indices.len()
                );
            }
        }
        // Now sort all arrays by indices
        for arr in arrs.iter() {
            resolve_any_scalar!(self, arr:a);
            let arr = self.o.resolve_variable_place_anyhow(arr)?;
            match unsafe { arr.as_unpacked_mut_unchecked() } {
                FlatArrayValueRefMut::ArrInt(x) => {
                    let stride: usize = x.dims[1..].iter().map(|&x| x as usize).product();
                    crate::util::apply_permutation_in_place_with_fn(
                        |a, b| {
                            let (a, b) = (a.min(b), a.max(b));
                            let (a, b) = (a * stride, b * stride);
                            let split_pos = a + stride;
                            let (slice1, slice2) = x.get_vals_mut().split_at_mut(split_pos);
                            let b = b - split_pos;
                            slice1[a..a + stride].swap_with_slice(&mut slice2[b..b + stride]);
                        },
                        &indices,
                    );
                }
                FlatArrayValueRefMut::ArrStr(x) => {
                    let stride: usize = x.dims[1..].iter().map(|&x| x as usize).product();
                    crate::util::apply_permutation_in_place_with_fn(
                        |a, b| {
                            let (a, b) = (a.min(b), a.max(b));
                            let (a, b) = (a * stride, b * stride);
                            let split_pos = a + stride;
                            let (slice1, slice2) = x.get_vals_mut().split_at_mut(split_pos);
                            let b = b - split_pos;
                            slice1[a..a + stride].swap_with_slice(&mut slice2[b..b + stride]);
                        },
                        &indices,
                    );
                }
            }
        }

        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::ArrayMSort { subs_cnt }.bytes_len() as i32);
        Ok(())
    }

    fn instr_array_copy(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arr_from:a, arr_to:a);
        if arr_from != arr_to {
            let arr_from = self.o.resolve_variable_place_anyhow(arr_from)?;
            let arr_to = self.o.resolve_variable_place_anyhow(arr_to)?;

            let dims_count = arr_from.dims().len().min(arr_to.dims().len());
            let mut dims = EraVarDims::with_capacity(dims_count);
            for i in (0..dims_count).rev() {
                let dim1 = arr_from.dims()[arr_from.dims().len() - 1 - i];
                let dim2 = arr_to.dims()[arr_to.dims().len() - 1 - i];
                dims.push(dim1.min(dim2));
            }
            let arr_from = arr_from.as_unpacked();
            let mut arr_to = unsafe { arr_to.as_unpacked_mut_unchecked() };

            let mut idxs = vec![0; dims_count];
            'outer: loop {
                match (arr_from, &mut arr_to) {
                    (FlatArrayValueRef::ArrInt(arr_from), FlatArrayValueRefMut::ArrInt(arr_to)) => {
                        *arr_to.get_mut(&idxs).unwrap() = arr_from.get(&idxs).unwrap().clone();
                    }
                    (FlatArrayValueRef::ArrStr(arr_from), FlatArrayValueRefMut::ArrStr(arr_to)) => {
                        *arr_to.get_mut(&idxs).unwrap() = arr_from.get(&idxs).unwrap().clone();
                    }
                    _ => anyhow::bail!("array types do not match"),
                }
                // Increment indices
                *idxs.last_mut().unwrap() += 1;
                for i in (0..dims_count).rev() {
                    if idxs[i] == dims[i] {
                        idxs[i] = 0;
                        if i == 0 {
                            break 'outer;
                        }
                        idxs[i - 1] += 1;
                    }
                }
            }
        }
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::ArrayCopy.bytes_len() as i32);
        Ok(())
    }

    fn instr_array_shift(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arr:a, arr_idx:i, shift_count:i, value:any, start_idx:i, count:i);
        let start_idx = start_idx.max(0) as usize;
        let count = if count < 0 { usize::MAX } else { count as _ };
        let abs_shift_count = shift_count.unsigned_abs() as usize;
        match resolve_array_kind!(self, arr) {
            ArrayValueKind::ArrInt => {
                resolve_array_mut_unsafe!(self, arr:i;arr_idx;-1);
                resolve_any_scalar!(self, value:i);
                let end_idx = start_idx.saturating_add(count);
                let end_idx = end_idx.min(arr.len());
                if start_idx < end_idx {
                    let arr = arr.as_slice_mut().unwrap();
                    let arr = &mut arr[start_idx..end_idx];
                    let value = IntValue { val: value };
                    if abs_shift_count >= end_idx - start_idx {
                        arr.fill(value);
                    } else {
                        if shift_count < 0 {
                            arr[..abs_shift_count].fill(value);
                            arr.rotate_left(abs_shift_count);
                        } else {
                            arr.rotate_right(abs_shift_count);
                            arr[..abs_shift_count].fill(value);
                        }
                    }
                }
            }
            ArrayValueKind::ArrStr => {
                resolve_array_mut_unsafe!(self, arr:s;arr_idx;-1);
                resolve_any_scalar!(self, value:s);
                let end_idx = start_idx.saturating_add(count);
                let end_idx = end_idx.min(arr.len());
                if start_idx < end_idx {
                    let arr = arr.as_slice_mut().unwrap();
                    let arr = &mut arr[start_idx..end_idx];
                    let value = StrValue { val: value.clone() };
                    if abs_shift_count >= end_idx - start_idx {
                        arr.fill(value);
                    } else {
                        if shift_count < 0 {
                            arr[..abs_shift_count].fill(value);
                            arr.rotate_left(abs_shift_count);
                        } else {
                            arr.rotate_right(abs_shift_count);
                            arr[..abs_shift_count].fill(value);
                        }
                    }
                }
            }
        }

        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::ArrayShift.bytes_len() as i32);
        Ok(())
    }

    fn instr_print_with_flags<const INSTR_LEN: usize>(
        &mut self,
        flags: EraPrintExtendedFlags,
    ) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, val:any);
        match val.as_unpacked() {
            RefFlatStackValue::ArrRef(_) => self.o.ctx.callback.on_print("<array>", flags),
            RefFlatStackValue::Int(x) => {
                let mut buf = itoa::Buffer::new();
                self.o.ctx.callback.on_print(buf.format(x.val), flags);
            }
            RefFlatStackValue::Str(x) => self.o.ctx.callback.on_print(&x.val, flags),
        };
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(INSTR_LEN as i32);
        Ok(())
    }

    fn instr_reuse_last_line(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, content:s);
        self.o.ctx.callback.on_reuselastline(content);
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::ReuseLastLine.bytes_len() as i32);
        Ok(())
    }

    fn instr_clear_line(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, count:i);
        self.o.ctx.callback.on_clearline(count);
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::ClearLine.bytes_len() as i32);
        Ok(())
    }

    fn instr_wait(&mut self, flags: EraWaitFlags) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let any_key = flags.any_key();
        let is_force = flags.is_force();
        self.o.ctx.callback.on_wait(any_key, is_force);
        self.add_ip_offset(Bc::Wait { flags }.bytes_len() as i32);
        Ok(())
    }

    fn instr_twait(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, duration:i, is_force:b);
        self.o.ctx.callback.on_twait(duration, is_force);
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::TWait.bytes_len() as i32);
        Ok(())
    }

    fn instr_input(&mut self, flags: EraInputExtendedFlags) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let mut final_stack_count = 0;
        let var_result = self.var_result_place;
        let var_results = self.var_results_place;

        match (flags.is_string(), flags.is_one(), flags.is_timed()) {
            (false, false, false) => {
                let r = if flags.has_default_value() {
                    view_stack!(self, stack_count, default_value:i, can_click:b, allow_skip:b);
                    final_stack_count = stack_count;
                    (self.o.ctx.callback)
                        .on_input_int(Some(default_value), can_click, allow_skip)
                        .continue_anyhow()?
                } else {
                    view_stack!(self, stack_count, can_click:b, allow_skip:b);
                    final_stack_count = stack_count;
                    (self.o.ctx.callback)
                        .on_input_int(None, can_click, allow_skip)
                        .continue_anyhow()?
                };
                if let Some(r) = r {
                    resolve_array_mut_unsafe!(self, var_result:i;0);
                    var_result.val = r;
                }
            }
            (true, false, false) => {
                let r = if flags.has_default_value() {
                    view_stack!(self, stack_count, default_value:s, can_click:b, allow_skip:b);
                    final_stack_count = stack_count;
                    (self.o.ctx.callback)
                        .on_input_str(Some(default_value), can_click, allow_skip)
                        .continue_anyhow()?
                } else {
                    view_stack!(self, stack_count, can_click:b, allow_skip:b);
                    final_stack_count = stack_count;
                    (self.o.ctx.callback)
                        .on_input_str(None, can_click, allow_skip)
                        .continue_anyhow()?
                };
                if let Some(r) = r {
                    resolve_array_mut_unsafe!(self, var_results:s;0);
                    var_results.val = r.into();
                }
            }
            (false, false, true) => {
                view_stack!(self, stack_count, time_limit:i, default_value:i, show_prompt:b, expiry_msg:s, can_click:b);
                final_stack_count = stack_count;
                let r = (self.o.ctx.callback)
                    .on_tinput_int(
                        time_limit,
                        default_value,
                        show_prompt,
                        expiry_msg,
                        can_click,
                    )
                    .continue_anyhow()?;
                if let Some(r) = r {
                    resolve_array_mut_unsafe!(self, var_result:i;0);
                    var_result.val = r;
                }
            }
            (true, false, true) => {
                view_stack!(self, stack_count, time_limit:i, default_value:s, show_prompt:b, expiry_msg:s, can_click:b);
                final_stack_count = stack_count;
                let r = (self.o.ctx.callback)
                    .on_tinput_str(
                        time_limit,
                        default_value,
                        show_prompt,
                        expiry_msg,
                        can_click,
                    )
                    .continue_anyhow()?;
                if let Some(r) = r {
                    resolve_array_mut_unsafe!(self, var_results:s;0);
                    var_results.val = r.into();
                }
            }
            (false, true, false) => {
                let r = if flags.has_default_value() {
                    view_stack!(self, stack_count, default_value:i);
                    final_stack_count = stack_count;
                    (self.o.ctx.callback)
                        .on_oneinput_int(Some(default_value))
                        .continue_anyhow()?
                } else {
                    (self.o.ctx.callback)
                        .on_oneinput_int(None)
                        .continue_anyhow()?
                };
                if let Some(r) = r {
                    resolve_array_mut_unsafe!(self, var_result:i;0);
                    var_result.val = r;
                }
            }
            (true, true, false) => {
                let r = if flags.has_default_value() {
                    view_stack!(self, stack_count, default_value:s);
                    final_stack_count = stack_count;
                    (self.o.ctx.callback)
                        .on_oneinput_str(Some(default_value))
                        .continue_anyhow()?
                } else {
                    (self.o.ctx.callback)
                        .on_oneinput_str(None)
                        .continue_anyhow()?
                };
                if let Some(r) = r {
                    resolve_array_mut_unsafe!(self, var_results:s;0);
                    var_results.val = r.into();
                }
            }
            (false, true, true) => {
                view_stack!(self, stack_count, time_limit:i, default_value:i, show_prompt:b, expiry_msg:s, can_click:b);
                final_stack_count = stack_count;
                let r = (self.o.ctx)
                    .callback
                    .on_toneinput_int(
                        time_limit,
                        default_value,
                        show_prompt,
                        expiry_msg,
                        can_click,
                    )
                    .continue_anyhow()?;
                if let Some(r) = r {
                    resolve_array_mut_unsafe!(self, var_result:i;0);
                    var_result.val = r;
                }
            }
            (true, true, true) => {
                view_stack!(self, stack_count, time_limit:i, default_value:s, show_prompt:b, expiry_msg:s, can_click:b);
                final_stack_count = stack_count;
                let r = (self.o.ctx)
                    .callback
                    .on_toneinput_str(
                        time_limit,
                        default_value,
                        show_prompt,
                        expiry_msg,
                        can_click,
                    )
                    .continue_anyhow()?;
                if let Some(r) = r {
                    resolve_array_mut_unsafe!(self, var_results:s;0);
                    var_results.val = r.into();
                }
            }
        }
        self.o.stack.replace_tail(final_stack_count, []);
        self.add_ip_offset(Bc::Input { flags }.bytes_len() as i32);
        Ok(())
    }

    fn instr_kb_get_key_state(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, keycode:i);
        let r = self.o.ctx.callback.on_get_key_state(keycode);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::KbGetKeyState.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_caller_func_name(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        // TODO: Make this more efficient without needing to call `remake_site`.
        // DANGER: Touches the site execution state. Watch out for UB.
        let o = unsafe { self.optr.as_mut() };
        let mut _temp_str = ArcStr::new();
        let name = if o.frames.len() < 2 {
            ""
        } else {
            let EraExecIp { chunk, offset } = o.frames[o.frames.len() - 2].ip;
            (self.o.ctx)
                .func_info_from_chunk_pos(chunk as _, offset as _)
                .map_or("", |x| &x.name)
        };
        let r = StackValue::new_str(name.into());
        self.remake_site()?;
        self.o.stack.push(r);
        self.add_ip_offset(Bc::GetCallerFuncName.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_chara_num(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let r = StackValue::new_int(self.charas_count as _);
        self.o.stack.push(r);
        self.add_ip_offset(Bc::GetCharaNum.bytes_len() as i32);
        Ok(())
    }

    fn instr_csv_get_num(&mut self, kind: EraCsvVarKind) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, name:s);
        let r = self.o.ctx.get_csv_num(kind, name).map_or(-1, |x| x as _);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::CsvGetNum { kind }.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_random_range(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, min:i, max:i);
        if min >= max {
            anyhow::bail!("invalid range: {}..{}", min, max);
        }
        let range_len = (max - min) as u64;
        let rand_num =
            (self.i.rand_gen).gen_range(0..range_len, || self.o.ctx.callback.on_get_rand());
        let result = min.wrapping_add_unsigned(rand_num);
        let r = StackValue::new_int(result);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GetRandomRange.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_random_max(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, max:i);
        if max <= 0 {
            anyhow::bail!("invalid range: 0..{}", max);
        }
        let result =
            (self.i.rand_gen).gen_range(0..max as u64, || self.o.ctx.callback.on_get_rand());
        let r = StackValue::new_int(result as i64);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GetRandomMax.bytes_len() as i32);
        Ok(())
    }

    fn instr_row_assign(&mut self, vals_cnt: u8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arr:a, arr_idx:i, values:any:vals_cnt);
        let arr_idx = arr_idx as usize;
        let arr = self.o.resolve_variable_place_anyhow(arr)?;
        let dim = *arr.dims().last().unwrap() as usize;
        let idx_start = arr_idx % dim;
        let idx_base = arr_idx - idx_start;
        let idx_end = idx_start + values.len();
        if idx_end > dim {
            anyhow::bail!("index {} is out of bounds of {:?}", idx_end, arr.dims());
        }
        match unsafe { arr.as_unpacked_mut_unchecked() } {
            FlatArrayValueRefMut::ArrInt(x) => {
                let x = x.vals.as_mut_slice();
                for (i, val) in values.iter().enumerate() {
                    x[idx_base + idx_start + i] =
                        val.as_int().context_unlikely("expected integer")?.clone();
                }
            }
            FlatArrayValueRefMut::ArrStr(x) => {
                let x = x.vals.as_mut_slice();
                for (i, val) in values.iter().enumerate() {
                    x[idx_base + idx_start + i] =
                        val.as_str().context_unlikely("expected string")?.clone();
                }
            }
        }
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::RowAssign { vals_cnt }.bytes_len() as i32);
        Ok(())
    }

    fn instr_for_loop_step(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        // NOTE: ForLoopStep does not eat any stack values. It only modifies the array,
        //       and pushes a boolean value to the stack to indicate whether the loop
        //       should continue.
        view_stack!(self, _, arr:a, arr_idx:i, end:i, step:i);
        resolve_array_mut_unsafe!(self, arr:i;arr_idx);
        arr.val = arr.val.wrapping_add(step);
        let r = if step < 0 {
            arr.val > end
        } else {
            arr.val < end
        };
        self.o.stack.push(StackValue::new_int(r as _));
        self.add_ip_offset(Bc::ForLoopStep.bytes_len() as i32);
        Ok(())
    }

    fn instr_for_loop_no_step(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        // NOTE: ForLoopNoStep does not eat any stack values. It only pushes a boolean value
        //       to the stack to indicate whether the loop should continue.
        view_stack!(self, _, arr:a, arr_idx:i, end:i, step:i);
        resolve_array!(self, arr:i;arr_idx);
        let r = if step < 0 {
            arr.val > end
        } else {
            arr.val < end
        };
        self.o.stack.push(StackValue::new_int(r as _));
        self.add_ip_offset(Bc::ForLoopNoStep.bytes_len() as i32);
        Ok(())
    }

    fn instr_extend_str_to_width(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

        view_stack!(self, stack_count, s:s, dest_width:i);
        let dest_width = dest_width.max(0) as usize;
        let src_width = s.width();
        let r = if src_width == 0 {
            String::new()
        } else {
            let repeat_count = dest_width / src_width;
            let residual_width = dest_width % src_width;
            let mut buf = String::with_capacity(s.len() * (repeat_count + 1));
            // Repeat
            for _ in 0..repeat_count {
                buf.push_str(s);
            }
            // Residual
            let mut i = 0;
            for ch in s.chars() {
                i += ch.width().unwrap_or(0);
                if i > residual_width {
                    break;
                }
                buf.push(ch);
            }
            buf
        };
        let r = StackValue::new_str(r.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::ExtendStrToWidth.bytes_len() as i32);
        Ok(())
    }

    fn instr_html_print(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, html:s, no_single:i);
        self.o.ctx.callback.on_html_print(html, no_single);
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::HtmlPrint.bytes_len() as i32);
        Ok(())
    }

    fn instr_html_pop_printing_str(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let r = self.o.ctx.callback.on_html_popprintingstr();
        let r = StackValue::new_str(r.into());
        self.o.stack.push(r);
        self.add_ip_offset(Bc::HtmlPopPrintingStr.bytes_len() as i32);
        Ok(())
    }

    fn instr_html_get_printed_str(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, line_no:i);
        let r = self.o.ctx.callback.on_html_getprintedstr(line_no);
        let r = StackValue::new_str(r.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::HtmlGetPrintedStr.bytes_len() as i32);
        Ok(())
    }

    fn instr_html_string_len(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, content:s, return_pixel:i);
        let r = (self.o.ctx.callback).on_html_stringlen(content, return_pixel != 0);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::HtmlStringLen.bytes_len() as i32);
        Ok(())
    }

    fn instr_print_button(&mut self, flags: EraPrintExtendedFlags) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, content:s, value:s);
        self.o.ctx.callback.on_print_button(content, value, flags);
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::PrintButton { flags }.bytes_len() as i32);
        Ok(())
    }

    fn instr_print_img(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        // TODO: PrintImg, PrintImg4 ?
        anyhow::bail!("PrintImg is not yet implemented");
    }

    fn instr_print_img_with_color_matrix(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        // TODO: PrintImg, PrintImg4 ?
        anyhow::bail!("PrintImg is not yet implemented");
    }

    fn instr_print_rect(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, x:i, y:i, width:i, height:i);
        let html = format!("<shape type='rect' param='{x},{y},{width},{height}'>");
        self.o.ctx.callback.on_html_print(&html, 1);
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::PrintRect.bytes_len() as i32);
        Ok(())
    }

    fn instr_print_space(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, size:i);
        let html = format!("<shape type='space' param='{size}'>");
        self.o.ctx.callback.on_html_print(&html, 1);
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::PrintRect.bytes_len() as i32);
        Ok(())
    }

    fn instr_split_string(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, input:s, separator:s, dest:a, dest_idx:i, dest_count:a, dest_count_idx:i);
        resolve_array_mut_unsafe!(self, dest:s;dest_idx;-1, dest_count:i;dest_count_idx);
        let mut count = 0;
        for part in input.split(separator.as_str()) {
            let dest = dest
                .get_mut(count)
                .context_unlikely("invalid indices into array")?;
            dest.val = part.into();
            count += 1;
        }
        dest_count.val = count as _;
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::SplitString.bytes_len() as i32);
        Ok(())
    }

    fn instr_gcreate(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, gid:i, width:i, height:i);
        let r = self.o.ctx.callback.on_gcreate(gid, width, height);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GCreate.bytes_len() as i32);
        Ok(())
    }

    fn instr_gcreate_from_file(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, gid:i, file_path:s);
        let r = self.o.ctx.callback.on_gcreatefromfile(gid, file_path);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GCreateFromFile.bytes_len() as i32);
        Ok(())
    }

    fn instr_gdispose(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, gid:i);
        let r = self.o.ctx.callback.on_gdispose(gid);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GDispose.bytes_len() as i32);
        Ok(())
    }

    fn instr_gcreated(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, gid:i);
        let r = self.o.ctx.callback.on_gcreated(gid);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GCreated.bytes_len() as i32);
        Ok(())
    }

    fn instr_gdraw_sprite(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, gid:i, sprite_name:s, dest_x:i, dest_y:i, dest_width:i, dest_height:i);
        let r = self.o.ctx.callback.on_gdrawsprite(
            gid,
            sprite_name,
            dest_x,
            dest_y,
            dest_width,
            dest_height,
            None,
        );
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GDrawSprite.bytes_len() as i32);
        Ok(())
    }

    fn instr_gdraw_sprite_with_color_matrix(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, gid:i, sprite_name:s, dest_x:i, dest_y:i, dest_width:i, dest_height:i, color_matrix:a);
        // Parse ColorMatrix
        let color_matrix = {
            // TODO: EmuEra uses VariableTerm to convey indices info,
            //       support this by adding full indices as a single
            //       VM stack value.
            resolve_array!(self, color_matrix:i);
            if color_matrix.dims.len() < 2 {
                anyhow::bail!("expected 2D array for color matrix");
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
        let r = self.o.ctx.callback.on_gdrawsprite(
            gid,
            sprite_name,
            dest_x,
            dest_y,
            dest_width,
            dest_height,
            Some(&color_matrix),
        );
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GDrawSpriteWithColorMatrix.bytes_len() as i32);
        Ok(())
    }

    fn instr_gclear(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, gid:i, color:i);
        let r = self.o.ctx.callback.on_gclear(gid, color);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GClear.bytes_len() as i32);
        Ok(())
    }

    fn instr_sprite_create(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, sprite_name:s, gid:i, x:i, y:i, width:i, height:i);
        let r = (self.o.ctx.callback).on_spritecreate(sprite_name, gid, x, y, width, height);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::SpriteCreate.bytes_len() as i32);
        Ok(())
    }

    fn instr_sprite_dispose(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, name:s);
        let r = self.o.ctx.callback.on_spritedispose(name);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::SpriteDispose.bytes_len() as i32);
        Ok(())
    }

    fn instr_sprite_created(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, name:s);
        let r = self.o.ctx.callback.on_spritecreated(name);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::SpriteCreated.bytes_len() as i32);
        Ok(())
    }

    fn instr_sprite_anime_create(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, name:s, width:i, height:i);
        let r = self
            .o
            .ctx
            .callback
            .on_spriteanimecreate(name, width, height);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::SpriteAnimeCreate.bytes_len() as i32);
        Ok(())
    }

    fn instr_sprite_anime_add_frame(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, name:s, gid:i, x:i, y:i, width:i, height:i, offset_x:i, offset_y:i, delay:i);
        let r = (self.o.ctx.callback)
            .on_spriteanimeaddframe(name, gid, x, y, width, height, offset_x, offset_y, delay);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::SpriteAnimeAddFrame.bytes_len() as i32);
        Ok(())
    }

    fn instr_sprite_width(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, name:s);
        let r = self.o.ctx.callback.on_spritewidth(name);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::SpriteWidth.bytes_len() as i32);
        Ok(())
    }

    fn instr_sprite_height(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, name:s);
        let r = self.o.ctx.callback.on_spriteheight(name);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::SpriteHeight.bytes_len() as i32);
        Ok(())
    }

    fn instr_check_font(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, font_name:s);
        let r = self.o.ctx.callback.on_check_font(font_name);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::CheckFont.bytes_len() as i32);
        Ok(())
    }

    fn instr_save_text(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        anyhow::bail!("SaveText not yet implemented");
    }

    fn instr_load_text(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        anyhow::bail!("LoadText not yet implemented");
    }

    fn instr_generic_find_element<const INSTR_LEN: usize>(
        &mut self,
        is_first: bool,
        dim_pos: i64,
    ) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arr:a, arr_idx:i, value:any, start_idx:i, end_idx:i);
        let start_idx = start_idx.max(0) as usize;
        let end_idx = if end_idx < 0 {
            usize::MAX
        } else {
            end_idx as usize
        };
        let r = match resolve_array_kind!(self, arr) {
            ArrayValueKind::ArrInt => {
                resolve_array!(self, arr:i;arr_idx;dim_pos);
                resolve_any_scalar!(self, value:i);
                let end_idx = end_idx.min(arr.len());

                let mut iter = arr
                    .iter()
                    .skip(start_idx)
                    .take(end_idx.saturating_sub(start_idx));
                let idx = if is_first {
                    iter.position(|x| x.val == value)
                } else {
                    iter.rposition(|x| x.val == value)
                };
                idx.map_or(-1, |x| (x + start_idx) as _)
            }
            ArrayValueKind::ArrStr => {
                resolve_array!(self, arr:s;arr_idx;dim_pos);
                resolve_any_scalar!(self, value:s);
                let end_idx = end_idx.min(arr.len());

                let mut iter = arr
                    .iter()
                    .skip(start_idx)
                    .take(end_idx.saturating_sub(start_idx));
                let idx = if is_first {
                    iter.position(|x| x.val == *value)
                } else {
                    iter.rposition(|x| x.val == *value)
                };
                idx.map_or(-1, |x| (x + start_idx) as _)
            }
        };
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(INSTR_LEN as i32);
        Ok(())
    }

    fn instr_generic_find_element_with_match<const INSTR_LEN: usize>(
        &mut self,
        is_first: bool,
        dim_pos: i64,
    ) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arr:a, arr_idx:i, value:any, start_idx:i, end_idx:i, complete_match:b);
        let start_idx = start_idx.max(0) as usize;
        let end_idx = if end_idx < 0 {
            usize::MAX
        } else {
            end_idx as usize
        };
        let r = match resolve_array_kind!(self, arr) {
            ArrayValueKind::ArrInt => {
                resolve_array!(self, arr:i;arr_idx;dim_pos);
                resolve_any_scalar!(self, value:i);
                let end_idx = end_idx.min(arr.len());

                let mut iter = arr
                    .iter()
                    .skip(start_idx)
                    .take(end_idx.saturating_sub(start_idx));
                let idx = if is_first {
                    iter.position(|x| x.val == value)
                } else {
                    iter.rposition(|x| x.val == value)
                };
                idx.map_or(-1, |x| (x + start_idx) as _)
            }
            ArrayValueKind::ArrStr => {
                resolve_array!(self, arr:s;arr_idx;dim_pos);
                resolve_any_scalar!(self, value:s);
                let end_idx = end_idx.min(arr.len());

                // Compile regex
                let re_str = if complete_match {
                    &rcstr::format!("^(?:{})$", value)
                } else {
                    value
                };
                let re = (self.i.regex_cache)
                    .try_get_or_insert(re_str.clone(), || {
                        // Compile twice to ensure input is safe
                        regex::Regex::new(&value).and_then(|_| regex::Regex::new(&re_str))
                    })
                    .with_context_unlikely(|| format!("invalid regex: `{:?}`", value))?;

                let mut iter = arr
                    .iter()
                    .skip(start_idx)
                    .take(end_idx.saturating_sub(start_idx));
                let idx = if is_first {
                    iter.position(|x| re.is_match(&x.val))
                } else {
                    iter.rposition(|x| re.is_match(&x.val))
                };
                idx.map_or(-1, |x| (x + start_idx) as _)
            }
        };
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(INSTR_LEN as i32);
        Ok(())
    }

    fn instr_var_set(&mut self, dim_pos: i64) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, arr:a, arr_idx:i, value:any, start_idx:i, end_idx:i);
        let start_idx = start_idx.max(0) as usize;
        let end_idx = if end_idx < 0 {
            usize::MAX
        } else {
            end_idx as usize
        };
        match resolve_array_kind!(self, arr) {
            ArrayValueKind::ArrInt => {
                resolve_array_mut_unsafe!(self, arr:i;arr_idx;dim_pos);
                resolve_any_scalar!(self, value:i);
                let end_idx = end_idx.min(arr.len());
                if let Some(arr) = arr.as_slice_mut() {
                    // Optimize for 1-D (dim_pos = 0)
                    arr.get_mut(start_idx..end_idx)
                        .context_unlikely("invalid indices into array")?
                        .fill_with(|| IntValue { val: value.clone() });
                } else {
                    for i in start_idx..end_idx {
                        arr.get_mut(i)
                            .context_unlikely("invalid indices into array")?
                            .val = value.clone();
                    }
                }
            }
            ArrayValueKind::ArrStr => {
                resolve_array_mut_unsafe!(self, arr:s;arr_idx;dim_pos);
                resolve_any_scalar!(self, value:s);
                let end_idx = end_idx.min(arr.len());
                if let Some(arr) = arr.as_slice_mut() {
                    // Optimize for 1-D (dim_pos = 0)
                    arr.get_mut(start_idx..end_idx)
                        .context_unlikely("invalid indices into array")?
                        .fill_with(|| StrValue { val: value.clone() });
                } else {
                    for i in start_idx..end_idx {
                        arr.get_mut(i)
                            .context_unlikely("invalid indices into array")?
                            .val = value.clone();
                    }
                }
            }
        }
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::VarSet.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_var_size_by_name(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, name:s, dim_pos:i);
        let var = (self.o.get_var_by_name(name))
            .with_context_unlikely(|| format!("variable `{}` not found", name))?;
        let dims = var.dims();
        let r = if dim_pos < 0 {
            let dim_pos = dim_pos.wrapping_add_unsigned(dims.len() as _) as usize;
            dims.get(dim_pos).copied()
        } else {
            dims.get(dim_pos as usize).copied()
        }
        .with_context_unlikely(|| format!("dimension index `{dim_pos}` out of bounds"))?;
        let r = StackValue::new_int(r as _);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GetVarSizeByName.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_var_all_size(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let dims = {
            view_stack!(self, _, arr:a);
            let arr = self.o.resolve_variable_place_anyhow(arr)?;
            arr.dims().clone()
        };
        let var_result = self.var_result_place;
        resolve_array_mut_unsafe!(self, var_result:i;0;-1);
        let var_result = var_result.as_slice_mut().unwrap();
        let len = dims.len().min(var_result.len());
        for i in 0..len {
            var_result[i].val = dims[i] as i64;
        }
        self.o.stack.replace_tail(1, []);
        self.add_ip_offset(Bc::GetVarAllSize.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_host_time_raw(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let r = self.o.ctx.callback.on_get_host_time();
        self.o.stack.push(StackValue::new_int(r as _));
        self.add_ip_offset(Bc::GetHostTimeRaw.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_host_time(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use chrono::*;
        // NOTE: Native time zone info is used
        let result = self.o.ctx.callback.on_get_host_time();
        let t = DateTime::from_timestamp_millis(result as _).unwrap();
        let t: DateTime<Local> = t.into();
        let mut r = t.year() as i64;
        r = r * 100 + t.month() as i64;
        r = r * 100 + t.day() as i64;
        r = r * 100 + t.hour() as i64;
        r = r * 100 + t.minute() as i64;
        r = r * 100 + t.second() as i64;
        r = r * 1000 + (t.nanosecond() as i64) / 1_000_000;
        self.o.stack.push(StackValue::new_int(r));
        self.add_ip_offset(Bc::GetHostTime.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_host_time_s(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use chrono::*;

        // NOTE: Native time zone info is used
        let r = self.o.ctx.callback.on_get_host_time();
        let t = DateTime::from_timestamp_millis(r as _).unwrap();
        let t: DateTime<Local> = t.into();
        let r = t.format("%Y/%m/%d %H:%M:%S");
        let r = StackValue::new_str(r.to_string().into());
        self.o.stack.push(r);
        self.add_ip_offset(Bc::GetHostTimeS.bytes_len() as i32);
        Ok(())
    }

    fn instr_csv_get_prop_2(&mut self, csv_kind: EraCharaCsvPropType) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use EraCharaCsvPropType::*;

        view_stack!(self, stack_count, chara_no:i, index:i);
        let chara_no = chara_no as u32;
        let index = index as u32;
        let chara_tmpl = self
            .o
            .ctx
            .chara_templates
            .get(&chara_no)
            .with_context_unlikely(|| format!("chara template {} not found", chara_no))?;
        let r = match csv_kind {
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
                let r = r.cloned().unwrap_or_default();
                StackValue::new_str(r)
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
                let r = r.cloned().unwrap_or_default();
                StackValue::new_int(r)
            }
        };
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::CsvGetProp2 { csv_kind }.bytes_len() as i32);
        Ok(())
    }

    fn instr_chara_csv_exists(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, chara_no:i);
        let chara_no = chara_no as u32;
        let r = self.o.ctx.chara_templates.contains_key(&chara_no);
        let r = StackValue::new_int(r as _);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::CharaCsvExists.bytes_len() as i32);
        Ok(())
    }

    fn instr_generic_get_lv<const INSTR_LEN: usize>(
        &mut self,
        target_arr: &str,
    ) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, value:i, max_lv:i);
        let target_arr = self
            .o
            .ctx
            .variables
            .get_var(target_arr)
            .with_context_unlikely(|| format!("variable `{}` not found", target_arr))?;
        let target_arr = target_arr.as_arrint().context_unlikely("expected ArrInt")?;
        let mut r = 0;
        while r < max_lv.min(target_arr.vals.len() as i64) {
            let limit = target_arr.vals[(r + 1) as usize].val;
            if value < limit {
                break;
            }
            r += 1;
        }
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(INSTR_LEN as i32);
        Ok(())
    }

    fn instr_add_chara(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, chara_tmpl_no:i);
        if let Some(cur_charas_cap) = (self.o.ctx.variables).charas_var_capacity() {
            let chara_reg_slot = self.charas_count;
            if chara_reg_slot >= cur_charas_cap {
                // Grow charas variable
                (self.o.ctx.variables)
                    .grow_charas_var_capacity(crate::v2::engine::CHARA_CAP_GROWTH_STEP);
            }

            // Add character
            let chara_reg_slot = chara_reg_slot as usize;
            let chara_tmpl = (self.o.ctx.i.chara_templates)
                .get(&(chara_tmpl_no as u32))
                .with_context_unlikely(|| format!("chara template {} not found", chara_tmpl_no))?;
            for chara_var in self.o.ctx.i.variables.chara_vars_iter_mut() {
                chara_var.val.ensure_alloc();
                let dims = chara_var.val.dims();
                let stride: usize = dims.iter().skip(1).map(|&x| x as usize).product();
                let start_idx = chara_reg_slot * stride;
                let end_idx = (chara_reg_slot + 1) * stride;
                match chara_var.val.as_unpacked_mut() {
                    FlatArrayValueRefMut::ArrInt(x) => match chara_var.name.as_ref() {
                        "NO" => {
                            x.vals[start_idx].val = chara_tmpl_no;
                        }
                        _ => {
                            let src = match chara_var.name.as_ref() {
                                "BASE" => &chara_tmpl.maxbase,
                                "MAXBASE" => &chara_tmpl.maxbase,
                                "MARK" => &chara_tmpl.mark,
                                "EXP" => &chara_tmpl.exp,
                                "ABL" => &chara_tmpl.abl,
                                "TALENT" => &chara_tmpl.talent,
                                "RELATION" => &chara_tmpl.relation,
                                "CFLAG" => &chara_tmpl.cflag,
                                "EQUIP" => &chara_tmpl.equip,
                                "JUEL" => &chara_tmpl.juel,
                                _ => &Default::default(),
                            };
                            x.vals[start_idx..end_idx].fill(Default::default());
                            for (&sk, &sv) in src {
                                x.vals[start_idx + sk as usize].val = sv;
                            }
                        }
                    },
                    FlatArrayValueRefMut::ArrStr(x) => match chara_var.name.as_ref() {
                        "NAME" | "CALLNAME" | "NICKNAME" | "MASTERNAME" => {
                            let src = match chara_var.name.as_ref() {
                                "NAME" => &chara_tmpl.name,
                                "CALLNAME" => &chara_tmpl.callname,
                                "NICKNAME" => &chara_tmpl.nickname,
                                "MASTERNAME" => &chara_tmpl.mastername,
                                _ => unreachable!(),
                            };
                            x.vals[start_idx].val = src.clone();
                        }
                        _ => {
                            let src = match chara_var.name.as_ref() {
                                "CSTR" => &chara_tmpl.cstr,
                                _ => &Default::default(),
                            };
                            x.vals[start_idx..end_idx].fill(Default::default());
                            for (&sk, sv) in src {
                                x.vals[start_idx + sk as usize] = StrValue { val: sv.clone() };
                            }
                        }
                    },
                }
            }
        }
        self.charas_count += 1;
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::AddChara.bytes_len() as i32);
        Ok(())
    }

    fn instr_add_void_chara(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        if let Some(cur_charas_cap) = (self.o.ctx.variables).charas_var_capacity() {
            let chara_reg_slot = self.charas_count;
            if chara_reg_slot >= cur_charas_cap {
                // Grow charas variable
                (self.o.ctx.variables)
                    .grow_charas_var_capacity(crate::v2::engine::CHARA_CAP_GROWTH_STEP);
            }

            // Add character
            let chara_reg_slot = chara_reg_slot as usize;
            for chara_var in self.o.ctx.i.variables.chara_vars_iter_mut() {
                chara_var.val.ensure_alloc();
                let dims = chara_var.val.dims();
                let stride: usize = dims.iter().skip(1).map(|&x| x as usize).product();
                let start_idx = chara_reg_slot * stride;
                let end_idx = (chara_reg_slot + 1) * stride;
                match chara_var.val.as_unpacked_mut() {
                    FlatArrayValueRefMut::ArrInt(x) => {
                        x.vals[start_idx..end_idx].fill(Default::default())
                    }
                    FlatArrayValueRefMut::ArrStr(x) => {
                        x.vals[start_idx..end_idx].fill(Default::default())
                    }
                }
            }
        }
        self.charas_count += 1;
        self.add_ip_offset(Bc::AddVoidChara.bytes_len() as i32);
        Ok(())
    }

    fn instr_pick_up_chara(&mut self, charas_cnt: u8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use crate::util::swap_slice_with_stride;

        view_stack!(self, stack_count, chara_nos:any:charas_cnt);
        let chara_nos = dedup_chara_numbers(chara_nos, self.charas_count as _)?;

        // Start pickup
        let mut chara_nos = chara_nos.iter().map(|&x| x as usize).collect_vec();
        // FIXME: Adjust TARGET:0 & MASTER:0 accordingly
        for orig_idx in 0..chara_nos.len() {
            let pickup_idx = chara_nos[orig_idx] as usize;
            if orig_idx == pickup_idx {
                continue;
            }
            for chara_var in self.o.ctx.variables.chara_vars_iter_mut() {
                match chara_var.val.as_unpacked_mut() {
                    FlatArrayValueRefMut::ArrInt(x) => {
                        let stride: usize = x.dims.iter().skip(1).map(|&x| x as usize).product();
                        swap_slice_with_stride(&mut x.vals, stride, orig_idx, pickup_idx);
                    }
                    FlatArrayValueRefMut::ArrStr(x) => {
                        let stride: usize = x.dims.iter().skip(1).map(|&x| x as usize).product();
                        swap_slice_with_stride(&mut x.vals, stride, orig_idx, pickup_idx);
                    }
                }
            }
            // Update indices as the consequence of swapping
            if let Some(idx) = chara_nos.iter().position(|&x| x == orig_idx) {
                chara_nos[idx] = pickup_idx;
            }
        }

        self.charas_count = chara_nos.len() as _;
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::PickUpChara { charas_cnt }.bytes_len() as i32);
        Ok(())
    }

    fn instr_delete_chara(&mut self, charas_cnt: u8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use crate::util::swap_slice_with_stride;

        view_stack!(self, stack_count, chara_nos:any:charas_cnt);
        let chara_nos = sort_dedup_chara_numbers(chara_nos, self.charas_count as _)?;

        // Delete characters
        let charas_count = self.charas_count as usize;
        for chara_var in self.o.ctx.variables.chara_vars_iter_mut() {
            let mut rd = 0;
            let mut wr = 0;
            let mut chara_nos = chara_nos.iter().map(|&x| x as usize).peekable();

            let dims = chara_var.val.dims();
            let stride: usize = dims.iter().skip(1).map(|&x| x as usize).product();
            match chara_var.val.as_unpacked_mut() {
                FlatArrayValueRefMut::ArrInt(x) => {
                    while rd < charas_count {
                        if Some(rd) == chara_nos.peek().copied() {
                            // Delete character
                            chara_nos.next();
                        } else {
                            // Retain character
                            swap_slice_with_stride(&mut x.vals, stride, rd, wr);
                            wr += 1;
                        }
                        rd += 1;
                    }
                    x.vals[wr * stride..].fill(Default::default());
                }
                FlatArrayValueRefMut::ArrStr(x) => {
                    while rd < charas_count {
                        if Some(rd) == chara_nos.peek().copied() {
                            // Delete character
                            chara_nos.next();
                        } else {
                            // Retain character
                            swap_slice_with_stride(&mut x.vals, stride, rd, wr);
                            wr += 1;
                        }
                        rd += 1;
                    }
                    x.vals[wr * stride..].fill(Default::default());
                }
            }
        }

        self.charas_count -= chara_nos.len() as u32;
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::DeleteChara { charas_cnt }.bytes_len() as i32);
        Ok(())
    }

    fn instr_swap_chara(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use crate::util::swap_slice_with_stride;

        view_stack!(self, stack_count, chara_no1:i, chara_no2:i);
        let chara_no1 = sanitize_chara_no(chara_no1, self.charas_count)? as usize;
        let chara_no2 = sanitize_chara_no(chara_no2, self.charas_count)? as usize;

        for chara_var in self.o.ctx.variables.chara_vars_iter_mut() {
            let dims = chara_var.val.dims();
            let stride: usize = dims.iter().skip(1).map(|&x| x as usize).product();
            match chara_var.val.as_unpacked_mut() {
                FlatArrayValueRefMut::ArrInt(x) => {
                    swap_slice_with_stride(&mut x.vals, stride, chara_no1, chara_no2);
                }
                FlatArrayValueRefMut::ArrStr(x) => {
                    swap_slice_with_stride(&mut x.vals, stride, chara_no1, chara_no2);
                }
            }
        }

        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::SwapChara.bytes_len() as i32);
        Ok(())
    }

    fn instr_add_copy_chara(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        anyhow::bail!("AddCopyChara not yet implemented");
    }

    fn instr_load_data(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use crate::v2::savefs::EraSaveFileType;

        view_stack!(self, stack_count, save_id:i);
        let file = self.o.ctx.get_save_path(save_id as _);
        let r = match self
            .o
            .routine_load_data(&file, true, EraSaveFileType::Normal)
        {
            Ok(r) => {
                if let Some(charas_count) = r.charas_count {
                    self.charas_count = charas_count;
                }
                r.file_exists.into()
            }
            Err(e) => {
                let mut diag = Diagnostic::new();
                diag.span_err(
                    self.o.cur_filename(),
                    self.o.cur_bc_span(),
                    format!("failed to load data from `{}`: {}", file, e),
                );
                self.o.ctx.emit_diag(diag);
                -1
            }
        };
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::LoadData.bytes_len() as i32);
        Ok(())
    }

    fn instr_save_data(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, save_id:i, save_info:s);
        let save_info = &save_info.clone();
        let file = self.o.ctx.get_save_path(save_id as _);
        let r = match self
            .o
            .routine_save_data(&file, save_info, self.charas_count)
        {
            Ok(()) => 1,
            Err(e) => {
                let mut diag = Diagnostic::new();
                diag.span_err(
                    self.o.cur_filename(),
                    self.o.cur_bc_span(),
                    format!("failed to save data to `{}`: {}", file, e),
                );
                self.o.ctx.emit_diag(diag);
                0
            }
        };
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::SaveData.bytes_len() as i32);
        Ok(())
    }

    fn instr_check_data(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let vresult = self.var_result_place;
        let vresults = self.var_results_place;

        view_stack!(self, stack_count, save_id:i);
        let file = self.o.ctx.get_save_path(save_id as _);
        let r = match self
            .o
            .routine_check_data(&file)
            .map(|x| (x.status, x.timestamp, x.save_info))
            .unwrap_or_else(|e| (4, 0, e.to_string()))
        {
            (0, timestamp, save_info) => {
                resolve_array_mut_unsafe!(self, vresult:i;0, vresults:s;0);
                vresult.val = timestamp as _;
                vresults.val = save_info.into();
                0
            }
            (status, _, error) => {
                resolve_array_mut_unsafe!(self, vresults:s;0);
                vresults.val = error.into();
                status
            }
        };
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::CheckData.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_chara_reg_num(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, chara_tmpl_no:i);
        let var_no = self.o.get_global_var_int("NO")?;
        let var_no = MaskedArr::try_new(var_no, 0, -2)
            .context_unlikely("expected 1D array of integers for `NO`")?;
        let r = var_no
            .iter()
            .position(|x| x.val == chara_tmpl_no)
            .map(|x| x as i64)
            .unwrap_or(-1);
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GetCharaRegNum.bytes_len() as i32);
        Ok(())
    }

    fn instr_load_global(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        use crate::v2::savefs::EraSaveFileType;

        let file = self.o.ctx.get_global_save_path();
        let r = match self
            .o
            .routine_load_data(&file, false, EraSaveFileType::Global)
        {
            Ok(_) => 1,
            Err(e) => {
                let mut diag = Diagnostic::new();
                diag.span_err(
                    self.o.cur_filename(),
                    self.o.cur_bc_span(),
                    format!("failed to load global data from `{}`: {}", file, e),
                );
                self.o.ctx.emit_diag(diag);
                0
            }
        };
        self.o.stack.push(StackValue::new_int(r));
        self.add_ip_offset(Bc::LoadGlobal.bytes_len() as i32);
        Ok(())
    }

    fn instr_save_global(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        let file = self.o.ctx.get_global_save_path();
        let r = match self.o.routine_save_global_data(&file) {
            Ok(_) => 1,
            Err(e) => {
                let mut diag = Diagnostic::new();
                diag.span_err(
                    self.o.cur_filename(),
                    self.o.cur_bc_span(),
                    format!("failed to save global data to `{}`: {}", file, e),
                );
                self.o.ctx.emit_diag(diag);
                0
            }
        };
        self.o.stack.push(StackValue::new_int(r));
        self.add_ip_offset(Bc::SaveGlobal.bytes_len() as i32);
        Ok(())
    }

    fn instr_reset_data(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        self.o.routine_reset_data()?;
        self.charas_count = 0;
        self.add_ip_offset(Bc::ResetData.bytes_len() as i32);
        Ok(())
    }

    fn instr_reset_chara_stain(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, chara_no:i);
        let chara_no = sanitize_chara_no(chara_no, self.charas_count)?;
        let var_stain = unsafe { self.o.get_global_var_int_mut_unchecked("STAIN")? };
        let var_default_stain = self.o.get_global_var_int("DEFAULT_STAIN")?;
        let stride: usize = var_stain.dims[1..].iter().map(|&x| x as usize).product();
        let mut var_stain = MaskedArr::try_new(var_stain, (chara_no as usize * stride) as _, -1)
            .context_unlikely("invalid indices into array")?;
        let var_default_stain = MaskedArr::try_new(var_default_stain, 0, -1)
            .context_unlikely("variable `DEFAULT_STAIN` is empty")?;
        let default_stain = var_default_stain
            .iter()
            .map(|x| x.val)
            .chain(std::iter::repeat(0));
        for (stain, default_stain) in var_stain.iter_mut().zip(default_stain) {
            stain.val = default_stain;
        }
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::ResetCharaStain.bytes_len() as i32);
        Ok(())
    }

    fn instr_save_chara(&mut self, charas_cnt: u8) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, filename:s, memo:s, chara_nos:any:charas_cnt);
        let chara_nos = dedup_chara_numbers(chara_nos, self.charas_count)?;
        // TODO: Bc::SaveChara
        {
            let mut diag = Diagnostic::new();
            diag.span_err(
                self.o.cur_filename(),
                self.o.cur_bc_span(),
                "SaveChara not yet implemented",
            );
            self.o.ctx.emit_diag(diag);
        }
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::SaveChara { charas_cnt }.bytes_len() as i32);
        Ok(())
    }

    fn instr_load_chara(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, filename:s);
        // TODO: Bc::LoadChara
        {
            let mut diag = Diagnostic::new();
            diag.span_err(
                self.o.cur_filename(),
                self.o.cur_bc_span(),
                "LoadChara not yet implemented",
            );
            self.o.ctx.emit_diag(diag);
        }
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::LoadChara.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_config(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, key:s);
        let r = self.o.ctx.callback.on_get_config_int(key)?;
        let r = StackValue::new_int(r);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GetConfig.bytes_len() as i32);
        Ok(())
    }

    fn instr_get_config_s(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, key:s);
        let r = self.o.ctx.callback.on_get_config_str(key)?;
        let r = StackValue::new_str(r.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::GetConfigS.bytes_len() as i32);
        Ok(())
    }

    fn instr_find_chara_data_file(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, filename:s);
        // TODO: Bc::FindCharaDataFile
        {
            let mut diag = Diagnostic::new();
            diag.span_err(
                self.o.cur_filename(),
                self.o.cur_bc_span(),
                "FindCharaDataFile not yet implemented",
            );
            self.o.ctx.emit_diag(diag);
        }
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::FindCharaDataFile.bytes_len() as i32);
        Ok(())
    }

    fn instr_eval_str_form(&mut self) -> anyhow::Result<()> {
        self.instr_eval_str_form_with_return_value()?;
        Ok(())
    }

    fn instr_eval_str_form_with_return_value(&mut self) -> anyhow::Result<usize> {
        self.ensure_pre_step_instruction()?;

        // DANGER: Touches the site execution state. Watch out for UB.
        let instr_len = Bc::EvalStrForm.bytes_len() as u32;
        view_stack!(self, _, eval_str:s);
        let eval_str = arcstr::ArcStr::from(eval_str.as_str());

        // Check call stack depth
        let ret_ip = EraExecIp {
            chunk: self.o.cur_frame.ip.chunk,
            offset: self.o.cur_frame.ip.offset + instr_len,
        };
        let o: &mut EraVirtualMachineState = self.ensure_call_stack_and_get_o()?;
        let mut new_frame = o
            .diverge_transient_exec_frame_from(o.frames.len() - 1)
            .context_unlikely("failed to diverge transient frame")?;
        let env_func = {
            let ip = new_frame.ip;
            let (env_func, _) = (self.o.ctx)
                .func_idx_and_info_from_chunk_pos(ip.chunk as _, ip.offset as _)
                .context_unlikely("failed to get function info")?;
            env_func
        };
        new_frame.real_stack_start -= 1;
        new_frame.ret_ip = ret_ip;
        new_frame.ignore_return_value = false;

        // Add to sources map and compile
        let filename = ArcStr::from(self.o.ctx.generate_next_transient_src_name());
        if !self.o.ctx.push_source_file(
            filename.clone(),
            eval_str.clone(),
            EraSourceFileKind::Source,
            true,
        ) {
            anyhow::bail!("source file `{filename}` already exists");
        }
        let ast = {
            let mut lexer = EraLexer::new(filename.clone(), &eval_str, false);
            lexer.set_ignore_newline_suppression(true);
            let mut is_str_var_fn = |_: &str| false;
            // TODO: Use interner from transient_ctx
            let mut parser = EraParser::new(
                &mut self.o.ctx.callback,
                &self.o.ctx.i,
                &mut lexer,
                &self.o.ctx.i.interner(),
                &self.o.ctx.i.global_replace,
                &self.o.ctx.i.global_define,
                &mut is_str_var_fn,
                false,
                true,
            );
            parser.parse_string_form()
        };
        let bc_chunk = {
            // TODO: Use interner from transient_ctx
            let func_entries = self.o.ctx.func_entries.clone();
            let env_func = func_entries
                .get_index(env_func)
                .unwrap()
                .1
                .as_ref()
                .unwrap();
            let mut compiler = EraCodeGenerator::new(self.o.ctx, true, true, true);
            let Ok(chunk) =
                compiler.compile_str_expr(filename.clone(), env_func, &ast.nodes, ast.root_node)
            else {
                anyhow::bail!("failed to compile `{}`", eval_str);
            };
            chunk
        };
        let bc_chunks =
            Arc::get_mut(&mut self.o.ctx.bc_chunks).context_unlikely("bc_chunks is shared")?;
        let chunk_idx = bc_chunks.len();
        let bc_chunk_len = bc_chunk.len();
        bc_chunks.push(bc_chunk);
        let func_entries = Arc::get_mut(&mut self.o.ctx.func_entries)
            .context_unlikely("func_entries is shared")?;
        let func_idx = func_entries.len();
        let func_name = filename.clone();
        func_entries.insert(
            Ascii::new(func_name.clone()),
            Some(EraFuncInfo {
                name: func_name.clone(),
                name_span: Default::default(),
                frame_info: Default::default(),
                chunk_idx: chunk_idx as _,
                bc_offset: 0,
                bc_size: bc_chunk_len as _,
                ret_kind: ScalarValueKind::Str,
                is_transient: true,
            }),
        );

        // Prepare for execution environment
        o.stack.pop().unwrap();

        new_frame.ip = EraExecIp {
            chunk: chunk_idx as _,
            offset: 0,
        };

        // Create a new execution frame for the function
        o.frames.push(new_frame);
        self.remake_site()?;

        Ok(func_idx)
    }

    fn instr_eval_int_expr(&mut self) -> anyhow::Result<()> {
        self.instr_eval_int_expr_with_return_value()?;
        Ok(())
    }

    fn instr_eval_int_expr_with_return_value(&mut self) -> anyhow::Result<usize> {
        self.ensure_pre_step_instruction()?;

        // DANGER: Touches the site execution state. Watch out for UB.
        let instr_len = Bc::EvalStrForm.bytes_len() as u32;
        view_stack!(self, _, eval_str:s);
        let eval_str = arcstr::ArcStr::from(eval_str.as_str());

        // Check call stack depth
        let ret_ip = EraExecIp {
            chunk: self.o.cur_frame.ip.chunk,
            offset: self.o.cur_frame.ip.offset + instr_len,
        };
        let o: &mut EraVirtualMachineState = self.ensure_call_stack_and_get_o()?;
        let mut new_frame = o
            .diverge_transient_exec_frame_from(o.frames.len() - 1)
            .context_unlikely("failed to diverge transient frame")?;
        let env_func = {
            let ip = new_frame.ip;
            let (env_func, _) = (self.o.ctx)
                .func_idx_and_info_from_chunk_pos(ip.chunk as _, ip.offset as _)
                .context_unlikely("failed to get function info")?;
            env_func
        };
        new_frame.real_stack_start -= 1;
        new_frame.ret_ip = ret_ip;
        new_frame.ignore_return_value = false;

        // Add to sources map and compile
        let filename = ArcStr::from(self.o.ctx.generate_next_transient_src_name());
        if !self.o.ctx.push_source_file(
            filename.clone(),
            eval_str.clone(),
            EraSourceFileKind::Source,
            true,
        ) {
            anyhow::bail!("source file `{filename}` already exists");
        }
        let ast = {
            let mut lexer = EraLexer::new(filename.clone(), &eval_str, false);
            lexer.set_ignore_newline_suppression(true);
            let mut is_str_var_fn = |_: &str| false;
            // TODO: Use interner from transient_ctx
            let mut parser = EraParser::new(
                &mut self.o.ctx.callback,
                &self.o.ctx.i,
                &mut lexer,
                &self.o.ctx.i.interner(),
                &self.o.ctx.i.global_replace,
                &self.o.ctx.i.global_define,
                &mut is_str_var_fn,
                false,
                true,
            );
            parser.parse_expression()
        };
        let bc_chunk = {
            // TODO: Use interner from transient_ctx
            let func_entries = self.o.ctx.func_entries.clone();
            let env_func = func_entries
                .get_index(env_func)
                .unwrap()
                .1
                .as_ref()
                .unwrap();
            let mut compiler = EraCodeGenerator::new(self.o.ctx, true, true, true);
            let Ok(chunk) =
                compiler.compile_int_expr(filename.clone(), env_func, &ast.nodes, ast.root_node)
            else {
                anyhow::bail!("failed to compile `{}`", eval_str);
            };
            chunk
        };
        let bc_chunks =
            Arc::get_mut(&mut self.o.ctx.bc_chunks).context_unlikely("bc_chunks is shared")?;
        let chunk_idx = bc_chunks.len();
        let bc_chunk_len = bc_chunk.len();
        bc_chunks.push(bc_chunk);
        let func_entries = Arc::get_mut(&mut self.o.ctx.func_entries)
            .context_unlikely("func_entries is shared")?;
        let func_idx = func_entries.len();
        let func_name = filename.clone();
        func_entries.insert(
            Ascii::new(func_name.clone()),
            Some(EraFuncInfo {
                name: func_name.clone(),
                name_span: Default::default(),
                frame_info: Default::default(),
                chunk_idx: chunk_idx as _,
                bc_offset: 0,
                bc_size: bc_chunk_len as _,
                ret_kind: ScalarValueKind::Int,
                is_transient: true,
            }),
        );

        // Prepare for execution environment
        o.stack.pop().unwrap();

        new_frame.ip = EraExecIp {
            chunk: chunk_idx as _,
            offset: 0,
        };

        // Create a new execution frame for the function
        o.frames.push(new_frame);
        self.remake_site()?;

        Ok(func_idx)
    }

    fn instr_eval_str_expr(&mut self) -> anyhow::Result<()> {
        self.instr_eval_str_expr_with_return_value()?;
        Ok(())
    }

    fn instr_eval_str_expr_with_return_value(&mut self) -> anyhow::Result<usize> {
        self.ensure_pre_step_instruction()?;

        // DANGER: Touches the site execution state. Watch out for UB.
        let instr_len = Bc::EvalStrForm.bytes_len() as u32;
        view_stack!(self, _, eval_str:s);
        let eval_str = arcstr::ArcStr::from(eval_str.as_str());

        // Check call stack depth
        let ret_ip = EraExecIp {
            chunk: self.o.cur_frame.ip.chunk,
            offset: self.o.cur_frame.ip.offset + instr_len,
        };
        let o: &mut EraVirtualMachineState = self.ensure_call_stack_and_get_o()?;
        let mut new_frame = o
            .diverge_transient_exec_frame_from(o.frames.len() - 1)
            .context_unlikely("failed to diverge transient frame")?;
        let env_func = {
            let ip = new_frame.ip;
            let (env_func, _) = (self.o.ctx)
                .func_idx_and_info_from_chunk_pos(ip.chunk as _, ip.offset as _)
                .context_unlikely("failed to get function info")?;
            env_func
        };
        new_frame.real_stack_start -= 1;
        new_frame.ret_ip = ret_ip;
        new_frame.ignore_return_value = false;

        // Add to sources map and compile
        let filename = ArcStr::from(self.o.ctx.generate_next_transient_src_name());
        if !self.o.ctx.push_source_file(
            filename.clone(),
            eval_str.clone(),
            EraSourceFileKind::Source,
            true,
        ) {
            anyhow::bail!("source file `{filename}` already exists");
        }
        let ast = {
            let mut lexer = EraLexer::new(filename.clone(), &eval_str, false);
            lexer.set_ignore_newline_suppression(true);
            let mut is_str_var_fn = |_: &str| false;
            // TODO: Use interner from transient_ctx
            let mut parser = EraParser::new(
                &mut self.o.ctx.callback,
                &self.o.ctx.i,
                &mut lexer,
                &self.o.ctx.i.interner(),
                &self.o.ctx.i.global_replace,
                &self.o.ctx.i.global_define,
                &mut is_str_var_fn,
                false,
                true,
            );
            parser.parse_expression()
        };
        let bc_chunk = {
            // TODO: Use interner from transient_ctx
            let func_entries = self.o.ctx.func_entries.clone();
            let env_func = func_entries
                .get_index(env_func)
                .unwrap()
                .1
                .as_ref()
                .unwrap();
            let mut compiler = EraCodeGenerator::new(self.o.ctx, true, true, true);
            let Ok(chunk) =
                compiler.compile_str_expr(filename.clone(), env_func, &ast.nodes, ast.root_node)
            else {
                anyhow::bail!("failed to compile `{}`", eval_str);
            };
            chunk
        };
        let bc_chunks =
            Arc::get_mut(&mut self.o.ctx.bc_chunks).context_unlikely("bc_chunks is shared")?;
        let chunk_idx = bc_chunks.len();
        let bc_chunk_len = bc_chunk.len();
        bc_chunks.push(bc_chunk);
        let func_entries = Arc::get_mut(&mut self.o.ctx.func_entries)
            .context_unlikely("func_entries is shared")?;
        let func_idx = func_entries.len();
        let func_name = filename.clone();
        func_entries.insert(
            Ascii::new(func_name.clone()),
            Some(EraFuncInfo {
                name: func_name.clone(),
                name_span: Default::default(),
                frame_info: Default::default(),
                chunk_idx: chunk_idx as _,
                bc_offset: 0,
                bc_size: bc_chunk_len as _,
                ret_kind: ScalarValueKind::Str,
                is_transient: true,
            }),
        );

        // Prepare for execution environment
        o.stack.pop().unwrap();

        new_frame.ip = EraExecIp {
            chunk: chunk_idx as _,
            offset: 0,
        };

        // Create a new execution frame for the function
        o.frames.push(new_frame);
        self.remake_site()?;

        Ok(func_idx)
    }

    fn instr_await(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, milliseconds:i);
        self.o.ctx.callback.on_await(milliseconds);
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::Await.bytes_len() as i32);
        Ok(())
    }

    fn instr_var_exists(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, var:s);
        let ip = self.o.cur_frame.ip;
        let func_info = (self.o.ctx)
            .func_info_from_chunk_pos(ip.chunk as _, ip.offset as _)
            .context_unlikely("function info not found")?;
        let r = func_info.frame_info.vars.contains_key(Ascii::new_str(var))
            || self.o.ctx.variables.get_var(var).is_some();
        let r = StackValue::new_int(r as _);
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::VarExists.bytes_len() as i32);
        Ok(())
    }

    fn instr_play_bgm(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, path:s);
        self.o.ctx.callback.on_play_sound(path, 0, true);
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::PlayBgm.bytes_len() as i32);
        Ok(())
    }

    fn instr_stop_bgm(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        self.o.ctx.callback.on_stop_sound(0);
        self.add_ip_offset(Bc::StopBgm.bytes_len() as i32);
        Ok(())
    }

    fn instr_play_sound(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, path:s);
        self.o.ctx.callback.on_play_sound(path, 1, false);
        self.o.stack.replace_tail(stack_count, []);
        self.add_ip_offset(Bc::PlaySound.bytes_len() as i32);
        Ok(())
    }

    fn instr_stop_sound(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        self.o.ctx.callback.on_stop_sound(0);
        self.add_ip_offset(Bc::StopSound.bytes_len() as i32);
        Ok(())
    }

    fn instr_intrinsic_get_next_event_handler(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        view_stack!(self, stack_count, event:s, cur_priority:i, cur_handler:s);
        let handler = (self.o.ctx)
            .event_func_registry
            .get(Ascii::new_str(event))
            .and_then(|x| x.get(cur_priority as usize))
            .and_then(|x| {
                if cur_handler.is_empty() {
                    x.get(0)
                } else {
                    x.iter()
                        .position(|x| x.as_ascii_ref() == Ascii::new_str(cur_handler))
                        .and_then(|h| x.get(h + 1))
                }
            })
            .map(|x| x.as_ref())
            .unwrap_or_default();
        let r = StackValue::new_str(handler.into());
        self.o.stack.replace_tail(stack_count, [r]);
        self.add_ip_offset(Bc::IntrinsicGetNextEventHandler.bytes_len() as i32);
        Ok(())
    }

    fn instr_raise_illegal_instruction(&mut self) -> anyhow::Result<()> {
        self.ensure_pre_step_instruction()?;

        self.break_reason = EraExecutionBreakReason::IllegalInstruction;
        Err(FireEscapeError(self.break_reason).into())
    }

    fn step_instruction_once(&mut self) -> anyhow::Result<()> {
        let s = self;
        let Some(bc_area) =
            s.o.cur_chunk
                .get_bc()
                .get(s.o.cur_frame.ip.offset as usize..)
        else {
            let mut diag = Diagnostic::new();
            diag.span_err(
                s.o.cur_chunk.name.clone(),
                s.o.cur_chunk
                    .lookup_src(s.o.cur_chunk.len() - 1)
                    .unwrap_or_default(),
                "unexpected end of bytecode",
            );
            s.o.ctx.emit_diag(diag);
            s.break_reason = EraExecutionBreakReason::IllegalInstruction;
            return Err(FireEscapeError(s.break_reason).into());
        };
        // let bc_span = s
        //     .cur_chunk
        //     .lookup_src(s.cur_frame.ip.offset as usize)
        //     .unwrap_or_default();
        let Some(inst) = EraBytecodeKind::from_bytes(bc_area) else {
            let mut diag = Diagnostic::new();
            diag.span_err(
                s.o.cur_filename(),
                s.o.cur_bc_span(),
                "unexpected end of bytecode",
            );
            s.o.ctx.emit_diag(diag);
            s.break_reason = EraExecutionBreakReason::IllegalInstruction;
            return Err(FireEscapeError(s.break_reason).into());
        };

        // dbg!(&s.o.stack);
        // dbg!((s.o.cur_frame.ip.offset, inst));

        // Execute instruction
        // NOTE: Execution of instructions is atomic: they are either fully executed or not at all.
        //       This is important for the stack to remain consistent.
        match inst {
            Bc::FailWithMsg => s.instr_fail_with_msg()?,
            Bc::Nop => s.instr_nop()?,
            Bc::DebugBreak => s.instr_debug_break()?,
            Bc::Quit => s.instr_quit()?,
            Bc::Throw => s.instr_throw()?,
            Bc::ReturnVoid => s.instr_return_void()?,
            Bc::ReturnInt => s.instr_return_int()?,
            Bc::ReturnStr => s.instr_return_str()?,
            Bc::CallFun { args_cnt, func_idx } => s.instr_call_fun(args_cnt, func_idx)?,
            Bc::TryCallFun { args_cnt } => s.instr_try_call_fun(args_cnt, false)?,
            Bc::TryCallFunForce { args_cnt } => s.instr_try_call_fun(args_cnt, true)?,
            Bc::RestartExecAtFun => s.instr_restart_exec_at_fun()?,
            Bc::JumpWW { offset } => s.instr_jump_ww(offset)?,
            Bc::JumpIfWW { offset } => s.instr_jump_if_ww(offset)?,
            Bc::JumpIfNotWW { offset } => s.instr_jump_if_not_ww(offset)?,
            Bc::LoadConstStr { idx } => s.instr_load_const_str(idx)?,
            Bc::LoadImm8 { imm } => s.instr_load_imm8(imm)?,
            Bc::LoadImm16 { imm } => s.instr_load_imm16(imm)?,
            Bc::LoadImm32 { imm } => s.instr_load_imm32(imm)?,
            Bc::LoadImm64 { imm } => s.instr_load_imm64(imm)?,
            Bc::LoadVarWW { idx } => s.instr_load_var_ww(idx)?,
            Bc::LoadConstVarWW { idx } => s.instr_load_const_var_ww(idx)?,
            Bc::LoadLocalVar { idx } => s.instr_load_local_var(idx)?,
            Bc::Pop => s.instr_pop()?,
            Bc::PopAllN { count } => s.instr_pop_all_n(count)?,
            Bc::PopOneN { idx } => s.instr_pop_one_n(idx)?,
            Bc::Swap2 => s.instr_swap_2()?,
            Bc::Duplicate => s.instr_duplicate()?,
            Bc::DuplicateAllN { count } => s.instr_duplicate_all_n(count)?,
            Bc::DuplicateOneN { idx } => s.instr_duplicate_one_n(idx)?,
            Bc::AddInt => s.instr_add_int()?,
            Bc::SubInt => s.instr_sub_int()?,
            Bc::MulInt => s.instr_mul_int()?,
            Bc::DivInt => s.instr_div_int()?,
            Bc::ModInt => s.instr_mod_int()?,
            Bc::NegInt => s.instr_neg_int()?,
            Bc::BitAndInt => s.instr_bit_and_int()?,
            Bc::BitOrInt => s.instr_bit_or_int()?,
            Bc::BitXorInt => s.instr_bit_xor_int()?,
            Bc::BitNotInt => s.instr_bit_not_int()?,
            Bc::ShlInt => s.instr_shl_int()?,
            Bc::ShrInt => s.instr_shr_int()?,
            Bc::CmpIntLT => s.instr_cmp_int_lt()?,
            Bc::CmpIntLEq => s.instr_cmp_int_leq()?,
            Bc::CmpIntGT => s.instr_cmp_int_gt()?,
            Bc::CmpIntGEq => s.instr_cmp_int_geq()?,
            Bc::CmpIntEq => s.instr_cmp_int_eq()?,
            Bc::CmpIntNEq => s.instr_cmp_int_neq()?,
            Bc::CmpStrLT => s.instr_cmp_str_lt()?,
            Bc::CmpStrLEq => s.instr_cmp_str_leq()?,
            Bc::CmpStrGT => s.instr_cmp_str_gt()?,
            Bc::CmpStrGEq => s.instr_cmp_str_geq()?,
            Bc::CmpStrEq => s.instr_cmp_str_eq()?,
            Bc::CmpStrNEq => s.instr_cmp_str_neq()?,
            Bc::LogicalNot => s.instr_logical_not()?,
            Bc::MaxInt => s.instr_max_int()?,
            Bc::MinInt => s.instr_min_int()?,
            Bc::ClampInt => s.instr_clamp_int()?,
            Bc::InRangeInt => s.instr_in_range_int()?,
            Bc::InRangeStr => s.instr_in_range_str()?,
            Bc::GetBit => s.instr_get_bit()?,
            Bc::SetBit => s.instr_set_bit()?,
            Bc::ClearBit => s.instr_clear_bit()?,
            Bc::InvertBit => s.instr_invert_bit()?,
            Bc::BuildString { count } => s.instr_build_string(count)?,
            Bc::PadString { flags } => s.instr_pad_string(flags)?,
            Bc::RepeatStr => s.instr_repeat_str()?,
            Bc::BuildArrIdxFromMD { count } => s.instr_build_arr_idx_from_md(count)?,
            Bc::GetArrValFlat => s.instr_get_arr_val_flat()?,
            Bc::SetArrValFlat => s.instr_set_arr_val_flat()?,
            Bc::TimesFloat => s.instr_times_float()?,
            Bc::FunExists => s.instr_fun_exists()?,
            Bc::ReplaceStr => s.instr_replace_str()?,
            Bc::SubStr | Bc::SubStrU => s.instr_sub_str()?,
            Bc::StrFind | Bc::StrFindU => s.instr_str_find()?,
            Bc::StrLen | Bc::StrLenU => s.instr_str_len()?,
            Bc::CountSubStr => s.instr_count_sub_str()?,
            Bc::StrCharAtU => s.instr_str_char_at()?,
            Bc::IntToStr => s.instr_int_to_str()?,
            Bc::StrToInt => s.instr_str_to_int()?,
            Bc::FormatIntToStr => s.instr_format_int_to_str()?,
            Bc::StrIsValidInt => s.instr_str_is_valid_int()?,
            Bc::StrToUpper => s.instr_str_to_upper()?,
            Bc::StrToLower => s.instr_str_to_lower()?,
            Bc::StrToHalf => s.instr_str_to_half()?,
            Bc::StrToFull => s.instr_str_to_full()?,
            Bc::BuildBarStr => s.instr_build_bar_str()?,
            Bc::EscapeRegexStr => s.instr_escape_regex_str()?,
            Bc::EncodeToUnicode => s.instr_encode_to_unicode()?,
            Bc::UnicodeToStr => s.instr_unicode_to_str()?,
            Bc::IntToStrWithBase => s.instr_int_to_str_with_base()?,
            Bc::HtmlTagSplit => s.instr_html_tag_split()?,
            Bc::HtmlToPlainText => s.instr_html_to_plain_text()?,
            Bc::HtmlEscape => s.instr_html_escape()?,
            Bc::PowerInt => s.instr_power_int()?,
            Bc::SqrtInt => s.instr_sqrt_int()?,
            Bc::CbrtInt => s.instr_cbrt_int()?,
            Bc::LogInt => s.instr_log_int()?,
            Bc::Log10Int => s.instr_log_10_int()?,
            Bc::ExponentInt => s.instr_exponent_int()?,
            Bc::AbsInt => s.instr_abs_int()?,
            Bc::SignInt => s.instr_sign_int()?,
            Bc::GroupMatch { count } => s.instr_group_match(count)?,
            Bc::ArrayCountMatches => s.instr_array_count_matches(-1)?,
            Bc::CArrayCountMatches => s.instr_array_count_matches(0)?,
            Bc::SumArray => s.instr_array_aggregate::<crate::util::SumAggregator, 1>(-1)?,
            Bc::SumCArray => s.instr_array_aggregate::<crate::util::SumAggregator, 1>(0)?,
            Bc::MaxArray => s.instr_array_aggregate::<crate::util::MaxAggregator, 1>(-1)?,
            Bc::MaxCArray => s.instr_array_aggregate::<crate::util::MaxAggregator, 1>(0)?,
            Bc::MinArray => s.instr_array_aggregate::<crate::util::MinAggregator, 1>(-1)?,
            Bc::MinCArray => s.instr_array_aggregate::<crate::util::MinAggregator, 1>(0)?,
            Bc::InRangeArray => s.instr_array_in_range(-1)?,
            Bc::InRangeCArray => s.instr_array_in_range(0)?,
            Bc::ArrayRemove => s.instr_array_remove()?,
            Bc::ArraySortAsc => s.instr_array_sort(true)?,
            Bc::ArraySortDesc => s.instr_array_sort(false)?,
            Bc::ArrayMSort { subs_cnt } => s.instr_array_multi_sort(subs_cnt)?,
            Bc::ArrayCopy => s.instr_array_copy()?,
            Bc::ArrayShift => s.instr_array_shift()?,
            Bc::Print => s.instr_print_with_flags::<1>(EraPrintExtendedFlags::new())?,
            Bc::PrintLine => {
                s.instr_print_with_flags::<1>(EraPrintExtendedFlags::new().with_is_line(true))?
            }
            Bc::PrintExtended { flags } => s.instr_print_with_flags::<2>(flags)?,
            Bc::ReuseLastLine => s.instr_reuse_last_line()?,
            Bc::ClearLine => s.instr_clear_line()?,
            Bc::Wait { flags } => s.instr_wait(flags)?,
            Bc::TWait => s.instr_twait()?,
            Bc::Input { flags } => s.instr_input(flags)?,
            Bc::KbGetKeyState => s.instr_kb_get_key_state()?,
            Bc::GetCallerFuncName => s.instr_get_caller_func_name()?,
            Bc::GetCharaNum => s.instr_get_chara_num()?,
            Bc::CsvGetNum { kind } => s.instr_csv_get_num(kind)?,
            Bc::GetRandomRange => s.instr_get_random_range()?,
            Bc::GetRandomMax => s.instr_get_random_max()?,
            Bc::RowAssign { vals_cnt } => s.instr_row_assign(vals_cnt)?,
            Bc::ForLoopStep => s.instr_for_loop_step()?,
            Bc::ForLoopNoStep => s.instr_for_loop_no_step()?,
            Bc::ExtendStrToWidth => s.instr_extend_str_to_width()?,
            Bc::HtmlPrint => s.instr_html_print()?,
            Bc::HtmlPopPrintingStr => s.instr_html_pop_printing_str()?,
            Bc::HtmlGetPrintedStr => s.instr_html_get_printed_str()?,
            Bc::HtmlStringLen => s.instr_html_string_len()?,
            Bc::PrintButton { flags } => s.instr_print_button(flags)?,
            Bc::PrintImg => s.instr_print_img()?,
            Bc::PrintImgWithColorMatrix => s.instr_print_img_with_color_matrix()?,
            Bc::PrintRect => s.instr_print_rect()?,
            Bc::PrintSpace => s.instr_print_space()?,
            Bc::SplitString => s.instr_split_string()?,
            Bc::GCreate => s.instr_gcreate()?,
            Bc::GCreateFromFile => s.instr_gcreate_from_file()?,
            Bc::GDispose => s.instr_gdispose()?,
            Bc::GCreated => s.instr_gcreated()?,
            Bc::GDrawSprite => s.instr_gdraw_sprite()?,
            Bc::GDrawSpriteWithColorMatrix => s.instr_gdraw_sprite_with_color_matrix()?,
            Bc::GClear => s.instr_gclear()?,
            Bc::SpriteCreate => s.instr_sprite_create()?,
            Bc::SpriteDispose => s.instr_sprite_dispose()?,
            Bc::SpriteCreated => s.instr_sprite_created()?,
            Bc::SpriteAnimeCreate => s.instr_sprite_anime_create()?,
            Bc::SpriteAnimeAddFrame => s.instr_sprite_anime_add_frame()?,
            Bc::SpriteWidth => s.instr_sprite_width()?,
            Bc::SpriteHeight => s.instr_sprite_height()?,
            Bc::CheckFont => s.instr_check_font()?,
            Bc::SaveText => s.instr_save_text()?,
            Bc::LoadText => s.instr_load_text()?,
            Bc::FindElement => s.instr_generic_find_element_with_match::<2>(true, -1)?,
            Bc::FindLastElement => s.instr_generic_find_element_with_match::<2>(false, -1)?,
            Bc::FindChara => s.instr_generic_find_element::<2>(true, 0)?,
            Bc::FindLastChara => s.instr_generic_find_element::<2>(false, 0)?,
            Bc::VarSet => s.instr_var_set(-1)?,
            Bc::CVarSet => s.instr_var_set(0)?,
            Bc::GetVarSizeByName => s.instr_get_var_size_by_name()?,
            Bc::GetVarAllSize => s.instr_get_var_all_size()?,
            Bc::GetHostTimeRaw => s.instr_get_host_time_raw()?,
            Bc::GetHostTime => s.instr_get_host_time()?,
            Bc::GetHostTimeS => s.instr_get_host_time_s()?,
            Bc::CsvGetProp2 { csv_kind } => s.instr_csv_get_prop_2(csv_kind)?,
            Bc::CharaCsvExists => s.instr_chara_csv_exists()?,
            Bc::GetPalamLv => s.instr_generic_get_lv::<2>("PALAMLV")?,
            Bc::GetExpLv => s.instr_generic_get_lv::<2>("EXPLV")?,
            Bc::AddChara => s.instr_add_chara()?,
            Bc::AddVoidChara => s.instr_add_void_chara()?,
            Bc::PickUpChara { charas_cnt } => s.instr_pick_up_chara(charas_cnt)?,
            Bc::DeleteChara { charas_cnt } => s.instr_delete_chara(charas_cnt)?,
            Bc::SwapChara => s.instr_swap_chara()?,
            Bc::AddCopyChara => s.instr_add_copy_chara()?,
            Bc::LoadData => s.instr_load_data()?,
            Bc::SaveData => s.instr_save_data()?,
            Bc::CheckData => s.instr_check_data()?,
            Bc::GetCharaRegNum => s.instr_get_chara_reg_num()?,
            Bc::LoadGlobal => s.instr_load_global()?,
            Bc::SaveGlobal => s.instr_save_global()?,
            Bc::ResetData => s.instr_reset_data()?,
            Bc::ResetCharaStain => s.instr_reset_chara_stain()?,
            Bc::SaveChara { charas_cnt } => s.instr_save_chara(charas_cnt)?,
            Bc::LoadChara => s.instr_load_chara()?,
            Bc::GetConfig => s.instr_get_config()?,
            Bc::GetConfigS => s.instr_get_config_s()?,
            Bc::FindCharaDataFile => s.instr_find_chara_data_file()?,
            Bc::EvalStrForm => s.instr_eval_str_form()?,
            Bc::EvalIntExpr => s.instr_eval_int_expr()?,
            Bc::EvalStrExpr => s.instr_eval_str_expr()?,
            Bc::Await => s.instr_await()?,
            Bc::VarExists => s.instr_var_exists()?,
            Bc::PlayBgm => s.instr_play_bgm()?,
            Bc::StopBgm => s.instr_stop_bgm()?,
            Bc::PlaySound => s.instr_play_sound()?,
            Bc::StopSound => s.instr_stop_sound()?,
            Bc::IntrinsicGetNextEventHandler => s.instr_intrinsic_get_next_event_handler()?,
            // _ => s.instr_raise_illegal_instruction()?,
            // _ => {
            //     let mut diag = Diagnostic::new();
            //     diag.span_err(
            //         s.o.cur_filename(),
            //         s.o.cur_bc_span(),
            //         format!("unimplemented bytecode `{:?}`", inst),
            //     );
            //     s.o.ctx.emit_diag(diag);
            //     s.break_reason = EraExecutionBreakReason::IllegalInstruction;
            //     return Err(FireEscapeError(s.break_reason).into());
            // }
        }

        Ok(())
    }
}

impl<'i, Callback: EraCompilerCallback> EraVmExecSite<'_, 'i, '_, Callback> {
    /// Generates JITed code for the given function if it is not already generated, and
    /// updates the JIT lookup table accordingly.
    fn ensure_jit_function(&mut self, func_idx: u32) -> anyhow::Result<()> {
        use arch::{AssemblerRoutineReturnValue, JitUnwindRegistryGuard, NextLocalLabel};

        let jit_func_slot = self
            .jit_compiled_functions
            .get_mut(func_idx as usize)
            .with_context_unlikely(|| format!("function index {} out of bounds", func_idx))?;
        match jit_func_slot {
            Some(jit_func) => {
                if jit_func.is_invalid() {
                    // Function is not JITable, give up
                    anyhow::bail!("function index {} is not JITable", func_idx);
                }
                // The function is already JITed, so we skip it
                return Ok(());
            }
            None => (),
        }
        *jit_func_slot = Some(EraJitCompiledFunction::new_invalid(func_idx as _));

        // Step 1: Scan for all possible instruction entry points
        let mut ops = arch::new_assembler();

        // Gather all ip offsets that need updating
        // let mut ip_map = {
        //     let o = unsafe { self.optr.as_mut() };
        //     let mut ip_map = HashMap::new();
        //     for (i, frame) in o.frames.iter().enumerate() {
        //         let jit_frame = &mut self.jit_side_frame[i];
        //         if jit_frame.func_idx != func_idx {
        //             continue;
        //         }
        //         ip_map.insert(frame.ip.offset, AssemblyOffset(usize::MAX));
        //         if let Some(next_frame) = o.frames.get(i + 1) {
        //             ip_map.insert(next_frame.ret_ip.offset, AssemblyOffset(usize::MAX));
        //         }
        //     }
        //     self.remake_site()?;
        //     ip_map
        // };
        let mut ip_map = HashMap::new();

        let func_info = (self.o.ctx.func_entries)
            .get_index(func_idx as usize)
            .map(|(_, v)| v.as_ref())
            .flatten()
            .with_context_unlikely(|| format!("function index {} out of bounds", func_idx))?;
        let func_bc = self.o.ctx.bc_chunks[func_info.chunk_idx as usize].get_bc();
        let mut bc = func_bc
            .get(func_info.bc_offset as usize..(func_info.bc_offset + func_info.bc_size) as usize)
            .with_context_unlikely(|| format!("invalid function ip {:?}", func_info))?;

        macro_rules! make_subroutine {
            ($routine:ident $(-> $ret:ty)? $(,$arg:ident:$type:ty)*) => {{
                paste::paste! {
                    extern "C-unwind" fn [<jit_ $routine>]<Callback: EraCompilerCallback>(
                        s: &mut EraVmExecSite<Callback>,
                        $($arg: $type),*
                    ) $(-> $ret)? {
                        match s.$routine($($arg),*) {
                            Ok(r) => r,
                            Err(e) => {
                                std::panic::panic_any(e);
                            }
                        }
                    }
                    [<jit_ $routine>]::<Callback>
                }
            }};
        }
        macro_rules! make_subroutine_closure {
            ($routine:ident $(-> $ret:ty)?, $clo:expr $(,$arg:ident:$type:ty)*) => {{
                paste::paste! {
                    extern "C-unwind" fn [<jit_ $routine>]<Callback: EraCompilerCallback>(
                        s: &mut EraVmExecSite<Callback>,
                        $($arg: $type),*
                    ) $(-> $ret)? {
                        match $clo(s, $($arg),*) {
                            Ok(r) => r,
                            Err(e) => {
                                std::panic::panic_any(e);
                            }
                        }
                    }
                    [<jit_ $routine>]::<Callback>
                }
            }};
        }

        macro_rules! call_subroutine {
            ($routine:ident $(-> $ret:ty)?) => {{
                let subroutine = make_subroutine!($routine $(-> $ret)?);
                arch::emit_call_subroutine_0(&mut ops, subroutine);
            }};
            ($routine:ident $(-> $ret:ty)?, $arg1:ident:$type1:ty) => {{
                let subroutine = make_subroutine!($routine $(-> $ret)?, $arg1:$type1);
                arch::emit_call_subroutine_1(&mut ops, subroutine, $arg1);
            }};
            ($routine:ident $(-> $ret:ty)?, $arg1:ident:$type1:ty, $arg2:ident:$type2:ty) => {{
                let subroutine = make_subroutine!($routine $(-> $ret)?, $arg1:$type1, $arg2:$type2);
                arch::emit_call_subroutine_2(&mut ops, subroutine, $arg1, $arg2);
            }};
            ($routine:ident $(-> $ret:ty)?, $arg1:ident:$type1:ty, $arg2:ident:$type2:ty, $arg3:ident:$type3:ty) => {{
                let subroutine = make_subroutine!($routine $(-> $ret)?, $arg1:$type1, $arg2:$type2, $arg3:$type3);
                arch::emit_call_subroutine_3(&mut ops, subroutine, $arg1, $arg2, $arg3);
            }};
        }
        macro_rules! call_subroutine_closure {
            ($routine:ident $(-> $ret:ty)?, $clo:expr) => {{
                let subroutine = make_subroutine_closure!($routine $(-> $ret)?, $clo);
                arch::emit_call_subroutine_0(&mut ops, subroutine);
            }};
            ($routine:ident $(-> $ret:ty)?, $clo:expr, $arg1:ident:$type1:ty) => {{
                let subroutine = make_subroutine_closure!($routine $(-> $ret)?, $clo, $arg1:$type1);
                arch::emit_call_subroutine_1(&mut ops, subroutine, $arg1);
            }};
            ($routine:ident $(-> $ret:ty)?, $clo:expr, $arg1:ident:$type1:ty, $arg2:ident:$type2:ty) => {{
                let subroutine = make_subroutine_closure!($routine $(-> $ret)?, $clo, $arg1:$type1, $arg2:$type2);
                arch::emit_call_subroutine_2(&mut ops, subroutine, $arg1, $arg2);
            }};
            ($routine:ident $(-> $ret:ty)?, $clo:expr, $arg1:ident:$type1:ty, $arg2:ident:$type2:ty, $arg3:ident:$type3:ty) => {{
                let subroutine = make_subroutine_closure!($routine $(-> $ret)?, $clo, $arg1:$type1, $arg2:$type2, $arg3:$type3);
                arch::emit_call_subroutine_3(&mut ops, subroutine, $arg1, $arg2, $arg3);
            }};
        }
        macro_rules! call_subroutine_return {
            ($routine:ident) => {
                paste::paste! {{
                    let subroutine = {
                        extern "C-unwind" fn [<jit_ $routine>]<Callback: EraCompilerCallback>(
                            site: &mut EraVmExecSite<Callback>,
                        ) -> usize {
                            // NB: Do NOT pop site.jit_compiled_functions here, it will invalidate the JIT code.
                            // if site.o.cur_frame.is_transient {
                            //     site.jit_compiled_functions.pop();
                            // }
                            if let Err(e) = site.$routine() {
                                std::panic::panic_any(e);
                            }
                            site.jit_side_frame.pop();
                            site.jit_side_frame.last().map_or(0, |x| x.ip)
                        }
                        [<jit_ $routine>]::<Callback>
                    };
                    arch::emit_call_subroutine_0(&mut ops, subroutine);
                    arch::emit_test_jump_to_return_ip_or_break(&mut ops);
                }}
            };
        }
        macro_rules! call_subroutine_eval {
            ($routine:ident) => {
                paste::paste! {{
                    let subroutine = {
                        extern "C-unwind" fn [<jit_ $routine>]<Callback: EraCompilerCallback>(
                            site: &mut EraVmExecSite<Callback>,
                            next_rip: usize,
                        ) -> usize {
                            // NB: Clear the JIT code cache here instead of at return, so that we don't get crashes.
                            site.jit_compiled_functions.truncate(site.o.ctx.func_entries.len());

                            let func_idx = match site.[<$routine _with_return_value>]() {
                                Ok(x) => x,
                                Err(e) => std::panic::panic_any(e),
                            };
                            site.jit_side_frame.last_mut().unwrap().ip = next_rip;
                            // Alloc slot
                            // TODO: Do we really need func_idx?
                            site.jit_compiled_functions.push(None);
                            let asm_ip = site.jit_compiled_functions[func_idx as usize]
                                .as_ref()
                                .map_or(0, |x| x.code.ptr(AssemblyOffset(0)) as usize);
                            site.jit_side_frame.push(EraJitExecFrame {
                                func_idx: func_idx as _,
                                ip: asm_ip,
                            });
                            asm_ip
                        }
                        [<jit_ $routine>]::<Callback>
                    };
                    arch::emit_call_subroutine_1(&mut ops, subroutine, NextLocalLabel);
                    arch::emit_test_jump_to_return_ip_or_break(&mut ops);
                    NextLocalLabel::add_here(&mut ops);
                }}
            };
        }

        let mut dyn_labels = HashMap::new();
        while let Some((inst, len)) = Bc::with_len_from_bytes(&mut bc) {
            let inst_offset = unsafe { bc.as_ptr().offset_from(func_bc.as_ptr()) as u32 };
            bc = &bc[len as usize..];
            // Update ip
            ip_map.insert(inst_offset, ops.offset());
            if let Some(label) = dyn_labels.remove(&inst_offset) {
                ops.dynamic_label(label);
                // dynasm!(ops
                //     ; =>label
                // );
            }
            // Translate bytecode
            match inst {
                Bc::FailWithMsg => call_subroutine!(instr_fail_with_msg),
                Bc::Nop => call_subroutine!(instr_nop),
                Bc::DebugBreak => call_subroutine!(instr_debug_break),
                Bc::Quit => call_subroutine!(instr_quit),
                Bc::Throw => call_subroutine!(instr_throw),
                Bc::ReturnVoid => call_subroutine_return!(instr_return_void),
                Bc::ReturnInt => call_subroutine_return!(instr_return_int),
                Bc::ReturnStr => call_subroutine_return!(instr_return_str),
                Bc::CallFun { args_cnt, func_idx } => {
                    let subroutine = {
                        extern "C-unwind" fn jit_instr_call_fun<Callback: EraCompilerCallback>(
                            site: &mut EraVmExecSite<Callback>,
                            args_cnt: u8,
                            func_idx: u32,
                            next_rip: usize,
                        ) -> usize {
                            let func_idx =
                                match site.instr_call_fun_with_return_value(args_cnt, func_idx) {
                                    Ok(x) => x,
                                    Err(e) => std::panic::panic_any(e),
                                };
                            site.jit_side_frame.last_mut().unwrap().ip = next_rip;
                            let asm_ip = site.jit_compiled_functions[func_idx as usize]
                                .as_ref()
                                .map_or(0, |x| x.code.ptr(AssemblyOffset(0)) as usize);
                            site.jit_side_frame.push(EraJitExecFrame {
                                func_idx: func_idx as _,
                                ip: asm_ip,
                            });
                            asm_ip
                        }
                        jit_instr_call_fun::<Callback>
                    };
                    arch::emit_call_subroutine_3(
                        &mut ops,
                        subroutine,
                        args_cnt,
                        func_idx,
                        NextLocalLabel,
                    );
                    arch::emit_test_jump_to_return_ip_or_break(&mut ops);
                    NextLocalLabel::add_here(&mut ops);
                }
                Bc::TryCallFun { args_cnt } => {
                    let subroutine = {
                        extern "C-unwind" fn jit_instr_try_call_fun<
                            Callback: EraCompilerCallback,
                        >(
                            site: &mut EraVmExecSite<Callback>,
                            args_cnt: u8,
                            next_rip: usize,
                        ) -> usize {
                            let func_idx =
                                match site.instr_try_call_fun_with_return_value(args_cnt, false) {
                                    Ok(x) => x,
                                    Err(e) => std::panic::panic_any(e),
                                };
                            let frames_changed = func_idx != usize::MAX;
                            site.jit_side_frame.last_mut().unwrap().ip = next_rip;
                            if frames_changed {
                                let asm_ip = site.jit_compiled_functions[func_idx as usize]
                                    .as_ref()
                                    .map_or(0, |x| x.code.ptr(AssemblyOffset(0)) as usize);
                                site.jit_side_frame.push(EraJitExecFrame {
                                    func_idx: func_idx as _,
                                    ip: asm_ip,
                                });
                                asm_ip
                            } else {
                                next_rip
                            }
                        }
                        jit_instr_try_call_fun::<Callback>
                    };
                    arch::emit_call_subroutine_2(&mut ops, subroutine, args_cnt, NextLocalLabel);
                    arch::emit_test_jump_to_return_ip_or_break(&mut ops);
                    NextLocalLabel::add_here(&mut ops);
                }
                Bc::TryCallFunForce { args_cnt } => {
                    let subroutine = {
                        extern "C-unwind" fn jit_instr_try_call_fun_force<
                            Callback: EraCompilerCallback,
                        >(
                            site: &mut EraVmExecSite<Callback>,
                            args_cnt: u8,
                            next_rip: usize,
                        ) -> usize {
                            let func_idx =
                                match site.instr_try_call_fun_with_return_value(args_cnt, true) {
                                    Ok(x) => x,
                                    Err(e) => std::panic::panic_any(e),
                                };
                            site.jit_side_frame.last_mut().unwrap().ip = next_rip;
                            let asm_ip = site.jit_compiled_functions[func_idx as usize]
                                .as_ref()
                                .map_or(0, |x| x.code.ptr(AssemblyOffset(0)) as usize);
                            site.jit_side_frame.push(EraJitExecFrame {
                                func_idx: func_idx as _,
                                ip: asm_ip,
                            });
                            asm_ip
                        }
                        jit_instr_try_call_fun_force::<Callback>
                    };
                    arch::emit_call_subroutine_2(&mut ops, subroutine, args_cnt, NextLocalLabel);
                    arch::emit_test_jump_to_return_ip_or_break(&mut ops);
                    NextLocalLabel::add_here(&mut ops);
                }
                Bc::RestartExecAtFun => {
                    let subroutine = {
                        extern "C-unwind" fn jit_instr_restart_exec_at_fun<
                            Callback: EraCompilerCallback,
                        >(
                            site: &mut EraVmExecSite<Callback>,
                        ) {
                            let func_idx = match site.instr_restart_exec_at_fun_with_return_value()
                            {
                                Ok(x) => x,
                                Err(e) => std::panic::panic_any(e),
                            };
                            site.jit_side_frame.clear();
                            site.jit_compiled_functions
                                .truncate(site.o.ctx.func_entries.len());
                            let asm_ip = site.jit_compiled_functions[func_idx as usize]
                                .as_ref()
                                .map_or(0, |x| x.code.ptr(AssemblyOffset(0)) as usize);
                            site.jit_side_frame.push(EraJitExecFrame {
                                func_idx: func_idx as _,
                                ip: asm_ip,
                            });
                        }
                        jit_instr_restart_exec_at_fun::<Callback>
                    };
                    arch::emit_call_subroutine_0(&mut ops, subroutine);
                    arch::emit_epilogue(&mut ops);
                }
                Bc::JumpWW { offset } => {
                    call_subroutine!(instr_jump_ww, offset: i32);
                    let bc_ip = inst_offset.wrapping_add_signed(offset);
                    if let Some(asm_ip) = ip_map.get(&bc_ip) {
                        // Resolve immediately
                        arch::emit_jump(&mut ops, *asm_ip);
                    } else {
                        // Delay resolution
                        let label = *dyn_labels
                            .entry(bc_ip)
                            .or_insert_with(|| ops.new_dynamic_label());
                        arch::emit_jump_dynamic(&mut ops, label);
                    }
                }
                Bc::JumpIfWW { offset } => {
                    call_subroutine!(instr_jump_if_ww_with_return_value -> bool, offset: i32);
                    let bc_ip = inst_offset.wrapping_add_signed(offset);
                    if let Some(asm_ip) = ip_map.get(&bc_ip) {
                        // Resolve immediately
                        bool::emit_cmp_0_and_jump_if(&mut ops, *asm_ip);
                    } else {
                        // Delay resolution
                        let label = *dyn_labels
                            .entry(bc_ip)
                            .or_insert_with(|| ops.new_dynamic_label());
                        bool::emit_cmp_0_and_jump_dynamic_if(&mut ops, label);
                    }
                }
                Bc::JumpIfNotWW { offset } => {
                    call_subroutine!(instr_jump_if_not_ww_with_return_value -> bool, offset: i32);
                    let bc_ip = inst_offset.wrapping_add_signed(offset);
                    if let Some(asm_ip) = ip_map.get(&bc_ip) {
                        // Resolve immediately
                        bool::emit_cmp_0_and_jump_if(&mut ops, *asm_ip);
                    } else {
                        // Delay resolution
                        let label = *dyn_labels
                            .entry(bc_ip)
                            .or_insert_with(|| ops.new_dynamic_label());
                        bool::emit_cmp_0_and_jump_dynamic_if(&mut ops, label);
                    }
                }
                Bc::LoadConstStr { idx } => {
                    call_subroutine!(instr_load_const_str, idx: u32);
                }
                Bc::LoadImm8 { imm } => {
                    call_subroutine!(instr_load_imm8, imm: i8);
                }
                Bc::LoadImm16 { imm } => {
                    call_subroutine!(instr_load_imm16, imm: i16);
                }
                Bc::LoadImm32 { imm } => {
                    call_subroutine!(instr_load_imm32, imm: i32);
                }
                Bc::LoadImm64 { imm } => {
                    call_subroutine!(instr_load_imm64, imm: i64);
                }
                Bc::LoadVarWW { idx } => {
                    call_subroutine!(instr_load_var_ww, idx: u32);
                }
                Bc::LoadConstVarWW { idx } => {
                    call_subroutine!(instr_load_const_var_ww, idx: u32);
                }
                Bc::LoadLocalVar { idx } => {
                    call_subroutine!(instr_load_local_var, idx: u8);
                }
                Bc::Pop => call_subroutine!(instr_pop),
                Bc::PopAllN { count } => call_subroutine!(instr_pop_all_n, count: u8),
                Bc::PopOneN { idx } => call_subroutine!(instr_pop_one_n, idx: u8),
                Bc::Swap2 => call_subroutine!(instr_swap_2),
                Bc::Duplicate => call_subroutine!(instr_duplicate),
                Bc::DuplicateAllN { count } => {
                    call_subroutine!(instr_duplicate_all_n, count: u8)
                }
                Bc::DuplicateOneN { idx } => call_subroutine!(instr_duplicate_one_n, idx: u8),
                Bc::AddInt => call_subroutine!(instr_add_int),
                Bc::SubInt => call_subroutine!(instr_sub_int),
                Bc::MulInt => call_subroutine!(instr_mul_int),
                Bc::DivInt => call_subroutine!(instr_div_int),
                Bc::ModInt => call_subroutine!(instr_mod_int),
                Bc::NegInt => call_subroutine!(instr_neg_int),
                Bc::BitAndInt => call_subroutine!(instr_bit_and_int),
                Bc::BitOrInt => call_subroutine!(instr_bit_or_int),
                Bc::BitXorInt => call_subroutine!(instr_bit_xor_int),
                Bc::BitNotInt => call_subroutine!(instr_bit_not_int),
                Bc::ShlInt => call_subroutine!(instr_shl_int),
                Bc::ShrInt => call_subroutine!(instr_shr_int),
                Bc::CmpIntLT => call_subroutine!(instr_cmp_int_lt),
                Bc::CmpIntLEq => call_subroutine!(instr_cmp_int_leq),
                Bc::CmpIntGT => call_subroutine!(instr_cmp_int_gt),
                Bc::CmpIntGEq => call_subroutine!(instr_cmp_int_geq),
                Bc::CmpIntEq => call_subroutine!(instr_cmp_int_eq),
                Bc::CmpIntNEq => call_subroutine!(instr_cmp_int_neq),
                Bc::CmpStrLT => call_subroutine!(instr_cmp_str_lt),
                Bc::CmpStrLEq => call_subroutine!(instr_cmp_str_leq),
                Bc::CmpStrGT => call_subroutine!(instr_cmp_str_gt),
                Bc::CmpStrGEq => call_subroutine!(instr_cmp_str_geq),
                Bc::CmpStrEq => call_subroutine!(instr_cmp_str_eq),
                Bc::CmpStrNEq => call_subroutine!(instr_cmp_str_neq),
                Bc::LogicalNot => call_subroutine!(instr_logical_not),
                Bc::MaxInt => call_subroutine!(instr_max_int),
                Bc::MinInt => call_subroutine!(instr_min_int),
                Bc::ClampInt => call_subroutine!(instr_clamp_int),
                Bc::InRangeInt => call_subroutine!(instr_in_range_int),
                Bc::InRangeStr => call_subroutine!(instr_in_range_str),
                Bc::GetBit => call_subroutine!(instr_get_bit),
                Bc::SetBit => call_subroutine!(instr_set_bit),
                Bc::ClearBit => call_subroutine!(instr_clear_bit),
                Bc::InvertBit => call_subroutine!(instr_invert_bit),
                Bc::BuildString { count } => call_subroutine!(instr_build_string, count: u8),
                Bc::PadString { flags } => {
                    call_subroutine!(instr_pad_string, flags: EraPadStringFlags)
                }
                Bc::RepeatStr => call_subroutine!(instr_repeat_str),
                Bc::BuildArrIdxFromMD { count } => {
                    call_subroutine!(instr_build_arr_idx_from_md, count: u8)
                }
                Bc::GetArrValFlat => call_subroutine!(instr_get_arr_val_flat),
                Bc::SetArrValFlat => call_subroutine!(instr_set_arr_val_flat),
                Bc::TimesFloat => call_subroutine!(instr_times_float),
                Bc::FunExists => call_subroutine!(instr_fun_exists),
                Bc::ReplaceStr => call_subroutine!(instr_replace_str),
                Bc::SubStr | Bc::SubStrU => call_subroutine!(instr_sub_str),
                Bc::StrFind | Bc::StrFindU => call_subroutine!(instr_str_find),
                Bc::StrLen | Bc::StrLenU => call_subroutine!(instr_str_len),
                Bc::CountSubStr => call_subroutine!(instr_count_sub_str),
                Bc::StrCharAtU => call_subroutine!(instr_str_char_at),
                Bc::IntToStr => call_subroutine!(instr_int_to_str),
                Bc::StrToInt => call_subroutine!(instr_str_to_int),
                Bc::FormatIntToStr => call_subroutine!(instr_format_int_to_str),
                Bc::StrIsValidInt => call_subroutine!(instr_str_is_valid_int),
                Bc::StrToUpper => call_subroutine!(instr_str_to_upper),
                Bc::StrToLower => call_subroutine!(instr_str_to_lower),
                Bc::StrToHalf => call_subroutine!(instr_str_to_half),
                Bc::StrToFull => call_subroutine!(instr_str_to_full),
                Bc::BuildBarStr => call_subroutine!(instr_build_bar_str),
                Bc::EscapeRegexStr => call_subroutine!(instr_escape_regex_str),
                Bc::EncodeToUnicode => call_subroutine!(instr_encode_to_unicode),
                Bc::UnicodeToStr => call_subroutine!(instr_unicode_to_str),
                Bc::IntToStrWithBase => call_subroutine!(instr_int_to_str_with_base),
                Bc::HtmlTagSplit => call_subroutine!(instr_html_tag_split),
                Bc::HtmlToPlainText => call_subroutine!(instr_html_to_plain_text),
                Bc::HtmlEscape => call_subroutine!(instr_html_escape),
                Bc::PowerInt => call_subroutine!(instr_power_int),
                Bc::SqrtInt => call_subroutine!(instr_sqrt_int),
                Bc::CbrtInt => call_subroutine!(instr_cbrt_int),
                Bc::LogInt => call_subroutine!(instr_log_int),
                Bc::Log10Int => call_subroutine!(instr_log_10_int),
                Bc::ExponentInt => call_subroutine!(instr_exponent_int),
                Bc::AbsInt => call_subroutine!(instr_abs_int),
                Bc::SignInt => call_subroutine!(instr_sign_int),
                Bc::GroupMatch { count } => call_subroutine!(instr_group_match, count: u8),
                Bc::ArrayCountMatches => {
                    call_subroutine_closure!(instr_array_count_matches, |s: &mut EraVmExecSite<
                        Callback,
                    >| s
                        .instr_array_count_matches(-1))
                }
                Bc::CArrayCountMatches => {
                    call_subroutine_closure!(
                        instr_c_array_count_matches,
                        |s: &mut EraVmExecSite<Callback>| s.instr_array_count_matches(0)
                    )
                }
                Bc::SumArray => {
                    call_subroutine_closure!(instr_sum_array, |s: &mut EraVmExecSite<Callback>| {
                        s.instr_array_aggregate::<crate::util::SumAggregator, 1>(-1)
                    })
                }
                Bc::SumCArray => {
                    call_subroutine_closure!(instr_sum_c_array, |s: &mut EraVmExecSite<
                        Callback,
                    >| {
                        s.instr_array_aggregate::<crate::util::SumAggregator, 1>(0)
                    })
                }
                Bc::MaxArray => {
                    call_subroutine_closure!(instr_max_array, |s: &mut EraVmExecSite<Callback>| {
                        s.instr_array_aggregate::<crate::util::MaxAggregator, 1>(-1)
                    })
                }
                Bc::MaxCArray => {
                    call_subroutine_closure!(instr_max_c_array, |s: &mut EraVmExecSite<
                        Callback,
                    >| {
                        s.instr_array_aggregate::<crate::util::MaxAggregator, 1>(0)
                    })
                }
                Bc::MinArray => {
                    call_subroutine_closure!(instr_min_array, |s: &mut EraVmExecSite<Callback>| {
                        s.instr_array_aggregate::<crate::util::MinAggregator, 1>(-1)
                    })
                }
                Bc::MinCArray => {
                    call_subroutine_closure!(instr_min_c_array, |s: &mut EraVmExecSite<
                        Callback,
                    >| {
                        s.instr_array_aggregate::<crate::util::MinAggregator, 1>(0)
                    })
                }
                Bc::InRangeArray => {
                    call_subroutine_closure!(instr_in_range_array, |s: &mut EraVmExecSite<
                        Callback,
                    >| {
                        s.instr_array_in_range(-1)
                    })
                }
                Bc::InRangeCArray => {
                    call_subroutine_closure!(instr_in_range_c_array, |s: &mut EraVmExecSite<
                        Callback,
                    >| {
                        s.instr_array_in_range(0)
                    })
                }
                Bc::ArrayRemove => call_subroutine!(instr_array_remove),
                Bc::ArraySortAsc => {
                    call_subroutine_closure!(instr_array_sort_asc, |s: &mut EraVmExecSite<
                        Callback,
                    >| {
                        s.instr_array_sort(true)
                    })
                }
                Bc::ArraySortDesc => {
                    call_subroutine_closure!(instr_array_sort_desc, |s: &mut EraVmExecSite<
                        Callback,
                    >| {
                        s.instr_array_sort(false)
                    })
                }
                Bc::ArrayMSort { subs_cnt } => {
                    call_subroutine!(instr_array_multi_sort, subs_cnt: u8)
                }
                Bc::ArrayCopy => call_subroutine!(instr_array_copy),
                Bc::ArrayShift => call_subroutine!(instr_array_shift),
                Bc::Print => {
                    call_subroutine_closure!(instr_print, |s: &mut EraVmExecSite<Callback>| {
                        s.instr_print_with_flags::<1>(EraPrintExtendedFlags::new())
                    })
                }
                Bc::PrintLine => {
                    call_subroutine_closure!(instr_print_line, |s: &mut EraVmExecSite<Callback>| {
                        s.instr_print_with_flags::<1>(
                            EraPrintExtendedFlags::new().with_is_line(true),
                        )
                    })
                }
                Bc::PrintExtended { flags } => call_subroutine_closure!(
                    instr_print_extended,
                    |s: &mut EraVmExecSite<Callback>, flags: EraPrintExtendedFlags| {
                        s.instr_print_with_flags::<2>(flags)
                    },
                    flags: EraPrintExtendedFlags
                ),
                Bc::ReuseLastLine => call_subroutine!(instr_reuse_last_line),
                Bc::ClearLine => call_subroutine!(instr_clear_line),
                Bc::Wait { flags } => call_subroutine!(instr_wait, flags: EraWaitFlags),
                Bc::TWait => call_subroutine!(instr_twait),
                Bc::Input { flags } => call_subroutine!(instr_input, flags: EraInputExtendedFlags),
                Bc::KbGetKeyState => call_subroutine!(instr_kb_get_key_state),
                Bc::GetCallerFuncName => call_subroutine!(instr_get_caller_func_name),
                Bc::GetCharaNum => call_subroutine!(instr_get_chara_num),
                Bc::CsvGetNum { kind } => {
                    call_subroutine!(instr_csv_get_num, kind: EraCsvVarKind)
                }
                Bc::GetRandomRange => call_subroutine!(instr_get_random_range),
                Bc::GetRandomMax => call_subroutine!(instr_get_random_max),
                Bc::RowAssign { vals_cnt } => {
                    call_subroutine!(instr_row_assign, vals_cnt: u8)
                }
                Bc::ForLoopStep => call_subroutine!(instr_for_loop_step),
                Bc::ForLoopNoStep => call_subroutine!(instr_for_loop_no_step),
                Bc::ExtendStrToWidth => call_subroutine!(instr_extend_str_to_width),
                Bc::HtmlPrint => call_subroutine!(instr_html_print),
                Bc::HtmlPopPrintingStr => call_subroutine!(instr_html_pop_printing_str),
                Bc::HtmlGetPrintedStr => call_subroutine!(instr_html_get_printed_str),
                Bc::HtmlStringLen => call_subroutine!(instr_html_string_len),
                Bc::PrintButton { flags } => {
                    call_subroutine!(instr_print_button, flags: EraPrintExtendedFlags)
                }
                Bc::PrintImg => call_subroutine!(instr_print_img),
                Bc::PrintImgWithColorMatrix => call_subroutine!(instr_print_img_with_color_matrix),
                Bc::PrintRect => call_subroutine!(instr_print_rect),
                Bc::PrintSpace => call_subroutine!(instr_print_space),
                Bc::SplitString => call_subroutine!(instr_split_string),
                Bc::GCreate => call_subroutine!(instr_gcreate),
                Bc::GCreateFromFile => call_subroutine!(instr_gcreate_from_file),
                Bc::GDispose => call_subroutine!(instr_gdispose),
                Bc::GCreated => call_subroutine!(instr_gcreated),
                Bc::GDrawSprite => call_subroutine!(instr_gdraw_sprite),
                Bc::GDrawSpriteWithColorMatrix => {
                    call_subroutine!(instr_gdraw_sprite_with_color_matrix)
                }
                Bc::GClear => call_subroutine!(instr_gclear),
                Bc::SpriteCreate => call_subroutine!(instr_sprite_create),
                Bc::SpriteDispose => call_subroutine!(instr_sprite_dispose),
                Bc::SpriteCreated => call_subroutine!(instr_sprite_created),
                Bc::SpriteAnimeCreate => call_subroutine!(instr_sprite_anime_create),
                Bc::SpriteAnimeAddFrame => call_subroutine!(instr_sprite_anime_add_frame),
                Bc::SpriteWidth => call_subroutine!(instr_sprite_width),
                Bc::SpriteHeight => call_subroutine!(instr_sprite_height),
                Bc::CheckFont => call_subroutine!(instr_check_font),
                Bc::SaveText => call_subroutine!(instr_save_text),
                Bc::LoadText => call_subroutine!(instr_load_text),
                Bc::FindElement => {
                    call_subroutine_closure!(instr_find_element, |s: &mut EraVmExecSite<
                        Callback,
                    >| s
                        .instr_generic_find_element_with_match::<2>(true, -1))
                }
                Bc::FindLastElement => {
                    call_subroutine_closure!(instr_find_last_element, |s: &mut EraVmExecSite<
                        Callback,
                    >| s
                        .instr_generic_find_element_with_match::<2>(false, -1))
                }
                Bc::FindChara => {
                    call_subroutine_closure!(instr_find_chara, |s: &mut EraVmExecSite<Callback>| s
                        .instr_generic_find_element::<2>(true, 0))
                }
                Bc::FindLastChara => {
                    call_subroutine_closure!(instr_find_last_chara, |s: &mut EraVmExecSite<
                        Callback,
                    >| s
                        .instr_generic_find_element::<2>(false, 0))
                }
                Bc::VarSet => {
                    call_subroutine_closure!(instr_var_set, |s: &mut EraVmExecSite<Callback>| s
                        .instr_var_set(-1))
                }
                Bc::CVarSet => {
                    call_subroutine_closure!(instr_c_var_set, |s: &mut EraVmExecSite<Callback>| s
                        .instr_var_set(0))
                }
                Bc::GetVarSizeByName => call_subroutine!(instr_get_var_size_by_name),
                Bc::GetVarAllSize => call_subroutine!(instr_get_var_all_size),
                Bc::GetHostTimeRaw => call_subroutine!(instr_get_host_time_raw),
                Bc::GetHostTime => call_subroutine!(instr_get_host_time),
                Bc::GetHostTimeS => call_subroutine!(instr_get_host_time_s),
                Bc::CsvGetProp2 { csv_kind } => {
                    call_subroutine!(instr_csv_get_prop_2, csv_kind: EraCharaCsvPropType)
                }
                Bc::CharaCsvExists => call_subroutine!(instr_chara_csv_exists),
                Bc::GetPalamLv => {
                    call_subroutine_closure!(instr_get_palam_lv, |s: &mut EraVmExecSite<
                        Callback,
                    >| s
                        .instr_generic_get_lv::<2>("PALAMLV"))
                }
                Bc::GetExpLv => {
                    call_subroutine_closure!(instr_get_exp_lv, |s: &mut EraVmExecSite<Callback>| s
                        .instr_generic_get_lv::<2>("EXPLV"))
                }
                Bc::AddChara => call_subroutine!(instr_add_chara),
                Bc::AddVoidChara => call_subroutine!(instr_add_void_chara),
                Bc::PickUpChara { charas_cnt } => {
                    call_subroutine!(instr_pick_up_chara, charas_cnt: u8)
                }
                Bc::DeleteChara { charas_cnt } => {
                    call_subroutine!(instr_delete_chara, charas_cnt: u8)
                }
                Bc::SwapChara => call_subroutine!(instr_swap_chara),
                Bc::AddCopyChara => call_subroutine!(instr_add_copy_chara),
                Bc::LoadData => call_subroutine!(instr_load_data),
                Bc::SaveData => call_subroutine!(instr_save_data),
                Bc::CheckData => call_subroutine!(instr_check_data),
                Bc::GetCharaRegNum => call_subroutine!(instr_get_chara_reg_num),
                Bc::LoadGlobal => call_subroutine!(instr_load_global),
                Bc::SaveGlobal => call_subroutine!(instr_save_global),
                Bc::ResetData => call_subroutine!(instr_reset_data),
                Bc::ResetCharaStain => call_subroutine!(instr_reset_chara_stain),
                Bc::SaveChara { charas_cnt } => {
                    call_subroutine!(instr_save_chara, charas_cnt: u8)
                }
                Bc::LoadChara => call_subroutine!(instr_load_chara),
                Bc::GetConfig => call_subroutine!(instr_get_config),
                Bc::GetConfigS => call_subroutine!(instr_get_config_s),
                Bc::FindCharaDataFile => call_subroutine!(instr_find_chara_data_file),
                Bc::EvalStrForm => call_subroutine_eval!(instr_eval_str_form),
                Bc::EvalIntExpr => call_subroutine_eval!(instr_eval_int_expr),
                Bc::EvalStrExpr => call_subroutine_eval!(instr_eval_str_expr),
                Bc::Await => call_subroutine!(instr_await),
                Bc::VarExists => call_subroutine!(instr_var_exists),
                Bc::PlayBgm => call_subroutine!(instr_play_bgm),
                Bc::StopBgm => call_subroutine!(instr_stop_bgm),
                Bc::PlaySound => call_subroutine!(instr_play_sound),
                Bc::StopSound => call_subroutine!(instr_stop_sound),
                Bc::IntrinsicGetNextEventHandler => {
                    call_subroutine!(instr_intrinsic_get_next_event_handler)
                } // _ => anyhow::bail!("unimplemented bytecode {:?}", inst),
            }
        }
        if !bc.is_empty() {
            anyhow::bail!("found bad bytecode {:?}", bc);
        }

        if !dyn_labels.is_empty() {
            anyhow::bail!("unresolved dynamic labels {:?}", dyn_labels);
        }
        for (ip, new_ip) in ip_map.iter() {
            if *new_ip == AssemblyOffset(usize::MAX) {
                anyhow::bail!("ip offset {} not updated", ip);
            }
        }

        // Code generated, now write stack unwinding information
        // NOTE: Unwind information must be placed aligned.
        dynasm!(ops
            ; .align 4
        );
        let func_size = ops.offset().0 as u32;
        arch::emit_unwind_data(&mut ops);

        ops.commit()?;
        let buf = ops.finalize().unwrap();

        // Apply ip updates
        {
            let o = unsafe { self.optr.as_mut() };
            for (i, frame) in o.frames.iter().enumerate() {
                let jit_frame = &mut self.jit_side_frame[i];
                if jit_frame.func_idx != func_idx {
                    continue;
                }
                // TODO: Store pointer instead to conform to Pointer Provenance
                jit_frame.ip = buf.ptr(ip_map[&frame.ip.offset]) as usize;
                if let Some(next_frame) = o.frames.get(i + 1) {
                    jit_frame.ip = buf.ptr(ip_map[&next_frame.ret_ip.offset]) as usize;
                }
            }
            self.remake_site()?;
        }

        // Invoke RtlAddFunctionTable to support unwinding
        let jit_guard = JitUnwindRegistryGuard::add_function_table(
            buf.ptr(AssemblyOffset(func_size as _)) as _,
            buf.ptr(AssemblyOffset(0)) as _,
        );

        let jit_func_slot = self
            .jit_compiled_functions
            .get_mut(func_idx as usize)
            .unwrap();
        *jit_func_slot = Some(EraJitCompiledFunction::new(
            func_idx as usize,
            buf,
            jit_guard,
        ));

        Ok(())
    }
}
// ----- End of VM Instruction Implementations -----

impl<'ctx, 'i, 's, Callback: EraCompilerCallback> EraVirtualMachine<'ctx, 'i, 's, Callback> {
    fn execute_inner(
        &mut self,
        run_flag: &AtomicBool,
        max_inst_cnt: u64,
    ) -> anyhow::Result<EraExecutionBreakReason> {
        let mut s = EraVmExecSite::try_new(self.ctx, self.state, run_flag, max_inst_cnt)?;

        // NOTE: On every iteration, we dispatch to subroutines, which handles logic execution and
        //       advances the instruction pointer at the same time. This allows us to implement JIT
        //       compilation more easily.

        // TODO: In JIT, we compile an entire function at a time, and then executes from the instruction
        //       corresponding to the current IP. If there are errors, we panic and recover from the loop,
        //       then pass the caught error to the caller. For ill-formed instructions, we panic with
        //       FireEscapeError(IllegalInstruction) and fall back to bytecode interpretation (to emit
        //       the proper diagnostics). For other special control-flow-changing instructions such as
        //       ReturnVoid, we emit `ret` immediately after writing the `call` instruction, so that
        //       we can handle JIT transitions more easily.

        if !self.enable_jit {
            // Interpret bytecode only
            loop {
                s.step_instruction_once()?;
            }
        }

        // Interpret bytecode until JIT threshold
        let enter_jit_threshold: u64 = std::env::var("ERA_JIT_THRESHOLD")
            .ok()
            .and_then(|x| x.parse().ok())
            .unwrap_or(10000);

        for _ in 0..enter_jit_threshold {
            s.step_instruction_once()?;
        }

        // Generate prologue for jumping to JITed code
        let mut ops = arch::new_assembler();
        arch::emit_prologue(&mut ops);
        ops.commit()?;
        let prologue_buf = ops.finalize().unwrap();
        let prologue_fn: unsafe extern "C-unwind" fn(*mut EraVmExecSite<Callback>, usize) =
            unsafe { std::mem::transmute(prologue_buf.ptr(AssemblyOffset(0))) };

        // Run JIT loop
        {
            let funcs_count = s.o.ctx.func_entries.len();
            let o = unsafe { s.optr.as_mut() };
            s.jit_side_frame.clear();
            for frame in &o.frames {
                let (func_idx, _) =
                    s.o.ctx
                        .func_idx_and_info_from_chunk_pos(frame.ip.chunk as _, frame.ip.offset as _)
                        .with_context_unlikely(|| format!("invalid function ip {:?}", frame.ip))?;
                s.jit_side_frame.push(EraJitExecFrame {
                    func_idx: func_idx as _,
                    ip: 0,
                });
            }
            s.jit_compiled_functions.clear();
            s.jit_compiled_functions.resize_with(funcs_count, || None);
        }
        s.remake_site()?;
        loop {
            let func_idx = s
                .jit_side_frame
                .last()
                .context_unlikely("no JIT side frame")?
                .func_idx;
            match s.ensure_jit_function(func_idx) {
                Ok(()) => unsafe {
                    let ip = s.jit_side_frame.last().unwrap().ip;
                    let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        prologue_fn(&mut s, ip);
                    }));
                    if let Err(e) = r {
                        let e = match e.downcast::<anyhow::Error>() {
                            Ok(e) => e,
                            Err(e) => std::panic::resume_unwind(e),
                        };
                        return Err(*e);
                    }

                    // Update JIT frames
                    let o = s.optr.as_mut();
                    while o.frames.len() < s.jit_side_frame.len() {
                        s.jit_side_frame.pop();
                    }
                    s.remake_site()?;
                },
                Err(e) => {
                    // If the function is not JITable, we fall back to bytecode interpretation
                    // and emit diagnostics for the error
                    {
                        let mut diag = Diagnostic::new();
                        diag.span_warn(
                            s.o.cur_filename(),
                            s.o.cur_bc_span(),
                            format!("failed to JIT function: {}", e),
                        );
                        s.o.ctx.emit_diag(diag);
                    }

                    loop {
                        s.step_instruction_once()?;
                    }
                }
            }
        }
    }
}

fn sanitize_chara_no(chara_no: i64, charas_count: u32) -> anyhow::Result<u32> {
    if chara_no < 0 || chara_no >= charas_count as i64 {
        anyhow::bail!("chara number out of bounds: {}", chara_no);
    }
    Ok(chara_no as _)
}

/// Deduplicate character numbers, preserving order.
fn dedup_chara_numbers(chara_nos: &[StackValue], charas_count: u32) -> anyhow::Result<Vec<u32>> {
    use itertools::Itertools;

    let chara_nos = chara_nos
        .iter()
        .map(|x| {
            x.as_int()
                .context_unlikely("expected integer")
                .and_then(|x| sanitize_chara_no(x.val, charas_count))
        })
        .process_results(|x| x.unique().collect_vec())?;
    Ok(chara_nos)
}

/// Deduplicate character numbers, in ascending order.
fn sort_dedup_chara_numbers(
    chara_nos: &[StackValue],
    charas_count: u32,
) -> anyhow::Result<Vec<u32>> {
    use itertools::Itertools;

    let chara_nos = chara_nos
        .iter()
        .map(|x| {
            x.as_int()
                .context_unlikely("expected integer")
                .and_then(|x| sanitize_chara_no(x.val, charas_count))
        })
        .process_results(|x| x.sorted_unstable().dedup().collect_vec())?;
    Ok(chara_nos)
}
