#[cfg(target_arch = "aarch64")]
mod aarch64;
pub mod inst;
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

use super::savefs::EraSaveFileType;

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

    pub unsafe fn ensure_call_stack_and_get_o(
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

    fn routine_write_save_header(
        &mut self,
        file_type: EraSaveFileType,
        save_info: &str,
        file: &mut (impl std::io::Write + std::io::Seek),
    ) -> anyhow::Result<()> {
        use crate::v2::savefs::*;
        use binrw::BinWrite;

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
            file_type,
            game_code,
            game_version: game_ver,
            save_info: save_info.to_owned(),
        };
        header.write_le(file)?;

        Ok(())
    }

    fn routine_write_chara(
        &mut self,
        chara_i: u32,
        file: &mut (impl std::io::Write + std::io::Seek),
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryWriter;
        use crate::v2::savefs::*;

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
        for var in self.ctx.variables.chara_vars_iter_mut() {
            if !var.is_savedata || !var.is_charadata || var.is_global {
                continue;
            }

            var.val.ensure_alloc();

            if routines::is_builtin_chara_var(var.name.as_ref()) {
                write_one_chara_var(file, var, chara_i)?;
            } else {
                user_chara_vars.push(var);
            }
        }

        // Then write user-defined chara variables
        if !user_chara_vars.is_empty() {
            file.write_u8(EraSaveDataType::Separator as _)?;
            for var in user_chara_vars {
                write_one_chara_var(file, var, chara_i)?;
            }
        }

        // Write separator
        file.write_u8(EraSaveDataType::EOC as _)?;

        Ok(())
    }

    fn routine_write_normal_var(
        &self,
        var: &EraVarInfo,
        file: &mut (impl std::io::Write + std::io::Seek),
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryWriter;
        use crate::v2::savefs::*;

        let is_str = var.val.is_arrstr();
        let var_dims_cnt = var.val.dims_cnt();
        let var_type = EraSaveDataType::new_var(is_str, var_dims_cnt)
            .with_context_unlikely(|| format!("cannot save variable `{}`", var.name))?;
        file.write_u8(var_type as _)?;
        file.write_utf16_string(var.name.as_ref())?;
        file.write_var(var_type, var, None)?;

        Ok(())
    }

    fn routine_load_data(
        &mut self,
        save_path: &str,
        reset: bool,
        expected_file_type: EraSaveFileType,
        old_charas_count: u32,
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

        // Load character variables
        let charas_count = if matches!(
            file_type,
            EraSaveFileType::Normal | EraSaveFileType::CharVar
        ) {
            let charas_count: u32 = file
                .read_i64()?
                .try_into()
                .context_unlikely("invalid character count")?;
            let chara_start_idx = if file_type == EraSaveFileType::CharVar {
                // Loading a CharVar save file appends characters
                old_charas_count
            } else {
                0
            };
            let chara_end_idx = chara_start_idx + charas_count;
            let old_charas_cap = (self.ctx.variables)
                .charas_var_capacity()
                .context_unlikely("no chara variables")?;
            if chara_end_idx > old_charas_cap {
                // Grow chara variables
                use crate::v2::engine::calc_chara_cap_grow_count;
                let grow_count = calc_chara_cap_grow_count(chara_end_idx - old_charas_cap);
                self.ctx.variables.grow_charas_var_capacity(grow_count);
            }
            for chara_i in chara_start_idx..chara_end_idx {
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
            Some(chara_end_idx)
        } else {
            None
        };

        // Load normal variables
        if file_type != EraSaveFileType::CharVar {
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
                            .with_context_unlikely(|| {
                                format!("read variable `{}` failed", var_name)
                            })?;
                    }
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

        self.ctx.preallocate_variables();

        let file = self.ctx.callback.on_open_host_file(save_path, true)?;
        let mut file = std::io::BufWriter::new(file);

        self.routine_write_save_header(EraSaveFileType::Normal, save_info, &mut file)?;

        // Write charas variables
        file.write_i64(charas_count as _)?;
        for chara_i in 0..charas_count {
            self.routine_write_chara(chara_i, &mut file)?;
        }

        // Write normal variables
        for var in self.ctx.variables.iter() {
            if !var.is_savedata || var.is_charadata || var.is_global {
                continue;
            }

            self.routine_write_normal_var(var, &mut file)?;
        }

        // Write EOF
        file.write_u8(EraSaveDataType::EOF as _)?;

        Ok(())
    }

    fn routine_save_global_data(&mut self, save_path: &str) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryWriter;
        use crate::v2::savefs::*;

        self.ctx.preallocate_variables();

        let file = self.ctx.callback.on_open_host_file(save_path, true)?;
        let mut file = std::io::BufWriter::new(file);

        self.routine_write_save_header(EraSaveFileType::Global, "", &mut file)?;

        // Write normal global variables
        for var in self.ctx.variables.iter() {
            if !var.is_savedata || var.is_charadata || !var.is_global {
                continue;
            }

            self.routine_write_normal_var(var, &mut file)?;
        }

        // Write EOF
        file.write_u8(EraSaveDataType::EOF as _)?;

        Ok(())
    }

    fn routine_check_data(
        &mut self,
        save_path: &str,
        expected_save_type: EraSaveFileType,
    ) -> anyhow::Result<CheckDataResult> {
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
        if result.save_type != expected_save_type {
            anyhow::bail!(
                "invalid save file type {:?}, expected {:?}",
                result.save_type,
                expected_save_type
            );
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

    fn routine_save_text(&mut self, text: &str, save_path: &str) -> anyhow::Result<()> {
        use std::io::Write;

        let mut file = (self.ctx.callback)
            .on_open_host_file(save_path, true)
            .with_context_unlikely(|| format!("failed to open file {:?}", save_path))?;

        // Write UTF-8 BOM
        file.write_all(&[0xEF, 0xBB, 0xBF])?;
        // Write text
        file.write_all(text.as_bytes())?;

        Ok(())
    }

    fn routine_load_text(&mut self, save_path: &str) -> anyhow::Result<String> {
        use std::io::Read;

        if !self.ctx.callback.on_check_host_file_exists(save_path)? {
            // File does not exist, return empty string
            return Ok(String::new());
        }

        let file = (self.ctx.callback)
            .on_open_host_file(save_path, false)
            .with_context_unlikely(|| format!("failed to open file {:?}", save_path))?;
        let mut file = std::io::BufReader::new(file);

        let mut text = Vec::new();
        file.read_to_end(&mut text)?;
        let text = if text.starts_with(&[0xEF, 0xBB, 0xBF]) {
            &text[3..]
        } else {
            &text
        };
        let text = String::from_utf8_lossy(text).into_owned();

        Ok(text)
    }

    fn routine_save_chara(
        &mut self,
        save_path: &str,
        memo: &str,
        chara_nos: Vec<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryWriter;
        use crate::v2::savefs::*;

        self.ctx.preallocate_variables();

        let file = self.ctx.callback.on_open_host_file(save_path, true)?;
        let mut file = std::io::BufWriter::new(file);

        self.routine_write_save_header(EraSaveFileType::CharVar, memo, &mut file)?;

        // Write charas variables
        file.write_i64(chara_nos.len() as _)?;
        for chara_i in chara_nos {
            self.routine_write_chara(chara_i, &mut file)?;
        }

        // Write EOF
        file.write_u8(EraSaveDataType::EOF as _)?;

        Ok(())
    }
}

impl<'i, Callback: EraCompilerCallback> EraVmExecSite<'_, 'i, '_, Callback> {
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
            Bc::SpritePosX => s.instr_sprite_pos_x()?,
            Bc::SpritePosY => s.instr_sprite_pos_y()?,
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
            Bc::CheckCharaDataFile => s.instr_check_chara_data_file()?,
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
                Bc::SpritePosX => call_subroutine!(instr_sprite_pos_x),
                Bc::SpritePosY => call_subroutine!(instr_sprite_pos_y),
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
                Bc::CheckCharaDataFile => call_subroutine!(instr_check_chara_data_file),
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

        // NOTE: In JIT, we compile an entire function at a time, and then executes from the instruction
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
