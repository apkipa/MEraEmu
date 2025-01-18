use std::{
    mem::{ManuallyDrop, MaybeUninit},
    num::NonZeroUsize,
    ptr::NonNull,
    sync::atomic::AtomicU32,
};

const MAX_INLINE_DATA_LEN: usize = std::mem::size_of::<NonNull<()>>() - 1;
#[cfg(target_endian = "little")]
const INLINE_DATA_OFFSET: usize = 1;
#[cfg(target_endian = "big")]
const INLINE_DATA_OFFSET: usize = 0;

macro_rules! rcstr_impl {
    (from $ty:ident) => {
        impl From<&str> for $ty {
            fn from(s: &str) -> Self {
                Self::from_str(s)
            }
        }
        impl From<String> for $ty {
            fn from(s: String) -> Self {
                Self::from_str(&s)
            }
        }
        impl From<&String> for $ty {
            fn from(s: &String) -> Self {
                Self::from_str(s)
            }
        }
        impl<'a> From<std::borrow::Cow<'a, str>> for $ty {
            fn from(s: std::borrow::Cow<'a, str>) -> Self {
                Self::from_str(&s)
            }
        }

        impl Default for $ty {
            fn default() -> Self {
                Self::new()
            }
        }
    };
    (deref $ty:ident) => {
        impl core::ops::Deref for $ty {
            type Target = str;

            fn deref(&self) -> &str {
                self.i.as_str()
            }
        }

        impl core::fmt::Debug for $ty {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                core::fmt::Debug::fmt(&self[..], f)
            }
        }

        impl core::fmt::Display for $ty {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                core::fmt::Display::fmt(&self[..], f)
            }
        }

        impl AsRef<str> for $ty {
            fn as_ref(&self) -> &str {
                &self[..]
            }
        }

        impl AsRef<[u8]> for $ty {
            fn as_ref(&self) -> &[u8] {
                self.as_bytes()
            }
        }

        impl core::borrow::Borrow<str> for $ty {
            #[inline]
            fn borrow(&self) -> &str {
                self
            }
        }

        impl PartialEq for $ty {
            fn eq(&self, other: &Self) -> bool {
                self.as_str() == other.as_str()
            }
        }
        impl Eq for $ty {}
        impl PartialOrd for $ty {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                self.as_str().partial_cmp(other.as_str())
            }
        }
        impl Ord for $ty {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.as_str().cmp(other.as_str())
            }
        }

        impl core::hash::Hash for $ty {
            #[inline]
            fn hash<H: core::hash::Hasher>(&self, h: &mut H) {
                self.as_str().hash(h)
            }
        }
    };
    (serde $ty:ident) => {
        impl serde::Serialize for $ty {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                self.as_str().serialize(serializer)
            }
        }

        impl<'de> serde::Deserialize<'de> for $ty {
            fn deserialize<D>(deserializer: D) -> Result<$ty, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                let s = String::deserialize(deserializer)?;
                Ok($ty::from(s))
            }
        }
    };
    ($ty:ident) => {
        rcstr_impl!(from $ty);
        rcstr_impl!(deref $ty);
        rcstr_impl!(serde $ty);
    };
}

/// A single-threaded reference-counted pointer-sized string. Can store up to 7 bytes
/// inline on x64.
#[repr(transparent)]
pub struct RcStr {
    i: RcStrInnerPtr,
}

impl RcStr {
    pub fn new() -> Self {
        Self {
            i: RcStrInnerPtr::new_inline(),
        }
    }
    pub fn try_from_str(s: &str) -> Option<Self> {
        let s = BoxedRcStr::try_from_str(s)?;
        Some(s.into_rc())
    }
    pub fn from_str(s: &str) -> Self {
        let s = BoxedRcStr::from_str(s);
        s.into_rc()
    }
    pub fn into_shared(self) -> SharedRcStr {
        let s = ManuallyDrop::new(self);
        let i = s.i.clone();
        SharedRcStr { i }
    }
    pub fn as_str(&self) -> &str {
        self.i.as_str()
    }
    /// # Safety
    ///
    /// The caller must ensure that `self` is the only owner of the `RcStr`.
    pub unsafe fn into_boxed_unchecked(self) -> RcStr {
        let s = ManuallyDrop::new(self);
        let i = s.i.clone();
        RcStr { i }
    }
    /// Converts the `SharedRcStr` into a `BoxedRcStr` if it is the only owner.
    pub fn into_boxed(self) -> Result<BoxedRcStr, Self> {
        if self.i.safe_refcnt() <= 1 {
            let s = ManuallyDrop::new(self);
            let i = s.i.clone();
            Ok(BoxedRcStr { i })
        } else {
            Err(self)
        }
    }
    pub fn try_repeat(s: &str, n: usize) -> Option<Self> {
        BoxedRcStr::try_repeat(s, n).map(|s| s.into_rc())
    }
}

impl Drop for RcStr {
    fn drop(&mut self) {
        if self.i.safe_dec_ref() == 0 {
            RcStrInnerPtr::drop_ptr(self.i.clone());
        }
    }
}

impl Clone for RcStr {
    fn clone(&self) -> Self {
        self.i.safe_inc_ref();
        Self { i: self.i.clone() }
    }
}

/// A more strict version of `RcStr` that can be shared between threads, i.e.
/// `Sync` (but not `Send`). As such, it is not possible to clone a `SharedRcStr`
/// directly. If you need to convert a `SharedRcStr` back to a `RcStr`, use the
/// unsafe `SharedRcStr::into_inner` method, provided that proper synchronization
/// is in place.
#[repr(transparent)]
pub struct SharedRcStr {
    i: RcStrInnerPtr,
}

unsafe impl Sync for SharedRcStr {}

impl SharedRcStr {
    pub fn new() -> Self {
        Self {
            i: RcStrInnerPtr::new_inline(),
        }
    }
    pub fn try_from_str(s: &str) -> Option<Self> {
        let s = BoxedRcStr::try_from_str(s)?;
        Some(s.into_rc().into_shared())
    }
    pub fn from_str(s: &str) -> Self {
        let s = BoxedRcStr::from_str(s);
        s.into_rc().into_shared()
    }
    pub fn into_unshared(self) -> RcStr {
        let s = ManuallyDrop::new(self);
        let i = s.i.clone();
        RcStr { i }
    }
    pub fn as_str(&self) -> &str {
        self.i.as_str()
    }
}

impl Drop for SharedRcStr {
    fn drop(&mut self) {
        if self.i.safe_dec_ref() == 0 {
            RcStrInnerPtr::drop_ptr(self.i.clone());
        }
    }
}

/// An owned pointer-sized immutable string. Can store up to 7 bytes inline on x64.
/// The layout of `BoxedRcStr` is the same as `RcStr`, but it is owned (refcnt is unused)
/// and is `Send` and `Sync`.
#[repr(transparent)]
pub struct BoxedRcStr {
    i: RcStrInnerPtr,
}

// NOTE: For inline data, the LSB of the pointer is set to 1.

unsafe impl Send for BoxedRcStr {}
unsafe impl Sync for BoxedRcStr {}

impl BoxedRcStr {
    /// Creates a empty `BoxedRcStr`.
    pub fn new() -> Self {
        Self {
            i: RcStrInnerPtr::new_inline(),
        }
    }

    /// Creates a `BoxedRcStr` from a `str`. If the string is too long or the heap
    /// allocation fails, returns `None`.
    pub fn try_from_str(s: &str) -> Option<Self> {
        if s.len() > u32::MAX as usize {
            return None;
        }
        let len = s.len() as u32;
        let uninit_src: &[MaybeUninit<u8>] = unsafe { std::mem::transmute(s.as_bytes()) };
        if len <= MAX_INLINE_DATA_LEN as u32 {
            let mut i = RcStrInnerPtr::new_inline();
            unsafe {
                i.put_inline_len(len as u8);
                i.inline_bytes_mut().copy_from_slice(uninit_src);
            }
            Some(Self { i })
        } else {
            let mut i = RcStrInnerPtr::try_new_heap(len).ok()?;
            unsafe {
                i.put_heap_len(len);
                i.heap_bytes_mut().copy_from_slice(uninit_src);
            }
            Some(Self { i })
        }
    }

    /// Creates a `BoxedRcStr` from a `str`. Panics if the string is too long or the heap
    /// allocation fails.
    pub fn from_str(s: &str) -> Self {
        if s.len() > u32::MAX as usize {
            panic!("string too long");
        }
        let len = s.len() as u32;
        let uninit_src: &[MaybeUninit<u8>] = unsafe { std::mem::transmute(s.as_bytes()) };
        if len <= MAX_INLINE_DATA_LEN as u32 {
            let mut i = RcStrInnerPtr::new_inline();
            unsafe {
                i.put_inline_len(len as u8);
                i.inline_bytes_mut().copy_from_slice(uninit_src);
            }
            Self { i }
        } else {
            let mut i = RcStrInnerPtr::new_heap(len);
            unsafe {
                i.put_heap_len(len);
                i.heap_bytes_mut().copy_from_slice(uninit_src);
            }
            Self { i }
        }
    }

    /// Converts the `BoxedRcStr` into a `RcStr`.
    pub fn into_rc(self) -> RcStr {
        let s = ManuallyDrop::new(self);
        let i = s.i.clone();
        i.safe_put_refcnt(1);
        RcStr { i }
    }

    /// Converts the `BoxedRcStr` into an `AncStr`.
    pub fn into_arc(self) -> ArcStr {
        let s = ManuallyDrop::new(self);
        let i = s.i.clone();
        i.safe_put_refcnt(1);
        ArcStr { i }
    }

    pub fn as_str(&self) -> &str {
        self.i.as_str()
    }

    pub fn try_repeat(s: &str, n: usize) -> Option<Self> {
        let len = s.len().checked_mul(n)?;
        if len > u32::MAX as usize {
            return None;
        }
        let mut builder = RcStrBuilder::try_new(len as u32)?;
        for _ in 0..n {
            builder.push_str(s);
        }
        Some(builder.finish())
    }
}

impl Drop for BoxedRcStr {
    fn drop(&mut self) {
        RcStrInnerPtr::drop_ptr(self.i.clone());
    }
}

impl Clone for BoxedRcStr {
    fn clone(&self) -> Self {
        Self::from_str(self)
    }
}

// impl From<&str> for BoxedRcStr {
//     fn from(s: &str) -> Self {
//         Self::from_str(s)
//     }
// }

impl From<BoxedRcStr> for RcStr {
    fn from(s: BoxedRcStr) -> Self {
        s.into_rc()
    }
}

/// An unsafe version of `RcStr` that bypasses the Rust's concurrency checks. DO NOT USE
/// UNLESS YOU KNOW WHAT YOU ARE DOING!
#[repr(transparent)]
pub struct UnsafeRcStr {
    i: RcStrInnerPtr,
}

unsafe impl Send for UnsafeRcStr {}
unsafe impl Sync for UnsafeRcStr {}

impl UnsafeRcStr {
    /// Converts a `RcStr` into a `UnsafeRcStr`.
    pub unsafe fn from_rc(s: RcStr) -> Self {
        let s = ManuallyDrop::new(s);
        let i = s.i.clone();
        Self { i }
    }
    pub fn as_str(&self) -> &str {
        self.i.as_str()
    }
}

impl Drop for UnsafeRcStr {
    fn drop(&mut self) {
        if self.i.safe_dec_ref() == 0 {
            RcStrInnerPtr::drop_ptr(self.i.clone());
        }
    }
}

/// An atomic `RcStr` that can be shared between threads. It is `Send` and `Sync`.
#[repr(transparent)]
pub struct ArcStr {
    i: RcStrInnerPtr,
}

unsafe impl Send for ArcStr {}
unsafe impl Sync for ArcStr {}

impl ArcStr {
    pub fn new() -> Self {
        Self {
            i: RcStrInnerPtr::new_inline(),
        }
    }
    pub fn try_from_str(s: &str) -> Option<Self> {
        let s = BoxedRcStr::try_from_str(s)?;
        Some(s.into_arc())
    }
    pub fn from_str(s: &str) -> Self {
        let s = BoxedRcStr::from_str(s);
        s.into_arc()
    }
    pub fn as_str(&self) -> &str {
        self.i.as_str()
    }
    pub fn try_repeat(s: &str, n: usize) -> Option<Self> {
        BoxedRcStr::try_repeat(s, n).map(|s| s.into_arc())
    }

    /// # Safety
    ///
    /// The caller must ensure that the referenced `(A)RcStr` is never manipulated
    /// concurrently, until the reference is dropped and all other references are
    /// `ArcStr`.
    pub unsafe fn as_rc(&self) -> &RcStr {
        &*(self as *const Self as *const RcStr)
    }

    /// # Safety
    ///
    /// The caller must ensure that the referenced `(A)RcStr` is never manipulated
    /// concurrently, until the reference is dropped and all other references are
    /// `ArcStr`.
    pub unsafe fn as_rc_mut(&mut self) -> &mut RcStr {
        &mut *(self as *mut Self as *mut RcStr)
    }
}

impl Drop for ArcStr {
    fn drop(&mut self) {
        if self.i.safe_atomic_dec_ref() == 0 {
            // NOTE: ArcStr is immutable, so AcqRel is not required.
            RcStrInnerPtr::drop_ptr(self.i.clone());
        }
    }
}

impl Clone for ArcStr {
    fn clone(&self) -> Self {
        self.i.safe_atomic_inc_ref();
        Self { i: self.i.clone() }
    }
}

impl From<BoxedRcStr> for ArcStr {
    fn from(s: BoxedRcStr) -> Self {
        s.into_arc()
    }
}

rcstr_impl!(RcStr);
rcstr_impl!(SharedRcStr);
rcstr_impl!(BoxedRcStr);
rcstr_impl!(deref UnsafeRcStr);
rcstr_impl!(ArcStr);

/// A builder for `BoxedRcStr` that allows for efficient construction of strings.
pub struct RcStrBuilder {
    s: RcStrInnerPtr,
    real_len: u32,
    cap: u32,
}

impl RcStrBuilder {
    pub fn new(cap: u32) -> Self {
        unsafe {
            if cap <= MAX_INLINE_DATA_LEN as u32 {
                let mut s = RcStrInnerPtr::new_inline();
                s.put_inline_len(cap as _);
                Self {
                    s,
                    real_len: 0,
                    cap,
                }
            } else {
                let s = RcStrInnerPtr::new_heap(cap);
                s.put_heap_len(cap);
                Self {
                    s,
                    real_len: 0,
                    cap,
                }
            }
        }
    }

    pub fn try_new(cap: u32) -> Option<Self> {
        unsafe {
            if cap <= MAX_INLINE_DATA_LEN as u32 {
                let mut s = RcStrInnerPtr::new_inline();
                s.put_inline_len(cap as _);
                Some(Self {
                    s,
                    real_len: 0,
                    cap,
                })
            } else {
                let s = RcStrInnerPtr::try_new_heap(cap).ok()?;
                s.put_heap_len(cap);
                Some(Self {
                    s,
                    real_len: 0,
                    cap,
                })
            }
        }
    }

    /// Appends a string slice to the end of the string.
    ///
    /// # Panics
    ///
    /// Panics if the capacity is exceeded.
    pub fn push_str(&mut self, s: &str) {
        if s.len() > u32::MAX as usize {
            panic!("string too long: {}", s.len());
        }
        if self.real_len as usize + s.len() > self.cap as usize {
            panic!(
                "capacity exceeded: {} + {} > {}",
                self.real_len,
                s.len(),
                self.cap
            );
        }
        let new_len = self.real_len + s.len() as u32;
        unsafe {
            let src: &[MaybeUninit<u8>] = std::mem::transmute(s.as_bytes());
            if self.s.is_inline() {
                let dst = self.s.inline_bytes_mut();
                dst[self.real_len as usize..new_len as usize].copy_from_slice(src);
            } else {
                let dst = self.s.heap_bytes_mut();
                dst[self.real_len as usize..new_len as usize].copy_from_slice(src);
            }
        }
        self.real_len = new_len;
    }

    /// Appends a single character to the end of the string.
    ///
    /// # Panics
    ///
    /// Panics if the final length does not equal the capacity.
    pub fn finish(self) -> BoxedRcStr {
        if self.real_len != self.cap {
            panic!(
                "final length does not equal capacity: {} != {}",
                self.real_len, self.cap
            );
        }
        let s = ManuallyDrop::new(self);
        BoxedRcStr { i: s.s.clone() }
    }
}

impl Drop for RcStrBuilder {
    fn drop(&mut self) {
        // Drop the string if it was not finished
        RcStrInnerPtr::drop_ptr(self.s.clone());
    }
}

// TODO: Use decicated method & SSO for formatting
macro_rules! format {
    ($($toks:tt)*) => {
        crate::util::rcstr::BoxedRcStr::from(&std::fmt::format(core::format_args!($($toks)*))[..]).into()
    };
}

macro_rules! literal {
    ($text:expr $(,)?) => {
        crate::util::rcstr::BoxedRcStr::from($text).into()
    };
}

pub(crate) use format;
pub(crate) use literal;

// WARN: For exposition only, do NOT use!
struct RcStrInner {
    refcnt: u32,
    len: u32,
    data: [u8; 0],
}

const INNER_REFCNT_OFFSET: usize = 0;
const INNER_LEN_OFFSET: usize = std::mem::size_of::<u32>() as usize;
const INNER_DATA_OFFSET: usize = INNER_LEN_OFFSET + std::mem::size_of::<u32>() as usize;

#[cfg(target_pointer_width = "64")]
#[repr(transparent)]
#[derive(Clone)]
struct RcStrInnerPtr(NonNull<u8>);

impl RcStrInnerPtr {
    // NOTE: Memory is not zeroed.
    /// Creates a empty inline `RcStrInnerPtr`.
    fn new_inline() -> Self {
        let i = std::ptr::null_mut::<u8>().with_addr(1);
        let i = unsafe { NonNull::new_unchecked(i) };
        Self(i)
    }
    // NOTE: Memory is not zeroed.
    fn new_heap(len: u32) -> Self {
        match Self::try_new_heap(len) {
            Ok(i) => i,
            Err(layout) => std::alloc::handle_alloc_error(layout),
        }
    }
    // NOTE: Memory is not zeroed.
    fn try_new_heap(len: u32) -> Result<Self, std::alloc::Layout> {
        use std::alloc::*;

        unsafe {
            let layout = Layout::from_size_align_unchecked(
                INNER_DATA_OFFSET + len as usize,
                std::mem::align_of::<u32>(),
            );
            let p = alloc(layout);
            if p.is_null() {
                return Err(layout);
            }
            let p = NonNull::new_unchecked(p);
            let p = p.cast::<u8>();
            Ok(Self(p))
        }
    }
    fn is_inline(&self) -> bool {
        self.0.addr().get() & 1 == 1
    }
    fn is_heap(&self) -> bool {
        !self.is_inline()
    }
    /// # Safety
    ///
    /// `self` must be a heap pointer.
    unsafe fn refcnt(&self) -> u32 {
        unsafe {
            let p = self.0.as_ptr().add(INNER_REFCNT_OFFSET).cast::<u32>();
            p.read()
        }
    }
    /// # Safety
    ///
    /// `self` must be a heap pointer.
    unsafe fn put_refcnt(&self, refcnt: u32) {
        unsafe {
            let p = self.0.as_ptr().add(INNER_REFCNT_OFFSET).cast::<u32>();
            p.write(refcnt);
        }
    }
    /// # Safety
    ///
    /// `self` must be a heap pointer.
    unsafe fn inc_ref(&self) -> u32 {
        unsafe {
            let refcnt = self.refcnt();
            if refcnt == u32::MAX {
                panic!("refcnt overflow");
            }
            self.put_refcnt(refcnt + 1);
            refcnt + 1
        }
    }
    /// # Safety
    ///
    /// `self` must be a heap pointer.
    unsafe fn dec_ref(&self) -> u32 {
        unsafe {
            let refcnt = self.refcnt();
            let refcnt = refcnt.wrapping_sub(1);
            self.put_refcnt(refcnt);
            refcnt
        }
    }
    /// # Safety
    ///
    /// `self` must be a heap pointer.
    unsafe fn atomic_inc_ref(&self) -> u32 {
        unsafe {
            let refcnt = &*(self.0.as_ptr().add(INNER_REFCNT_OFFSET).cast::<AtomicU32>());
            let r = refcnt.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            if r == u32::MAX {
                panic!("refcnt overflow");
            }
            r + 1
        }
    }
    /// # Safety
    ///
    /// `self` must be a heap pointer.
    unsafe fn atomic_dec_ref(&self) -> u32 {
        unsafe {
            let refcnt = &*(self.0.as_ptr().add(INNER_REFCNT_OFFSET).cast::<AtomicU32>());
            refcnt.fetch_sub(1, std::sync::atomic::Ordering::Relaxed) - 1
        }
    }
    /// # Safety
    ///
    /// `self` must be a heap pointer.
    unsafe fn heap_len(&self) -> u32 {
        unsafe {
            let p = self.0.as_ptr().add(INNER_LEN_OFFSET).cast::<u32>();
            p.read()
        }
    }
    /// # Safety
    ///
    /// `self` must be a heap pointer. Can be used only during construction.
    unsafe fn put_heap_len(&self, len: u32) {
        unsafe {
            let p = self.0.as_ptr().add(INNER_LEN_OFFSET).cast::<u32>();
            p.write(len);
        }
    }
    /// # Safety
    ///
    /// `self` must be a heap pointer. Can be used only after construction.
    unsafe fn heap_bytes(&self) -> &[u8] {
        unsafe {
            let len = self.heap_len() as usize;
            let p = self.0.as_ptr().add(INNER_DATA_OFFSET);
            std::slice::from_raw_parts(p, len)
        }
    }
    /// # Safety
    ///
    /// `self` must be a heap pointer. Can be used only during construction.
    unsafe fn heap_bytes_mut(&mut self) -> &mut [MaybeUninit<u8>] {
        unsafe {
            let len = self.heap_len() as usize;
            let p = self.0.as_ptr().add(INNER_DATA_OFFSET);
            std::slice::from_raw_parts_mut(p.cast(), len)
        }
    }
    /// # Safety
    ///
    /// `self` must be a inline pointer.
    unsafe fn inline_len(&self) -> u32 {
        let p = self.0.addr().get() as u8;
        (p >> 1) as u32
    }
    /// # Safety
    ///
    /// `self` must be a inline pointer. Can be used only during construction.
    /// `len` must be <= `MAX_INLINE_DATA_LEN`.
    unsafe fn put_inline_len(&mut self, len: u8) {
        unsafe {
            self.0 = self.0.map_addr(|p| {
                let b = (len << 1) | 1;
                let p = (p.get() >> 8) << 8 | b as usize;
                NonZeroUsize::new_unchecked(p)
            });
        }
    }
    /// # Safety
    ///
    /// `self` must be a inline pointer. Can be used only after construction.
    unsafe fn inline_bytes(&self) -> &[u8] {
        let len = self.inline_len() as usize;
        let p = (&raw const self.0).cast::<u8>().add(INLINE_DATA_OFFSET);
        std::slice::from_raw_parts(p, len)
    }
    /// # Safety
    ///
    /// `self` must be a inline pointer. Can be used only during construction.
    unsafe fn inline_bytes_mut(&mut self) -> &mut [MaybeUninit<u8>] {
        let len = self.inline_len() as usize;
        let p = (&raw mut self.0).cast::<u8>().add(INLINE_DATA_OFFSET);
        std::slice::from_raw_parts_mut(p.cast(), len)
    }
    fn drop_ptr(p: Self) {
        unsafe {
            if p.is_heap() {
                use std::alloc::*;
                let layout = Layout::from_size_align_unchecked(
                    INNER_DATA_OFFSET + p.heap_len() as usize,
                    std::mem::align_of::<u32>(),
                );
                dealloc(p.0.cast::<u8>().as_ptr(), layout);
            }
        }
    }
    fn as_bytes(&self) -> &[u8] {
        if self.is_inline() {
            unsafe { self.inline_bytes() }
        } else {
            unsafe { self.heap_bytes() }
        }
    }
    fn as_str(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.as_bytes()) }
    }
    fn safe_refcnt(&self) -> u32 {
        if self.is_heap() {
            unsafe { self.refcnt() }
        } else {
            0
        }
    }
    fn safe_put_refcnt(&self, refcnt: u32) {
        if self.is_heap() {
            unsafe { self.put_refcnt(refcnt) }
        }
    }
    fn safe_inc_ref(&self) -> u32 {
        if self.is_heap() {
            unsafe { self.inc_ref() }
        } else {
            0
        }
    }
    fn safe_dec_ref(&self) -> u32 {
        if self.is_heap() {
            unsafe { self.dec_ref() }
        } else {
            1
        }
    }
    fn safe_atomic_inc_ref(&self) -> u32 {
        if self.is_heap() {
            unsafe { self.atomic_inc_ref() }
        } else {
            0
        }
    }
    fn safe_atomic_dec_ref(&self) -> u32 {
        if self.is_heap() {
            unsafe { self.atomic_dec_ref() }
        } else {
            1
        }
    }
    fn len(&self) -> u32 {
        if self.is_heap() {
            unsafe { self.heap_len() }
        } else {
            unsafe { self.inline_len() }
        }
    }
}

#[test]
fn test_rcstr() {
    // Test default RcStr
    let s = RcStr::default();
    assert_eq!(&*s, "");

    // Test RcStr from &str
    let s = RcStr::from("hello");
    assert_eq!(&*s, "hello");

    // Test RcStr cloning
    let s1 = RcStr::from("world");
    let s2 = s1.clone();
    assert_eq!(&*s1, "world");
    assert_eq!(&*s2, "world");

    // Test RcStr into_shared
    let s = RcStr::from("shared");
    let shared = s.into_shared();
    assert_eq!(&*shared, "shared");

    // Test RcStr try_from_str
    let s = RcStr::try_from_str("try").unwrap();
    assert_eq!(&*s, "try");

    // Test RcStr deref
    let s = RcStr::from("deref");
    assert_eq!(&*s, "deref");

    // Test RcStr Debug
    let s = RcStr::from("debug");
    assert_eq!(std::format!("{:?}", s), "\"debug\"");

    // Test RcStr Display
    let s = RcStr::from("display");
    assert_eq!(std::format!("{}", s), "display");

    // Test RcStr AsRef<str>
    let s = RcStr::from("asref");
    assert_eq!(AsRef::<str>::as_ref(&s), "asref");

    // Test RcStr Drop
    {
        let s = RcStr::from("drop");
        assert_eq!(&*s, "drop");
    } // s should be dropped here

    // Test RcStr with inline data
    let s = RcStr::from("inline");
    assert_eq!(&*s, "inline");

    // Test RcStr with heap data
    let s = RcStr::from("this is a longer string that should be stored on the heap");
    assert_eq!(
        &*s,
        "this is a longer string that should be stored on the heap"
    );
    let s_clone = s.clone();
    assert_eq!(
        &*s_clone,
        "this is a longer string that should be stored on the heap"
    );

    // Test conversion to SharedRcStr and back
    let s = RcStr::from("shared");
    let s = s.into_boxed().unwrap();
    assert_eq!(&*s, "shared");
    let s = RcStr::from("shared long string");
    let s = s.into_boxed().unwrap();
    assert_eq!(&*s, "shared long string");

    // Test builder
    let mut builder = RcStrBuilder::new(5);
    builder.push_str("he");
    builder.push_str("llo");
    let s = builder.finish();
    assert_eq!(&*s, "hello");
    let mut builder = RcStrBuilder::new(10);
    builder.push_str("hello");
    builder.push_str("world");
    let s = builder.finish();
    assert_eq!(&*s, "helloworld");

    // Test format macro
    let s: BoxedRcStr = format!("format {}", "macro");
    assert_eq!(&*s, "format macro");
}
