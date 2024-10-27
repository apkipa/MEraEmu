pub mod ascii;
pub mod dhashmap;
pub mod html;
pub mod interning;
pub mod io;
pub mod number;
pub mod random;
pub mod rcstr;
pub mod syntax;

use std::{
    borrow::Borrow,
    fmt::Display,
    hash::Hash,
    mem::ManuallyDrop,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    rc::Rc,
};

/// An ASCII-caseless string slice type.
#[derive(Debug)]
#[repr(transparent)]
pub struct CaselessStr(str);
impl PartialEq for CaselessStr {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ignore_ascii_case(&other.0)
    }
}
impl Eq for CaselessStr {}
impl PartialOrd for CaselessStr {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for CaselessStr {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // We convert everything to lowercase for comparison
        for (x, y) in self.0.bytes().zip(other.0.bytes()) {
            let (x, y) = (x.to_ascii_lowercase(), y.to_ascii_lowercase());
            let r = x.cmp(&y);
            if r != std::cmp::Ordering::Equal {
                return r;
            }
        }
        self.0.len().cmp(&other.0.len())
    }
}
impl CaselessStr {
    pub fn new(value: &str) -> &Self {
        unsafe { std::mem::transmute(value) }
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
    pub fn starts_with(&self, other: &CaselessStr) -> bool {
        let other_len = other.0.len();
        if self.0.len() < other_len {
            return false;
        }
        Self::new(&self.0[..other_len]) == other
    }
}

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct CaselessString(String);
impl PartialEq for CaselessString {
    fn eq(&self, other: &Self) -> bool {
        self.deref().eq(other)
    }
}
impl Eq for CaselessString {}
impl PartialOrd for CaselessString {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for CaselessString {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.deref().cmp(other)
    }
}
impl Deref for CaselessString {
    type Target = CaselessStr;

    fn deref(&self) -> &Self::Target {
        CaselessStr::new(&self.0)
    }
}
impl From<String> for CaselessString {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}
impl From<&str> for CaselessString {
    fn from(value: &str) -> Self {
        Self::new(value.to_owned())
    }
}
impl AsRef<CaselessStr> for CaselessString {
    fn as_ref(&self) -> &CaselessStr {
        &self
    }
}
impl Borrow<CaselessStr> for CaselessString {
    fn borrow(&self) -> &CaselessStr {
        &self
    }
}
impl CaselessString {
    pub fn new(value: String) -> Self {
        Self(value)
    }
    pub fn into_inner(self) -> String {
        self.0
    }
}

// impl From<String> for Rc<CaselessStr> {
//     fn from(value: String) -> Self {
//         CaselessString::new(value).into()
//     }
// }
impl From<CaselessString> for Rc<CaselessStr> {
    fn from(value: CaselessString) -> Self {
        Rc::from(value.deref())
    }
}
impl From<&CaselessStr> for Rc<CaselessStr> {
    fn from(value: &CaselessStr) -> Self {
        let rc = Rc::<[u8]>::from(value.0.as_bytes());
        unsafe { Rc::from_raw(Rc::into_raw(rc) as *const CaselessStr) }
    }
}
impl Hash for CaselessStr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for c in self.0.as_bytes() {
            c.to_ascii_lowercase().hash(state);
        }
    }
}
impl Hash for CaselessString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}

impl Display for CaselessStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl Display for CaselessString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

pub use ascii::Ascii;

macro_rules! uppercase_bliteral {
    ($value:expr) => {{
        const STR_IN: &[u8] = $value;
        const STR_OUT: [u8; STR_IN.len()] = {
            let mut out = [0; STR_IN.len()];
            let mut i = 0;
            while i < STR_IN.len() {
                out[i] = STR_IN[i].to_ascii_uppercase();
                i += 1;
            }
            out
        };
        &STR_OUT
    }};
}

// TODO: Does compiler optimize bmatch_caseless! well enough?
macro_rules! bmatch_caseless {
    ($pattern:expr => $value:expr $(, $patterns:expr => $values:expr)*, _ => $default_value:expr $(,)?) => {{
        use crate::util::uppercase_bliteral;

        // const ALL_PATTERNS: &[&[u8]] = &[$pattern, $($patterns,)*];
        const MAX_LEN: usize = {
            let mut max_len = 0;
            const fn get_max(a: usize, b: usize) -> usize {
                if a > b {
                    a
                } else {
                    b
                }
            }
            max_len = get_max(max_len, $pattern.len());
            $(max_len = get_max(max_len, $patterns.len());)*
            max_len
        };

        |value: &[u8]| {
            if value.len() > MAX_LEN {
                return $default_value;
            }

            let uppercased = {
                let mut uppercased = [0; MAX_LEN];
                for (i, &c) in value.iter().enumerate() {
                    uppercased[i] = c.to_ascii_uppercase();
                }
                // uppercased[..value.len()].copy_from_slice(value);
                // uppercased.make_ascii_uppercase();
                uppercased
            };
            let uppercased = &uppercased[..value.len()];

            if uppercased == uppercase_bliteral!($pattern) {
                $value
            } $(else if uppercased == uppercase_bliteral!($patterns) {
                $values
            })* else {
                $default_value
            }
        }
    }};
}

pub(crate) use bmatch_caseless;
pub(crate) use uppercase_bliteral;

// Source: https://stackoverflow.com/a/50781657
pub trait SubsliceOffset {
    /**
    Returns the byte offset of an inner slice relative to an enclosing outer slice.

    Examples

    ```ignore
    let string = "a\nb\nc";
    let lines: Vec<&str> = string.lines().collect();
    assert!(string.subslice_offset(lines[0]) == Some(0)); // &"a"
    assert!(string.subslice_offset(lines[1]) == Some(2)); // &"b"
    assert!(string.subslice_offset(lines[2]) == Some(4)); // &"c"
    assert!(string.subslice_offset("other!") == None);
    ```
    */
    fn subslice_offset(&self, inner: &Self) -> Option<usize>;
}

impl SubsliceOffset for str {
    fn subslice_offset(&self, inner: &Self) -> Option<usize> {
        let self_range = self.as_bytes().as_ptr_range();
        let self_range = (self_range.start as usize)..(self_range.end as usize);
        let inner = inner.as_ptr() as usize;
        // if inner < self_range.start || inner + inner.len() > self_range.end {
        if inner < self_range.start || inner > self_range.end {
            None
        } else {
            Some(inner - self_range.start)
        }
    }
}

pub trait StrReplaceCount {
    fn replace_count(&self, from: &str, to: &str) -> (String, usize);
}

impl StrReplaceCount for str {
    fn replace_count(&self, from: &str, to: &str) -> (String, usize) {
        let mut count = 0;
        let mut last_end = 0;
        let mut result = String::new();
        for (start, part) in self.match_indices(from) {
            count += 1;
            result.push_str(&self[last_end..start]);
            result.push_str(to);
            last_end = start + part.len();
        }
        result.push_str(&self[last_end..]);
        (result, count)
    }
}

pub fn inline_to_ascii_uppercase<const LEN: usize>(
    value: &[u8],
) -> Option<arrayvec::ArrayVec<u8, LEN>> {
    if value.len() > LEN {
        return None;
    }
    let mut out = arrayvec::ArrayVec::<u8, LEN>::new();
    for b in value {
        out.push(b.to_ascii_uppercase());
    }
    Some(out)
}

pub fn array_try_from_fn<T, E, const N: usize, F>(mut cb: F) -> Result<[T; N], E>
where
    F: FnMut(usize) -> Result<T, E>,
{
    let out = [const { std::mem::MaybeUninit::uninit() }; N];
    let mut out = scopeguard::guard((out, 0usize), |(mut out, count)| {
        for i in 0..count {
            unsafe {
                // Drop in place
                out[i].assume_init_drop();
            }
        }
    });
    for i in 0..N {
        out.0[i].write(cb(i)?);
        out.1 += 1;
    }
    let (out, _) = scopeguard::ScopeGuard::into_inner(out);
    Ok(unsafe { core::mem::transmute_copy(&out) })
}

pub fn pack_u32_from_iter(iter: impl Iterator<Item = u32> + Clone) -> (u8, Vec<u8>) {
    let mut count = 0;
    let max = iter.clone().inspect(|_| count += 1).max().unwrap_or(0);
    let bits = u32::BITS - max.leading_zeros();
    let num_size = if bits <= 8 {
        1
    } else if bits <= 16 {
        2
    } else if bits <= 24 {
        3
    } else {
        4
    };
    let mut bytes = Vec::with_capacity(num_size as usize * count);
    for num in iter {
        if 0 < num_size {
            bytes.push((num >> 0) as u8);
        }
        if 1 < num_size {
            bytes.push((num >> 8) as u8);
        }
        if 2 < num_size {
            bytes.push((num >> 16) as u8);
        }
        if 3 < num_size {
            bytes.push((num >> 24) as u8);
        }
    }
    (num_size, bytes)
}

pub fn read_packed_u32(num_size: u8, data: &[u8]) -> u32 {
    assert_eq!(num_size as usize, data.len());
    let mut num = 0;
    for i in 0..num_size {
        num |= (data[i as usize] as u32) << (i * 8);
    }
    num
}

/// A RAII pointer to a heap-allocated value.
#[derive(Debug)]
#[repr(transparent)]
pub struct BoxPtr<T>(NonNull<T>);

impl<T> From<Box<T>> for BoxPtr<T> {
    fn from(value: Box<T>) -> Self {
        unsafe {
            let ptr = Box::into_raw(value);
            Self(NonNull::new_unchecked(ptr))
        }
    }
}

impl<T> Deref for BoxPtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<T> DerefMut for BoxPtr<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

impl<T> Drop for BoxPtr<T> {
    fn drop(&mut self) {
        unsafe {
            let _ = Box::from_raw(self.0.as_ptr());
        }
    }
}

union DataUnion2<T, U> {
    t: ManuallyDrop<T>,
    u: ManuallyDrop<U>,
}

pub struct SmallSlice<'a, T> {
    data: DataUnion2<T, *const T>,
    len: usize,
    _marker: std::marker::PhantomData<&'a T>,
}

impl std::fmt::Debug for SmallSlice<'_, u32> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_slice().fmt(f)
    }
}

impl<'a> SmallSlice<'a, u32> {
    pub fn new(data: &'a [u32]) -> Self {
        let len = data.len();
        if len == 1 {
            Self::new_inline(data[0])
        } else {
            Self {
                data: DataUnion2 {
                    u: ManuallyDrop::new(data.as_ptr()),
                },
                len,
                _marker: std::marker::PhantomData,
            }
        }
    }

    pub fn new_inline(data: u32) -> Self {
        Self {
            data: DataUnion2 {
                t: ManuallyDrop::new(data),
            },
            len: 1,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn empty() -> Self {
        Self {
            data: DataUnion2 {
                u: ManuallyDrop::new(NonNull::dangling().as_ptr()),
            },
            len: 0,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn as_slice(&self) -> &[u32] {
        if self.len == 1 {
            std::slice::from_ref(unsafe { &self.data.t })
        } else {
            unsafe { std::slice::from_raw_parts(*self.data.u, self.len) }
        }
    }
}

impl Deref for SmallSlice<'_, u32> {
    type Target = [u32];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}
