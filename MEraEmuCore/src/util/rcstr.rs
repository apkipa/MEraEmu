use std::{borrow::Cow, mem::ManuallyDrop};

/// Fake arcstr::ArcStr, with SSO for strings up to `word size - 1`. Least
/// significant byte is used as inline marker and inline string length.
#[cfg(target_pointer_width = "64")]
#[repr(C)]
// TODO: Niche optimization
pub union ArcStr {
    i: usize,
    bytes: [u8; std::mem::size_of::<usize>()],
    // SAFETY: arcstr::ArcStr is #[repr(transparent)].
    s: ManuallyDrop<arcstr::ArcStr>,
}

impl Serialize for ArcStr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.as_str().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ArcStr {
    fn deserialize<D>(deserializer: D) -> Result<ArcStr, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(ArcStr::from(s))
    }
}

// TODO: Document UNSAFETY and all possible UBs.

impl ArcStr {
    const MAX_INLINE_LEN: usize = std::mem::size_of::<usize>() - 1;
    #[cfg(target_endian = "little")]
    const INLINE_OFFSET: usize = 1;
    #[cfg(target_endian = "big")]
    const INLINE_OFFSET: usize = 0;

    pub const fn new() -> Self {
        ArcStr { i: 1 }
    }
    #[inline]
    pub fn is_inline(&self) -> bool {
        unsafe { self.i & 1 == 1 }
    }
    /// Never larger than `std::mem::size_of::<usize>() - 1`.
    ///
    /// # Safety
    ///
    /// `self.is_inline()` must be true.
    #[inline]
    const unsafe fn inline_len(&self) -> usize {
        unsafe { (self.i >> 1) & 0x7f }
    }
    /// Only used for buffer filling.
    ///
    /// # Safety
    ///
    /// `self.is_inline()` must be true.
    #[inline]
    unsafe fn inline_bytes_mut(&mut self) -> &mut [u8] {
        unsafe {
            let len = self.inline_len();
            self.bytes
                .get_unchecked_mut(Self::INLINE_OFFSET..Self::INLINE_OFFSET + len)
        }
    }
    #[inline]
    pub fn as_str(&self) -> &str {
        self
    }
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        unsafe {
            if self.is_inline() {
                let len = self.inline_len();
                self.bytes
                    .get_unchecked(Self::INLINE_OFFSET..Self::INLINE_OFFSET + len)
            } else {
                self.s.as_bytes()
            }
        }
    }
    #[inline]
    pub const fn _private_new_inline_literal(__text: &str) -> Option<usize> {
        if __text.len() <= Self::MAX_INLINE_LEN {
            let mut i = __text.len() << 1;
            i |= 1;
            union PureArcStr {
                i: usize,
                bytes: [u8; std::mem::size_of::<usize>()],
            }
            let mut s = PureArcStr { i };
            unsafe {
                let mut offset = Self::INLINE_OFFSET;
                let mut in_buf = __text.as_bytes();
                while let [b, rest @ ..] = in_buf {
                    s.bytes[offset] = *b;
                    offset += 1;
                    in_buf = rest;
                }
                Some(s.i)
            }
        } else {
            None
        }
    }
    #[inline]
    pub const fn _private_new_inline_literal_2(i: usize) -> Self {
        ArcStr { i }
    }
    #[inline]
    pub const fn _private_new_heap_literal(__text: arcstr::ArcStr) -> Self {
        ArcStr {
            s: ManuallyDrop::new(__text),
        }
    }
}

impl ArcStr {
    // HACK: Used for interning
    #[inline]
    pub fn unique_mark(&self) -> usize {
        unsafe {
            if self.is_inline() {
                self.i
            } else {
                self.s.as_ptr() as _
            }
        }
    }
    pub fn try_repeat(source: &str, n: usize) -> Option<Self> {
        // If the source string is empty or the user asked for zero repetitions,
        // return an empty string
        if source.is_empty() || n == 0 {
            return Some(Self::new());
        }

        // Calculate the capacity for the allocated string
        let capacity = source.len().checked_mul(n)?;
        if capacity <= Self::MAX_INLINE_LEN {
            // Repeat inline
            // TODO: Optimize performance
            return Some(source.repeat(n).into());
        }

        arcstr::ArcStr::try_repeat(source, n).map(ArcStr::_private_new_heap_literal)
    }
}

impl Drop for ArcStr {
    fn drop(&mut self) {
        if !self.is_inline() {
            unsafe {
                ManuallyDrop::drop(&mut self.s);
            }
        }
    }
}

impl Default for ArcStr {
    fn default() -> Self {
        Self::new()
    }
}

impl From<&str> for ArcStr {
    fn from(value: &str) -> Self {
        unsafe {
            let s = if value.len() <= Self::MAX_INLINE_LEN {
                let mut i = value.len() << 1;
                i |= 1;
                let mut s = ArcStr { i };
                s.inline_bytes_mut()
                    .as_mut_ptr()
                    .copy_from_nonoverlapping(value.as_ptr(), value.len());
                s
            } else {
                let s = ArcStr {
                    s: ManuallyDrop::new(value.into()),
                };
                debug_assert_eq!(s.i & 1, 0);
                s
            };
            debug_assert_eq!(s.as_str(), value);
            s
        }
    }
}
impl From<String> for ArcStr {
    fn from(value: String) -> Self {
        Self::from(value.as_str())
    }
}
impl From<&String> for ArcStr {
    #[inline]
    fn from(s: &String) -> Self {
        Self::from(s.as_str())
    }
}
impl From<&ArcStr> for ArcStr {
    #[inline]
    fn from(s: &ArcStr) -> Self {
        s.clone()
    }
}
impl<'a> From<Cow<'a, str>> for ArcStr {
    #[inline]
    fn from(s: Cow<'a, str>) -> Self {
        Self::from(&*s)
    }
}
impl<'a> From<&'a ArcStr> for Cow<'a, str> {
    #[inline]
    fn from(s: &'a ArcStr) -> Self {
        Cow::Borrowed(s)
    }
}

impl core::ops::Deref for ArcStr {
    type Target = str;
    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { core::str::from_utf8_unchecked(self.as_bytes()) }
    }
}

impl Clone for ArcStr {
    fn clone(&self) -> Self {
        unsafe {
            let s = if self.is_inline() {
                ArcStr { i: self.i }
            } else {
                ArcStr {
                    s: ManuallyDrop::clone(&self.s),
                }
            };
            debug_assert_eq!(s.as_str(), self.as_str());
            s
        }
    }
}

impl PartialEq for ArcStr {
    fn eq(&self, other: &Self) -> bool {
        unsafe {
            if self.is_inline() {
                if other.is_inline() {
                    self.i == other.i
                } else {
                    false
                }
            } else {
                if other.is_inline() {
                    false
                } else {
                    self.s == other.s
                }
            }
        }
    }
}

impl Eq for ArcStr {}

impl PartialOrd for ArcStr {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for ArcStr {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        use std::ops::Deref;
        self.deref().cmp(other.deref())
    }
}

impl AsRef<str> for ArcStr {
    fn as_ref(&self) -> &str {
        self
    }
}

impl AsRef<[u8]> for ArcStr {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl core::borrow::Borrow<str> for ArcStr {
    #[inline]
    fn borrow(&self) -> &str {
        self
    }
}

impl core::fmt::Debug for ArcStr {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self.is_inline() {
            core::fmt::Display::fmt("(inline) ", f)?;
        }
        core::fmt::Debug::fmt(self.as_str(), f)
    }
}

impl core::fmt::Display for ArcStr {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(self.as_str(), f)
    }
}

impl core::hash::Hash for ArcStr {
    #[inline]
    fn hash<H: core::hash::Hasher>(&self, h: &mut H) {
        self.as_str().hash(h)
    }
}

macro_rules! format {
    ($($toks:tt)*) => {
        $crate::util::rcstr::ArcStr::from(std::fmt::format(core::format_args!($($toks)*)))
    };
}

// Derived from: arcstr crate
macro_rules! literal {
    ($text:expr $(,)?) => {{
        const __TEXT: &str = $text;
        const S: $crate::util::rcstr::ArcStr =
            match $crate::util::rcstr::ArcStr::_private_new_inline_literal(__TEXT) {
                Some(s) => $crate::util::rcstr::ArcStr::_private_new_inline_literal_2(s),
                None => {
                    $crate::util::rcstr::ArcStr::_private_new_heap_literal(arcstr::literal!($text))
                }
            };
        S
    }};
}

pub(crate) use format;
pub(crate) use literal;
use serde::{Deserialize, Serialize};

#[test]
fn test_arcstr() {
    let s = ArcStr::from("hello");
    assert_eq!(s.as_str(), "hello");
    assert_eq!(s.as_bytes(), b"hello");
    assert_eq!(s.is_inline(), true);
    assert_eq!(s.clone().as_str(), "hello");
    assert_eq!(s.clone().is_inline(), true);

    let s = ArcStr::from("hello!");
    assert_eq!(s.as_str(), "hello!");
    assert_eq!(s.as_bytes(), b"hello!");
    assert_eq!(s.is_inline(), true);

    let s = ArcStr::from("hello, world!");
    assert_eq!(s.as_str(), "hello, world!");
    assert_eq!(s.as_bytes(), b"hello, world!");
    assert_eq!(s.is_inline(), false);

    let s = literal!("hello");
    assert_eq!(s.as_str(), "hello");
    assert_eq!(s.as_bytes(), b"hello");
    assert_eq!(s.is_inline(), true);

    let s = ArcStr::from("(");
    assert_eq!(s.as_str(), "(");
    assert_eq!(s.as_bytes(), b"(");
    assert_eq!(s.is_inline(), true);
    assert_eq!(s.clone().as_str(), "(");
    assert_eq!(s.clone().is_inline(), true);
}
