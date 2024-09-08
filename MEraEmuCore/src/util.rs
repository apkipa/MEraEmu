pub mod ascii;
pub mod html;
pub mod io;
pub mod number;
pub mod rcstr;

use std::{borrow::Borrow, fmt::Display, hash::Hash, ops::Deref, rc::Rc};

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

macro_rules! bmatch_caseless {
    // ($pattern:expr => $value:expr, _ => $default_value:expr) => {
    //     |value: &[u8]| if value.iter().map(|&c| c.to_ascii_uppercase()) == uppercase_bliteral!($pattern).iter().copied() {
    //         $value
    //     } else {
    //         $default_value
    //     }
    // };
    ($pattern:expr => $value:expr $(, $patterns:expr => $values:expr)*, _ => $default_value:expr $(,)?) => {{
        use crate::util::uppercase_bliteral;

        |value: &[u8]| if value.iter().map(|&c| c.to_ascii_uppercase()).eq(uppercase_bliteral!($pattern).iter().copied()) {
            $value
        } $(else if value.iter().map(|&c| c.to_ascii_uppercase()).eq(uppercase_bliteral!($patterns).iter().copied()) {
            $values
        })* else {
            $default_value
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
