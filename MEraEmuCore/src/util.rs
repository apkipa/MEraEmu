pub mod ascii;
pub mod hstring;
pub mod html;
pub mod number;

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
