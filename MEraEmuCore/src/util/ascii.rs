use std::{
    borrow::Borrow,
    fmt::Display,
    hash::{Hash, Hasher},
};

/// Case Insensitive wrapper of Ascii strings.
#[derive(Clone, Copy, Debug, Default)]
#[repr(transparent)]
pub struct Ascii<S: ?Sized>(S);
impl<S: AsRef<str>> Ascii<S> {
    pub fn new(value: S) -> Ascii<S> {
        Ascii(value)
    }
    pub fn into_inner(self) -> S {
        self.0
    }
}
impl Ascii<str> {
    pub fn new_str(value: &str) -> &Ascii<str> {
        value.into()
    }
}

impl<S: AsRef<str> + ?Sized> AsRef<str> for Ascii<S> {
    #[inline]
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl<S: AsRef<str> + ?Sized> Hash for Ascii<S> {
    #[inline]
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        for byte in self.as_ref().bytes().map(|b| b.to_ascii_lowercase()) {
            hasher.write_u8(byte);
        }
    }
}

impl<S: Display> Display for Ascii<S> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}
impl Display for &Ascii<str> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl<S: Borrow<str>> Borrow<Ascii<str>> for Ascii<S> {
    fn borrow(&self) -> &Ascii<str> {
        unsafe { std::mem::transmute(self.0.borrow()) }
    }
}

impl<S: AsRef<str> + ?Sized> PartialEq for Ascii<S> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ref().eq_ignore_ascii_case(other.0.as_ref())
    }
}
impl<S: AsRef<str> + ?Sized> Eq for Ascii<S> {}
impl<S: AsRef<str> + ?Sized> PartialOrd for Ascii<S> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<S: AsRef<str> + ?Sized> Ord for Ascii<S> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // We convert everything to lowercase for comparison
        for (x, y) in self.0.as_ref().bytes().zip(other.0.as_ref().bytes()) {
            let (x, y) = (x.to_ascii_lowercase(), y.to_ascii_lowercase());
            let r = x.cmp(&y);
            if r != std::cmp::Ordering::Equal {
                return r;
            }
        }
        self.0.as_ref().len().cmp(&other.0.as_ref().len())
    }
}

impl From<String> for Ascii<String> {
    fn from(value: String) -> Self {
        Ascii(value)
    }
}
impl From<&str> for &Ascii<str> {
    fn from(value: &str) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}
