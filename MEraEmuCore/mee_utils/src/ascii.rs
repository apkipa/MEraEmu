use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::fmt::Display;
use std::hash::{Hash, Hasher};

/// String wrapper that exposes the underlying string as case-insensitive
/// (uppercase) ASCII.
///
/// WARNING: When comparing `UpCase` values with strings, the caseness of the
/// string is ignored. For example, `UpCase("Foo") == "foo"` will return true.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Default)]
pub struct UpCase<T: ?Sized>(T);

impl<S: AsRef<str>> UpCase<S> {
    pub fn new(value: S) -> UpCase<S> {
        UpCase(value)
    }
    pub fn into_inner(self) -> S {
        self.0
    }
}

impl UpCase<str> {
    pub fn new_str(value: &str) -> &UpCase<str> {
        value.into()
    }
}

impl<T: Serialize> Serialize for UpCase<T> {
    fn serialize<D: serde::Serializer>(&self, serializer: D) -> Result<D::Ok, D::Error> {
        self.0.serialize(serializer)
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for UpCase<T> {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        T::deserialize(deserializer).map(UpCase)
    }
}

impl<S: AsRef<str> + ?Sized> AsRef<str> for UpCase<S> {
    #[inline]
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl<S: AsRef<str> + ?Sized> UpCase<S> {
    #[inline]
    pub fn as_ascii_ref(&self) -> &UpCase<str> {
        UpCase::new_str(self.0.as_ref())
    }
}

impl<S: AsRef<str> + ?Sized> Hash for UpCase<S> {
    #[inline]
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        // Hash as ASCII uppercase

        // write_prefix_length
        hasher.write_usize(self.0.as_ref().len());
        hasher.write_u64(crate::string::rustc_hash_bytes_ascii_uppercase(
            self.0.as_ref().as_bytes(),
        ));
    }
}

impl<S: Display> Display for UpCase<S> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}
impl Display for &UpCase<str> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<S: Borrow<str>> Borrow<UpCase<str>> for UpCase<S> {
    fn borrow(&self) -> &UpCase<str> {
        unsafe { std::mem::transmute(self.0.borrow()) }
    }
}

impl<S: AsRef<str> + ?Sized> PartialEq for UpCase<S> {
    fn eq(&self, other: &Self) -> bool {
        // Convert to ASCII uppercase for comparison
        crate::string::eq_ignore_ascii_case(self.0.as_ref().as_bytes(), other.0.as_ref().as_bytes())
    }
}
impl<S: AsRef<str> + ?Sized> Eq for UpCase<S> {}
impl<S: AsRef<str> + ?Sized> PartialOrd for UpCase<S> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<S: AsRef<str> + ?Sized> Ord for UpCase<S> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        crate::string::cmp_ignore_ascii_case(
            self.0.as_ref().as_bytes(),
            other.0.as_ref().as_bytes(),
        )
    }
}

impl From<String> for UpCase<String> {
    fn from(value: String) -> Self {
        UpCase(value)
    }
}
impl From<&str> for &UpCase<str> {
    fn from(value: &str) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}
