use cstree::interning::{InternKey, Interner, Resolver, TokenKey};
use rustc_hash::FxBuildHasher;
use serde::Serialize;
use std::{cell::UnsafeCell, mem::MaybeUninit};

use crate::util::rcstr::ArcStr;

// static mut rodeo: MaybeUninit<lasso::Rodeo<TokenKey, FxBuildHasher>> = MaybeUninit::uninit();

#[derive(Debug)]
pub struct ThreadedTokenInterner {
    rodeo: lasso::ThreadedRodeo<TokenKey, FxBuildHasher>,
    // For faster lookup
    strings: UnsafeCell<Vec<&'static str>>,
}

unsafe impl Sync for ThreadedTokenInterner {}
unsafe impl Send for ThreadedTokenInterner {}

impl Serialize for ThreadedTokenInterner {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.rodeo.serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for ThreadedTokenInterner {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let mut r = lasso::ThreadedRodeo::deserialize(deserializer).map(|rodeo| Self {
            rodeo,
            strings: UnsafeCell::new(Vec::new()),
        })?;
        r.optimize_lookup();
        Ok(r)
    }
}

impl Default for ThreadedTokenInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl ThreadedTokenInterner {
    pub fn new() -> Self {
        // unsafe {
        //     rodeo.write(lasso::Rodeo::with_hasher(Default::default()));
        // }
        Self {
            rodeo: lasso::ThreadedRodeo::with_hasher(Default::default()),
            strings: UnsafeCell::new(Vec::new()),
        }
    }

    pub fn optimize_lookup(&mut self) {
        // SAFETY: We uniquely borrow `self` and ensure that no other borrows exist.
        unsafe {
            self.optimize_lookup_unchecked();
        }
    }

    /// # Safety
    ///
    /// Caller must ensure that the `ThreadedTokenInterner` is not accessed concurrently.
    pub unsafe fn optimize_lookup_unchecked(&self) {
        let strings = &mut *self.strings.get();

        let rodeo_len = self.rodeo.len();
        if strings.len() == rodeo_len {
            return;
        }

        strings.resize(rodeo_len, "");
        for (k, v) in self.rodeo.iter() {
            // SAFETY: Self-referential strings
            strings[k.into_u32() as usize] = unsafe { std::mem::transmute(v) };
        }
    }

    fn get_strings(&self) -> &Vec<&'static str> {
        // SAFETY: We ensure that no other borrows exist.
        unsafe { &*self.strings.get() }
    }
}

impl Resolver<TokenKey> for ThreadedTokenInterner {
    #[inline]
    fn try_resolve(&self, key: TokenKey) -> Option<&str> {
        self.get_strings()
            .get(key.into_u32() as usize)
            .copied()
            .or_else(|| self.rodeo.try_resolve(&key))
        // unsafe { rodeo.assume_init_mut().try_resolve(&key) }
    }

    #[inline]
    fn resolve(&self, key: TokenKey) -> &str {
        self.get_strings()
            .get(key.into_u32() as usize)
            .copied()
            .unwrap_or_else(|| self.rodeo.resolve(&key))
        // unsafe { rodeo.assume_init_mut().resolve(&key) }
    }
}

impl Interner<TokenKey> for ThreadedTokenInterner {
    type Error = lasso::LassoError;

    #[inline]
    fn try_get_or_intern(&mut self, text: &str) -> Result<TokenKey, Self::Error> {
        (&*self).try_get_or_intern(text)
    }

    #[inline]
    fn get_or_intern(&mut self, text: &str) -> TokenKey {
        (&*self).get_or_intern(text)
    }
}

impl Resolver<TokenKey> for &ThreadedTokenInterner {
    #[inline]
    fn try_resolve(&self, key: TokenKey) -> Option<&str> {
        self.get_strings()
            .get(key.into_u32() as usize)
            .copied()
            .or_else(|| self.rodeo.try_resolve(&key))
        // unsafe { rodeo.assume_init_mut().try_resolve(&key) }
    }

    #[inline]
    fn resolve(&self, key: TokenKey) -> &str {
        self.get_strings()
            .get(key.into_u32() as usize)
            .copied()
            .unwrap_or_else(|| self.rodeo.resolve(&key))
        // unsafe { rodeo.assume_init_mut().resolve(&key) }
    }
}

impl Interner<TokenKey> for &ThreadedTokenInterner {
    type Error = lasso::LassoError;

    #[inline]
    fn try_get_or_intern(&mut self, text: &str) -> Result<TokenKey, Self::Error> {
        self.rodeo.try_get_or_intern(text)
        // unsafe { rodeo.assume_init_mut().try_get_or_intern(text) }
    }

    #[inline]
    fn get_or_intern(&mut self, text: &str) -> TokenKey {
        self.rodeo.get_or_intern(text)
        // unsafe { rodeo.assume_init_mut().get_or_intern(text) }
    }
}

// Faster version with higher memory usage
#[derive(Debug)]
pub struct VecThreadedTokenInterner {
    vec: orx_concurrent_vec::ConcurrentVec<ArcStr>,
}

impl Default for VecThreadedTokenInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl VecThreadedTokenInterner {
    pub fn new() -> Self {
        Self {
            vec: orx_concurrent_vec::ConcurrentVec::new(),
        }
    }

    pub fn optimize_lookup(&mut self) {
        // Do nothing
    }

    /// # Safety
    ///
    /// Caller must ensure that the `VecThreadedTokenInterner` is not accessed concurrently.
    pub unsafe fn optimize_lookup_unchecked(&self) {
        // Do nothing
    }
}

impl Resolver<TokenKey> for VecThreadedTokenInterner {
    #[inline]
    fn try_resolve(&self, key: TokenKey) -> Option<&str> {
        unsafe {
            self.vec
                .get_ref(key.into_u32() as usize)
                .map(|s| s.as_str())
        }
    }

    #[inline]
    fn resolve(&self, key: TokenKey) -> &str {
        self.try_resolve(key).expect("TokenKey does not exist")
    }
}

impl Interner<TokenKey> for VecThreadedTokenInterner {
    type Error = std::convert::Infallible;

    #[inline]
    fn try_get_or_intern(&mut self, text: &str) -> Result<TokenKey, Self::Error> {
        let arc = ArcStr::from(text);
        let index = self.vec.push(arc);
        Ok(TokenKey::try_from_u32(index as u32).unwrap())
    }

    #[inline]
    fn get_or_intern(&mut self, text: &str) -> TokenKey {
        self.try_get_or_intern(text).expect("Failed to intern")
    }
}

impl Resolver<TokenKey> for &VecThreadedTokenInterner {
    #[inline]
    fn try_resolve(&self, key: TokenKey) -> Option<&str> {
        unsafe {
            self.vec
                .get_ref(key.into_u32() as usize)
                .map(|s| s.as_str())
        }
    }

    #[inline]
    fn resolve(&self, key: TokenKey) -> &str {
        self.try_resolve(key).expect("TokenKey does not exist")
    }
}

impl Interner<TokenKey> for &VecThreadedTokenInterner {
    type Error = std::convert::Infallible;

    #[inline]
    fn try_get_or_intern(&mut self, text: &str) -> Result<TokenKey, Self::Error> {
        let arc = ArcStr::from(text);
        let index = self.vec.push(arc);
        Ok(TokenKey::try_from_u32(index as u32).unwrap())
    }

    #[inline]
    fn get_or_intern(&mut self, text: &str) -> TokenKey {
        self.try_get_or_intern(text).expect("Failed to intern")
    }
}
