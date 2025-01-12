use cstree::interning::{InternKey, Interner, Resolver, TokenKey};
use rustc_hash::FxBuildHasher;

use std::{cell::UnsafeCell, mem::MaybeUninit};

// static mut rodeo: MaybeUninit<lasso::Rodeo<TokenKey, FxBuildHasher>> = MaybeUninit::uninit();

#[derive(Debug)]
pub struct ThreadedTokenInterner {
    rodeo: lasso::ThreadedRodeo<TokenKey, FxBuildHasher>,
    // For faster lookup
    strings: UnsafeCell<Vec<&'static str>>,
}

unsafe impl Sync for ThreadedTokenInterner {}
unsafe impl Send for ThreadedTokenInterner {}

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
