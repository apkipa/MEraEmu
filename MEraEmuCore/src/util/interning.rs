use cstree::interning::{InternKey, Interner, Resolver, TokenKey};
use rustc_hash::FxBuildHasher;

#[derive(Debug)]
pub struct ThreadedTokenInterner {
    rodeo: lasso::ThreadedRodeo<TokenKey, FxBuildHasher>,
    // For faster lookup
    strings: Vec<&'static str>,
}

impl Default for ThreadedTokenInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl ThreadedTokenInterner {
    pub fn new() -> Self {
        Self {
            rodeo: lasso::ThreadedRodeo::with_hasher(Default::default()),
            strings: Vec::new(),
        }
    }

    pub fn optimize_lookup(&mut self) {
        let rodeo_len = self.rodeo.len();
        if self.strings.len() == rodeo_len {
            return;
        }

        self.strings.resize(rodeo_len, "");
        for (k, v) in self.rodeo.iter() {
            // SAFETY: Self-referential strings
            self.strings[k.into_u32() as usize] = unsafe { std::mem::transmute(v) };
        }
    }
}

impl Resolver<TokenKey> for ThreadedTokenInterner {
    #[inline]
    fn try_resolve(&self, key: TokenKey) -> Option<&str> {
        self.strings
            .get(key.into_u32() as usize)
            .copied()
            .or_else(|| self.rodeo.try_resolve(&key))
    }

    #[inline]
    fn resolve(&self, key: TokenKey) -> &str {
        self.strings
            .get(key.into_u32() as usize)
            .copied()
            .unwrap_or_else(|| self.rodeo.resolve(&key))
    }
}

impl Interner<TokenKey> for ThreadedTokenInterner {
    type Error = lasso::LassoError;

    #[inline]
    fn try_get_or_intern(&mut self, text: &str) -> Result<TokenKey, Self::Error> {
        self.rodeo.try_get_or_intern(text)
    }

    #[inline]
    fn get_or_intern(&mut self, text: &str) -> TokenKey {
        self.rodeo.get_or_intern(text)
    }
}

impl Resolver<TokenKey> for &ThreadedTokenInterner {
    #[inline]
    fn try_resolve(&self, key: TokenKey) -> Option<&str> {
        self.strings
            .get(key.into_u32() as usize)
            .copied()
            .or_else(|| self.rodeo.try_resolve(&key))
    }

    #[inline]
    fn resolve(&self, key: TokenKey) -> &str {
        self.strings
            .get(key.into_u32() as usize)
            .copied()
            .unwrap_or_else(|| self.rodeo.resolve(&key))
    }
}

impl Interner<TokenKey> for &ThreadedTokenInterner {
    type Error = lasso::LassoError;

    #[inline]
    fn try_get_or_intern(&mut self, text: &str) -> Result<TokenKey, Self::Error> {
        self.rodeo.try_get_or_intern(text)
    }

    #[inline]
    fn get_or_intern(&mut self, text: &str) -> TokenKey {
        self.rodeo.get_or_intern(text)
    }
}
