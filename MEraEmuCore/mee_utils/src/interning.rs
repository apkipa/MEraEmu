use indexmap::{IndexMap, IndexSet};
use rustc_hash::FxBuildHasher;

#[repr(transparent)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TokenKey(u32);

impl TokenKey {
    #[inline]
    pub fn into_u32(self) -> u32 {
        self.0
    }

    #[inline]
    pub fn from_u32(value: u32) -> Self {
        Self(value)
    }
}

// Currently used as a constant pool when building code chunk.
pub struct TokenInterner {
    map: IndexSet<crate::rcstr::ArcStr, FxBuildHasher>,
}

impl TokenInterner {
    pub fn new() -> Self {
        Self {
            map: IndexSet::with_hasher(FxBuildHasher::default()),
        }
    }
    
    pub fn try_intern(&mut self, value: crate::rcstr::ArcStr) -> Option<TokenKey> {
        let index = self.map.insert_full(value).0;
        Some(TokenKey(index.try_into().ok()?))
    }

    pub fn intern(&mut self, value: crate::rcstr::ArcStr) -> TokenKey {
        self.try_intern(value).expect("TokenKey overflow")
    }

    pub fn try_get(&self, key: TokenKey) -> Option<&crate::rcstr::ArcStr> {
        self.map.get_index(key.0 as usize)
    }

    pub fn get(&self, key: TokenKey) -> &crate::rcstr::ArcStr {
        self.try_get(key).expect("TokenKey not found in interner")
    }
}
