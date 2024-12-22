use std::{
    hash::{BuildHasher, Hash},
    mem::ManuallyDrop,
};

use hashbrown::{hash_map::DefaultHashBuilder, HashMap};

/// A hash map with two keys. It is a **logic error** for `K1` and `K2` not to be bijective
/// (i.e. one `K1` must correspond to exactly one `K2` and vice versa).
#[derive(Debug, Clone)]
pub struct DHashMap<K1, K2, V, S = DefaultHashBuilder> {
    values: Vec<ManuallyDrop<V>>,
    vacants: Vec<usize>,
    keys1_map: HashMap<K1, usize, S>,
    keys2_map: HashMap<K2, usize, S>,
}

impl<K1, K2, V, S> Drop for DHashMap<K1, K2, V, S> {
    fn drop(&mut self) {
        for (index, mut value) in self.values.drain(..).enumerate() {
            if self.vacants.contains(&index) {
                continue;
            }
            unsafe {
                ManuallyDrop::drop(&mut value);
            }
        }
    }
}

impl<K1, K2, V, S: Default> Default for DHashMap<K1, K2, V, S> {
    fn default() -> Self {
        Self {
            values: Vec::new(),
            vacants: Vec::new(),
            keys1_map: HashMap::default(),
            keys2_map: HashMap::default(),
        }
    }
}

impl<K1, K2, V> DHashMap<K1, K2, V, DefaultHashBuilder> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_hasher(capacity, DefaultHashBuilder::default())
    }
}

impl<K1, K2, V, S: Clone> DHashMap<K1, K2, V, S> {
    pub fn with_hasher(hash_builder: S) -> Self {
        Self {
            values: Vec::new(),
            vacants: Vec::new(),
            keys1_map: HashMap::with_hasher(hash_builder.clone()),
            keys2_map: HashMap::with_hasher(hash_builder),
        }
    }

    pub fn with_capacity_and_hasher(capacity: usize, hash_builder: S) -> Self {
        Self {
            values: Vec::with_capacity(capacity),
            vacants: Vec::new(),
            keys1_map: HashMap::with_capacity_and_hasher(capacity, hash_builder.clone()),
            keys2_map: HashMap::with_capacity_and_hasher(capacity, hash_builder),
        }
    }
}

impl<K1, K2, V, S> DHashMap<K1, K2, V, S>
where
    K1: Eq + Hash,
    K2: Eq + Hash,
    S: BuildHasher,
{
    pub fn insert(&mut self, key1: K1, key2: K2, value: V) -> Option<V> {
        let (v1, v2) = (
            self.keys1_map.get(&key1).copied(),
            self.keys2_map.get(&key2).copied(),
        );
        assert_eq!(v1, v2, "DHashMap: key1 and key2 are not bijective");

        if let Some(index) = v1 {
            let old_value = std::mem::replace(&mut self.values[index], ManuallyDrop::new(value));
            self.keys1_map.insert(key1, index);
            self.keys2_map.insert(key2, index);
            Some(ManuallyDrop::into_inner(old_value))
        } else {
            let index = if let Some(index) = self.vacants.pop() {
                self.values[index] = ManuallyDrop::new(value);
                index
            } else {
                let index = self.values.len();
                self.values.push(ManuallyDrop::new(value));
                index
            };
            self.keys1_map.insert(key1, index);
            self.keys2_map.insert(key2, index);
            None
        }
    }

    pub fn get_by_key1<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K1: std::borrow::Borrow<Q>,
        Q: Hash + Eq,
    {
        self.keys1_map
            .get(key)
            .map(|&index| &self.values[index])
            .map(|v| &**v)
    }

    pub fn get_by_key2<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K2: std::borrow::Borrow<Q>,
        Q: Hash + Eq,
    {
        self.keys2_map
            .get(key)
            .map(|&index| &self.values[index])
            .map(|v| &**v)
    }

    pub fn remove_by_key1<Q1: ?Sized, Q2: ?Sized>(
        &mut self,
        key: &Q1,
        get_key2: impl FnOnce(&V) -> &Q2,
    ) -> Option<V>
    where
        K1: std::borrow::Borrow<Q1>,
        K2: std::borrow::Borrow<Q2>,
        Q1: Hash + Eq,
        Q2: Hash + Eq,
    {
        self.keys1_map.remove(key).map(|index1| {
            let key2 = get_key2(&self.values[index1]);
            let index2 = self.keys2_map.remove(key2);
            assert_eq!(
                Some(index1),
                index2,
                "DHashMap: key1 and key2 are not bijective"
            );
            // Mark the index as vacant
            self.vacants.push(index1);
            unsafe { ManuallyDrop::take(&mut self.values[index1]) }
        })
    }

    pub fn remove_by_key2<Q1: ?Sized, Q2: ?Sized>(
        &mut self,
        key: &Q2,
        get_key1: impl FnOnce(&V) -> &Q1,
    ) -> Option<V>
    where
        K1: std::borrow::Borrow<Q1>,
        K2: std::borrow::Borrow<Q2>,
        Q1: Hash + Eq,
        Q2: Hash + Eq,
    {
        self.keys2_map.remove(key).map(|index2| {
            let key1 = get_key1(&self.values[index2]);
            let index1 = self.keys1_map.remove(key1);
            assert_eq!(
                Some(index2),
                index1,
                "DHashMap: key1 and key2 are not bijective"
            );
            // Mark the index as vacant
            self.vacants.push(index2);
            unsafe { ManuallyDrop::take(&mut self.values[index2]) }
        })
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
}

impl<K1, K2, V, S> DHashMap<K1, K2, V, S> {
    pub fn k1v_iter(&self) -> impl Iterator<Item = (&K1, &V)> {
        self.keys1_map
            .iter()
            .map(|(k1, &index)| (k1, &**&self.values[index]))
    }

    pub fn k2v_iter(&self) -> impl Iterator<Item = (&K2, &V)> {
        self.keys2_map
            .iter()
            .map(|(k2, &index)| (k2, &**&self.values[index]))
    }

    // pub fn k1v_iter_mut(&mut self) -> impl Iterator<Item = (&K1, &mut V)> {
    //     self.keys1_map.iter().map(|(k1, &index)| (k1, &mut self.values[index]))
    // }

    // pub fn k2v_iter_mut(&mut self) -> impl Iterator<Item = (&K2, &mut V)> {
    //     self.keys2_map.iter().map(|(k2, &index)| (k2, &mut self.values[index]))
    // }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.values
            .iter()
            .enumerate()
            .filter(|(i, _)| !self.vacants.contains(i))
            .map(|(_, v)| &**v)
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.values
            .iter_mut()
            .enumerate()
            .filter(|(i, _)| !self.vacants.contains(i))
            .map(|(_, v)| &mut **v)
    }
}
