use std::collections::HashMap;
use std::collections::hashmap::{Entries, Keys};
use std::fmt::Show;
use std::hash::Hash;
use std::mem;

#[deriving(Clone)]
pub struct SynonymMap<K, V> {
    vals: HashMap<K, V>,
    syns: HashMap<K, K>,
}

impl<K: Eq + Hash, V> SynonymMap<K, V> {
    pub fn new() -> SynonymMap<K, V> {
        SynonymMap {
            vals: HashMap::new(),
            syns: HashMap::new(),
        }
    }

    pub fn insert_synonym(&mut self, from: K, to: K) -> bool {
        assert!(self.vals.contains_key(&to));
        self.syns.insert(from, to)
    }

    pub fn keys<'a>(&'a self) -> Keys<'a, K, V> {
        self.vals.keys()
    }

    pub fn iter<'a>(&'a self) -> Entries<'a, K, V> {
        self.vals.iter()
    }

    pub fn synonyms<'a>(&'a self) -> Entries<'a, K, K> {
        self.syns.iter()
    }

    fn with_key<T>(&self, k: &K, with: |&K| -> T) -> T {
        if self.syns.contains_key(k) {
            with(self.syns.get(k))
        } else {
            with(k)
        }
    }
}

impl<K: Eq + Hash + Clone, V> SynonymMap<K, V> {
    pub fn resolve(&self, k: &K) -> K {
        self.with_key(k, |k| k.clone())
    }
    pub fn get<'a>(&'a self, k: &K) -> &'a V {
        self.find(k).unwrap()
    }
    pub fn get_mut<'a>(&'a mut self, k: &K) -> &'a mut V {
        self.find_mut(k).unwrap()
    }
}

impl<K: Eq + Hash, V> Collection for SynonymMap<K, V> {
    fn len(&self) -> uint { self.vals.len() }
}

impl<K: Eq + Hash, V> Mutable for SynonymMap<K, V> {
    fn clear(&mut self) { self.vals.clear(); self.syns.clear(); }
}

impl<K: Eq + Hash, V> Map<K, V> for SynonymMap<K, V> {
    fn find<'a>(&'a self, k: &K) -> Option<&'a V> {
        self.with_key(k, |k| self.vals.find(k))
    }
    fn contains_key(&self, k: &K) -> bool {
        self.with_key(k, |k| self.vals.contains_key(k))
    }
}

impl<K: Eq + Hash + Clone, V> MutableMap<K, V> for SynonymMap<K, V> {
    fn find_mut<'a>(&'a mut self, k: &K) -> Option<&'a mut V> {
        if self.syns.contains_key(k) {
            self.vals.find_mut(self.syns.get(k))
        } else {
            self.vals.find_mut(k)
        }
    }
    fn swap(&mut self, k: K, mut new: V) -> Option<V> {
        if self.syns.contains_key(&k) {
            let old = self.vals.find_mut(&k).unwrap();
            mem::swap(old, &mut new);
            Some(new)
        } else {
            self.vals.swap(k, new)
        }
    }
    fn pop(&mut self, k: &K) -> Option<V> {
        if self.syns.contains_key(k) {
            let old = self.vals.pop(self.syns.get(k));
            self.syns.remove(k);
            old
        } else {
            self.vals.pop(k)
        }
    }
}

impl<K: Eq + Hash + Clone, V> FromIterator<(K, V)> for SynonymMap<K, V> {
    fn from_iter<T: Iterator<(K, V)>>(mut iter: T) -> SynonymMap<K, V> {
        let mut map = SynonymMap::new();
        for (k, v) in iter {
            map.insert(k, v);
        }
        map
    }
}

impl<K: Eq + Hash + Show, V: Show> Show for SynonymMap<K, V> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        try!(self.vals.fmt(f));
        write!(f, " (synomyns: {})", self.syns.to_string())
    }
}
