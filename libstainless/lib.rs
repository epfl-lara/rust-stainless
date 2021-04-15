pub use num_bigint::BigInt;
pub use stainless_macros::*;

use std::hash::Hash;

use im::{HashMap, HashSet};

pub struct Set<T> {
  set: HashSet<T>,
}

impl<T> Set<T>
where
  T: Eq + Hash + Clone,
{
  pub fn empty() -> Self {
    Self {
      set: HashSet::new(),
    }
  }
  pub fn singleton(t: T) -> Self {
    Self {
      set: HashSet::unit(t),
    }
  }

  pub fn add(&self, t: T) -> Self {
    Self {
      set: self.set.update(t),
    }
  }

  pub fn contains(&self, t: &T) -> bool {
    self.set.contains(t)
  }

  pub fn union(self, other: Set<T>) -> Self {
    Self {
      set: self.set.union(other.set),
    }
  }

  pub fn intersection(self, other: Set<T>) -> Self {
    Self {
      set: self.set.intersection(other.set),
    }
  }

  pub fn difference(self, other: Set<T>) -> Self {
    Self {
      set: self.set.difference(other.set),
    }
  }

  pub fn is_subset_of(&self, other: &Set<T>) -> bool {
    self.set.is_subset(&other.set)
  }
}

pub struct Map<K, V> {
  map: HashMap<K, V>,
}

impl<K, V> Map<K, V>
where
  K: Eq + Hash + Clone,
  V: Clone,
{
  pub fn empty() -> Self {
    Self {
      map: HashMap::new(),
    }
  }

  pub fn get(&self, key: &K) -> Option<&V> {
    self.map.get(key)
  }

  pub fn get_or_else<'a>(&'a self, key: &K, elze: &'a V) -> &'a V {
    self.get(key).unwrap_or(elze)
  }

  /// Panics if the key is not in the map.
  pub fn apply(&self, key: &K) -> &V {
    self.get(key).unwrap()
  }

  pub fn contains(&self, key: &K) -> bool {
    self.map.contains_key(key)
  }

  pub fn updated(&self, key: K, val: V) -> Self {
    Self {
      map: self.map.update(key, val),
    }
  }

  pub fn removed(&self, key: &K) -> Self {
    Self {
      map: self.map.without(key),
    }
  }
}

pub trait Implies {
  fn implies(self, b: Self) -> bool;
}

impl Implies for bool {
  /// Simple helper to provide the implies expression on booleans.
  #[inline(always)]
  fn implies(self, b: bool) -> bool {
    !self || b
  }
}
