pub use num_bigint::BigInt;
pub use stainless_macros::*;

use std::marker::PhantomData;

#[derive(Copy, Clone, PartialEq)]
pub struct Set<T> {
  phantom: PhantomData<T>,
}

impl<T> Set<T> {
  pub fn empty() -> Self {
    unimplemented!()
  }
  pub fn singleton(_t: &T) -> Self {
    unimplemented!()
  }

  pub fn add(&self, _t: &T) -> Self {
    unimplemented!()
  }

  pub fn union(&self, _other: &Set<T>) -> Self {
    unimplemented!()
  }
  pub fn intersection(&self, _other: &Set<T>) -> Self {
    unimplemented!()
  }
  pub fn difference(&self, _other: &Set<T>) -> Self {
    unimplemented!()
  }
  pub fn is_subset_of(&self, _other: &Set<T>) -> bool {
    unimplemented!()
  }
}

#[derive(Copy, Clone)]
pub struct Map<K, V> {
  keys: PhantomData<K>,
  vals: PhantomData<V>,
}

impl<K, V> Map<K, V> {
  pub fn empty() -> Self {
    unimplemented!()
  }

  /// Panics if the key is not in the map.
  pub fn apply(&self, _key: &K) -> &V {
    unimplemented!()
  }
  pub fn get_or_else(&self, _key: &K, _else: &V) -> &V {
    unimplemented!()
  }
  pub fn get(&self, _key: &K) -> Option<&V> {
    unimplemented!()
  }
  pub fn contains(&self, _t: &K) -> bool {
    unimplemented!()
  }

  pub fn updated(&self, _key: &K, _val: &V) -> Self {
    unimplemented!()
  }

  pub fn removed(&self, _key: &K) -> Self {
    unimplemented!()
  }
}

pub trait Implies {
  fn implies(self, b: Self) -> bool;
}

impl Implies for bool {
  #[inline(always)]
  fn implies(self, b: bool) -> bool {
    !self || b
  }
}
