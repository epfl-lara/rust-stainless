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

  // TODO: Only take 'self' as a reference and also take the other parameters
  //   only by reference.
  pub fn singleton(_t: &T) -> Self {
    unimplemented!()
  }
  pub fn add(&self, _t: &T) -> Set<T> {
    unimplemented!()
  }
  pub fn contains(&self, _t: &T) -> bool {
    unimplemented!()
  }

  pub fn union(&self, _other: &Set<T>) -> Set<T> {
    unimplemented!()
  }
  pub fn intersection(&self, _other: &Set<T>) -> Set<T> {
    unimplemented!()
  }
  pub fn difference(&self, _other: &Set<T>) -> Set<T> {
    unimplemented!()
  }
  pub fn is_subset_of(&self, _other: &Set<T>) -> bool {
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
