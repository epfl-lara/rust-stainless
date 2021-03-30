extern crate stainless;
use stainless::*;

trait Equals {
  fn eq(&self, other: &Self) -> bool;
}

trait Clone {
  fn clone(&self) -> Self;
}

#[external]
#[pure]
#[post(x.clone().eq(&x))]
#[allow(unused_variables)]
fn clone_preserves_eq<T: Clone + Equals>(x: T) {}
