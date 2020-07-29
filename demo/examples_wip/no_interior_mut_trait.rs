#![feature(negative_impls)]
#![feature(optin_builtin_traits)]
#![allow(unused_imports)]
extern crate stainless;

use stainless::NoInteriorMut;
use std::cell::{Cell, RefCell};

fn check_no_interior_mut<T: NoInteriorMut>(_: T) {}

fn main() {
  // NoInteriorMut (i.e., these type-check)
  check_no_interior_mut(123);
  check_no_interior_mut([0; 32]);
  check_no_interior_mut(vec![0]);
  check_no_interior_mut(Box::new(0));

  // !NoInteriorMut (i.e., these result in compile-time errors)
  // check_no_interior_mut(Cell::new(0));
  // check_no_interior_mut(RefCell::new(0));
  // check_no_interior_mut(Box::new(Cell::new(0)));
  // check_no_interior_mut(vec![Cell::new(0)]);
}
