#![allow(unused)]

extern crate stainless;

// This should pass because place is only locally mutable.
pub fn change<'a, T>(mut place: &'a T, new: &'a T) -> &'a T {
  place = new;
  place
}

pub fn main() {
  // Here, we don't modify anything locally.
  let y = 10;
  let z = 5;
  assert!(z == 5);
  let res = change(&z, &y);
  assert!(z == 5);
  assert!(*res == y);
}
