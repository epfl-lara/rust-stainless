#![allow(unused_assignments)]

extern crate stainless;

pub fn main() {
  let mut a = 123;
  let mut b = 456;
  let mut r = &mut a;
  r = &mut b;
  assert!(*r == 456);
}
