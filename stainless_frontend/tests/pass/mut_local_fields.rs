extern crate stainless;
use stainless::*;

#[var(field)]
struct S {
  field: i32,
}

pub fn main() {
  // field assignment
  let mut s = S { field: 123 };
  assert!(s.field == 123);
  s.field = 456;
  assert!(s.field == 456);
}
