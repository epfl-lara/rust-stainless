#![allow(dead_code, unused_assignments)]

extern crate stainless;
use stainless::*;

#[var(field)]
struct S {
  field: i32,
}

fn set_field(mut s: S) -> S {
  s.field = 789;
  s
}

fn set_int(mut s: i32) -> i32 {
  s = 1000;
  s
}

pub fn main() {
  // field assignment
  let mut s = S { field: 123 };
  assert!(s.field == 123);
  s.field = 456;
  assert!(s.field == 456);

  let s = set_field(s);
  assert!(s.field == 789);

  let i = set_int(12);
  assert!(i == 1000);
}
