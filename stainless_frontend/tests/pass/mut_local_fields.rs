#![allow(dead_code, unused_assignments)]

extern crate stainless;
use stainless::*;

#[var(field)]
struct S {
  field: i32,
}

#[pure]
fn set_field(mut s: S) -> S {
  s.field = 456;
  s
}

struct Outer(S);

#[pure]
fn set_inner_field(mut o: Outer) -> Outer {
  o.0.field = 101112;
  o
}

// As control, the same for a primitive type
fn set_int(mut s: i32) -> i32 {
  s = 1000;
  s
}

pub fn main() {
  // field assignment
  let mut s = S { field: 123 };
  assert!(s.field == 123);
  s = set_field(s);
  assert!(s.field == 456);

  s.field = -111;
  assert!(s.field == -111);

  let o = Outer(s);
  let o = set_inner_field(o);
  assert!(o.0.field == 101112);

  let i = set_int(12);
  assert!(i == 1000);
}
