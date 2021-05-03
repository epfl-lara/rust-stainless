#![allow(dead_code, unused_assignments)]

extern crate stainless;
use stainless::*;

#[var(field)]
struct S {
  field: i32,
}

fn set_field(s: S) -> S {
  // current work-around for anti-aliasing
  let mut s = S { ..s };
  s.field = 789;
  s
}

struct Outer(S);

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
  s.field = 456;
  assert!(s.field == 456);

  let s = set_field(s);
  assert!(s.field == 789);

  let o = Outer(s);
  let o = set_inner_field(o);
  assert!(o.0.field == 101112);

  let i = set_int(12);
  assert!(i == 1000);
}
