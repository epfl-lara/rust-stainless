#![allow(unused)]

extern crate stainless;

pub enum IntOption {
  None,
  Some(i32),
}

struct S {
  field: i32,
}

pub fn main() {
  // Local mutation is ok.
  let mut x = 1;
  assert!(x == 1);
  x = 3;
  assert!(x == 3);

  // Again, local mutation
  let option = IntOption::Some(2);
  let mut option2 = IntOption::None;
  let mut option = IntOption::None;
  option2 = option;
  option = IntOption::Some(123);

  // field assignment
  let mut s = S { field: 123 };
  assert!(s.field == 123);
  s.field = 456;
  assert!(s.field == 456);
}
