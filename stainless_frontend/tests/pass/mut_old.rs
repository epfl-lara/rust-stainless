extern crate stainless;
use stainless::*;

pub struct S(i32);

#[post(*i == *old(i) + 1)]
fn change_int(i: &mut i32) {
  *i = *i + 1;
}

#[post(s.0 == old(s).0 + 1)]
fn change_s(s: &mut S) {
  s.0 = s.0 + 1;
}

pub fn main() {
  let mut a = 123;
  change_int(&mut a);
  assert!(a == 124);
}
