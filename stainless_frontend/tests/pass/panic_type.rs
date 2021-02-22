extern crate stainless;
use stainless::*;

#[pre(a >= 0)]
fn is_it_123(a: i32) -> bool {
  match a {
    123 => true,
    _ if a < 0 => panic!("This is not allowed"),
    _ => false,
  }
}

pub fn main() {
  assert!(is_it_123(123));
  assert!(!is_it_123(1));
}
