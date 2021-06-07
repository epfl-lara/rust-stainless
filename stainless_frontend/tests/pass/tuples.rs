extern crate stainless;
use stainless::*;

#[pre(x > -2147483648)]
#[post(ret >= 0)]
pub fn abs(x: i32) -> i32 {
  if x >= 0 {
    x
  } else {
    -x
  }
}

#[pre(x > -2147483648)]
pub fn abs_pair(x: i32) -> (i32, i32) {
  (x, abs(x))
}

pub fn swap(t: (i32, i32)) -> (i32, i32) {
  (t.1, t.0)
}

pub fn main() {
  #[allow(unused_variables)]
  let unit_tuple = ();

  let unary_tuple = (123,);
  assert!(unary_tuple.0 == 123);

  let huge_tuple = (
    0, "hello", 2, "world", -4, "!", 6, "how", 8, "are", 10, "you", 12, "?",
  );

  assert!(
    huge_tuple.0
      + huge_tuple.2
      + huge_tuple.4
      + huge_tuple.6
      + huge_tuple.8
      + huge_tuple.10
      + huge_tuple.12
      == 34
  )
}
