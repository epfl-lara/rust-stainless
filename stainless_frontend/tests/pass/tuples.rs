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
