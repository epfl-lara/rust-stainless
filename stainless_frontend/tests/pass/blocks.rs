extern crate stainless;
use stainless::*;

fn foo(_n: i32) -> () {}

pub fn bar(x: i32) -> i32 {
  foo(x);
  let y = x / 2;
  foo(y);

  if y <= 0 {
    1
  } else if x > 10 {
    bar(x - 1) * x
  } else {
    y
  }
}

#[pre(y != 123)]
pub fn sole_if(y: i32) {
  if y == 123 {
    panic!()
  }
}
