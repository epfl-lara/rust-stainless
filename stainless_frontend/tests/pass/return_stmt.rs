extern crate stainless;
use stainless::*;

#[pre(x >= 0 && x < 10)]
#[post(ret >= 0)]
pub fn fact(x: i32) -> i32 {
  if x <= 0 {
    return 1;
  }

  fact(x - 1) * x
}

pub fn return_when_wanted(arg: u32) {
  if arg < 10 {
    return;
  }
  let a = 1;
  let useless = 3;
  let computation = useless + a;

  if computation > 3 {
    return;
  }

  panic!("will never reach me")
}
