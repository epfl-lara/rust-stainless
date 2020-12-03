extern crate stainless;
use stainless::*;

pub struct NiceStruct;

impl NiceStruct {
  pub fn is_cool(&self) -> bool {
    #[pre(x > 0 && x < 100 y > 0 && y < 100)]
    #[post(ret > 0)]
    fn inner_fn(x: i32, y: i32) -> i32 {
      x * y
    }

    inner_fn(2, 3) == 123
  }
}

pub fn main() {
  NiceStruct.is_cool();
}
