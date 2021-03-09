extern crate stainless;
use stainless::*;

trait SpecsNotLaws {
  #[post(ret > 0)]
  fn always_positive(&self) -> i32;

  #[pre(b)]
  fn always_call_true(b: bool) {
    let _a = 1;
  }
}

impl SpecsNotLaws for u32 {
  fn always_positive(&self) -> i32 {
    123
  }

  fn always_call_true(_b: bool) {}
}

pub fn main() {
  let number: u32 = 465;
  u32::always_call_true(true);
  assert!(number.always_positive() > 0)
}
