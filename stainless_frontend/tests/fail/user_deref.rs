extern crate stainless;

use std::ops::Deref;

struct DerefExample {
  value: i32,
}

impl Deref for DerefExample {
  type Target = i32;

  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

pub fn main() {
  let x = DerefExample { value: 1234 };

  assert!(1234 == *x);
}
