#![allow(dead_code)]

extern crate stainless;
use stainless::*;

struct A(i32);

impl A {
  pub fn is_positive(&self) -> bool {
    self.0 > 0
  }

  #[post((x > 0).implies(self.is_positive()))]
  pub fn add(&mut self, x: i32) {
    self.0 = x;
  }
}
