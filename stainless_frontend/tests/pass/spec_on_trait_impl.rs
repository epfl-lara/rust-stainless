extern crate stainless;
use stainless::*;

trait SomeTrait {
  fn with_method(&self) -> bool;

  #[post(self.with_method())]
  fn with_mut_method(&self) -> i32;
}

impl SomeTrait for i32 {
  #[post(ret)]
  fn with_method(&self) -> bool {
    true
  }

  fn with_mut_method(&self) -> i32 {
    123
  }
}
