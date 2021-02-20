extern crate stainless;
use stainless::*;

trait SomeTrait {
  fn with_method(&self) -> bool;
}

impl SomeTrait for i32 {
  #[post(ret == false)]
  fn with_method(&self) -> bool {
    false
  }
}
