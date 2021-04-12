extern crate stainless;
use stainless::*;

#[post((arg > 0).implies(ret) && (arg == -100).implies(!ret))]
pub fn is_pos(arg: i32) -> bool {
  arg > 0
}
