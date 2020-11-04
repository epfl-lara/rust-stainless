extern crate stainless;

pub enum IntOption {
  None,
  Some(i32),
}

/// This spec should always fail because some things are immutable.
/// rust-stainless builds upon the fact that there is no immutability for the
/// moment.
pub fn main() {
  // This should fail because of the mut binding.
  let mut x = 1;
  println!("x = {}", x);
  x = 3;

  let option = IntOption::Some(2);
  let mut option2 = IntOption::None;
  let mut option = IntOption::None;

  option2 = option;
  option = IntOption::Some(123)
}
