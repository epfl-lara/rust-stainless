extern crate stainless;

pub fn main() {
  let one = 1;
  let one_box = Box::new(one);
  let two = *one_box + 1;

  assert!(two == 2)
}
