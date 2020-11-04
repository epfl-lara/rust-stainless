extern crate stainless;

pub fn main() {
  let one = 1;
  let one_box = Box::new(one);

  // This fails in verification because the .as_ref() method is not known by
  // stainless.
  let two = *(one_box.as_ref()) + 1;

  assert!(two == 2)
}
