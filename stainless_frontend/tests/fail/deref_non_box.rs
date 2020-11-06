extern crate stainless;

pub fn main() {
  let one = 1;
  let one_box = Box::new(one);

  // This fails because of the .as_ref() currently because there is no way to
  // dereference something other than a box without creating it first.
  let two = *(one_box.as_ref()) + 1;

  assert!(two == 2)
}
