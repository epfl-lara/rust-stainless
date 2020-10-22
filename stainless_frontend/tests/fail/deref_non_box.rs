pub fn main() {
  let one = 1;
  let one_box = Box::new(one);
  let two = *(one_box.as_ref()) + 1;

  assert!(two == 2)
}
