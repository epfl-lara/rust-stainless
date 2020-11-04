extern crate stainless;

pub enum IntOption {
  None,
  Some(i32),
}

fn does_not_consume(option: &IntOption, v: i32) -> bool {
  match option {
    IntOption::None => false,
    IntOption::Some(x) => *x == v,
  }
}

pub fn main() {
  let o1 = IntOption::Some(1);

  // Can use it multiple times, because the function does not consume the
  // option.
  assert!(does_not_consume(&o1, 1));
  assert!(!does_not_consume(&o1, 2));
  assert!(!does_not_consume(&o1, 3));
  assert!(!does_not_consume(&o1, 4));
}
