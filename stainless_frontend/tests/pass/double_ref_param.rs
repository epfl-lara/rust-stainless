extern crate stainless;

pub enum IntOption {
  None,
  Some(i32),
}

fn does_not_consume(option: &&IntOption, v: i32) -> bool {
  match option {
    IntOption::None => false,
    IntOption::Some(x) => *x == v,
  }
}

pub fn main() {
  let o1 = IntOption::Some(1);

  let primary_ref = &o1;

  assert!(does_not_consume(&primary_ref, 1));
  assert!(!does_not_consume(&primary_ref, 2));
  assert!(!does_not_consume(&primary_ref, 3));
  assert!(!does_not_consume(&primary_ref, 4));
}
