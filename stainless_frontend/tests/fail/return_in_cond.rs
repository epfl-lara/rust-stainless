extern crate stainless;

fn bar() -> i32 {
  if 12 == 12 && return 0 {
    return 1;
  } else {
    return 2;
  }
}

pub fn main() {
  assert!(bar() == 0)
}
