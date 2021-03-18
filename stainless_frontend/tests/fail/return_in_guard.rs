extern crate stainless;

#[allow(unused_variables)]
fn foo(x: Option<i32>) -> i32 {
  match x {
    Option::Some(i) if return 0 => i,
    Option::Some(_) => 42,
    Option::None => -42,
  }
}

pub fn main() {
  assert!(foo(Option::Some(123)) == 0);
  assert!(foo(Option::None) == -42)
}
