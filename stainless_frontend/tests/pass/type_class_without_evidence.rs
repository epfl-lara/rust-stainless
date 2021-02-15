extern crate stainless;

trait Default {
  fn default() -> Self;
}

pub enum Option<T> {
  Some(T),
  None,
}

// The crucial bit here, is that the type parameter doesn't have any bounds.
// E.g. the type class instance can be created without evidence arguments.
impl<T> Default for Option<T> {
  fn default() -> Option<T> {
    Option::None
  }
}

pub fn main() {
  let a: Option<i32> = Default::default();

  assert!(match a {
    Option::None => true,
    _ => false,
  })
}
