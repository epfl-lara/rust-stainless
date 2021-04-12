extern crate stainless;
use stainless::*;

#[pre(a >= 0)]
fn is_it_123(a: i32) -> bool {
  match a {
    123 => true,
    _ if a < 0 => panic!("This is not allowed"),
    _ => false,
  }
}

pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

impl<T> List<T> {
  #[pre(matches!(self, List::Cons(..)))]
  fn first(&self) -> &T {
    match self {
      List::Cons(v, _) => &v,
      _ => panic!("Empty list"),
    }
  }
}

pub fn main() {
  assert!(is_it_123(123));
  assert!(!is_it_123(1));

  assert!(*List::Cons(123, Box::new(List::Nil)).first() == 123)
}
