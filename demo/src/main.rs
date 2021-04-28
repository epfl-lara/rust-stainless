//! This demo project showcases the use of different modules. For more examples,
//! see `../examples` or `../../stainless_frontend/tests/pass`.

extern crate stainless;
use stainless::*;

mod list;
use list::*;

pub fn main() {
  let list = List::Cons(123, Box::new(List::Nil));
  let list = list.insert(456);
  assert!(list.contains(&456));
  let list = list.remove(&123);
  assert!(!list.contains(&123));
}
