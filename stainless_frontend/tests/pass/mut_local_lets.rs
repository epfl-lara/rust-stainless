#![allow(unused)]

extern crate stainless;

pub fn f<T>(option: Option<T>) {
  // Again, local mutation
  let mut option2 = None;
  let mut option = None;
  option2 = option;
  option = Some(123);
}

pub struct Thing<T> {
  field: T,
}

pub fn change_thing<T>(thing: Thing<T>, t: T) -> Thing<T> {
  let mut new_field = thing.field;
  new_field = t;
  Thing { field: new_field }
}

pub fn main() {
  // Local mutation is ok.
  let mut x = 1;
  assert!(x == 1);
  x = 3;
  assert!(x == 3);
}
