extern crate stainless;

pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

pub fn main() {
  let a: u32 = 1234;

  let boxx: Box<List<i64>> = Box::new(List::Cons(-123, Box::new(List::Nil)));

  let c: i64 = match *boxx {
    List::Cons(a, _) => a,
    List::Nil => 0,
  };

  assert!(a == 1234 && c == -123)
}
