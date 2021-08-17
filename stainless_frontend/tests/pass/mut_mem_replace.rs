extern crate stainless;
use stainless::*;

pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

impl List<u32> {
  #[measure(self)]
  pub fn contents(&self) -> Set<&u32> {
    match self {
      List::Nil => Set::new(),
      List::Cons(head, tail) => tail.contents().insert(head),
    }
  }

  #[post(
    !self.contents().contains(&t)
    && self.contents().is_subset(&old(&self).contents())
    )]
  pub fn remove(&mut self, t: &u32) {
    let list = std::mem::replace(self, List::Nil);
    let result = match list {
      List::Nil => List::Nil,
      List::Cons(head, mut tail) => {
        tail.remove(t);
        if head == *t {
          *tail
        } else {
          List::Cons(head, tail)
        }
      }
    };
    *self = result;
  }
}

pub fn main() {
  // List is 5, 2, 5.
  let mut list = List::Cons(
    5,
    Box::new(List::Cons(2, Box::new(List::Cons(5, Box::new(List::Nil))))),
  );

  list.remove(&5);
  assert!(matches!(
    &list,
    List::Cons(2, tail) if matches!(**tail, List::Nil)
  ));

  list.remove(&1);
  assert!(matches!(
    &list,
    List::Cons(2, tail) if matches!(**tail, List::Nil)
  ));

  list.remove(&2);
  assert!(matches!(list, List::Nil));
}
