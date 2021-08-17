extern crate stainless;
use stainless::*;

#[allow(dead_code)]
pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

impl List<u8> {
  pub fn contains(&self, t: &u8) -> bool {
    self.contents().contains(&t)
  }

  #[measure(self)]
  pub fn contents(&self) -> Set<&u8> {
    match self {
      List::Nil => Set::new(),
      List::Cons(head, tail) => tail.contents().insert(head),
    }
  }

  #[post(
    !ret.contains(&t)
    && ret.contents().is_subset(&self.contents())
  )]
  pub fn remove(self, t: &u8) -> Self {
    match self {
      List::Nil => List::Nil,
      List::Cons(head, tail) if head == *t => tail.remove(t),
      List::Cons(head, tail) => List::Cons(head, Box::new(tail.remove(t))),
    }
  }
}

pub fn remove_from_list(mut list: List<u8>) {
  if let List::Cons(first_elem, _) = list {
    list = list.remove(&first_elem);
    assert!(!list.contains(&first_elem));
  }
}
