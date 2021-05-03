use super::*;

pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

impl List<u32> {
  #[measure(self)]
  pub fn contains(&self, t: &u32) -> bool {
    match self {
      List::Nil => false,
      List::Cons(head, tail) => *head == *t || tail.contains(t),
    }
  }

  #[post(!ret.contains(t))]
  pub fn remove(self, t: &u32) -> Self {
    match self {
      List::Nil => List::Nil,
      List::Cons(head, tail) if head == *t => tail.remove(t),
      List::Cons(head, tail) => List::Cons(head, Box::new(tail.remove(t))),
    }
  }

  #[post(ret.contains(&t))]
  pub fn insert(self, t: u32) -> Self {
    match self {
      List::Nil => List::Cons(t, Box::new(List::Nil)),
      _ => List::Cons(t, Box::new(self)),
    }
  }
}
