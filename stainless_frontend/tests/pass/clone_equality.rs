extern crate stainless;
use stainless::*;

trait Equals {
  fn eq(&self, other: &Self) -> bool;

  #[law]
  fn law_reflexive(&self) -> bool {
    self.eq(self)
  }
}

trait Clone: Sized {
  fn clone(&self) -> Self;
}

trait EqualClone: Clone + Equals {
  #[law]
  fn law_preserve_equality(&self) -> bool {
    self.clone().eq(self)
  }
}
impl<T: Clone + Equals> EqualClone for T {
  fn law_preserve_equality(&self) -> bool {
    self.law_reflexive()
  }
}

pub struct Key(String);
impl Equals for Key {
  fn eq(&self, other: &Self) -> bool {
    self.0 == other.0
  }
}
impl Clone for Key {
  fn clone(&self) -> Self {
    Key(self.0.clone())
  }
}

pub struct Door {
  key_lock: Key,
}

impl Door {
  fn try_open(&self, key: &Key) -> Result<(), &'static str> {
    if key.eq(&self.key_lock) {
      Ok(())
    } else {
      Err("can't open that door")
    }
  }
}

pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

impl<T: Equals> Equals for List<T> {
  #[measure(self)]
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (List::Nil, List::Nil) => true,
      (List::Cons(x, xs), List::Cons(y, ys)) => x.eq(y) && xs.eq(ys),
      _ => false,
    }
  }

  fn law_reflexive(&self) -> bool {
    match self {
      List::Cons(x, xs) => x.law_reflexive() && xs.law_reflexive(),
      List::Nil => true,
    }
  }
}

impl<T: Clone> Clone for List<T> {
  #[measure(self)]
  fn clone(&self) -> Self {
    match self {
      List::Cons(h, tail) => List::Cons(h.clone(), Box::new((*tail).clone())),
      _ => List::Nil,
    }
  }
}

impl<T: Equals + Clone> EqualClone for List<T> {
  fn law_preserve_equality(&self) -> bool {
    self.law_reflexive()
      && match self {
        List::Nil => true,
        List::Cons(head, tail) => head.law_preserve_equality() && tail.law_preserve_equality(),
      }
  }
}

pub struct Person {
  key: Key,
}

// If the person has the key of the door, it implies that they can open it.
#[post(!(Equals::eq(&person.key, &door.key_lock)) || matches!(ret, Ok(output)))]
pub fn main(door: Door, person: Person) -> Result<(), &'static str> {
  door.try_open(&person.key.clone())
}
