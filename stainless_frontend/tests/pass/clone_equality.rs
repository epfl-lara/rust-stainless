extern crate stainless;
use stainless::*;

trait EqualClone: Sized {
  fn clone(&self) -> Self;

  fn eq(&self, other: &Self) -> bool;

  #[law]
  fn preserve_equality(a: &Self, b: &Self) -> bool {
    (a.eq(b) == a.clone().eq(b)) == (a.eq(&b.clone()) == a.clone().eq(&b.clone()))
  }
}

pub struct Key(String);
impl EqualClone for Key {
  fn clone(&self) -> Self {
    Key(self.0.clone())
  }

  fn eq(&self, other: &Self) -> bool {
    self.0 == other.0
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

pub struct Person {
  key: Key,
}

// If the person has the key of the door, it implies that they can open it.
#[post(!(EqualClone::eq(&person.key, &door.key_lock)) || matches!(ret, Ok(output)))]
pub fn main(door: Door, person: Person) -> Result<(), &'static str> {
  door.try_open(&person.key.clone())
}
