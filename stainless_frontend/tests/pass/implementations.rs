extern crate stainless;

pub enum IntOption {
  None,
  Some { value: i32 },
}

impl IntOption {
  pub fn is_none(&self) -> bool {
    match self {
      IntOption::None => true,
      _ => false,
    }
  }

  pub fn is_some(&self) -> bool {
    !self.is_none()
  }
}

pub fn main() {
  let some = IntOption::Some { value: 1234 };
  let none = IntOption::None;

  assert!(some.is_some());
  assert!(!some.is_none());
  assert!(none.is_none());
}
