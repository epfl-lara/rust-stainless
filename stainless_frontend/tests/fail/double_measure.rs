extern crate stainless;
use stainless::*;

pub enum IntOption {
  None,
  Some { value: i32 },
}

#[measure(a)]
#[measure(b)]
pub fn dummy_for_specs_2(a: i32, b: i32) -> i32 {
  a + dummy_for_specs_2(1, b)
}
