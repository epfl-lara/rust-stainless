#![allow(unused_imports, unused_variables, dead_code)]

extern crate stainless;

mod use_std;
use use_std::*;

mod fact;
use fact::fact;

mod int_option;
use int_option::IntOption;

pub fn main() {
  let opt = IntOption::Some(5);

  foo();
  assert!(fact(3) == 6);
}
