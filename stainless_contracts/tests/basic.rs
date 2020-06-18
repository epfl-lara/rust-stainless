#[macro_use]
extern crate stainless_contracts;

#[require(x >= 0)]
#[ensuring(|res| res > x)]
fn inc(x: i32) -> i32 {
  x + 1
}

#[test]
fn require_pos_test() {
  assert!(__precondition_inc(1));
}

#[test]
fn require_neg_test() {
  assert!(!__precondition_inc(-42));
}

#[test]
fn ensuring_test() {
  assert!(__postcondition_inc(inc(1), 1));
}

#[test]
#[should_panic]
fn ensuring_with_overflow() {
  // Yet postcondition is wrong...
  // but this test is bs because rust does its own check of int overflow
  assert!(!__postcondition_inc(
    inc(i32::max_value()),
    i32::max_value()
  ));
}
