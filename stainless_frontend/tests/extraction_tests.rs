#![feature(rustc_private)]
extern crate stainless_frontend;

pub mod utilities;
use utilities::*;
use Outcome::*;

macro_rules! emit_check {
  ($verify:expr, $outcome:expr, $test_path:ident) => {
    assert_eq!(run_extraction_test($test_path, $verify), $outcome);
  };
}

macro_rules! select_check {
  (pass, extraction, $test_path:ident) => {
    emit_check!(false, Success { verified: false }, $test_path)
  };
  (pass, verification, $test_path:ident) => {
    emit_check!(true, Success { verified: true }, $test_path)
  };
  (fail, extraction, $test_path:ident) => {
    emit_check!(false, ErrorInExtraction, $test_path)
  };
  (fail, verification, $test_path:ident) => {
    emit_check!(true, ErrorInVerification, $test_path)
  };
}

macro_rules! define_test {
  ($pass_or_fail:ident, $stage:ident, $test_name:ident, $name:ident) => {
    #[test]
    fn $test_name() {
      let relative_path = format!(
        "tests/{}/{}.rs",
        stringify!($pass_or_fail),
        stringify!($name)
      );
      let test_path = manifest_relative_path(relative_path);
      select_check!($pass_or_fail, $stage, test_path);
    }
  };
}

macro_rules! select_test {
  (pass, $name:ident) => {
    mod $name {
      use super::*;
      define_test!(pass, extraction, extraction, $name);
      define_test!(pass, verification, verification, $name);
    }
  };
  (fail_extraction, $name:ident) => {
    define_test!(fail, extraction, $name, $name);
  };
  (fail_verification, $name:ident) => {
    define_test!(fail, verification, $name, $name);
  };
}

macro_rules! define_tests {
  ($($kind:ident : $name:ident),*) => {
    $(
      select_test!($kind, $name);
    )*
  }
}

// Defines all the individual test cases to be run.
// Test cases are found in the `tests/pass` and `tests/fail` subdirectories.
// E.g. `pass: adts` refers to the test case in `tests/pass/adts.rs`.
// Tests can be defined in one of three modes:
//  - pass: ensures that both extraction and verification succeed
//  - fail_extraction: ensures that extraction rejects the program
//  - fail_verification: ensures that verification rejects the program
define_tests!(
  pass: adts,
  pass: blocks,
  pass: boxes,
  pass: double_ref_param,
  pass: external_fn,
  pass: fact,
  pass: fn_ref_param,
  pass: generic_id,
  pass: generic_option,
  pass: impl_fns,
  pass: insertion_sort,
  pass: int_operators,
  pass: int_option,
  pass: let_type,
  pass: list_binary_search,
  pass: monoid,
  pass: nested_spec,
  pass: nested_spec_impl,
  pass: tuple_match,
  pass: tuples,
  pass: type_class,
  fail_verification: box_as_ref,
  fail_extraction: double_measure,
  fail_extraction: double_measure_impl,
  fail_verification: liskov_rectangle,
  fail_extraction: mut_lets,
  fail_extraction: switch_ref,
  fail_extraction: switch_int,
  fail_extraction: user_deref
);
