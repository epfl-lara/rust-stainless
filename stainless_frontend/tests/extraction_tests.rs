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
  (crash, verification, $test_path:ident) => {
    emit_check!(true, CrashInVerification, $test_path)
  };
}

macro_rules! test_folder {
  (crash) => {
    "fail"
  };
  ($outcome:ident) => {
    stringify!($outcome)
  };
}

macro_rules! relative_path {
  ($outcome:ident, $name:ident) => {
    concat!(
      "tests/",
      test_folder!($outcome),
      "/",
      stringify!($name),
      ".rs"
    )
  };
}

macro_rules! define_test {
  ($outcome:ident, $stage:ident, $test_name:ident, $name:ident) => {
    #[test]
    fn $test_name() {
      let test_path = manifest_relative_path(relative_path!($outcome, $name));
      select_check!($outcome, $stage, test_path);
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
  (crash_verification, $name:ident) => {
    define_test!(crash, verification, $name, $name);
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
  pass: adt_use_before_declare,
  pass: adts,
  pass: blocks,
  pass: boxes,
  pass: cast_correctness,
  pass: clone,
  pass: double_ref_param,
  pass: external_fn,
  pass: fact,
  pass: fn_ref_param,
  pass: generic_id,
  pass: generic_option,
  pass: generic_result,
  pass: impl_fns,
  pass: impl_mut_spec,
  pass: implies,
  pass: insertion_sort,
  pass: int_operators,
  pass: int_option,
  pass: let_type,
  pass: list_binary_search,
  pass: map_ops,
  pass: monoid,
  pass: mut_clone,
  pass: mut_local_fields,
  pass: mut_local_lets,
  pass: mut_local_params,
  pass: mut_params,
  pass: mut_ref_borrow_0,
  pass: mut_ref_borrow_1,
  pass: mut_ref_borrow_2,
  pass: mut_ref_borrow_3,
  pass: mut_ref_borrow_5,
  pass: mut_ref_borrow_6,
  pass: mut_ref_borrow_7,
  pass: mut_ref_borrow_8,
  pass: mut_ref_borrow_9,
  pass: mut_ref_borrow_10,
  pass: mut_ref_borrow_11,
  pass: mut_ref_borrow_12,
  pass: mut_ref_clone,
  pass: mut_ref_immut_borrow,
  pass: mut_ref_tuple,
  pass: mut_return,
  pass: mut_tuple,
  pass: nested_spec,
  pass: nested_spec_impl,
  pass: panic_type,
  pass: phantom_data,
  pass: return_stmt,
  pass: set_ops,
  pass: spec_on_trait_impl,
  pass: trait_bounds,
  pass: struct_update,
  pass: tuple_match,
  pass: tuple_result,
  pass: tuples,
  pass: type_class,
  pass: type_class_multi_lookup,
  pass: type_class_specs,
  pass: type_class_without_evidence,
  pass: use_local,
  pass: use_std,
  pass: strings,
  fail_verification: box_as_ref,
  fail_extraction: double_measure,
  fail_extraction: double_measure_impl,
  fail_verification: liskov_rectangle,
  fail_verification: mut_clone,
  fail_extraction: mut_ref_borrow_4,
  fail_verification: mut_ref_clone,
  crash_verification: mut_ref_nested,
  fail_verification: mut_ref_tuple,
  crash_verification: return_in_cond,
  crash_verification: return_in_guard,
  fail_extraction: user_deref
);
