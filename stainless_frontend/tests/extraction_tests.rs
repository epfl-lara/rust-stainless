#![feature(rustc_private)]
extern crate stainless_frontend;

pub mod utilities;

macro_rules! define_tests {
  ($($kind:ident : $name:ident),*) => {
    #[cfg(test)]
    mod tests {
      use tempfile::tempdir;
      use super::utilities::*;

      $(
        #[test]
        fn $name() {
          let output_dir = tempdir().expect("Failed to create temporary output dir");
          let relative_path = format!("tests/{}/{}.rs", stringify!($kind), stringify!($name));
          let test_path = manifest_relative_path(relative_path);
          assert!(extract_without_errors(test_path, output_dir.path().to_owned()));
        }
      )*
    }
  }
}

define_tests!(
  pass: adts,
  pass: blocks,
  pass: external_fn,
  pass: fact,
  pass: generic_id,
  pass: generic_option,
  pass: int_option,
  pass: tuples
);
