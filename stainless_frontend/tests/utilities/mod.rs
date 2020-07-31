use std::env;
use std::path::{Path, PathBuf};

use stainless_backend::messages::Report;
use stainless_backend::{verify_program, Config};
use stainless_data::ast as st;

#[derive(Debug, PartialEq, Eq)]
pub enum Outcome {
  Success { verified: bool },
  CrashInExtraction,
  ErrorInExtraction,
  CrashInVerification,
  ErrorInVerification,
}

pub fn run_extraction_test<S: AsRef<Path>>(source_path: S, verify: bool) -> Outcome {
  let args = compiler_args(source_path);

  let mut outcome: Outcome = Outcome::CrashInExtraction;
  let had_xt_crashes = stainless_frontend::run(args, |tcx, symbols| {
    if tcx.sess.has_errors() {
      outcome = Outcome::ErrorInExtraction;
    } else if !verify {
      outcome = Outcome::Success { verified: false };
    } else {
      outcome = run_verification_test(&symbols);
    }
  })
  .is_err();

  assert!(!had_xt_crashes || outcome == Outcome::CrashInExtraction);
  outcome
}

fn run_verification_test(symbols: &st::Symbols) -> Outcome {
  if let Ok(report) = verify_program(Config::default(), symbols) {
    let all_valid = match report {
      Report::Verification { results, .. } => results.iter().all(|result| result.status.is_valid()),
    };
    if all_valid {
      Outcome::Success { verified: true }
    } else {
      Outcome::ErrorInVerification
    }
  } else {
    Outcome::CrashInVerification
  }
}

pub fn manifest_relative_path<S: AsRef<Path>>(relative_path: S) -> PathBuf {
  PathBuf::new()
    .join(env!("CARGO_MANIFEST_DIR"))
    .join(relative_path)
}

/// Helpers

fn _find_tests() -> Vec<PathBuf> {
  let mut test_paths = std::fs::read_dir(".")
    .expect("Failed to read test case directory")
    .map(|res| res.map(|e| e.path()))
    .collect::<Result<Vec<_>, std::io::Error>>()
    .expect("Failed to enumerate files in test case directory");
  test_paths.sort();
  test_paths
}

fn find_sysroot() -> PathBuf {
  match (option_env!("RUSTUP_HOME"), option_env!("RUSTUP_TOOLCHAIN")) {
    (Some(home), Some(toolchain)) => PathBuf::new().join(home).join("toolchains").join(toolchain),
    _ => {
      panic!("Missing RUSTUP_HOME or RUSTUP_TOOLCHAIN in the environment. Are you using rustup?")
    }
  }
}

fn compiler_args<S: AsRef<Path>>(source_path: S) -> Vec<String> {
  use tempfile::tempdir;
  let output_dir = tempdir().expect("Failed to create temporary output dir");
  compiler_args_with_output(source_path, output_dir.path())
}

fn compiler_args_with_output<S, O>(source_path: S, output_path: O) -> Vec<String>
where
  S: AsRef<Path>,
  O: AsRef<Path>,
{
  let sysroot_path = find_sysroot();

  let deps_path = manifest_relative_path("../target/debug/deps");
  let deps = format!("dependency={}", deps_path.to_str().unwrap());
  let stainless_lib_path = manifest_relative_path("../target/debug/libstainless.rlib");
  let stainless_lib = format!("stainless={}", stainless_lib_path.to_str().unwrap());

  vec![
    "--crate-name test".into(),
    source_path.as_ref().to_str().unwrap().into(),
    "--crate-type".into(),
    "lib".into(),
    "--sysroot".into(),
    sysroot_path.to_str().unwrap().into(),
    "--out-dir".into(),
    output_path.as_ref().to_str().unwrap().into(),
    "-L".into(),
    deps,
    "--extern".into(),
    stainless_lib,
  ]
}
