use std::env;
use std::path::{Path, PathBuf};

pub fn extract_without_errors<P: AsRef<Path>>(source_path: P, output_path: P) -> bool {
  let args = compiler_args(source_path, output_path);

  let mut had_errors = true;
  let had_crashes = stainless_frontend::run(args, |tcx, _symbols| {
    had_errors = tcx.sess.has_errors();
  })
  .is_err();

  !had_crashes && !had_errors
}

pub fn manifest_relative_path<P: AsRef<Path>>(relative_path: P) -> PathBuf {
  PathBuf::new()
    .join(env!("CARGO_MANIFEST_DIR"))
    .join(relative_path)
}

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

fn compiler_args<P: AsRef<Path>>(source_path: P, output_path: P) -> Vec<String> {
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
