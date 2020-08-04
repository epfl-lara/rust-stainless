extern crate clap;
extern crate serde_json;

use clap::{App, Arg};
use serde_json::{Map, Value};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::*;

#[derive(Debug)]
struct Config {
  debug: bool,
  example_opt: Option<String>,
  export_path_opt: Option<String>,
}

#[derive(Debug)]
struct Build {
  package_name: String,
  sysroot: String,
  env: HashMap<String, String>,
  args: Vec<String>,
}

fn parsing_error(msg: String) -> ! {
  eprintln!("Error parsing build plan: {}", msg);
  exit(1)
}

fn unpack_name(value: &Value) -> String {
  if let Value::String(name) = value {
    name.clone()
  } else {
    parsing_error("Expected invocation to contain 'package_name'".into())
  }
}

fn unpack_build_env(value: &Value) -> HashMap<String, String> {
  if let Value::Object(env) = value {
    let mut map: HashMap<String, String> = HashMap::with_capacity(env.len());
    for (k, v) in env {
      if let Value::String(v) = v {
        map.insert(k.clone(), v.clone());
      }
    }
    map
  } else {
    parsing_error("Expected invocation to contain 'env' object".into())
  }
}

fn unpack_build_args(value: &Value) -> Vec<String> {
  if let Value::Array(args) = value {
    args
      .iter()
      .flat_map(|arg| {
        if let Value::String(arg) = arg {
          if !arg.starts_with("--error-format") && !arg.starts_with("--json") {
            Some(arg.clone())
          } else {
            None
          }
        } else {
          None
        }
      })
      .collect()
  } else {
    parsing_error("Expected invocation to contain 'env' object".into())
  }
}

fn find_sysroot(env: &HashMap<String, String>) -> String {
  let cargo_path = env
    .get("CARGO")
    .expect("Expected CARGO env to be present in build plan");
  let mut sysroot_path = PathBuf::from(cargo_path);
  sysroot_path.pop();
  sysroot_path.pop();
  sysroot_path.into_os_string().into_string().unwrap()
}

fn unpack_invocation<'a>(config: &Config, invocations: &'a [Value]) -> &'a Map<String, Value> {
  let value = match &config.example_opt {
    Some(example) => invocations
      .iter()
      .find(
        |inv| match (inv.pointer("/target_kind"), inv.pointer("/args")) {
          (Some(Value::Array(kinds)), Some(Value::Array(args))) => {
            kinds.iter().any(|kind| match kind {
              Value::String(kind) => kind == "example",
              _ => false,
            }) && args.iter().any(|arg| match arg {
              Value::String(arg) => arg.contains(&format!("{}.rs", example)),
              _ => false,
            })
          }
          _ => false,
        },
      )
      .unwrap_or_else(|| parsing_error("Couldn't find target for the specified example".into())),
    None => invocations
      .last()
      .expect("Expected at least one invocation"),
  };
  if let Value::Object(invocation) = value {
    invocation
  } else {
    parsing_error("Expected invocation to be an object".into())
  }
}

fn parse_build(config: &Config, data: &[u8]) -> Build {
  let plan: Value =
    serde_json::from_reader(data).unwrap_or_else(|e| parsing_error(format!("{}", e)));
  if config.debug {
    println!(
      "[Debug] Read the following build plan:\n\n-----{:#}----\n",
      plan
    );
  }

  if let Value::Array(invocations) = plan
    .pointer("/invocations")
    .expect("Expected 'invocations' key")
  {
    let invocation = unpack_invocation(config, invocations);
    let program = invocation.get("program").unwrap();
    assert_eq!(program, "rustc");
    let env = unpack_build_env(invocation.get("env").unwrap());
    Build {
      package_name: unpack_name(invocation.get("package_name").unwrap()),
      sysroot: find_sysroot(&env),
      args: unpack_build_args(invocation.get("args").unwrap()),
      env,
    }
  } else {
    parsing_error("Expected 'invocations' to be an array".into())
  }
}

// Use cargo's build-plan flag to extract rustc arguments as a JSON object.
//
// Note that this feature is unstable and broken in that it mysteriously
// invalidates existing files in the target/ directory.
// We therefore include a hacky workaround that protects the target/debug/
// subdirectory by moving it during the generation of the build-plan in cargo.
#[allow(unused_must_use)]
fn fetch_build_plan(config: &Config) -> Output {
  // TODO: Extract this directory from Cargo? (These hardcoded paths currently
  // prevent us from working in the presence of Cargo workspaces.)
  const TARGET_CACHE_DIR: &str = "target/debug";
  const TARGET_CACHE_DIR_TMP: &str = "target/debug__tmp";

  // Protect target dir
  fs::rename(TARGET_CACHE_DIR, TARGET_CACHE_DIR_TMP)
    .expect("Couldn't temporarily rename target cache dir");

  // Get the build plan from cargo
  let mut build_cmd = Command::new("cargo");
  build_cmd
    .arg("build")
    .arg("-Z")
    .arg("unstable-options")
    .arg("--build-plan");
  if config.example_opt.is_some() {
    build_cmd.arg("--all-targets");
  }

  let build_output = build_cmd
    .output()
    .expect("Couldn't start cargo build to extract plan");
  if !build_output.status.success() {
    parsing_error("Failed to run cargo build to extract plan".into());
  }

  // Restore target dir
  fs::remove_dir_all(TARGET_CACHE_DIR); // hope for the best.
  fs::rename(TARGET_CACHE_DIR_TMP, TARGET_CACHE_DIR).expect("Couldn't move target cache dir back!");

  build_output
}

fn main() -> ! {
  let matches = App::new("cargo-stainless")
    .version("0.0.1")
    .author("Georg Schmid <georg.schmid@epfl.ch>")
    .about("A cargo subcommand for rust-stainless extraction")
    .arg(
      Arg::with_name("dummy")
        .help("Dummy argument passed by cargo (the subcommand name)")
        .required(true)
        .index(1),
    )
    .arg(
      Arg::with_name("example")
        .long("example")
        .takes_value(true)
        .help("Try to extract an example of the crate instead of the last target"),
    )
    .arg(
      Arg::with_name("debug")
        .long("debug")
        .help("Print the read build plan and the chosen target"),
    )
    .arg(
      Arg::with_name("export")
        .long("export")
        .takes_value(true)
        .help("Do not verify, but only export the extracted program to the given path"),
    )
    .get_matches();

  let config = Config {
    debug: matches.is_present("debug"),
    example_opt: matches.value_of("example").map(|s| s.into()),
    export_path_opt: matches.value_of("export").map(|s| s.into()),
  };

  // Parse build plan
  let build_output = fetch_build_plan(&config);

  // NOTE: When no example is specified we currently just pick the last build
  // invocation, which should work well enough for simple, single-target builds.
  let mut build = parse_build(&config, &build_output.stdout);

  // Run extraction
  if config.debug {
    println!("Parsed build plan:\n{:#?}", build);
  }

  match config.example_opt {
    Some(example) => println!("cargo-stainless: Found example target '{}'.\n", example),
    None => println!(
      "cargo-stainless: Found default target '{}'.\n",
      build.package_name
    ),
  }

  if let Some(export_path) = config.export_path_opt {
    build.env.insert("RUSTSTAINLESS_EXPORT".into(), export_path);
  }

  // Pass through certain flags
  for &var_name in &["STAINLESS_FLAGS"] {
    if let Ok(value) = env::var(var_name) {
      let var_name: String = var_name.into();
      build.env.entry(var_name).or_insert(value);
    }
  }

  let status = Command::new("rustc_to_stainless")
    .envs(build.env.iter())
    .arg("--sysroot")
    .arg(build.sysroot)
    .args(build.args.iter())
    .status()
    .expect("Couldn't start rustc_to_stainless");
  exit(status.code().unwrap_or(1));
}
