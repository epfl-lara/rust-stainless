extern crate serde_json;

use serde_json::Value;
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::*;

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

fn parse_build(data: &[u8]) -> Build {
  let plan: Value =
    serde_json::from_reader(data).unwrap_or_else(|e| parsing_error(format!("{}", e)));
  if let Value::Array(invocations) = plan
    .pointer("/invocations")
    .expect("Expected 'invocations' key")
  {
    if let Value::Object(invocation) = invocations
      .last()
      .expect("Expected at least one invocation")
    {
      let program = invocation.get("program").unwrap();
      assert_eq!(program, "rustc");
      let env = unpack_build_env(invocation.get("env").unwrap());
      Build {
        package_name: unpack_name(invocation.get("package_name").unwrap()),
        sysroot: find_sysroot(&env),
        env: env,
        args: unpack_build_args(invocation.get("args").unwrap()),
      }
    } else {
      parsing_error("Expected invocation to be an object".into())
    }
  } else {
    parsing_error("Expected 'invocations' to be an array".into())
  }
}

fn main() -> ! {
  // Parse build plan
  let build_output = Command::new("cargo")
    .arg("build")
    .arg("-Z")
    .arg("unstable-options")
    .arg("--build-plan")
    .output()
    .expect("Couldn't start cargo build to extract plan");
  if !build_output.status.success() {
    parsing_error("Failed to run cargo build to extract plan".into());
  }

  // NOTE: We currently just pick the last build invocation, which should
  // work well enough for simple, single target builds.
  let build = parse_build(&build_output.stdout);

  // Run extraction
  // println!("Parsed build plan:\n{:#?}", build);
  println!("cargo-stainless: Found target {}.\n", build.package_name);

  let status = Command::new("rustc_to_stainless")
    .envs(build.env.iter())
    .arg("--sysroot")
    .arg(build.sysroot)
    .args(build.args.iter())
    .status()
    .expect("Couldn't start rustc_to_stainless");
  exit(status.code().unwrap_or(1));
}
