extern crate serde;
extern crate serde_json;

use std::env;
use std::io::{LineWriter, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::result::Result;

use stainless_data::ast as st;

pub mod messages;
use messages::{Report, Response};

pub struct Config {
  timeout: usize,
  print_ids: bool,
  print_types: bool,
  debug_trees: bool,
  debug_phases: Vec<String>,
  strict_arithmetic: bool,
}

impl Default for Config {
  fn default() -> Self {
    Self {
      timeout: 30,
      print_ids: false,
      print_types: false,
      debug_trees: false,
      debug_phases: vec![],
      strict_arithmetic: false,
    }
  }
}

#[allow(dead_code)]
pub struct Backend {
  config: Config,
  child: Child,
}

impl Backend {
  pub fn create(config: Config) -> Result<Self, String> {
    let home_path = find_stainless_home()?;
    let exec_path = home_path.join("stainless");
    if !exec_path.is_file() {
      return Err(format!(
        "Could not find stainless executable at {}",
        exec_path.to_string_lossy()
      ));
    }

    let mut cmd = Command::new(exec_path);
    cmd
      .arg("--interactive")
      .arg("--batched")
      .arg("--vc-cache=false")
      // FIXME: https://github.com/epfl-lara/rust-stainless/issues/86
      .arg("--check-measures=false")
      .arg("--infer-measures=false");

    // Stainless prioritizes the first occurrence of CLI arguments, therefore
    // the flags need to come first, in order to overwrite the below.
    if let Ok(extra_flags) = env::var("STAINLESS_FLAGS") {
      cmd.args(extra_flags.split(' '));
    }

    cmd
      .arg(format!("--timeout={}", config.timeout))
      .arg(format!("--print-ids={}", config.print_ids))
      .arg(format!("--print-types={}", config.print_types))
      .arg(format!("--strict-arithmetic={}", config.strict_arithmetic));

    if config.debug_trees {
      cmd
        .arg("--debug=trees")
        .arg(format!("--debug-phases={}", config.debug_phases.join(",")));
    }

    let child = cmd
      .stdin(Stdio::piped())
      .stdout(Stdio::piped())
      .spawn()
      .map_err(|err| format!("Could not spawn stainless: {}", err))?;
    Ok(Self { config, child })
  }

  pub fn query<P: AsRef<Path>>(&mut self, query_path: P) -> Result<Response, String> {
    let stdin = self
      .child
      .stdin
      .as_mut()
      .expect("Stdin for stainless process is missing");
    let mut stdin = LineWriter::new(stdin);
    writeln!(stdin, "{}", query_path.as_ref().to_str().unwrap())
      .map_err(|_| "Could not write query to stainless stdin")?;

    let stdout = self
      .child
      .stdout
      .as_mut()
      .expect("Stdout for stainless process is missing");

    // NOTE: A hacky way to debug unparsable responses from stainless:
    // use std::io::Read;
    // let mut buffer = String::new();
    // stdout.read_to_string(&mut buffer).unwrap();
    // eprintln!("Got the following from: {}", buffer);
    // panic!();

    let deserializer = serde_json::Deserializer::from_reader(stdout);
    let mut iterator = deserializer.into_iter::<Response>();
    iterator
      .next()
      .ok_or("Stainless stopped before responding")?
      .map_err(|err| format!("Failed to parse response: {}", err))
  }

  pub fn query_for_program(&mut self, symbols: &st::Symbols) -> Result<Response, String> {
    use stainless_data::ser::*;
    use tempfile::NamedTempFile;

    let mut file = NamedTempFile::new().expect("Unable to create temporary example file");
    let mut s = BufferSerializer::new();
    symbols
      .serialize(&mut s)
      .expect("Failed to serialize stainless program");
    file
      .write_all(s.as_slice())
      .expect("Unable to write example file");

    self.query(file.path())
  }
}

fn find_stainless_home() -> Result<PathBuf, String> {
  match std::env::var("STAINLESS_HOME") {
    Ok(home_path) => Ok(PathBuf::from(home_path)),
    Err(_) => {
      Err("Could not find stainless directory. Please make sure STAINLESS_HOME is set.".into())
    }
  }
}

/// Convenience method to verify a single program
pub fn verify_program(config: Config, symbols: &st::Symbols) -> Result<Report, String> {
  let mut backend = Backend::create(config)?;
  let response = backend.query_for_program(symbols)?;
  response.into_verification_report()
}
