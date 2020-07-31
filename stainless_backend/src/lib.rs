extern crate serde;
extern crate serde_json;

use std::io::{LineWriter, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::result::Result;

pub mod messages;
use messages::Response;

pub struct Config {
  timeout: usize,
  print_ids: bool,
  print_types: bool,
  debug_trees: bool,
  debug_phases: Vec<String>,
}

impl Default for Config {
  fn default() -> Self {
    Self {
      timeout: 30,
      print_ids: false,
      print_types: false,
      debug_trees: false,
      debug_phases: vec![],
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
      .arg(format!("--timeout={}", config.timeout))
      .arg(format!("--print-ids={}", config.print_ids))
      .arg(format!("--print-types={}", config.print_types));
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
}

fn find_stainless_home() -> Result<PathBuf, String> {
  match std::env::var("STAINLESS_HOME") {
    Ok(home_path) => Ok(PathBuf::from(home_path)),
    Err(_) => {
      Err("Could not find stainless directory. Please make sure STAINLESS_HOME is set.".into())
    }
  }
}
