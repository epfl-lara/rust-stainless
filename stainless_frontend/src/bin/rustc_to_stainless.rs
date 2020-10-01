#![feature(rustc_private)]
extern crate rustc_middle;
extern crate rustc_session;

use std::env;
use std::path::PathBuf;

use rustc_middle::ty::TyCtxt;
use rustc_session::config::ErrorOutputType;
use rustc_session::early_error;

use stainless_backend::messages::*;
use stainless_backend::{verify_program, Config};
use stainless_data::ast as st;

fn main() -> Result<(), ()> {
  let args = std::env::args_os()
    .enumerate()
    .map(|(i, arg)| {
      arg.into_string().unwrap_or_else(|arg| {
        early_error(
          ErrorOutputType::default(),
          &format!("Argument {} is not valid Unicode: {:?}", i, arg),
        )
      })
    })
    .collect::<Vec<_>>();

  stainless_frontend::run(args, |tcx, symbols| {
    tcx.sess.abort_if_errors();
    match env::var("RUSTSTAINLESS_EXPORT").ok() {
      Some(export_path) => {
        tcx
          .sess
          .note_without_error(format!("Exporting extracted program to {}.", export_path).as_str());
        let output_path = PathBuf::from(export_path);
        output_program(output_path, symbols);
      }
      None => verify_program_and_report(tcx, symbols),
    }
  })
}

fn output_program<P: AsRef<std::path::Path>>(path: P, symbols: st::Symbols) {
  use stainless_data::ser::{BufferSerializer, Serializable};
  let mut ser = BufferSerializer::new();
  symbols
    .serialize(&mut ser)
    .expect("Unable to serialize stainless program");
  std::fs::write(path, ser.as_slice()).expect("Unable to write serialized stainless program");
}

fn verify_program_and_report(tcx: TyCtxt, symbols: st::Symbols) {
  // fn build_id_map<'l>(syms: &st::Symbols<'l>) -> HashMap<usize, &'l st::SymbolIdentifier<'l>> {
  //   let mut map = HashMap::new();
  //   for &id in syms.sorts.keys().chain(syms.functions.keys()) {
  //     map.insert(id.id.globalId as usize, id);
  //   }
  //   map
  // }

  fn print_results<'l>(_tcx: TyCtxt, _symbols: &st::Symbols<'l>, results: Vec<VerificationResult>) {
    // let id_map = build_id_map(symbols);
    let data: Vec<Vec<String>> = results
      .iter()
      .map(|result| {
        // let id = id_map[&result.id.gid];
        // let name = id.symbol_path.join("::");
        let name = result.id.name.clone();
        let time = format!("{:.1}", (result.time as f32) / 1000.0);
        vec![name, result.kind.clone(), time, result.status.to_string()]
      })
      .collect();

    for row in data {
      println!(
        "- {:<38} {:^32} {:>7} {:>18}",
        row[0], row[1], row[2], row[3]
      );
    }
    println!();
  }

  let sess = tcx.sess;
  match verify_program(Config::default(), &symbols) {
    Ok(Report::Verification { results, sources }) => {
      sess.note_without_error(format!("Verified {} items.", sources.len()).as_str());
      let invalids = results
        .iter()
        .filter(|result| !result.status.is_valid())
        .collect::<Vec<_>>();
      if invalids.is_empty() {
        sess.note_without_error("All VCs passed!\n");
      } else {
        sess.warn(format!("Failed to prove {} VCs:", invalids.len()).as_str());
      }
      print_results(tcx, &symbols, results);
    }

    Err(msg) => sess.fatal(
      format!(
        "An error occurred while trying to verify the extracted program: {}",
        msg,
      )
      .as_str(),
    ),
  }
}
