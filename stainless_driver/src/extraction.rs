mod extractor;
mod extractor_rules;
mod utils;

use extractor::{Extraction, Extractor};

use rustc_middle::ty::TyCtxt;

use stainless_data::ast as st;

pub fn extract_and_output_crate(tcx: TyCtxt<'_>, crate_name: String) -> () {
  let factory = st::Factory::new();
  let mut extraction = Extraction::new(&factory);
  let mut xtor = Extractor::new(tcx, &mut extraction);
  xtor.process_crate(crate_name);

  // Output extracted Stainless program
  let (adts, functions) = xtor.into_result();

  eprintln!("[ Extracted ADTs and functions ]");
  for adt in &adts {
    eprintln!(" - ADT {}", adt.id);
  }
  for fd in &functions {
    eprintln!(" - Fun {}", fd.id);
  }

  let output_path = std::path::Path::new("./output.inoxser");
  output_program(output_path, st::Symbols::new(adts, functions));
}

fn output_program<P: AsRef<std::path::Path>>(path: P, symbols: st::Symbols) -> () {
  use stainless_data::ser::{BufferSerializer, Serializable};
  let mut ser = BufferSerializer::new();
  symbols
    .serialize(&mut ser)
    .expect("Unable to serialize stainless program");
  std::fs::write(path, ser.as_slice()).expect("Unable to write serialized stainless program");
}
