extern crate rustc;
extern crate syntax;
extern crate stainless_data;

mod extractor;
mod extractor_rules;
mod utils;

use rustc::ty::{TyCtxt, TypeckTables};
use syntax::ast;
use stainless_data::ast as st;

pub fn playground<'tcx>(tcx: TyCtxt<'tcx>, crate_name: String, krate: &ast::Crate) -> () {
  let empty_tables = TypeckTables::empty(None);
  let factory = &st::Factory::new();
  let mut extraction = extractor::Extraction {
    factory: factory,
    definitions: vec![],
  };
  let mut xtor = extractor::Extractor::new(tcx, crate_name, &empty_tables, &mut extraction);
  xtor.process_mod(&xtor.crate_name.clone(), &krate.module);
}
