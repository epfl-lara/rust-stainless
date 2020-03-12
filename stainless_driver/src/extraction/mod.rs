extern crate rustc;
extern crate rustc_ast;
extern crate stainless_data;

mod extractor;
mod extractor_rules;
mod utils;

use rustc::ty::{TyCtxt, TypeckTables};
use rustc_ast::ast;
use stainless_data::ast as st;

pub fn playground<'tcx>(tcx: TyCtxt<'tcx>, crate_name: String, _krate: &ast::Crate) -> () {
  let empty_tables = TypeckTables::empty(None);
  let factory = &st::Factory::new();
  let mut extraction = extractor::Extraction {
    factory: factory,
    definitions: vec![],
  };
  let mut xtor = extractor::Extractor::new(tcx, crate_name, &empty_tables, &mut extraction);
  xtor.process_crate(&xtor.crate_name.clone());
}
