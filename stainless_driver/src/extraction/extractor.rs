extern crate rustc;
extern crate rustc_hir;
extern crate rustc_session;
extern crate stainless_data;
extern crate syntax;

use std::collections::HashMap;

use rustc::ty::{TyCtxt, TypeckTables};
use rustc_hir::HirId;
use syntax::ast::Ident;

use stainless_data::ast as st;

use super::utils::UniqueCounter;

/// A mapping between Rust ids and Stainless ids
pub struct SymbolMapping<'l> {
  global_id_counter: UniqueCounter<()>,
  local_id_counter: UniqueCounter<String>,
  r2i: HashMap<HirId, &'l st::SymbolIdentifier<'l>>,
}

/// Extraction is the result of extracting stainless trees for a Rust AST.
pub struct Extraction<'l> {
  pub factory: &'l st::Factory,
  pub definitions: Vec<&'l st::Definition<'l>>,
}

/// Extractor traverses an AST after analysis and extracts stainless definitions.
pub struct Extractor<'l, 'tcx> {
  pub tcx: TyCtxt<'tcx>,
  pub crate_name: String,
  pub tables: &'l TypeckTables<'tcx>,
  empty_tables: &'l TypeckTables<'tcx>,

  pub mapping: SymbolMapping<'l>,
  pub extraction: &'l mut Extraction<'l>,
}

impl<'l, 'tcx> Extractor<'l, 'tcx> {
  pub fn new(
    tcx: TyCtxt<'tcx>,
    crate_name: String,
    empty_tables: &'l TypeckTables<'tcx>,
    extraction: &'l mut Extraction<'l>,
  ) -> Self {
    Self {
      tcx: tcx,
      crate_name: crate_name,
      tables: empty_tables,
      empty_tables: empty_tables,

      mapping: SymbolMapping {
        global_id_counter: UniqueCounter::new(),
        local_id_counter: UniqueCounter::new(),
        r2i: HashMap::new(),
      },
      extraction: extraction,
    }
  }

  pub fn nest_tables<F>(&mut self, hir_id: HirId, f: F)
  where
    F: FnOnce(&mut Self),
  {
    let def_id = self.tcx.hir().local_def_id(hir_id);

    let tables = if self.tcx.has_typeck_tables(def_id) {
      self.tcx.typeck_tables_of(def_id)
    } else {
      self.empty_tables
    };

    let old_tables = self.tables;
    self.tables = tables;
    f(self);
    self.tables = old_tables;
  }

  pub fn fetch_id(&mut self, hir_id: HirId, ident: &Ident) -> &'l st::SymbolIdentifier<'l> {
    let f = &mut self.extraction.factory;
    let globals = &mut self.mapping.global_id_counter;
    let locals = &mut self.mapping.local_id_counter;
    self.mapping.r2i.entry(hir_id).or_insert_with(|| {
      // TODO: Extract fully-qualified names whenever possible?
      let simple_name = ident.name.to_string();
      let name_parts: Vec<String> = vec![simple_name.clone()];
      let global_id = globals.fresh(&());
      let local_id = locals.fresh(&simple_name);
      let id = f.Identifier(simple_name, global_id, local_id);
      let id = f.SymbolIdentifier(id, name_parts);
      id
    })
  }
}
