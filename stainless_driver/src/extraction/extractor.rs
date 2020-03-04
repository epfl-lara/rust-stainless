extern crate rustc;
extern crate rustc_session;
extern crate stainless_data;
extern crate syntax;

use std::collections::HashMap;

use rustc::ty::{TyCtxt, TypeckTables};
use rustc_session::Session;
use syntax::ast::{Ident, NodeId};
// use syntax::ast::{self, Attribute, NodeId, PatKind, DUMMY_NODE_ID};

use stainless_data::ast as st;

use super::utils::UniqueCounter;

/// A mapping between Rust ids and Stainless ids
pub struct SymbolMapping<'l> {
  global_id_counter: UniqueCounter<()>,
  local_id_counter: UniqueCounter<String>,
  r2i: HashMap<NodeId, &'l st::SymbolIdentifier<'l>>,
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

  pub fn session(&self) -> &'tcx Session {
    self.tcx.sess
  }

  pub fn nest_tables<F>(&mut self, item_id: NodeId, f: F)
  where
    F: FnOnce(&mut Self),
  {
    let item_def_id = self.tcx.hir().local_def_id_from_node_id(item_id);

    let tables = if self.tcx.has_typeck_tables(item_def_id) {
      self.tcx.typeck_tables_of(item_def_id)
    } else {
      self.empty_tables
    };

    let old_tables = self.tables;
    self.tables = tables;
    f(self);
    self.tables = old_tables;
  }

  pub fn fetch_id(&mut self, node_id: NodeId, ident: &Ident) -> &'l st::SymbolIdentifier<'l> {
    let tcx = &self.tcx;
    let f = &mut self.extraction.factory;
    let globals = &mut self.mapping.global_id_counter;
    let locals = &mut self.mapping.local_id_counter;
    self.mapping.r2i.entry(node_id).or_insert_with(|| {
      // let simple_name = tcx
      //   .hir()
      //   .item(tcx.hir().node_to_hir_id(node_id))
      //   .ident
      //   .name
      //   .to_string();
      let simple_name = ident.name.to_string();
      let full_name = match tcx.hir().opt_local_def_id_from_node_id(node_id) {
        None => simple_name.clone(),
        Some(def_id) => format!("::{}", tcx.def_path_str(def_id)),
      };
      let name_parts: Vec<String> = full_name.split("::").map(String::from).collect();
      let global_id = globals.fresh(&());
      let local_id = locals.fresh(&simple_name);
      let id = f.Identifier(simple_name, global_id, local_id);
      let id = f.SymbolIdentifier(id, name_parts);
      id
    })
  }
}
