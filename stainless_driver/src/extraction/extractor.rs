extern crate rustc;
extern crate rustc_hir;
extern crate rustc_session;
extern crate stainless_data;
extern crate rustc_ast;

use std::collections::HashMap;

use rustc::ty::{TyCtxt, TypeckTables};
use rustc_hir::HirId;
use rustc_ast::ast;

use stainless_data::ast as st;

use super::utils::UniqueCounter;

/// Helpful type aliases
pub type _StainlessId<'l> = &'l st::Identifier;
pub type StainlessSymId<'l> = &'l st::SymbolIdentifier<'l>;

/// A mapping between Rust ids and Stainless ids
pub struct SymbolMapping<'l> {
  global_id_counter: UniqueCounter<()>,
  local_id_counter: UniqueCounter<String>,
  r2i: HashMap<HirId, StainlessSymId<'l>>,
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

  pub(in crate) fn nest_tables<R, F>(&mut self, hir_id: HirId, f: F) -> R
  where
    F: FnOnce(&mut Self) -> R,
  {
    let def_id = self.tcx.hir().local_def_id(hir_id);

    let tables = if self.tcx.has_typeck_tables(def_id) {
      self.tcx.typeck_tables_of(def_id)
    } else {
      self.empty_tables
    };

    let old_tables = self.tables;
    self.tables = tables;
    let result = f(self);
    self.tables = old_tables;
    result
  }

  #[allow(dead_code)]
  pub(in crate) fn get_id_or_register<F>(&mut self, hir_id: HirId, f: F) -> StainlessSymId<'l>
  where
    F: FnOnce(&mut Self) -> StainlessSymId<'l>,
  {
    if self.mapping.r2i.contains_key(&hir_id) {
      self.mapping.r2i.get(&hir_id).unwrap()
    } else {
      f(self)
    }
  }

  fn register_id(
    &mut self,
    hir_id: HirId,
    name: String,
    symbol_path: Vec<String>,
  ) -> StainlessSymId<'l> {
    let global_id = self.mapping.global_id_counter.fresh(&());
    let local_id = self.mapping.local_id_counter.fresh(&name);
    let f = &mut self.extraction.factory;
    let id = f.Identifier(name, global_id, local_id);
    let id = f.SymbolIdentifier(id, symbol_path);
    assert!(self.mapping.r2i.insert(hir_id, id).is_none());
    id
  }

  pub(in crate) fn register_id_from_name(
    &mut self,
    hir_id: HirId,
    name: String,
  ) -> StainlessSymId<'l> {
    let path = vec![name.clone()];
    self.register_id(hir_id, name, path)
  }

  pub(in crate) fn register_id_from_ident(
    &mut self,
    hir_id: HirId,
    ident: &ast::Ident,
  ) -> StainlessSymId<'l> {
    // TODO: Extract fully-qualified name for symbol path whenever possible?
    let simple_name = ident.name.to_string();
    let path = vec![simple_name.clone()];
    self.register_id(hir_id, simple_name, path)
  }

  pub fn fetch_id(&self, hir_id: HirId) -> StainlessSymId<'l> {
    let id_opt = self.mapping.r2i.get(&hir_id);
    id_opt.expect("No Stainless id registered for given HIR node")
  }
}
