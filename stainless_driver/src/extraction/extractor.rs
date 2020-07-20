use std::collections::HashMap;

use rustc_hir::def_id::DefId;
use rustc_hir::HirId;
use rustc_middle::ty::{TyCtxt, TypeckTables};

use stainless_data::ast as st;

use super::utils::UniqueCounter;

/// Helpful type aliases
pub type _StainlessId<'l> = &'l st::Identifier;
pub type StainlessSymId<'l> = &'l st::SymbolIdentifier<'l>;

/// A mapping between Rust ids and Stainless ids
pub struct SymbolMapping<'l> {
  global_id_counter: UniqueCounter<()>,
  local_id_counter: UniqueCounter<String>,
  did_to_stid: HashMap<DefId, StainlessSymId<'l>>,
  hid_to_stid: HashMap<HirId, StainlessSymId<'l>>,
}

/// Extraction is the result of extracting stainless trees for a Rust AST.
pub struct Extraction<'l> {
  pub factory: &'l st::Factory,
  pub adts: HashMap<StainlessSymId<'l>, &'l st::ADTSort<'l>>,
  pub functions: HashMap<StainlessSymId<'l>, &'l st::FunDef<'l>>,
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
      tcx,
      crate_name,
      empty_tables,
      tables: empty_tables,

      extraction,
      mapping: SymbolMapping {
        global_id_counter: UniqueCounter::new(),
        local_id_counter: UniqueCounter::new(),
        did_to_stid: HashMap::new(),
        hid_to_stid: HashMap::new(),
      },
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

  /// Identifier mappings

  fn fresh_id(&mut self, name: String, symbol_path: Vec<String>) -> StainlessSymId<'l> {
    let global_id = self.mapping.global_id_counter.fresh(&());
    let local_id = self.mapping.local_id_counter.fresh(&name);
    let f = &mut self.extraction.factory;
    let id = f.Identifier(name, global_id, local_id);
    f.SymbolIdentifier(id, symbol_path)
  }

  pub(in crate) fn fresh_param_id(&mut self, index: usize) -> StainlessSymId<'l> {
    let name = format!("param{}", index);
    self.fresh_id(name.clone(), vec![name])
  }

  fn symbol_path_from_def_id(&self, def_id: DefId) -> Vec<String> {
    self
      .tcx
      .def_path_str(def_id)
      .split("::")
      .map(|s| s.into())
      .collect()
  }

  pub(in crate) fn register_def(&mut self, def_id: DefId) -> StainlessSymId<'l> {
    let symbol_path = self.symbol_path_from_def_id(def_id);
    let name = symbol_path.last().unwrap().clone();
    let id = self.fresh_id(name, symbol_path);

    assert!(self.mapping.did_to_stid.insert(def_id, id).is_none());
    id
  }

  pub(in crate) fn register_hir(&mut self, hir_id: HirId, name: String) -> StainlessSymId<'l> {
    let mut symbol_path = self.symbol_path_from_def_id(hir_id.owner.to_def_id());
    symbol_path.push(name.clone());
    let id = self.fresh_id(name, symbol_path);

    assert!(self.mapping.hid_to_stid.insert(hir_id, id).is_none());
    id
  }

  /// Conflates a Rust HIR identifier with the meaning of an existing Stainless id
  #[allow(unused)]
  pub(in crate) fn register_hir_alias(&mut self, hir_id: HirId, id: StainlessSymId<'l>) -> () {
    assert!(self.mapping.hid_to_stid.insert(hir_id, id).is_none());
  }

  #[inline]
  pub fn get_id_from_def(&self, def_id: DefId) -> Option<StainlessSymId<'l>> {
    self.mapping.did_to_stid.get(&def_id).copied()
  }

  #[inline]
  #[allow(unused)]
  pub fn fetch_id_from_def(&self, def_id: DefId) -> StainlessSymId<'l> {
    self
      .get_id_from_def(def_id)
      .expect("No Stainless id registered for the given definition id")
  }

  pub fn factory(&self) -> &'l st::Factory {
    self.extraction.factory
  }
}
