use std::collections::HashMap;

use rustc_hair::hair;
use rustc_hir::def_id::DefId;
use rustc_hir::HirId;
use rustc_infer::infer::{InferCtxt, TyCtxtInferExt};
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

/// Extraction encapsulates the state of extracting a Stainless program
pub struct Extraction<'l> {
  mapping: SymbolMapping<'l>,
  factory: &'l st::Factory,
  adts: HashMap<StainlessSymId<'l>, &'l st::ADTSort<'l>>,
  functions: HashMap<StainlessSymId<'l>, &'l st::FunDef<'l>>,
}

impl<'l> Extraction<'l> {
  pub fn new(factory: &'l st::Factory) -> Self {
    Self {
      mapping: SymbolMapping {
        global_id_counter: UniqueCounter::new(),
        local_id_counter: UniqueCounter::new(),
        did_to_stid: HashMap::new(),
        hid_to_stid: HashMap::new(),
      },
      factory,
      adts: HashMap::new(),
      functions: HashMap::new(),
    }
  }

  fn fresh_id(&mut self, name: String, symbol_path: Vec<String>) -> StainlessSymId<'l> {
    let global_id = self.mapping.global_id_counter.fresh(&());
    let local_id = self.mapping.local_id_counter.fresh(&name);
    let id = self.factory.Identifier(name, global_id, local_id);
    self.factory.SymbolIdentifier(id, symbol_path)
  }
}

/// Extractor combines rustc state with extraction state
pub struct Extractor<'l, 'tcx: 'l> {
  tcx: TyCtxt<'tcx>,
  extraction: Option<&'l mut Extraction<'l>>,
  pub tables: TypeckTables<'tcx>, // REMOVE ME
}

impl<'l, 'tcx> Extractor<'l, 'tcx> {
  pub fn new(tcx: TyCtxt<'tcx>, extraction: &'l mut Extraction<'l>) -> Self {
    Self {
      tcx,
      extraction: Some(extraction),
      tables: TypeckTables::empty(None),
    }
  }

  pub fn into_result(self) -> (Vec<&'l st::ADTSort<'l>>, Vec<&'l st::FunDef<'l>>) {
    self.with_extraction(|xt| {
      let adts: Vec<&st::ADTSort> = xt.adts.values().copied().collect();
      let functions: Vec<&st::FunDef> = xt.functions.values().copied().collect();
      (adts, functions)
    })
  }

  #[inline]
  pub fn tcx(&self) -> TyCtxt<'tcx> {
    self.tcx
  }

  #[inline]
  fn with_extraction<T, F: FnOnce(&Extraction<'l>) -> T>(&self, f: F) -> T {
    f(&**self.extraction.as_ref().expect("BodyExtractor active"))
  }

  #[inline]
  fn with_extraction_mut<T, F: FnOnce(&mut Extraction<'l>) -> T>(&mut self, f: F) -> T {
    f(*self.extraction.as_mut().expect("BodyExtractor active"))
  }

  #[inline]
  pub fn factory(&self) -> &'l st::Factory {
    self.with_extraction(|xt| xt.factory)
  }

  /// Identifier mappings

  pub(in crate) fn fresh_param_id(&mut self, index: usize) -> StainlessSymId<'l> {
    self.with_extraction_mut(|xt| {
      let name = format!("param{}", index);
      xt.fresh_id(name.clone(), vec![name])
    })
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

    self.with_extraction_mut(|xt| {
      let id = xt.fresh_id(name, symbol_path);
      assert!(xt.mapping.did_to_stid.insert(def_id, id).is_none());
      id
    })
  }

  pub(in crate) fn register_hir(&mut self, hir_id: HirId, name: String) -> StainlessSymId<'l> {
    let mut symbol_path = self.symbol_path_from_def_id(hir_id.owner.to_def_id());
    symbol_path.push(name.clone());

    self.with_extraction_mut(|xt| {
      let id = xt.fresh_id(name, symbol_path);
      assert!(xt.mapping.hid_to_stid.insert(hir_id, id).is_none());
      id
    })
  }

  /// Conflates a Rust HIR identifier with the meaning of an existing Stainless id
  #[allow(unused)]
  pub(in crate) fn register_hir_alias(&mut self, hir_id: HirId, id: StainlessSymId<'l>) -> () {
    self.with_extraction_mut(|xt| {
      assert!(xt.mapping.hid_to_stid.insert(hir_id, id).is_none());
    })
  }

  #[inline]
  pub fn get_id_from_def(&self, def_id: DefId) -> Option<StainlessSymId<'l>> {
    self.with_extraction(|xt| xt.mapping.did_to_stid.get(&def_id).copied())
  }

  #[inline]
  #[allow(unused)]
  pub fn fetch_id_from_def(&self, def_id: DefId) -> StainlessSymId<'l> {
    self
      .get_id_from_def(def_id)
      .expect("No Stainless id registered for the given definition id")
  }

  /// ADTs and Functions

  pub(in crate) fn add_adt(&mut self, id: StainlessSymId<'l>, adt: &'l st::ADTSort<'l>) {
    self.with_extraction_mut(|xt| {
      assert!(xt.adts.insert(id, adt).is_none());
    })
  }

  pub(in crate) fn add_function(&mut self, id: StainlessSymId<'l>, fd: &'l st::FunDef<'l>) {
    self.with_extraction_mut(|xt| {
      assert!(xt.functions.insert(id, fd).is_none());
    })
  }

  /// Get a BodyExtractor for some item with a body (like a function)
  pub fn enter_body<T, F>(&mut self, hir_id: HirId, f: F) -> T
  where
    F: FnOnce(&mut BodyExtractor<'_, 'l, 'tcx>) -> T,
  {
    self.tcx().infer_ctxt().enter(|infcx| {
      let mut bxtor = BodyExtractor::new(self, &infcx, hir_id);
      let result = f(&mut bxtor);
      self.extraction = bxtor.xtor.extraction;
      result
    })
  }
}

/// BodyExtractor is used to extract, for example, function bodies
pub struct BodyExtractor<'a, 'l, 'tcx: 'l> {
  xtor: Extractor<'l, 'tcx>,
  hcx: hair::cx::Cx<'a, 'tcx>,
  tables: &'a TypeckTables<'tcx>,
}

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  fn new(xtor: &mut Extractor<'l, 'tcx>, infcx: &'a InferCtxt<'a, 'tcx>, hir_id: HirId) -> Self {
    let tcx = xtor.tcx;
    let extraction = xtor.extraction.take();
    let xtor = Extractor::new(
      tcx,
      extraction.expect("Waiting for another BodyExtractor to finish"),
    );

    let hcx: hair::cx::Cx<'_, 'tcx> = hair::cx::Cx::new(infcx, hir_id);

    let def_id = tcx.hir().local_def_id(hir_id);
    assert!(tcx.has_typeck_tables(def_id));
    let tables = tcx.typeck_tables_of(def_id);

    BodyExtractor { xtor, hcx, tables }
  }

  #[inline]
  pub fn xtor(&mut self) -> &mut Extractor<'l, 'tcx> {
    &mut self.xtor
  }

  #[inline]
  pub fn tcx(&self) -> TyCtxt<'tcx> {
    self.hcx.tcx()
  }

  #[inline]
  pub fn hcx(&mut self) -> &mut hair::cx::Cx<'a, 'tcx> {
    &mut self.hcx
  }

  #[inline]
  pub fn tables(&mut self) -> &'a TypeckTables<'tcx> {
    self.tables
  }
}
