mod bindings;
mod expr;
mod krate;
mod literal;
mod ty;
mod utils;

use std::collections::{HashMap, HashSet};

use rustc_hair::hair;
use rustc_hir::def_id::DefId;
use rustc_hir::intravisit::{self, NestedVisitorMap, Visitor};
use rustc_hir::{self as hir, HirId};
use rustc_infer::infer::{InferCtxt, TyCtxtInferExt};
use rustc_middle::span_bug;
use rustc_middle::ty::{TyCtxt, TypeckTables};
use rustc_span::{MultiSpan, Span};

use stainless_data::ast as st;

use bindings::DefContext;
use utils::UniqueCounter;

/// The entrypoint into extraction
pub fn extract_and_output_crate(tcx: TyCtxt<'_>, crate_name: String) -> () {
  let factory = st::Factory::new();
  let mut extraction = Extraction::new(&factory);
  let mut xtor = BaseExtractor::new(tcx, &mut extraction);
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

/// Helpful type aliases
type StainlessSymId<'l> = &'l st::SymbolIdentifier<'l>;
type Params<'l> = Vec<&'l st::ValDef<'l>>;

/// A mapping between Rust ids and Stainless ids
struct SymbolMapping<'l> {
  global_id_counter: UniqueCounter<()>,
  local_id_counter: UniqueCounter<String>,
  did_to_stid: HashMap<DefId, StainlessSymId<'l>>,
  hid_to_stid: HashMap<HirId, StainlessSymId<'l>>,
}

/// Extraction encapsulates the state of extracting a Stainless program
struct Extraction<'l> {
  mapping: SymbolMapping<'l>,
  factory: &'l st::Factory,
  adts: HashMap<StainlessSymId<'l>, &'l st::ADTSort<'l>>,
  function_refs: HashSet<DefId>,
  functions: HashMap<StainlessSymId<'l>, &'l st::FunDef<'l>>,
}

impl<'l> Extraction<'l> {
  fn new(factory: &'l st::Factory) -> Self {
    Self {
      mapping: SymbolMapping {
        global_id_counter: UniqueCounter::new(),
        local_id_counter: UniqueCounter::new(),
        did_to_stid: HashMap::new(),
        hid_to_stid: HashMap::new(),
      },
      factory,
      adts: HashMap::new(),
      function_refs: HashSet::new(),
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
struct BaseExtractor<'l, 'tcx: 'l> {
  tcx: TyCtxt<'tcx>,
  extraction: Option<&'l mut Extraction<'l>>,
}

impl<'l, 'tcx> BaseExtractor<'l, 'tcx> {
  fn new(tcx: TyCtxt<'tcx>, extraction: &'l mut Extraction<'l>) -> Self {
    Self {
      tcx,
      extraction: Some(extraction),
    }
  }

  fn into_result(self) -> (Vec<&'l st::ADTSort<'l>>, Vec<&'l st::FunDef<'l>>) {
    self.with_extraction(|xt| {
      let adts: Vec<&st::ADTSort> = xt.adts.values().copied().collect();
      let functions: Vec<&st::FunDef> = xt.functions.values().copied().collect();
      (adts, functions)
    })
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
  fn factory(&self) -> &'l st::Factory {
    self.with_extraction(|xt| xt.factory)
  }

  /// Identifier mappings

  fn fresh_param_id(&mut self, index: usize) -> StainlessSymId<'l> {
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

  fn register_def(&mut self, def_id: DefId) -> StainlessSymId<'l> {
    let symbol_path = self.symbol_path_from_def_id(def_id);
    let name = symbol_path.last().unwrap().clone();

    self.with_extraction_mut(|xt| {
      let id = xt.fresh_id(name, symbol_path);
      assert!(xt.mapping.did_to_stid.insert(def_id, id).is_none());
      id
    })
  }

  fn register_hir(&mut self, hir_id: HirId, name: String) -> StainlessSymId<'l> {
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
  fn register_hir_alias(&mut self, hir_id: HirId, id: StainlessSymId<'l>) -> () {
    self.with_extraction_mut(|xt| {
      assert!(xt.mapping.hid_to_stid.insert(hir_id, id).is_none());
    })
  }

  #[inline]
  fn get_id_from_def(&self, def_id: DefId) -> Option<StainlessSymId<'l>> {
    self.with_extraction(|xt| xt.mapping.did_to_stid.get(&def_id).copied())
  }

  #[inline]
  #[allow(unused)]
  fn fetch_id_from_def(&self, def_id: DefId) -> StainlessSymId<'l> {
    self
      .get_id_from_def(def_id)
      .expect("No Stainless id registered for the given definition id")
  }

  /// ADTs and Functions

  fn add_adt(&mut self, id: StainlessSymId<'l>, adt: &'l st::ADTSort<'l>) {
    self.with_extraction_mut(|xt| {
      assert!(xt.adts.insert(id, adt).is_none());
    })
  }

  fn add_function_ref(&mut self, def_id: DefId) {
    self.with_extraction_mut(|xt| {
      assert!(xt.function_refs.insert(def_id));
    })
  }

  fn add_function(&mut self, id: StainlessSymId<'l>, fd: &'l st::FunDef<'l>) {
    self.with_extraction_mut(|xt| {
      assert!(xt.functions.insert(id, fd).is_none());
    })
  }

  /// Get a BodyExtractor for some item with a body (like a function)
  fn enter_body<T, F>(&mut self, hir_id: HirId, f: F) -> T
  where
    F: FnOnce(&mut BodyExtractor<'_, 'l, 'tcx>) -> T,
  {
    self.tcx.infer_ctxt().enter(|infcx| {
      // Note that upon its creation, BodyExtractor moves out our Extraction
      let mut bxtor = BodyExtractor::new(self, &infcx, hir_id);
      let result = f(&mut bxtor);
      // We reclaim the Extraction after the BodyExtractor's work is done
      self.extraction = bxtor.base.extraction;
      result
    })
  }

  /// Error reporting helpers

  fn unsupported<S: Into<MultiSpan>, M: Into<String>>(&self, span: S, msg: M) {
    let msg = msg.into();
    self
      .tcx
      .sess
      .span_err(span, format!("Unsupported tree: {}", msg).as_str());
  }
}

/// BodyExtractor is used to extract, for example, function bodies
struct BodyExtractor<'a, 'l, 'tcx: 'l> {
  base: BaseExtractor<'l, 'tcx>,
  hcx: hair::cx::Cx<'a, 'tcx>,
  tables: &'a TypeckTables<'tcx>,
  body: &'tcx hir::Body<'tcx>,
  dcx: DefContext<'l>,
}

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  fn new(
    base: &mut BaseExtractor<'l, 'tcx>,
    infcx: &'a InferCtxt<'a, 'tcx>,
    hir_id: HirId,
  ) -> Self {
    let tcx = base.tcx;
    let extraction = base.extraction.take();
    let base = BaseExtractor::new(
      tcx,
      extraction.expect("Waiting for another BodyExtractor to finish"),
    );

    // Set up HAIR context
    let hcx = hair::cx::Cx::new(infcx, hir_id);

    // Set up typing tables
    let def_id = tcx.hir().local_def_id(hir_id);
    assert!(tcx.has_typeck_tables(def_id));
    let tables = tcx.typeck_tables_of(def_id);

    // Fetch the body and the corresponding DefContext containing all bindings
    let body_id = tcx.hir().body_owned_by(hir_id);
    let body = tcx.hir().body(body_id);

    let mut bxtor = BodyExtractor {
      base: base,
      hcx,
      tables,
      body,
      dcx: DefContext::new(),
    };
    bxtor.populate_def_context();

    bxtor
  }

  #[inline]
  fn tcx(&self) -> TyCtxt<'tcx> {
    self.hcx.tcx()
  }

  #[inline]
  fn factory(&self) -> &'l st::Factory {
    self.base.factory()
  }

  fn fetch_var(&self, hir_id: HirId) -> &'l st::Variable<'l> {
    let span: Span = self.tcx().hir().span(hir_id);
    self
      .dcx
      .get_var(hir_id)
      .unwrap_or_else(|| unexpected(span, "unregistered variable"))
  }
}

fn unexpected<S: Into<MultiSpan>, M: Into<String>>(span: S, msg: M) -> ! {
  span_bug!(span, "Unexpected tree: {:?}", msg.into())
}
