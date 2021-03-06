#![feature(rustc_private)]
#![feature(box_patterns)]
#![feature(bool_to_option)]

#[macro_use]
extern crate lazy_static;

extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_hir_pretty;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_ty_utils;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use rustc_hir::def_id::{DefId, LocalDefId};
use rustc_hir::{self as hir, HirId};
use rustc_middle::mir::Mutability;
use rustc_middle::span_bug;
use rustc_middle::ty::{TyCtxt, TypeckResults, WithOptConstParam};
use rustc_mir_build::thir;
use rustc_span::{MultiSpan, Span};

use stainless_data::ast as st;

use bindings::DefContext;
use fns::TypeClassKey;
use stainless_data::ast::Type;
use std_items::{CrateItem, StdItem, StdItems};
use synth::SynthItem;
use ty::{Generics, TyExtractionCtxt};
use utils::UniqueCounter;

mod bindings;
mod classes;
mod expr;
mod flags;
mod fns;
mod krate;
mod std_items;
mod synth;
mod ty;
mod utils;

/// The entrypoint into extraction
pub fn extract_crate<'l, 'tcx: 'l>(
  tcx: TyCtxt<'tcx>,
  factory: &'l st::Factory,
  crate_name: String,
) -> st::Symbols<'l> {
  let extraction = Box::new(Extraction::new(factory));
  let std_items = Rc::new(StdItems::collect(tcx));
  let mut xtor = BaseExtractor::new(tcx, std_items, extraction);
  xtor.process_crate(crate_name);

  let symbols = xtor.into_symbols();

  // Output extracted Stainless program
  eprintln!("[ Extracted items ]");
  symbols.sorts.values().for_each(|adt| {
    eprintln!(" - ADT        {}", adt.id);
  });
  symbols.functions.values().for_each(|fd| {
    eprintln!(" - Function   {}", fd.id);
  });
  symbols.classes.values().for_each(|cd| {
    eprintln!(" - Type class {}", cd.id);
  });
  eprintln!();

  symbols
}

/// Helpful type aliases
type StainlessSymId<'l> = &'l st::SymbolIdentifier<'l>;
type Params<'l> = Vec<&'l st::ValDef<'l>>;

/// A mapping between Rust ids and Stainless ids
#[derive(Default)]
struct SymbolMapping<'l> {
  global_id_counter: UniqueCounter<()>,
  local_id_counter: UniqueCounter<String>,
  did_to_stid: HashMap<DefId, StainlessSymId<'l>>,
  hid_to_stid: HashMap<HirId, StainlessSymId<'l>>,
  synth_to_stid: HashMap<SynthItem, StainlessSymId<'l>>,
}

/// Extraction encapsulates the state of extracting a Stainless program
struct Extraction<'l> {
  mapping: SymbolMapping<'l>,
  factory: &'l st::Factory,
  adts: HashMap<StainlessSymId<'l>, &'l st::ADTSort<'l>>,
  function_refs: HashSet<DefId>,
  method_to_class: HashMap<StainlessSymId<'l>, &'l st::ClassDef<'l>>,
  generics: HashMap<StainlessSymId<'l>, Generics<'l>>,
  functions: HashMap<StainlessSymId<'l>, &'l st::FunDef<'l>>,
  classes: HashMap<StainlessSymId<'l>, &'l st::ClassDef<'l>>,
}

impl<'l> Extraction<'l> {
  fn new(factory: &'l st::Factory) -> Self {
    Self {
      factory,
      mapping: Default::default(),
      adts: Default::default(),
      function_refs: Default::default(),
      method_to_class: Default::default(),
      generics: Default::default(),
      functions: Default::default(),
      classes: Default::default(),
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
  std_items: Rc<StdItems>,
  extraction: Option<Box<Extraction<'l>>>,
}

fn sanitize_path_name(s: &str) -> String {
  // Remove forbidden characters
  let s = s.replace(&[' ', '<', '>'][..], "");
  assert!(
    !s.is_empty(),
    "Path name must have at least one alphanumeric character."
  );

  // Prepend an underscore to numerical-only identifiers for compatibility
  // with stainless
  if s.chars().all(char::is_numeric) {
    format!("_{}", s)
  } else {
    s
  }
}

impl<'l, 'tcx> BaseExtractor<'l, 'tcx> {
  fn new(tcx: TyCtxt<'tcx>, std_items: Rc<StdItems>, extraction: Box<Extraction<'l>>) -> Self {
    Self {
      tcx,
      std_items,
      extraction: Some(extraction),
    }
  }

  fn into_symbols(self) -> st::Symbols<'l> {
    self.with_extraction(|xt| {
      st::Symbols::new(
        xt.adts.values().copied().collect(),
        xt.functions.values().copied().collect(),
        xt.classes.values().copied().collect(),
      )
    })
  }

  #[inline]
  fn with_extraction<T, F: FnOnce(&Extraction<'l>) -> T>(&self, f: F) -> T {
    f(&**self.extraction.as_ref().expect("BodyExtractor active"))
  }

  #[inline]
  fn with_extraction_mut<T, F: FnOnce(&mut Extraction<'l>) -> T>(&mut self, f: F) -> T {
    f(self.extraction.as_mut().expect("BodyExtractor active"))
  }

  #[inline]
  fn factory(&self) -> &'l st::Factory {
    self.with_extraction(|xt| xt.factory)
  }

  /// Identifier mappings

  fn fresh_id(&mut self, name: String) -> StainlessSymId<'l> {
    self.with_extraction_mut(|xt| xt.fresh_id(name.clone(), vec![name]))
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
    self.register_def_with_path(def_id, self.symbol_path_from_def_id(def_id))
  }

  fn register_def_with_path(
    &mut self,
    def_id: DefId,
    symbol_path: Vec<String>,
  ) -> StainlessSymId<'l> {
    let path: Vec<String> = symbol_path.iter().map(|n| sanitize_path_name(n)).collect();
    let name = path.last().unwrap().clone();

    self.with_extraction_mut(|xt| {
      let id = xt.fresh_id(name, path);
      assert!(
        xt.mapping.did_to_stid.insert(def_id, id).is_none(),
        "A mapping for {:?} was already registered",
        def_id
      );
      id
    })
  }

  #[inline]
  fn get_id_from_def(&self, def_id: DefId) -> Option<StainlessSymId<'l>> {
    self.with_extraction(|xt| xt.mapping.did_to_stid.get(&def_id).copied())
  }

  fn get_or_register_def(&mut self, def_id: DefId) -> StainlessSymId<'l> {
    self
      .get_id_from_def(def_id)
      .unwrap_or_else(|| self.register_def(def_id))
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

  /// ADTs and Functions

  fn add_adt(&mut self, adt: &'l st::ADTSort<'l>) -> &'l st::ADTSort<'l> {
    self.with_extraction_mut(|xt| {
      assert!(xt.adts.insert(adt.id, adt).is_none());
    });
    adt
  }

  fn get_adt(&self, id: StainlessSymId<'l>) -> Option<&'l st::ADTSort<'l>> {
    self.with_extraction(|xt| xt.adts.get(id).copied())
  }

  fn add_function_ref(&mut self, def_id: DefId) {
    self.with_extraction_mut(|xt| {
      assert!(xt.function_refs.insert(def_id));
    })
  }

  fn add_function(&mut self, fd: &'l st::FunDef<'l>) {
    self.with_extraction_mut(|xt| {
      assert!(xt.functions.insert(fd.id, fd).is_none());
    })
  }

  /// Return a reference to the class definition on which the given function is
  /// defined as method. If the function is not a method, None is returned. The
  /// function is identified by its id.
  fn get_class_of_method(&mut self, id: StainlessSymId<'l>) -> Option<&'l st::ClassDef<'l>> {
    self.with_extraction(|xt| xt.method_to_class.get(&id).copied())
  }

  /// Add a class to the extraction along with references from all its methods
  /// to the class definition.
  fn add_class<I>(&mut self, cd: &'l st::ClassDef<'l>, methods: I)
  where
    I: IntoIterator<Item = StainlessSymId<'l>>,
  {
    self.with_extraction_mut(|xt| {
      assert!(xt.classes.insert(cd.id, cd).is_none());
      xt.method_to_class
        .extend(methods.into_iter().map(|method_id| (method_id, cd)));
    })
  }

  pub fn immutable_var_with_name(
    &mut self,
    var: &st::Variable<'l>,
    name: &str,
  ) -> &'l st::Variable<'l> {
    let new_id = self.fresh_id(name.into());
    let flags = var
      .flags
      .iter()
      .filter(|flag| !matches!(flag, st::Flag::IsVar(_)))
      .copied()
      .collect();
    self.factory().Variable(new_id, var.tpe, flags)
  }

  fn hir_to_def_id(&self, hir_id: HirId) -> DefId {
    self.tcx.hir().local_def_id(hir_id).to_def_id()
  }

  /// Returns None if the `def_id` is not local.
  fn ldef_to_hir_id(&self, ldi: LocalDefId) -> HirId {
    self.tcx.hir().local_def_id_to_hir_id(ldi)
  }

  /// Returns None if the `def_id` is not local.
  fn def_to_hir_id(&self, def_id: DefId) -> Option<HirId> {
    def_id.as_local().map(|ldi| self.ldef_to_hir_id(ldi))
  }

  /// Get a BodyExtractor for some item with a body (like a function)
  fn enter_body<T, F>(
    &mut self,
    hir_id: HirId,
    txtcx: TyExtractionCtxt<'l>,
    current_class: Option<&'l st::ClassDef<'l>>,
    f: F,
  ) -> T
  where
    F: FnOnce(&mut BodyExtractor<'_, 'l, 'tcx>) -> T,
  {
    let arena = thir::Arena::default();
    // Note that upon its creation, BodyExtractor moves out our Extraction
    let mut bxtor = BodyExtractor::new(self, &arena, hir_id, txtcx, current_class);
    let result = f(&mut bxtor);
    // We reclaim the Extraction after the BodyExtractor's work is done
    self.extraction = bxtor.base.extraction;
    result
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
  arena: &'a thir::Arena<'a, 'tcx>,
  base: BaseExtractor<'l, 'tcx>,
  tables: &'a TypeckResults<'tcx>,
  body: &'tcx hir::Body<'tcx>,
  txtcx: TyExtractionCtxt<'l>,
  dcx: DefContext<'l>,
  current_class: Option<&'l st::ClassDef<'l>>,
}

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  fn new(
    base: &mut BaseExtractor<'l, 'tcx>,
    arena: &'a thir::Arena<'a, 'tcx>,
    hir_id: HirId,
    txtcx: TyExtractionCtxt<'l>,
    current_class: Option<&'l st::ClassDef<'l>>,
  ) -> Self {
    let tcx = base.tcx;
    let extraction = base.extraction.take();
    let base = BaseExtractor::new(
      tcx,
      base.std_items.clone(),
      extraction.expect("Waiting for another BodyExtractor to finish"),
    );

    // Set up typing tables and signature
    let local_def_id = tcx.hir().local_def_id(hir_id);
    assert!(tcx.has_typeck_results(local_def_id));
    let tables = tcx.typeck(local_def_id);
    let body = base.hir_body(hir_id);

    BodyExtractor {
      arena,
      base,
      tables,
      body,
      txtcx,
      dcx: DefContext::default(),
      current_class,
    }
  }

  #[inline]
  fn tcx(&self) -> TyCtxt<'tcx> {
    self.base.tcx
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

  fn return_tpe(&mut self) -> st::Type<'l> {
    let hir_id = self.base.ldef_to_hir_id(self.tables.hir_owner);
    let span = self.tcx().hir().span(hir_id);
    let sig = self.base.ty_fn_sig(self.tables.hir_owner.to_def_id());
    self.base.extract_ty(sig.output(), &self.txtcx, span)
  }

  fn extract_body_expr(&mut self, ldi: LocalDefId) -> st::Expr<'l> {
    let body_expr = thir::build_thir(
      self.tcx(),
      WithOptConstParam::unknown(ldi),
      self.arena,
      &self.body.value,
    );
    self.extract_expr(body_expr)
  }

  /// Extracts the receiver object/instance of a method call for a type class.
  /// In a way, this function retrofits Scala's implicit object lookup to Rust's
  /// completely invisible trait implementation lookup.
  pub fn extract_method_receiver(&self, key: &TypeClassKey<'l>) -> Option<st::Expr<'l>> {
    let f = self.factory();

    // First, check whether the receiver is the current class aka 'this'
    self
      .extract_this_call(key)
      // then, check whether the receiver is one of the evidence args of the current class
      .or_else(|| self.extract_evidence_arg_call(key))
      // otherwise, go through the classes and search the receiver.
      .or_else(|| {
        let cls: Vec<_> = self
          .base
          .with_extraction(|xt| xt.classes.values().copied().collect());

        cls
          .iter()
          // The receiver can't be an  abstract class nor the current class.
          .filter(|cd| {
            !cd.flags.contains(&f.IsAbstract().into())
              && self.current_class.map_or(true, |tc| tc.id != cd.id)
          })
          .find_map(|cd| {
            let class_key = method_call_rcv_key(cd);

            match (class_key.recv_tps.as_slice(), key.recv_tps.as_slice()) {
              // If the class id is not the same, we're looking for the wrong type class.
              _ if class_key.id != key.id => None,

              // If the types match exactly, we can create a ground instance without evidence args.
              (ctps, ktps) if ctps == ktps => {
                Some(f.ClassConstructor(f.class_def_to_type(cd), vec![]).into())
              }

              // If the type arguments are of **the same** ADT type, we need to
              // match on _their_ type parameters too. Possibly recurse and find
              // the evidence arguments for the contained type parameters.
              (
                &[Type::ADTType(st::ADTType {
                  id: c_adt_id,
                  tps: ctps,
                }), ..],
                &[Type::ADTType(st::ADTType {
                  id: k_adt_id,
                  tps: ktps,
                }), ..],
              ) if c_adt_id == k_adt_id => {
                // Correlate the key type parameters with the ones from the class.
                let ctype_to_ktype: HashMap<Type<'l>, Type<'l>> =
                  ctps.clone().into_iter().zip(ktps.clone()).collect();

                // Recurse to find the evidence arguments in the fields. If
                // only one of them is not found, return None.
                let args: Option<Vec<_>> = cd
                  .fields
                  .iter()
                  .map(|v| self.find_evidence_arg(v, &ctype_to_ktype))
                  .collect::<Option<Vec<st::Expr<'l>>>>();

                args.map(|a| {
                  f.ClassConstructor(f.ClassType(cd.id, ktps.clone()), a)
                    .into()
                })
              }
              _ => None,
            }
          })
      })
  }

  // Find an evidence argument for a field of a type class. The fields usually
  // have generic types therefore a substitution to the actually needed types is
  // performed with the given map.
  fn find_evidence_arg(
    &self,
    field: &st::ValDef<'l>,
    type_substs: &HashMap<Type<'l>, Type<'l>>,
  ) -> Option<st::Expr<'l>> {
    let &st::ValDef {
      v: st::Variable { tpe, .. },
    } = field;

    match tpe {
      Type::ClassType(st::ClassType { id, tps }) => {
        // substitute the type parameters of the field with the actual types from the key
        tps
          .iter()
          .map(|t| type_substs.get(t).copied())
          .collect::<Option<Vec<Type>>>()
          // and recurse to find the argument
          .and_then(|recv_tps| self.extract_method_receiver(&TypeClassKey { id, recv_tps }))
      }
      _ => None,
    }
  }

  /// Tries to extract a call to 'this' as a method receiver, returns None if the
  /// key doesn't match the current class def aka 'this'.
  fn extract_this_call(&self, key: &TypeClassKey<'l>) -> Option<st::Expr<'l>> {
    let f = self.factory();
    self
      .current_class
      .filter(|cd| method_call_rcv_key(cd) == *key)
      .map(|cd| f.This(f.class_def_to_type(cd)).into())
  }

  /// Tries to extract an evidence argument of the current function or the current class.
  /// Returns None if no evidence argument matches the key.
  fn extract_evidence_arg_call(&self, key: &TypeClassKey<'l>) -> Option<st::Expr<'l>> {
    let f = self.factory();

    // First try the current functions arguments
    self
      .dcx
      .params()
      .iter()
      .find_map(|&vd| match vd.v.tpe {
        st::Type::ClassType(class_type) if key == class_type => Some(vd.v.into()),
        _ => None,
      })
      // then search for an evidence argument in the current class
      .or_else(|| {
        self.current_class.and_then(|cd| {
          cd.fields.iter().find_map(|&vd| match vd.v.tpe {
            st::Type::ClassType(class_type) if key == class_type => Some(
              f.ClassSelector(f.This(f.class_def_to_type(cd)).into(), vd.v.id)
                .into(),
            ),
            _ => None,
          })
        })
      })
  }
}

/// Find the type-class-key of the receiver of a method called on this class.
/// The receiver key is either:
///
/// 1. the trait this class implements along with the instantiated type params of the trait,
/// 2. if this class is a trait definition, it's the self type param aka its own key.
///
fn method_call_rcv_key<'l>(cd: &'l st::ClassDef<'l>) -> TypeClassKey {
  cd.parents
    .first()
    .map(|st::ClassType { id, tps }| TypeClassKey {
      id,
      recv_tps: tps.clone(),
    })
    .unwrap_or_else(|| TypeClassKey {
      id: cd.id,
      recv_tps: cd
        .tparams
        .iter()
        .map(|&&st::TypeParameterDef { tp }| tp.into())
        .collect::<Vec<_>>(),
    })
}

fn unexpected<S: Into<MultiSpan>, M: Into<String>>(span: S, msg: M) -> ! {
  span_bug!(span, "Unexpected tree: {:?}", msg.into())
}
