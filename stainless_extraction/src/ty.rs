use super::*;

use rustc_hir::Mutability;
use rustc_middle::ty::{
  AdtDef, GenericParamDef, GenericParamDefKind, IntTy, Predicate, PredicateKind, TraitRef, Ty,
  TyKind, UintTy,
};
use rustc_span::{Span, DUMMY_SP};

use std::collections::BTreeMap;

use stainless_data::ast as st;

/// Extraction of types

/// A Stainless type parameter or a type to replace it with
#[derive(Clone, Debug)]
pub(super) enum TyParam<'l> {
  Replaced(st::Type<'l>),
  Extracted(&'l st::TypeParameter<'l>),
}

impl<'a, 'l> From<&'a TyParam<'l>> for st::Type<'l> {
  fn from(tparam: &'a TyParam<'l>) -> st::Type<'l> {
    match tparam {
      TyParam::Replaced(tpe) => *tpe,
      TyParam::Extracted(tparam) => (*tparam).into(),
    }
  }
}

#[derive(Clone, Debug)]
pub(super) struct TyExtractionCtxt<'l> {
  /// The DefId of the surrounding item,
  pub(super) def_id: DefId,

  /// A mapping from parameter indices to stainless type parameters, this should
  /// be ordered by the index, hence BTreeMap.
  pub(super) index_to_tparam: BTreeMap<u32, TyParam<'l>>,
}

#[derive(Debug, Clone)]
pub(super) struct Generics<'l> {
  pub tparams: Vec<&'l st::TypeParameterDef<'l>>,
  pub txtcx: TyExtractionCtxt<'l>,
  pub trait_bounds: Vec<&'l st::ClassType<'l>>,
}

/// The width in bits of a pointer and hence also usize/isize. See [here for more details](
/// https://rust-lang.github.io/unsafe-code-guidelines/layout/scalars.html?highlight=usize#isize-and-usize)
#[inline]
fn pointer_bit_width(tcx: TyCtxt<'_>) -> u64 {
  tcx.data_layout.pointer_size.bits()
}

/// Get the bit width of an integer type (signed) which is either the hardcoded
/// `bit_width` in `ast` or the width of an isize, see [pointer_bit_width()].
#[inline]
pub fn int_bit_width(int_ty: &IntTy, tcx: TyCtxt<'_>) -> u64 {
  int_ty.bit_width().unwrap_or_else(|| pointer_bit_width(tcx))
}

/// Get the bit width of an integer type (unsigned) which is either the hardcoded
/// `bit_width` in `ast` or the width of an usize, see [pointer_bit_width()].
#[inline]
pub fn uint_bit_width(int_ty: &UintTy, tcx: TyCtxt<'_>) -> u64 {
  int_ty.bit_width().unwrap_or_else(|| pointer_bit_width(tcx))
}

impl<'l, 'tcx> BaseExtractor<'l, 'tcx> {
  pub(super) fn extract_ty(
    &mut self,
    ty: Ty<'tcx>,
    txtcx: &TyExtractionCtxt<'l>,
    span: Span,
  ) -> st::Type<'l> {
    let f = self.factory();
    match ty.kind() {
      TyKind::Bool => f.BooleanType().into(),

      // Integer types
      TyKind::Adt(adt_def, _) if self.is_bigint(adt_def) => f.IntegerType().into(),
      TyKind::Int(int_ty) => f
        .BVType(true, int_bit_width(int_ty, self.tcx) as i32)
        .into(),
      TyKind::Uint(uint_ty) => f
        .BVType(false, uint_bit_width(uint_ty, self.tcx) as i32)
        .into(),

      TyKind::Tuple(..) => {
        let arg_tps = self.extract_tys(ty.tuple_fields(), txtcx, span);
        match arg_tps.len() {
          0 => f.UnitType().into(),
          _ => self.synth().tuple_type(arg_tps),
        }
      }

      // Box type – We erase the indirection the box provides and replace it by
      // the contained type.
      TyKind::Adt(adt_def, substitutions) if adt_def.is_box() => {
        self.extract_ty(substitutions.type_at(0), txtcx, span)
      }

      // Immutably borrowed string slice, erased to a plain String
      TyKind::Ref(_, ty, Mutability::Not) if *ty.kind() == TyKind::Str => f.StringType().into(),

      // "Real" ADTs
      TyKind::Adt(adt_def, substitutions) => match self.std_items.def_to_item_opt(adt_def.did) {
        // Stainless types
        Some(StdItem::CrateItem(CrateItem::SetType)) => {
          let arg_ty = self.extract_ty(substitutions.type_at(0), txtcx, span);
          f.SetType(arg_ty).into()
        }
        Some(StdItem::CrateItem(CrateItem::MapType)) => {
          let arg_tps = self.extract_tys(substitutions.types(), txtcx, span);
          match &arg_tps[..] {
            [key_tpe, val_tpe] => f
              .MapType(*key_tpe, self.synth().std_option_type(*val_tpe))
              .into(),
            _ => {
              self.unsupported(
                span,
                format!("Cannot extract map type with arguments {:?}", arg_tps),
              );
              f.Untyped().into()
            }
          }
        }

        // String type
        Some(StdItem::CrateItem(CrateItem::StringType)) => f.StringType().into(),

        // Normal user-defined ADTs
        _ => {
          let sort_id = self.get_or_register_def(adt_def.did);
          let arg_tps = self.extract_tys(substitutions.types(), txtcx, span);
          f.ADTType(sort_id, arg_tps).into()
        }
      },

      // Immutable references
      TyKind::Ref(_, ty, Mutability::Not) => self.extract_ty(ty, txtcx, span),

      TyKind::Param(param_ty) => txtcx
        .index_to_tparam
        .get(&param_ty.index)
        .expect("Missing type parameter identifier")
        .into(),

      _ => {
        self.unsupported(span, format!("Cannot extract type {:?}", ty.kind()));
        f.Untyped().into()
      }
    }
  }

  pub(super) fn extract_tys<I>(
    &mut self,
    tys: I,
    txtcx: &TyExtractionCtxt<'l>,
    span: Span,
  ) -> Vec<st::Type<'l>>
  where
    I: IntoIterator<Item = Ty<'tcx>>,
  {
    tys
      .into_iter()
      .map(|ty| self.extract_ty(ty, txtcx, span))
      .collect()
  }

  /// Get the extracted generics for a def_id, usually a function or a class.
  /// Generics are cached after the first computation.
  pub(super) fn get_or_extract_generics(&mut self, def_id: DefId) -> Generics<'l> {
    let id = self.get_or_register_def(def_id);

    // We currently have to clone the generics because they otherwise mutably
    // borrow the entire `self`. Maybe there is a better way of doing this?
    self
      .with_extraction(|xt| xt.generics.get(id).cloned())
      .unwrap_or_else(|| {
        let gen = self.extract_generics(def_id);
        self.with_extraction_mut(|xt| xt.generics.entry(id).or_insert(gen).clone())
      })
  }

  fn extract_generics(&mut self, def_id: DefId) -> Generics<'l> {
    let f = self.factory();
    let tcx = self.tcx;
    let span = tcx.span_of_impl(def_id).unwrap_or(DUMMY_SP);
    let generics = tcx.generics_of(def_id);
    let predicates = tcx.predicates_defined_on(def_id);

    // Closures are strange. We just sanity check against the compiler internal type parameters
    // and don't try to extract anything else.
    if tcx.is_closure(def_id) {
      assert_eq!(generics.count(), 3);
      assert_eq!(predicates.predicates.len(), 0);
      return Generics {
        tparams: vec![],
        txtcx: TyExtractionCtxt {
          def_id,
          index_to_tparam: BTreeMap::new(),
        },
        trait_bounds: vec![],
      };
    }

    // Discovering HOF parameters.
    // We extract `F: Fn*(S) -> T` trait predicates by replacing `F` by `S => T`.
    // We also enumerate all other predicates, complain about all but the obviously innocuous ones.
    let mut tparam_to_fun_params: HashMap<u32, (Vec<Ty<'tcx>>, Span)> = HashMap::new();
    let mut tparam_to_fun_return: HashMap<u32, (Ty<'tcx>, Span)> = HashMap::new();
    let mut trait_bounds: HashSet<(TraitRef<'tcx>, Span)> = HashSet::new();

    for (predicate, span) in self.all_predicates_of(def_id).iter() {
      match predicate.kind().skip_binder() {
        PredicateKind::Trait(ref data, _) => {
          let trait_ref = data.trait_ref;
          let trait_did = trait_ref.def_id;
          // Certain unextracted traits we don't complain about at all.
          if self.std_items.is_sized_trait(trait_did) {
            continue;
          }

          if let TyKind::Param(param_ty) = trait_ref.self_ty().kind() {
            let param_def = generics.type_param(&param_ty, tcx);

            // Extract as a closure, supersedes trait bound
            if self.std_items.is_fn_like_trait(trait_did) {
              let params_ty = trait_ref.substs[1].expect_ty();
              let param_tys = params_ty.tuple_fields().collect();
              assert!(tparam_to_fun_params
                .insert(param_def.index, (param_tys, *span))
                .is_none());
              continue;
            }
            // Extract as a trait bound
            else {
              assert!(trait_bounds.insert((trait_ref, *span)));
              continue;
            }
          }
        }
        PredicateKind::Projection(ref data) => {
          let trait_ref = data.projection_ty.trait_ref(tcx);
          let trait_did = trait_ref.def_id;

          if let TyKind::Param(param_ty) = trait_ref.self_ty().kind() {
            let param_def = generics.type_param(&param_ty, tcx);
            if self.std_items.is_fn_like_trait(trait_did) {
              let return_ty = data.ty;
              assert!(tparam_to_fun_return
                .insert(param_def.index, (return_ty, *span))
                .is_none());
              continue;
            }
          }
        }
        _ => {}
      }
      // falling through in all other cases: warn
      tcx
        .sess
        .span_warn(*span, "Ignored predicate during extraction");
    }

    // Extract TypeParameterDefs for all normal generic parameters (ignoring HOF parameters)
    let index_to_tparam: BTreeMap<u32, TyParam<'l>> = all_generic_params_of(tcx, def_id)
      .iter()
      .filter_map(|param| match &param.kind {
        GenericParamDefKind::Type { .. } => {
          if tparam_to_fun_params.contains_key(&param.index) {
            None
          } else {
            let id = self.get_or_register_def(param.def_id);
            // TODO: Extract flags on type parameters
            let flags = vec![];
            let tparam = TyParam::Extracted(f.TypeParameter(id, flags));
            Some((param.index, tparam))
          }
        }
        kind => {
          self.unsupported(
            span,
            format!(
              "Cannot extract generic parameter {} of kind: {:#?}",
              tcx.def_path_str(param.def_id),
              kind
            ),
          );
          None
        }
      })
      .collect();

    // Create a half-completed TyExtractionCtxt
    let mut txtcx = TyExtractionCtxt {
      def_id,
      index_to_tparam,
    };

    // For each HOF parameter found above, record the corresponding generic parameter as
    // being replaced by the recovered function type.
    for (param_index, (param_tys, span)) in tparam_to_fun_params {
      let param_tps = self.extract_tys(param_tys, &txtcx, span);
      let (return_ty, span) = tparam_to_fun_return
        .remove(&param_index)
        .expect("Got parameter types for HOF parameter but no return type");
      let return_tpe = self.extract_ty(return_ty, &txtcx, span);
      let tparam = TyParam::Replaced(f.FunctionType(param_tps, return_tpe).into());
      assert!(txtcx.index_to_tparam.insert(param_index, tparam).is_none());
    }

    // Convert trait refs in the trait bounds to types
    let trait_bounds: Vec<&'l st::ClassType<'l>> = trait_bounds
      .iter()
      .filter_map(|&(ty::TraitRef { def_id, substs }, span)| {
        // Erase Clone trait bound
        (!matches!(
          self.std_items.def_to_item_opt(def_id),
          Some(StdItem::CrateItem(CrateItem::CloneTrait))
        ))
        .then(|| {
          let trait_id = self.get_or_register_def(def_id);
          let tps = self.extract_tys(substs.types(), &txtcx, span);
          &*f.ClassType(trait_id, tps)
        })
      })
      .collect();

    // And we're done.
    Generics {
      tparams: txtcx
        .index_to_tparam
        .values()
        .filter_map(|tparam| match tparam {
          TyParam::Replaced(_) => None,
          TyParam::Extracted(tparam) => Some(&*f.TypeParameterDef(tparam)),
        })
        .collect(),
      txtcx,
      trait_bounds,
    }
  }

  fn all_predicates_of(&self, def_id: DefId) -> Vec<&(Predicate<'tcx>, Span)> {
    let predicates = self.tcx.predicates_defined_on(def_id);
    let mut all_predicates = vec![predicates];
    while let Some(parent_id) = all_predicates.last().and_then(|g| g.parent) {
      all_predicates.push(self.tcx.predicates_defined_on(parent_id));
    }
    all_predicates
      .iter()
      .rev()
      .flat_map(|pred| pred.predicates)
      .collect()
  }

  // Various helpers

  #[inline]
  pub(super) fn is_bv_type(&self, ty: Ty<'tcx>) -> bool {
    matches!(ty.kind(), TyKind::Int(_) | TyKind::Uint(_))
  }

  #[inline]
  pub(super) fn is_signed_bv_type(&self, ty: Ty<'tcx>) -> bool {
    matches!(ty.kind(), TyKind::Int(_))
  }

  pub(super) fn is_bigint_type(&self, ty: Ty<'tcx>) -> bool {
    match ty.kind() {
      TyKind::Adt(adt_def, _) => self.is_bigint(adt_def),
      _ => false,
    }
  }

  pub(super) fn is_bigint(&self, adt_def: &'tcx AdtDef) -> bool {
    // TODO: Add a check for BigInt that avoids generating the string?
    self.tcx.def_path_str(adt_def.did) == "num_bigint::BigInt"
  }
}

// Generics helpers

pub fn all_generic_params_of(tcx: TyCtxt<'_>, def_id: DefId) -> Vec<&GenericParamDef> {
  let generics = tcx.generics_of(def_id);
  let mut all_generics = vec![generics];
  while let Some(parent_id) = all_generics.last().and_then(|g| g.parent) {
    all_generics.push(tcx.generics_of(parent_id));
  }
  all_generics
    .iter()
    .rev()
    .flat_map(|generics| generics.params.iter())
    .collect()
}
