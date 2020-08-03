use super::*;

use rustc_ast::ast;
use rustc_hir::lang_items::{
  FnMutTraitLangItem, FnOnceTraitLangItem, FnTraitLangItem, SizedTraitLangItem,
};
use rustc_middle::ty::{
  AdtDef, GenericParamDef, GenericParamDefKind, Generics, PredicateKind, Ty, TyKind,
};
use rustc_span::{Span, DUMMY_SP};

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
  /// A mapping from parameter indices to stainless type parameters
  pub(super) index_to_tparam: HashMap<u32, TyParam<'l>>,
}

impl<'l, 'tcx> BaseExtractor<'l, 'tcx> {
  pub(super) fn extract_ty(
    &mut self,
    ty: Ty<'tcx>,
    txtcx: &TyExtractionCtxt<'l>,
    span: Span,
  ) -> st::Type<'l> {
    let f = self.factory();
    match ty.kind {
      TyKind::Bool => f.BooleanType().into(),
      TyKind::Adt(adt_def, _) if self.is_bigint(adt_def) => f.IntegerType().into(),
      TyKind::Int(ast::IntTy::I32) => f.BVType(true, 32).into(),
      TyKind::Tuple(..) => {
        let arg_tps = self.extract_tys(ty.tuple_fields(), txtcx, span);
        match arg_tps.len() {
          0 => f.UnitType().into(),
          1 => {
            self.unsupported(span, "Cannot extract type of one-tuples");
            f.Untyped().into()
          }
          _ => f.TupleType(arg_tps).into(),
        }
      }
      TyKind::Adt(adt_def, substs) => {
        let sort = self.extract_adt(adt_def.did);
        let arg_tps = self.extract_tys(substs.types(), txtcx, span);
        f.ADTType(sort.id, arg_tps).into()
      }
      TyKind::Param(param_ty) => txtcx
        .index_to_tparam
        .get(&param_ty.index)
        .expect("Missing type parameter identifier")
        .into(),
      _ => {
        self.unsupported(span, format!("Cannot extract type {:?}", ty.kind));
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

  /// Generics

  pub(super) fn extract_generics(
    &mut self,
    def_id: DefId,
  ) -> (Vec<&'l st::TypeParameterDef<'l>>, TyExtractionCtxt<'l>) {
    let f = self.factory();
    let tcx = self.tcx;
    let span = tcx.span_of_impl(def_id).unwrap_or(DUMMY_SP);
    let generics = tcx.generics_of(def_id);
    let predicates = tcx.predicates_of(def_id);

    // Closures are strange. We just sanity check against the compiler internal type parameters
    // and don't try to extract anything else.
    if tcx.is_closure(def_id) {
      assert_eq!(generics.count(), 3);
      assert_eq!(predicates.predicates.len(), 0);
      return (
        vec![],
        TyExtractionCtxt {
          def_id,
          index_to_tparam: HashMap::new(),
        },
      );
    }

    // Extract all generic parameters

    let all_params = all_generic_params_of(tcx, def_id);

    // Certain unextracted traits we don't complain about at all.
    let should_silently_ignore_trait = |trait_did: DefId| {
      let check = |lang_item| trait_did == tcx.require_lang_item(lang_item, None);
      check(SizedTraitLangItem)
    };

    // Discovering HOF parameters.
    // We extract `F: Fn*(S) -> T` trait predicates by replacing `F` by `S => T`.
    // We also enumerate all other predicates, complain about all but the obviously innocuous ones.
    let mut tparam_to_fun_params: HashMap<u32, (Vec<Ty<'tcx>>, Span)> = HashMap::new();
    let mut tparam_to_fun_return: HashMap<u32, (Ty<'tcx>, Span)> = HashMap::new();
    for (predicate, span) in predicates.predicates {
      match predicate.kind() {
        PredicateKind::Trait(ref data, _) => {
          let trait_ref = data.skip_binder().trait_ref;
          let trait_did = trait_ref.def_id;
          if should_silently_ignore_trait(trait_did) {
            continue;
          }

          if let TyKind::Param(param_ty) = trait_ref.self_ty().kind {
            let param_def = generics.type_param(&param_ty, tcx);
            if self.is_fn_like_trait(trait_did) {
              let params_ty = trait_ref.substs[1].expect_ty();
              let param_tys = params_ty.tuple_fields().collect();
              assert!(tparam_to_fun_params
                .insert(param_def.index, (param_tys, *span))
                .is_none());
              continue;
            }
          }
        }
        PredicateKind::Projection(ref data) => {
          let trait_ref = data.skip_binder().projection_ty.trait_ref(tcx);
          let trait_did = trait_ref.def_id;

          if let TyKind::Param(param_ty) = trait_ref.self_ty().kind {
            let param_def = generics.type_param(&param_ty, tcx);
            if self.is_fn_like_trait(trait_did) {
              let return_ty = data.skip_binder().ty;
              assert!(tparam_to_fun_return
                .insert(param_def.index, (return_ty, *span))
                .is_none());
              continue;
            }
          }
        }
        _ => {}
      }
      tcx
        .sess
        .span_warn(*span, "Ignored predicate during extraction");
    }

    // Extract TypeParameterDefs for all normal generic parameters (ignoring HOF parameters)
    let index_to_tparam: HashMap<u32, TyParam<'l>> = all_params
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

    // And we're done.
    (
      txtcx
        .index_to_tparam
        .values()
        .filter_map(|tparam| match tparam {
          TyParam::Replaced(_) => None,
          TyParam::Extracted(tparam) => Some(&*f.TypeParameterDef(tparam)),
        })
        .collect(),
      txtcx,
    )
  }

  /// Various helpers

  pub(super) fn is_fn_like_trait(&self, def_id: DefId) -> bool {
    let tcx = self.tcx;
    let check = |lang_item| def_id == tcx.require_lang_item(lang_item, None);
    check(FnTraitLangItem) || check(FnMutTraitLangItem) || check(FnOnceTraitLangItem)
  }

  pub(super) fn is_bv_type(&self, ty: Ty<'tcx>) -> bool {
    match ty.kind {
      TyKind::Int(_) | TyKind::Uint(_) => true,
      _ => false,
    }
  }

  pub(super) fn is_signed_bv_type(&self, ty: Ty<'tcx>) -> bool {
    match ty.kind {
      TyKind::Int(_) => true,
      _ => false,
    }
  }

  pub(super) fn is_bigint_type(&self, ty: Ty<'tcx>) -> bool {
    match ty.kind {
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

fn all_generic_params_of(tcx: TyCtxt<'_>, def_id: DefId) -> Vec<&GenericParamDef> {
  let generics = tcx.generics_of(def_id);
  let mut all_generics: Vec<&Generics> = vec![generics];
  while let Some(parent_id) = all_generics.last().unwrap().parent {
    all_generics.push(tcx.generics_of(parent_id));
  }
  all_generics
    .iter()
    .rev()
    .flat_map(|generics| generics.params.iter().map(|param| param))
    .collect()
}
