use super::bindings::DefContext;
use super::*;

use rustc_ast::ast;
use rustc_middle::ty::{AdtDef, Ty, TyKind};
use rustc_span::Span;

use stainless_data::ast as st;

/// Extraction of types

impl<'l, 'tcx> BaseExtractor<'l, 'tcx> {
  pub(super) fn extract_ty(&self, ty: Ty<'tcx>, dctx: &DefContext<'l>, span: Span) -> st::Type<'l> {
    let f = self.factory();
    match ty.kind {
      TyKind::Bool => f.BooleanType().into(),
      TyKind::Adt(adt_def, _) if self.is_bigint(adt_def) => f.IntegerType().into(),
      TyKind::Int(ast::IntTy::I32) => f.BVType(true, 32).into(),
      TyKind::Tuple(..) => {
        let arg_tps = self.extract_tys(ty.tuple_fields(), dctx, span);
        if arg_tps.is_empty() {
          f.UnitType().into()
        } else {
          f.TupleType(arg_tps).into()
        }
      }
      _ => {
        self.unsupported(span, format!("Cannot extract type {:?}", ty.kind));
        f.Untyped().into()
      }
    }
  }

  fn extract_tys<I>(&self, tys: I, dctx: &DefContext<'l>, span: Span) -> Vec<st::Type<'l>>
  where
    I: IntoIterator<Item = Ty<'tcx>>,
  {
    tys
      .into_iter()
      .map(|ty| self.extract_ty(ty, dctx, span))
      .collect()
  }

  pub(super) fn is_bv_type(&self, ty: Ty<'tcx>) -> bool {
    match ty.kind {
      TyKind::Int(_) | TyKind::Uint(_) => true,
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

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_ty(&self, ty: Ty<'tcx>, span: Span) -> st::Type<'l> {
    self.base.extract_ty(ty, &self.dcx, span)
  }
}
