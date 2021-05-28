use super::*;

use rustc_hir::def_id::DefId;
use rustc_middle::ty::FnSig;

use stainless_data::ast as st;

/// Internal data type representing functions in either impl blocks, traits or
/// top-level scope. It can be seen as the subset of the common fields of
/// `rustc_hir::ItemKind::Fn` and `rustc_hir::ImplItemKind::Fn`/
/// `rustc_hir::TraitItemKind::Fn` that we need for extraction.
#[derive(Copy, Clone, Debug)]
pub struct FnItem<'a> {
  pub def_id: DefId,
  pub fd_id: &'a st::SymbolIdentifier<'a>,
  pub is_abstract: bool,
}

impl<'a> FnItem<'a> {
  pub fn new(def_id: DefId, fd_id: &'a st::SymbolIdentifier<'a>, is_abstract: bool) -> Self {
    FnItem {
      def_id,
      fd_id,
      is_abstract,
    }
  }
}

/// This does not include the tparams as they are contained in the generics.
#[derive(Clone, Debug)]
pub struct FnSignature<'l> {
  pub id: &'l st::SymbolIdentifier<'l>,
  pub tparams: Vec<&'l st::TypeParameterDef<'l>>,
  pub params: Params<'l>,
  pub return_tpe: st::Type<'l>,
  pub is_pure: bool,
}

impl<'l, 'tcx> BaseExtractor<'l, 'tcx> {
  /// Panics if the given HirId does not own a HIR body.
  pub(super) fn hir_body(&self, hir_id: HirId) -> &'tcx hir::Body<'tcx> {
    let body_id = self.tcx.hir().body_owned_by(hir_id);
    self.tcx.hir().body(body_id)
  }

  pub(super) fn ty_fn_sig(&self, def_id: DefId) -> FnSig<'tcx> {
    let poly_fn_sig = self.tcx.fn_sig(def_id);
    self.tcx.liberate_late_bound_regions(def_id, poly_fn_sig)
  }
}

/// Identifies the specific implementation/instance of a type class that is
/// needed at a method call site.
#[derive(Debug, Eq, PartialEq)]
pub struct TypeClassKey<'l> {
  /// If this is a trait method, then the identifier is the id of the trait
  /// where the method is defined. Otherwise, of the impl where it is defined.
  pub id: &'l st::SymbolIdentifier<'l>,

  /// The first element of the vector is the receiver type (aka the type of the
  /// first argument at call site). Additional types are optional type
  /// parameters of the receiver type.
  pub recv_tps: Vec<st::Type<'l>>,
}

impl PartialEq<st::ClassType<'_>> for TypeClassKey<'_> {
  fn eq(&self, other: &st::ClassType<'_>) -> bool {
    let st::ClassType { id, tps } = other;
    self.id == *id && self.recv_tps == *tps
  }
}
