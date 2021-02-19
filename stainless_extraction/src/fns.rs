use super::*;

use rustc_hir::def_id::DefId;

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
