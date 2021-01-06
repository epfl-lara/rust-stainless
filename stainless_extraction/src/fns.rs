use super::*;

use rustc_hir::def_id::DefId;
use rustc_span::symbol::Ident;

use stainless_data::ast as st;

use crate::spec::SpecType;

/// Internal data type representing functions in either impl blocks, traits or
/// top-level scope. It can be seen as the subset of the common fields of
/// `rustc_hir::ItemKind::Fn` and `rustc_hir::ImplItemKind::Fn`/
/// `rustc_hir::TraitItemKind::Fn` that we make use of.
///
#[derive(Copy, Clone, Debug)]
pub struct FnItem<'a> {
  pub def_id: DefId,
  pub fd_id: &'a st::SymbolIdentifier<'a>,
  pub span: Span,
  /// The type of the spec, if this is a spec function.
  pub spec_type: Option<SpecType>,

  /// The name of the corresponding function that is spec'd by this spec
  /// function, if this is a spec function.
  pub spec_fn_name: Option<Ident>,
  pub is_abstract: bool,
}

impl<'a> FnItem<'a> {
  /// Create a new FnItem by parsing its identifier and setting its spec
  /// function properties; if it's a spec function.
  ///
  /// Note that we currently don't store the identifier once it's parsed. This
  /// can be changed with a one-liner in the struct definition though.
  pub fn new(
    def_id: DefId,
    fd_id: &'a st::SymbolIdentifier<'a>,
    ident: Ident,
    span: Span,
    is_abstract: bool,
  ) -> Self {
    let (spec_type, spec_fn_name) = match SpecType::parse_spec_type_fn_name(&ident.as_str()) {
      Some((t, f)) => (Some(t), Some(Ident::from_str(&f))),
      None => (None, None),
    };

    FnItem {
      def_id,
      fd_id,
      span,
      is_abstract,
      spec_type,
      spec_fn_name,
    }
  }

  pub fn is_spec_fn(&self) -> bool {
    self.span.from_expansion() && self.spec_type.is_some()
  }
}

/// Identifies the specific implementation/instance of a type class that is
/// needed at a method call site.
///
/// 1) If this is a trait method, then the identifier is the id of the trait where
/// the method is defined. Otherwise, of the impl where it is defined.
///
/// 2) The type is the receiver type (aka the type of the first argument at call site).
/// 3) The other types are optional type parameters of the receiver type.
pub type TypeClassKey<'l> = (
  &'l st::SymbolIdentifier<'l>,
  st::Type<'l>,
  Vec<st::Type<'l>>,
);

pub type TypeClassInstances<'l> = HashMap<TypeClassKey<'l>, st::Expr<'l>>;
