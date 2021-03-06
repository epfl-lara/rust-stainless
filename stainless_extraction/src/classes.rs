use super::*;

use rustc_hir::def_id::DefId;
use rustc_middle::ty;
use rustc_span::Span;

use stainless_data::ast as st;

impl<'l, 'tcx> BaseExtractor<'l, 'tcx> {
  /// Extract a type class or type class implementation from a DefId.
  /// Case object implementations are also classes for stainless.
  ///
  pub(super) fn extract_class(
    &mut self,
    def_id: DefId,
    impl_trait_ref: Option<ty::TraitRef<'tcx>>,
    span: Span,
  ) -> &'l st::ClassDef<'l> {
    let f = self.factory();

    let id = self.get_or_register_def(def_id);
    let Generics {
      tparams,
      txtcx,
      trait_bounds,
    } = self.get_or_extract_generics(def_id);

    // If this is an 'impl for trait', we extract a concrete class
    if let Some(ty::TraitRef {
      def_id: trait_def_id,
      substs,
    }) = impl_trait_ref
    {
      // The parent (aka inherits from) the trait it implements
      let trait_id = self.get_or_register_def(trait_def_id);
      let tps = self.extract_tys(substs.types(), &txtcx, span);
      let parent = vec![&*f.ClassType(trait_id, tps)];

      let fields = self.evidence_params(trait_bounds);
      let flags = if tparams.is_empty() && fields.is_empty() {
        vec![f.IsCaseObject().into()]
      } else {
        vec![]
      };

      f.ClassDef(id, tparams, parent, fields, flags)
    }
    // Otherwise, we extract an abstract class from a trait
    else {
      f.ClassDef(
        id,
        tparams,
        // For a trait its parents are the trait bounds
        trait_bounds,
        // A trait doesn't have fields
        vec![],
        vec![f.IsAbstract().into()],
      )
    }
  }

  pub fn evidence_params<I>(&mut self, trait_bounds: I) -> Vec<&'l st::ValDef<'l>>
  where
    I: IntoIterator<Item = &'l st::ClassType<'l>>,
  {
    let f = self.factory();
    trait_bounds
      .into_iter()
      .enumerate()
      .map(|(index, ct)| {
        &*f.ValDef(f.Variable(
          self.fresh_id(format!("ev{}", index)),
          ct.into(),
          vec![f.evidence_flag()],
        ))
      })
      .collect()
  }
}
