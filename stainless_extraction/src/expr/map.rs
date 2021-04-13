use super::*;

use crate::syn::SyntheticItem::*;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_map_expr(
    &mut self,
    item: CrateItem,
    args: &'a [Expr<'a, 'tcx>],
    substs_ref: SubstsRef<'tcx>,
    span: Span,
  ) -> st::Expr<'l> {
    match (item, &self.extract_exprs(args)[..]) {
      (MapEmptyFn, []) => self.extract_map_creation(substs_ref, span),

      (MapApplyFn, [map, key]) => self.extract_map_apply(*map, *key, substs_ref, span),
      (MapContainsFn, [map, key]) => self.extract_map_contains(*map, *key, substs_ref, span),

      (op, _) if matches!(op, MapGetFn | MapUpdatedFn | MapRemovedFn) => {
        unimplemented!("{:?} not yet implemented", op)
      }
      (op, _) => self.unsupported_expr(
        span,
        format!("Cannot extract {:?} with {} arguments.", op, args.len()),
      ),
    }
  }

  fn extract_map_creation(&mut self, substs: SubstsRef<'tcx>, span: Span) -> st::Expr<'l> {
    let f = self.factory();
    let tps = self.base.extract_tys(substs.types(), &self.txtcx, span);
    let default = f.ADT(
      self.base.get_or_create_syn_item(MapValueAbsent),
      tps.clone(),
      vec![],
    );
    match &tps[..] {
      [kty, vty] => self
        .factory()
        .FiniteMap(vec![], default.into(), *kty, *vty)
        .into(),
      _ => unreachable!(),
    }
  }

  fn extract_map_apply(
    &mut self,
    map: st::Expr<'l>,
    key: st::Expr<'l>,
    substs: SubstsRef<'tcx>,
    span: Span,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let key_tpe = self.base.extract_ty(substs.type_at(1), &self.txtcx, span);
    let present_tpe = f
      .ADTType(
        self.base.get_or_create_syn_item(MapValuePresent),
        vec![key_tpe],
      )
      .into();

    f.Assert(
      f.IsInstanceOf(f.MapApply(map, key).into(), present_tpe)
        .into(),
      Some("Map undefined at this index".into()),
      f.ClassSelector(
        f.AsInstanceOf(f.MapApply(map, key).into(), present_tpe)
          .into(),
        self.base.get_or_create_syn_item(MapValuePresentField),
      )
      .into(),
    )
    .into()
  }

  fn extract_map_contains(
    &mut self,
    map: st::Expr<'l>,
    key: st::Expr<'l>,
    substs: SubstsRef<'tcx>,
    span: Span,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let key_tpe = self.base.extract_ty(substs.type_at(1), &self.txtcx, span);

    f.Not(
      f.Equals(
        f.MapApply(map, key).into(),
        f.ADT(
          self.base.get_or_create_syn_item(MapValueAbsent),
          vec![key_tpe],
          vec![],
        )
        .into(),
      )
      .into(),
    )
    .into()
  }
}
