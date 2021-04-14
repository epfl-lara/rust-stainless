use super::*;

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
      (MapRemovedFn, [map, key]) => self.extract_map_removed(*map, *key, substs_ref, span),
      (MapGetFn, [map, key]) => self.factory().MapApply(*map, *key).into(),
      (MapUpdatedFn, [map, key, val]) => {
        self.extract_map_updated(*map, *key, *val, substs_ref, span)
      }
      (MapGetOrElseFn, [map, key, or_else]) => {
        self.extract_map_get_or_else(*map, *key, *or_else, substs_ref, span)
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

    match &tps[..] {
      [key_tpe, val_tpe] => f
        .FiniteMap(
          vec![],
          self.base.std_option_none(*val_tpe),
          *key_tpe,
          self.base.std_option_type(*val_tpe),
        )
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
    let val_tpe = self.base.extract_ty(substs.type_at(1), &self.txtcx, span);
    let some_tpe = self.base.std_option_some_type(val_tpe);
    f.Assert(
      f.IsInstanceOf(f.MapApply(map, key).into(), some_tpe).into(),
      Some("Map undefined at this index".into()),
      self
        .base
        .std_option_some_value(f.AsInstanceOf(f.MapApply(map, key).into(), some_tpe).into()),
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
    let val_tpe = self.base.extract_ty(substs.type_at(1), &self.txtcx, span);
    f.Not(
      f.Equals(
        f.MapApply(map, key).into(),
        self.base.std_option_none(val_tpe),
      )
      .into(),
    )
    .into()
  }

  fn extract_map_removed(
    &mut self,
    map: st::Expr<'l>,
    key: st::Expr<'l>,
    substs: SubstsRef<'tcx>,
    span: Span,
  ) -> st::Expr<'l> {
    let val_tpe = self.base.extract_ty(substs.type_at(1), &self.txtcx, span);
    self
      .factory()
      .MapUpdated(map, key, self.base.std_option_none(val_tpe))
      .into()
  }

  fn extract_map_updated(
    &mut self,
    map: st::Expr<'l>,
    key: st::Expr<'l>,
    val: st::Expr<'l>,
    substs: SubstsRef<'tcx>,
    span: Span,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let val_tpe = self.base.extract_ty(substs.type_at(1), &self.txtcx, span);
    f.MapUpdated(map, key, self.base.std_option_some(val, val_tpe))
      .into()
  }

  fn extract_map_get_or_else(
    &mut self,
    map: st::Expr<'l>,
    key: st::Expr<'l>,
    or_else: st::Expr<'l>,
    substs: SubstsRef<'tcx>,
    span: Span,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let val_tpe = self.base.extract_ty(substs.type_at(1), &self.txtcx, span);
    let some_tpe = self.base.std_option_some_type(val_tpe);
    f.IfExpr(
      f.IsInstanceOf(f.MapApply(map, key).into(), some_tpe).into(),
      self
        .base
        .std_option_some_value(f.AsInstanceOf(f.MapApply(map, key).into(), some_tpe).into()),
      or_else,
    )
    .into()
  }
}
