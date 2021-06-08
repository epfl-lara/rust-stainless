use super::*;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_map_expr(
    &mut self,
    item: CrateItem,
    args: &'a [Expr<'a, 'tcx>],
    substs: SubstsRef<'tcx>,
    span: Span,
  ) -> st::Expr<'l> {
    let (key_tpe, val_tpe) = match &self.base.extract_tys(substs.types(), &self.txtcx, span)[..] {
      [key_tpe, val_tpe] => (*key_tpe, *val_tpe),
      _ => {
        return self.unsupported_expr(
          span,
          format!("Cannot extract {:?} with {} types.", item, substs.len()),
        )
      }
    };

    match (item, &self.extract_exprs(args)[..]) {
      (MapNewFn, []) => self.extract_map_creation(key_tpe, val_tpe),

      (MapIndexFn, [map, key]) => self.extract_map_apply(*map, *key, val_tpe),
      (MapContainsKeyFn, [map, key]) => self.extract_map_contains(*map, *key, val_tpe),
      (MapRemoveFn, [map, key]) => self.extract_map_remove(*map, *key, val_tpe),
      (MapGetFn, [map, key]) => self.factory().MapApply(*map, *key).into(),
      (MapInsertFn, [map, key, val]) => self.extract_map_insert(*map, *key, val_tpe, *val),
      (MapGetOrFn, [map, key, or_else]) => self.extract_map_get_or(*map, *key, val_tpe, *or_else),

      (op, _) => self.unsupported_expr(
        span,
        format!("Cannot extract {:?} with {} arguments.", op, args.len()),
      ),
    }
  }

  fn extract_map_creation(&mut self, key_tpe: st::Type<'l>, val_tpe: st::Type<'l>) -> st::Expr<'l> {
    self
      .factory()
      .FiniteMap(
        vec![],
        self.synth().std_option_none(val_tpe),
        key_tpe,
        self.synth().std_option_type(val_tpe),
      )
      .into()
  }

  fn extract_map_apply(
    &mut self,
    map: st::Expr<'l>,
    key: st::Expr<'l>,
    val_tpe: st::Type<'l>,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let some_tpe = self.synth().std_option_some_type(val_tpe);
    f.Assert(
      f.IsInstanceOf(f.MapApply(map, key).into(), some_tpe).into(),
      Some("Map undefined at this index".into()),
      self
        .synth()
        .std_option_some_value(f.AsInstanceOf(f.MapApply(map, key).into(), some_tpe).into()),
    )
    .into()
  }

  fn extract_map_contains(
    &mut self,
    map: st::Expr<'l>,
    key: st::Expr<'l>,
    val_tpe: st::Type<'l>,
  ) -> st::Expr<'l> {
    let f = self.factory();
    f.Not(
      f.Equals(
        f.MapApply(map, key).into(),
        self.synth().std_option_none(val_tpe),
      )
      .into(),
    )
    .into()
  }

  fn extract_map_get_or(
    &mut self,
    map: st::Expr<'l>,
    key: st::Expr<'l>,
    val_tpe: st::Type<'l>,
    or_else: st::Expr<'l>,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let some_tpe = self.synth().std_option_some_type(val_tpe);
    f.IfExpr(
      f.IsInstanceOf(f.MapApply(map, key).into(), some_tpe).into(),
      self
        .synth()
        .std_option_some_value(f.AsInstanceOf(f.MapApply(map, key).into(), some_tpe).into()),
      or_else,
    )
    .into()
  }

  fn extract_map_insert(
    &mut self,
    map: st::Expr<'l>,
    key: st::Expr<'l>,
    val_tpe: st::Type<'l>,
    val: st::Expr<'l>,
  ) -> st::Expr<'l> {
    let update_value = self.synth().std_option_some(val, val_tpe);
    self.extract_map_update(map, key, update_value)
  }

  fn extract_map_remove(
    &mut self,
    map: st::Expr<'l>,
    key: st::Expr<'l>,
    val_tpe: st::Type<'l>,
  ) -> st::Expr<'l> {
    let update_value = self.synth().std_option_none(val_tpe);
    self.extract_map_update(map, key, update_value)
  }

  fn extract_map_update(
    &mut self,
    map: st::Expr<'l>,
    key: st::Expr<'l>,
    update_value: st::Expr<'l>,
  ) -> st::Expr<'l> {
    self.factory().MapUpdated(map, key, update_value).into()
  }
}
