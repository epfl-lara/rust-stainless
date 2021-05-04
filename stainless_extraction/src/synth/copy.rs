use super::*;

/// Synthetisation of a structural deep copy method for ADTs. Needed to
/// circumvent anti-aliasing problems for mutation.
impl<'a, 'l, 'tcx> Synth<'a, 'l, 'tcx> {
  /// Synthesise a deep copy method for an ADT type.
  pub fn get_or_create_copy_fn(
    &mut self,
    adt_id: &'l st::SymbolIdentifier<'l>,
  ) -> &'l st::SymbolIdentifier<'l> {
    self
      .base
      .with_extraction(|xt| xt.copy_fns.get(adt_id).map(|fd| fd.id))
      .unwrap_or_else(|| {
        let fd = self.create_copy_fn(adt_id);
        self.base.add_function(fd);
        self
          .base
          .with_extraction_mut(|xt| xt.copy_fns.insert(adt_id, fd));
        fd.id
      })
  }

  fn create_copy_fn(&mut self, adt_id: &'l st::SymbolIdentifier<'l>) -> &'l st::FunDef<'l> {
    let f = self.factory();
    let sort = self.base.get_adt(adt_id).unwrap();

    let fn_id = self.base.fresh_id("_copy".into());
    let tps: Vec<_> = sort.tparams.iter().map(|tpd| tpd.tp.into()).collect();
    let adt_tpe = f.ADTType(adt_id, tps.clone()).into();

    let param = &*f.Variable(
      self.base.fresh_id("arg".into()),
      adt_tpe,
      vec![f.IsPure().into()],
    );

    let body = self.copy_pattern_match(adt_id, &tps, fn_id, param.into());

    f.FunDef(
      fn_id,
      sort.tparams.clone(),
      vec![&*f.ValDef(param)],
      adt_tpe,
      body,
      vec![
        f.IsPure().into(),
        f.Synthetic().into(),
        f.InlineOnce().into(),
      ],
    )
  }

  fn copy_pattern_match(
    &mut self,
    adt_id: &'l st::SymbolIdentifier<'l>,
    adt_tps: &[st::Type<'l>],
    fn_id: &'l st::SymbolIdentifier<'l>,
    to_copy: st::Expr<'l>,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let sort = self.base.get_adt(adt_id).unwrap();

    f.MatchExpr(
      to_copy,
      sort
        .constructors
        .iter()
        .map(|st::ADTConstructor { id, fields, .. }| {
          &*f.MatchCase(
            f.InstanceOfPattern(None, f.ADTType(id, adt_tps.to_vec()).into())
              .into(),
            None,
            f.ADT(
              id,
              adt_tps.to_vec(),
              fields
                .iter()
                .map(|st::ValDef { v }| match v.tpe {
                  st::Type::ADTType(st::ADTType { id, tps }) => {
                    let copy_id = if *id == adt_id {
                      fn_id
                    } else {
                      self.get_or_create_copy_fn(id)
                    };
                    f.FunctionInvocation(
                      copy_id,
                      tps.to_vec(),
                      vec![f.ADTSelector(to_copy.into(), v.id).into()],
                    )
                    .into()
                  }
                  _ => f.ADTSelector(to_copy.into(), v.id).into(),
                })
                .collect(),
            )
            .into(),
          )
        })
        .collect(),
    )
    .into()
  }
}
