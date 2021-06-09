use super::*;

/// Synthetisation of Tuple ADTs
impl<'a, 'l, 'tcx> Synth<'a, 'l, 'tcx> {
  pub fn tuple_type(&mut self, tps: Vec<st::Type<'l>>) -> st::Type<'l> {
    self.factory().ADTType(self.tuple_id(tps.len()), tps).into()
  }

  pub fn tuple(&mut self, tps: Vec<st::Type<'l>>, args: Vec<st::Expr<'l>>) -> st::Expr<'l> {
    assert_eq!(
      tps.len(),
      args.len(),
      "Tuple types and arguments need to have the same length"
    );

    // Wrap args in MutCell
    let args = args
      .into_iter()
      .zip(tps.iter())
      .map(|(a, t)| self.mut_cell(*t, a))
      .collect();

    self
      .factory()
      .ADT(self.tuple_id(tps.len()), tps, args)
      .into()
  }

  pub fn tuple_select(&mut self, arity: usize, lhs: st::Expr<'l>, index: usize) -> st::Expr<'l> {
    self
      .factory()
      .ClassSelector(
        lhs,
        self.tuple_adt(arity).constructors[0].fields[index].v.id,
      )
      .into()
  }

  pub fn tuple_id(&mut self, arity: usize) -> StainlessSymId<'l> {
    self.tuple_adt(arity).id
  }

  fn tuple_adt(&mut self, arity: usize) -> &'l st::ADTSort<'l> {
    assert!(arity > 0, "Empty tuple should be Unit");
    self
      .get_adt_from_synth(SynthItem::Tuple(arity))
      .unwrap_or_else(|| {
        let f = self.factory();

        let adt_id = self.fresh_synth_id(SynthItem::Tuple(arity), format!("Tuple{}", arity));
        let tparams = (0..arity)
          .map(|i| {
            &*f.TypeParameterDef(f.TypeParameter(
              self.base.fresh_id(format!("T{}", i)),
              vec![f.IsMutable().into()],
            ))
          })
          .collect::<Vec<_>>();
        let fields = tparams
          .iter()
          .enumerate()
          .map(|(i, t)| {
            &*f.ValDef(f.Variable(
              self.base.fresh_id(format!("_{}", i)),
              // All fields are MutCells
              self.mut_cell_type(t.tp.into()),
              vec![],
            ))
          })
          .collect();

        self.base.add_adt(f.ADTSort(
          adt_id,
          tparams,
          vec![f.ADTConstructor(adt_id, adt_id, fields)],
          vec![f.Synthetic().into()],
        ))
      })
  }
}
