use super::*;
use stainless_data::ast::ADTSort;

impl<'a, 'l, 'tcx> Synth<'a, 'l, 'tcx> {
  pub fn mut_cell(&mut self, tpe: st::Type<'l>, arg: st::Expr<'l>) -> st::Expr<'l> {
    self
      .factory()
      .ADT(self.mut_cell_id(), vec![tpe], vec![arg])
      .into()
  }

  pub fn mut_cell_type(&mut self, tpe: st::Type<'l>) -> st::Type<'l> {
    self.factory().ADTType(self.mut_cell_id(), vec![tpe]).into()
  }

  pub fn mut_cell_value(&mut self, arg: st::Expr<'l>) -> st::Expr<'l> {
    self
      .factory()
      .ADTSelector(arg, self.mut_cell_value_id())
      .into()
  }

  pub fn mut_cell_value_id(&mut self) -> StainlessSymId<'l> {
    self.mut_cell_adt().constructors[0].fields[0].v.id
  }

  pub fn mut_cell_id(&mut self) -> StainlessSymId<'l> {
    self.mut_cell_adt().id
  }

  fn mut_cell_adt(&mut self) -> &'l ADTSort<'l> {
    self
      .get_adt_from_synth(SynthItem::MutCell)
      .unwrap_or_else(|| {
        let f = self.factory();

        let adt_id = self.fresh_synth_id(SynthItem::MutCell, "MutCell".into());

        let field_id = self.base.fresh_id("value".into());
        let tparam = &*f.TypeParameter(self.base.fresh_id("T".into()), vec![f.IsMutable().into()]);

        let adt = f.ADTSort(
          adt_id,
          vec![f.TypeParameterDef(tparam)],
          vec![f.ADTConstructor(
            adt_id,
            adt_id,
            vec![f.ValDef(f.Variable(field_id, tparam.into(), vec![f.IsVar().into()]))],
          )],
          vec![f.Synthetic().into()],
        );
        self.base.add_adt(adt)
      })
  }
}
