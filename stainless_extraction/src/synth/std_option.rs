use super::*;

/// Synthetisation of std::option::Option trees in Stainless AST.
impl<'a, 'l, 'tcx> Synth<'a, 'l, 'tcx> {
  pub fn std_option_type(&mut self, tpe: st::Type<'l>) -> st::Type<'l> {
    self
      .factory()
      .ADTType(self.option_adt().id, vec![tpe])
      .into()
  }

  pub fn std_option_none(&mut self, tpe: st::Type<'l>) -> st::Expr<'l> {
    self.factory().ADT(self.none_id(), vec![tpe], vec![]).into()
  }

  pub fn std_option_some(&mut self, val: st::Expr<'l>, tpe: st::Type<'l>) -> st::Expr<'l> {
    self
      .factory()
      .ADT(self.some_id(), vec![tpe], vec![val])
      .into()
  }

  pub fn std_option_some_type(&mut self, tpe: st::Type<'l>) -> st::Type<'l> {
    self.factory().ADTType(self.some_id(), vec![tpe]).into()
  }

  pub fn std_option_some_value(&mut self, some: st::Expr<'l>) -> st::Expr<'l> {
    self
      .factory()
      .ClassSelector(some, self.some_value_id())
      .into()
  }

  fn none_id(&mut self) -> StainlessSymId<'l> {
    self.option_adt().constructors[0].id
  }

  fn some_id(&mut self) -> StainlessSymId<'l> {
    self.option_adt().constructors[1].id
  }

  fn some_value_id(&mut self) -> StainlessSymId<'l> {
    self.option_adt().constructors[1].fields[0].v.id
  }

  fn option_adt(&mut self) -> &'l st::ADTSort<'l> {
    self
      .get_adt_from_synth(SynthItem::StdOption)
      .unwrap_or_else(|| {
        let def_id = self
          .base
          .std_items
          .item_to_def(StdItem::CrateItem(CrateItem::OptionType));
        let adt = self.base.get_or_extract_adt(def_id);
        self.register_synth_id(SynthItem::StdOption, adt.id);
        adt
      })
  }
}
