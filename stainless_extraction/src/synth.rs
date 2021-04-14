use super::*;

/// This is just a proxy, a mutable reference to the base extractor. The idea of
/// `Synth` is just to scope some methods under a common namespace, therefore
/// the struct mainly provides a namespace/scope.
pub struct Synth<'a, 'l, 'tcx> {
  base: &'a mut BaseExtractor<'l, 'tcx>,
}

impl<'l, 'tcx> BaseExtractor<'l, 'tcx> {
  /// Returns a `Synth` which provides access to all methods that synthesise
  /// items/trees.
  pub fn synth<'a>(&'a mut self) -> Synth<'a, 'l, 'tcx> {
    Synth { base: self }
  }
}

impl<'b, 'l, 'tcx> BodyExtractor<'b, 'l, 'tcx> {
  /// Returns a `Synth` which provides access to all methods that synthesise
  /// items/trees.
  pub fn synth<'a>(&'a mut self) -> Synth<'a, 'l, 'tcx> {
    self.base.synth()
  }
}

/// Synthetisation of std::option::Option trees in Stainless AST.
impl<'a, 'l, 'tcx> Synth<'a, 'l, 'tcx> {
  pub(super) fn std_option_type(&mut self, tpe: st::Type<'l>) -> st::Type<'l> {
    self
      .factory()
      .ADTType(self.option_adt().id, vec![tpe])
      .into()
  }

  pub(super) fn std_option_none(&mut self, tpe: st::Type<'l>) -> st::Expr<'l> {
    self.factory().ADT(self.none_id(), vec![tpe], vec![]).into()
  }

  pub(super) fn std_option_some(&mut self, val: st::Expr<'l>, tpe: st::Type<'l>) -> st::Expr<'l> {
    self
      .factory()
      .ADT(self.some_id(), vec![tpe], vec![val])
      .into()
  }

  pub(super) fn std_option_some_type(&mut self, tpe: st::Type<'l>) -> st::Type<'l> {
    self.factory().ADTType(self.some_id(), vec![tpe]).into()
  }

  pub(super) fn std_option_some_value(&mut self, some: st::Expr<'l>) -> st::Expr<'l> {
    self
      .factory()
      .ClassSelector(some, self.some_value_id())
      .into()
  }

  fn option_adt(&mut self) -> &'l st::ADTSort<'l> {
    let def_id = self
      .base
      .std_items
      .item_to_def(StdItem::CrateItem(CrateItem::OptionType));
    self.base.get_or_extract_adt(def_id)
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

  fn factory(&self) -> &'l st::Factory {
    self.base.factory()
  }
}
