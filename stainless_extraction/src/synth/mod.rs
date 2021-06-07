use super::*;

mod std_option;

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

impl<'l> Synth<'_, 'l, '_> {
  fn factory(&self) -> &'l st::Factory {
    self.base.factory()
  }
}
