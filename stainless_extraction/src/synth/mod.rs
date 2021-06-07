use super::*;

mod std_option;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SynthItem {
  StdOption,
}

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

// Helpers
impl<'l> Synth<'_, 'l, '_> {
  fn factory(&self) -> &'l st::Factory {
    self.base.factory()
  }

  fn get_adt_from_synth(&self, synth: SynthItem) -> Option<&'l st::ADTSort<'l>> {
    self.base.with_extraction(|xt| {
      xt.mapping
        .synth_to_stid
        .get(&synth)
        .and_then(|id| xt.adts.get(id).copied())
    })
  }

  fn register_synth_id(&mut self, synth: SynthItem, id: StainlessSymId<'l>) {
    self
      .base
      .with_extraction_mut(|xt| xt.mapping.synth_to_stid.insert(synth, id));
  }
}
