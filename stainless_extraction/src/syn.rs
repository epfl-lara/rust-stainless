use super::*;

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum SyntheticItem {
  MapValueType,
  MapValueAbsent,
  MapValuePresent,
  MapValuePresentField,
}
use SyntheticItem::*;

impl<'l, 'tcx> BaseExtractor<'l, 'tcx> {
  pub fn get_or_create_syn_item(&mut self, item: SyntheticItem) -> StainlessSymId<'l> {
    self
      .with_extraction(|xt| xt.synthetic.get(&item).copied())
      .unwrap_or_else(|| {
        self.add_map_value_type();
        self.get_or_create_syn_item(item)
      })
  }

  fn add_syn_item(&mut self, item: SyntheticItem, id: StainlessSymId<'l>) {
    self.with_extraction_mut(|xt| {
      assert!(xt.synthetic.insert(item, id).is_none());
    })
  }

  fn add_map_value_type(&mut self) {
    let f = self.factory();

    let sort_id = self.fresh_id("MapValue".into());
    self.add_syn_item(MapValueType, sort_id);
    let absent_id = self.fresh_id("Absent".into());
    self.add_syn_item(MapValueAbsent, absent_id);
    let present_id = self.fresh_id("Present".into());
    self.add_syn_item(MapValuePresent, present_id);
    let field_id = self.fresh_id("value".into());
    self.add_syn_item(MapValuePresentField, field_id);
    let t = self.fresh_id("T".into());

    self.add_adt(f.ADTSort(
      sort_id,
      vec![f.TypeParameterDef(f.TypeParameter(t, vec![]))],
      vec![
        f.ADTConstructor(absent_id, sort_id, vec![]),
        f.ADTConstructor(
          present_id,
          sort_id,
          vec![f.ValDef(f.Variable(field_id, f.TypeParameter(t, vec![]).into(), vec![]))],
        ),
      ],
      vec![f.Synthetic().into()],
    ));
  }
}
