extern crate stainless;

pub enum SomeEnum {
  Uses(ThatStruct),
  OrUsesNothing,
  But(ThatStruct),
  IsOnlyDefined,
  Afterwards,
}

pub struct ThatStruct(u64);
