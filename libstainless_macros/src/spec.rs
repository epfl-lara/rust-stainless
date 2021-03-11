use std::convert::TryFrom;
use syn::{Attribute, Expr};

/// Specs (pre-, postconditions, ...)
#[derive(Debug)]
pub struct Spec {
  pub typ: SpecType,
  pub expr: Expr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum SpecType {
  Pre,
  Post,
  Measure,
}

impl SpecType {
  #[cfg(feature = "stainless")]
  pub fn name(&self) -> &'static str {
    match self {
      SpecType::Pre => "pre",
      SpecType::Post => "post",
      SpecType::Measure => "measure",
    }
  }
}
impl TryFrom<String> for SpecType {
  type Error = ();

  fn try_from(name: String) -> std::result::Result<Self, Self::Error> {
    match name.as_str() {
      "pre" => Ok(SpecType::Pre),
      "post" => Ok(SpecType::Post),
      "measure" => Ok(SpecType::Measure),
      _ => Err(()),
    }
  }
}

impl<'a> TryFrom<&'a Attribute> for SpecType {
  type Error = ();

  fn try_from(attr: &'a Attribute) -> std::result::Result<Self, Self::Error> {
    let segments = &attr.path.segments;
    if segments.len() == 1 {
      SpecType::try_from(segments[0].ident.to_string())
    } else {
      Err(())
    }
  }
}
