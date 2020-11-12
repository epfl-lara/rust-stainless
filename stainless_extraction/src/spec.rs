use std::convert::TryFrom;
use std::result::Result;

/// Types of spec functions (pre-, postconditions, ...) and some helping
/// implementations.
#[derive(Debug, PartialEq, Eq)]
pub enum SpecType {
  Pre,
  Post,
  Measure,
}

impl SpecType {
  /// Parses the spec type and the name of the function the spec belongs to from
  /// the name of the spec function.
  ///
  /// Spec functions are named according to the following format
  /// `__{spec_type}_{index}_{fn_name}`.
  ///
  pub fn parse_spec_type_fn_name(str: &str) -> Option<(SpecType, String)> {
    let parts: Vec<&str> = str.split('_').collect();
    match parts.as_slice() {
      ["", "", spec_type, index, ..] if index.chars().all(char::is_numeric) => {
        SpecType::try_from(*spec_type)
          .ok()
          .map(|sp| (sp, parts[4..].join("_")))
      }
      _ => None,
    }
  }
}

impl TryFrom<&str> for SpecType {
  type Error = ();

  fn try_from(name: &str) -> Result<Self, Self::Error> {
    match name {
      "pre" => Ok(SpecType::Pre),
      "post" => Ok(SpecType::Post),
      "measure" => Ok(SpecType::Measure),
      _ => Err(()),
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::spec::SpecType;

  #[test]
  fn test_parse_spec_type_fn_name() {
    assert_eq!(
      SpecType::parse_spec_type_fn_name("__pre_1_asdf"),
      Some((SpecType::Pre, "asdf".to_string()))
    );
    assert_eq!(
      SpecType::parse_spec_type_fn_name("__post_712341234_asdf"),
      Some((SpecType::Post, "asdf".to_string()))
    );
    assert_eq!(
      SpecType::parse_spec_type_fn_name("__pre__asdf"),
      Some((SpecType::Pre, "asdf".to_string()))
    );
    assert_eq!(
      SpecType::parse_spec_type_fn_name("__measure_2_dummy_for_specs_2"),
      Some((SpecType::Measure, "dummy_for_specs_2".to_string()))
    );

    assert_eq!(
      SpecType::parse_spec_type_fn_name("__measure_bcdf_asdf"),
      None
    );
    assert_eq!(SpecType::parse_spec_type_fn_name("_pre_asdf"), None);
    assert_eq!(SpecType::parse_spec_type_fn_name("___pre_asdf"), None);
    assert_eq!(SpecType::parse_spec_type_fn_name("__pr_asdf"), None);
  }
}
