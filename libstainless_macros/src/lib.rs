mod implementation;
mod spec;

use spec::SpecType;

use proc_macro::TokenStream;

/// Specs

/// Precondition
#[proc_macro_attribute]
pub fn pre(attr: TokenStream, item: TokenStream) -> TokenStream {
  specs(SpecType::Pre, attr, item)
}

/// Postcondition
#[proc_macro_attribute]
pub fn post(attr: TokenStream, item: TokenStream) -> TokenStream {
  specs(SpecType::Post, attr, item)
}

#[proc_macro_attribute]
pub fn measure(attr: TokenStream, item: TokenStream) -> TokenStream {
  specs(SpecType::Measure, attr, item)
}

/// Flags

macro_rules! define_flags {
  ($($flag:ident),*) => {
    $(
      #[proc_macro_attribute]
      pub fn $flag(attr: TokenStream, item: TokenStream) -> TokenStream {
        flags(stringify!($flag), attr, item)
      }
    )*
  }
}

define_flags!(external, pure, mutable, var, law);

#[cfg(feature = "stainless")]
mod entry_point {
  use super::*;
  use implementation::*;

  pub fn specs(
    first_spec_type: SpecType,
    first_attr_args: TokenStream,
    item: TokenStream,
  ) -> TokenStream {
    extract_specs_and_expand(first_spec_type, first_attr_args.into(), item.into()).into()
  }

  pub fn flags(flag_name: &'static str, args: TokenStream, item: TokenStream) -> TokenStream {
    rewrite_flag(flag_name, args.into(), item.into()).into()
  }
}

#[cfg(not(feature = "stainless"))]
mod entry_point {
  use super::*;

  pub fn specs(
    _first_spec_type: SpecType,
    _first_attr_args: TokenStream,
    item: TokenStream,
  ) -> TokenStream {
    item
  }

  pub fn flags(_flag_name: &'static str, _args: TokenStream, item: TokenStream) -> TokenStream {
    item
  }
}

use entry_point::*;
