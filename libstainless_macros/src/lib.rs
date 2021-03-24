use proc_macro::TokenStream;

mod implementation;
use implementation::*;

mod spec;
use spec::SpecType;

/// Specs

macro_rules! define_specs {
  ($($spec:ident : $t:expr),*) => {
    $(
      #[proc_macro_attribute]
      pub fn $spec(attr: TokenStream, item: TokenStream) -> TokenStream {
        extract_specs_and_expand($t, attr.into(), item.into()).into()
      }
    )*
  }
}

define_specs!(
  pre: SpecType::Pre,
  post: SpecType::Post,
  measure: SpecType::Measure
);

/// Flags

macro_rules! define_flags {
  ($($flag:ident),*) => {
    $(
      #[proc_macro_attribute]
      pub fn $flag(attr: TokenStream, item: TokenStream) -> TokenStream {
        rewrite_flag(stringify!($flag), attr.into(), item.into()).into()
      }
    )*
  }
}

define_flags!(external, pure, mutable, var, law);
