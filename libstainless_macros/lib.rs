use proc_macro::TokenStream;
mod implementation;
use implementation::*;

/// Specs

/// Precondition
#[proc_macro_attribute]
pub fn pre(attr: TokenStream, item: TokenStream) -> TokenStream {
  extract_specs_and_expand(SpecType::Pre, attr.into(), item.into()).into()
}

/// Postcondition
#[proc_macro_attribute]
pub fn post(attr: TokenStream, item: TokenStream) -> TokenStream {
  extract_specs_and_expand(SpecType::Post, attr.into(), item.into()).into()
}

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

define_flags!(external, pure, mutable, var);
