extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Expr, Item, ReturnType, Type};

#[proc_macro_attribute]
pub fn require(attr: TokenStream, item: TokenStream) -> TokenStream {
  let condition = parse_macro_input!(attr as Expr);
  let original_func = item.clone();
  let func = parse_macro_input!(original_func as Item);

  match func {
    Item::Fn(function) => {
      let fn_args = function.sig.inputs;
      let pc_ident = format_ident!("__precondition_{}", function.sig.ident);

      let item = proc_macro2::TokenStream::from(item);

      let augumented_item = quote! {
          #item

          #[allow(unused_variables, unused_mut)]
          fn #pc_ident(#fn_args) -> bool {
              #condition
          }
      };
      augumented_item.into()
    }
    _ => panic!("Expecting ensuring to decorate a function with postcondition lambda"),
  }
}

#[proc_macro_attribute]
pub fn ensuring(attr: TokenStream, item: TokenStream) -> TokenStream {
  let expr = parse_macro_input!(attr as Expr);
  let original_func = item.clone();
  let func = parse_macro_input!(original_func as Item);

  match (expr, func) {
    (Expr::Closure(closure), Item::Fn(function)) => {
      let ret_type: Type = match function.sig.output {
        ReturnType::Type(_, ty) => *ty,
        _ => {
          let q = TokenStream::from(quote! { () });
          parse_macro_input!(q as Type)
        }
      };
      let fn_args = function.sig.inputs;
      let pc_ident = format_ident!("__postcondition_{}", function.sig.ident);

      let condition = Expr::Closure(closure);
      let item = proc_macro2::TokenStream::from(item);

      let augumented_item = quote! {
          #item

          #[allow(unused_variables, unused_mut)]
          fn #pc_ident(__pc_res: #ret_type, #fn_args) -> bool {
              (#condition)(__pc_res)
          }
      };
      augumented_item.into()
    }
    _ => panic!("Expecting ensuring to decorate a function with postcondition lambda"),
  }
}

#[proc_macro_attribute]
pub fn ignore(_attr: TokenStream, item: TokenStream) -> TokenStream {
  if let Item::Fn(function) = parse_macro_input!(item as Item) {
    // TODO: figure out how to generate custom attribute to mark function for ignoring
    // function.attrs.push(???);
    let f = quote! {
        #function
    };
    f.into()
  } else {
    panic!("The 'ignore' attribute is expected to use with functions")
  }
}
