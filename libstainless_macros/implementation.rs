use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{parse_quote, Attribute, Expr, Item, ItemFn, Result, ReturnType, Stmt, Type};

use std::convert::TryFrom;
use std::iter;

/// Specs (pre-, postconditions, ...)

#[derive(Debug, PartialEq, Eq)]
pub enum SpecType {
  Pre,
  Post,
  Measure,
}

impl SpecType {
  fn name(&self) -> &'static str {
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

#[derive(Debug)]
struct Spec {
  typ: SpecType,
  expr: Expr,
}

/// Parse the decorated function and all specs
fn try_parse(
  first_typ: SpecType,
  first_attr_args: TokenStream,
  item: TokenStream,
) -> Result<(ItemFn, Vec<Spec>)> {
  let item_fn: ItemFn = parse_quote!(#item);
  let first_cond: Expr = parse_quote!(#first_attr_args);

  let pre_specs = item_fn.attrs.iter().filter_map(|attr: &Attribute| {
    if let Ok(typ) = SpecType::try_from(attr) {
      Some((typ, attr.parse_args()))
    } else {
      None
    }
  });
  let pre_specs: Vec<(SpecType, Result<Expr>)> = iter::once((first_typ, Ok(first_cond)))
    .chain(pre_specs)
    .collect();

  for (_, cond) in &pre_specs {
    if let Err(err) = cond {
      return Err(err.clone());
    }
  }
  let specs: Vec<Spec> = pre_specs
    .into_iter()
    .map(|(typ, cond)| Spec {
      typ,
      expr: cond.unwrap(),
    })
    .collect();

  Ok((item_fn, specs))
}

fn generate_fn_with_spec(mut item_fn: ItemFn, specs: Vec<Spec>) -> ItemFn {
  let fn_generics = &item_fn.sig.generics;
  let fn_arg_tys = &item_fn.sig.inputs;
  let fn_return_ty: Type = match &item_fn.sig.output {
    ReturnType::Type(_, ty) => *ty.clone(),
    ReturnType::Default => parse_quote! { () },
  };

  let make_spec_fn = |(index, spec): (usize, Spec)| -> Stmt {
    let fn_ident = format_ident!("__{}{}", spec.typ.name(), index + 1);
    let ret_param: TokenStream = match spec.typ {
      SpecType::Post => quote! { , ret: #fn_return_ty },
      _ => quote! {},
    };

    let expr = spec.expr;
    let (return_type, body): (Type, TokenStream) = match spec.typ {
      SpecType::Measure => (parse_quote!(()), parse_quote! { #expr; }),
      _ => (parse_quote!(bool), parse_quote! { #expr }),
    };

    let spec_fn: ItemFn = parse_quote! {
      #[doc(hidden)]
      #[allow(unused_variables)]
      fn #fn_ident#fn_generics(#fn_arg_tys#ret_param) -> #return_type {
        #body
      }
    };
    Stmt::Item(Item::Fn(spec_fn))
  };
  let (pre_specs, other): (Vec<Spec>, Vec<Spec>) = specs
    .into_iter()
    .partition(|spec| spec.typ == SpecType::Pre);

  let (post_specs, measure_specs): (Vec<Spec>, Vec<Spec>) = other
    .into_iter()
    .partition(|spec| spec.typ == SpecType::Post);

  let pre_spec_fns = pre_specs.into_iter().enumerate().map(make_spec_fn);
  let post_spec_fns = post_specs.into_iter().enumerate().map(make_spec_fn);
  let measure_spec_fns = measure_specs.into_iter().enumerate().map(make_spec_fn);

  #[allow(clippy::reversed_empty_ranges)]
  {
    item_fn.block.stmts.splice(0..0, measure_spec_fns);
    item_fn.block.stmts.splice(0..0, post_spec_fns);
    item_fn.block.stmts.splice(0..0, pre_spec_fns);
  }

  item_fn
}

/// Extract all the specs from a given function and insert spec functions
pub fn extract_specs_and_expand(
  first_typ: SpecType,
  first_attr_args: TokenStream,
  item: TokenStream,
) -> TokenStream {
  match try_parse(first_typ, first_attr_args, item) {
    Ok((mut item_fn, specs)) => {
      // Remove all remaining spec attributes
      item_fn
        .attrs
        .retain(|attr| SpecType::try_from(attr).is_err());

      // Build the spec function and insert it into the original function
      let item_fn = generate_fn_with_spec(item_fn, specs);
      item_fn.to_token_stream()
    }
    Err(err) => err.to_compile_error(),
  }
}

/// Flags

/// Note that we simply want to attach some attributes to the item in question. Currently,
/// Rust doesn't permit user-defined attributes that stay around until the later phases of
/// rustc. However, any attribute in the `clippy::` group is accepted, and clippy itself
/// doesn't seem to complain about unknown attributes. We therefore abuse this to attach
/// some attributes of our own for the stainless extraction pass to detect.
pub fn rewrite_flag(
  flag_name: &'static str,
  mut args: TokenStream,
  item: TokenStream,
) -> TokenStream {
  let flag_name = Ident::new(flag_name, Span::call_site());
  if !args.is_empty() {
    args = quote! { ( #args ) };
  }
  quote! {
    #[clippy::stainless::#flag_name#args]
    #item
  }
}
