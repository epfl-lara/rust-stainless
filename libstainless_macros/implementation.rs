use proc_macro2::{Group, Ident, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use syn::{
  parse_quote, Attribute, Expr, FnArg, ItemFn, Receiver, Result, ReturnType, Stmt, Token, Type,
};

use std::convert::TryFrom;
use std::iter;
use syn::punctuated::Punctuated;

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
  first_spec_type: SpecType,
  first_attr_args: TokenStream,
  item: TokenStream,
) -> Result<(ItemFn, Vec<Spec>)> {
  let item_fn: ItemFn = parse_quote!(#item);
  let first_args: Expr = parse_quote!(#first_attr_args);

  // Filter out & parse all the remaining specs
  let specs = item_fn.attrs.iter().filter_map(|attr: &Attribute| {
    SpecType::try_from(attr)
      .ok()
      .map(|typ| (typ, attr.parse_args()))
  });
  // Add the first spec to the head of the list
  let specs: Vec<(SpecType, Result<Expr>)> = iter::once((first_spec_type, Ok(first_args)))
    .chain(specs)
    .collect();

  // Check whether any parsing has caused errors and fail if so.
  if let Some((_, Err(err))) = specs.iter().find(|(_, cond)| cond.is_err()) {
    return Err(err.clone());
  }

  let specs: Vec<Spec> = specs
    .into_iter()
    .map(|(typ, cond)| Spec {
      typ,
      expr: cond.unwrap(),
    })
    .collect();

  Ok((item_fn, specs))
}

fn generate_fn_with_spec(mut item_fn: ItemFn, specs: Vec<Spec>) -> ItemFn {
  // Take the arguments of the actual function to create the arguments of the
  // closure.
  let fn_arg_tys: Vec<_> = item_fn.sig.inputs.iter().cloned().collect();

  // Replace the [&]self param with a _self: [&]Self
  let fn_arg_tys: Punctuated<FnArg, Token![,]> = (match &fn_arg_tys[..] {
    [FnArg::Receiver(Receiver {
      mutability: None,
      reference,
      ..
    }), args @ ..] => {
      let new_self: FnArg = if reference.is_some() {
        parse_quote! { _self: &Self }
      } else {
        parse_quote! { _self: Self }
      };
      [&[new_self], args].concat()
    }

    args => args.to_vec(),
  })
  .into_iter()
  .collect();

  let fn_return_ty: Type = match &item_fn.sig.output {
    ReturnType::Type(_, ty) => *ty.clone(),
    ReturnType::Default => parse_quote! { () },
  };

  let make_spec_fn = |spec: Spec| -> Stmt {
    let spec_type = Ident::new(spec.typ.name(), Span::call_site());

    let ret_param: TokenStream = match spec.typ {
      SpecType::Post => quote! { , ret: #fn_return_ty },
      _ => quote! {},
    };

    let expr: TokenStream = replace_ident(spec.expr.to_token_stream(), "self", "_self");

    let (return_type, body): (Type, TokenStream) = match spec.typ {
      SpecType::Measure => (parse_quote!(()), parse_quote! { #expr; }),
      _ => (parse_quote!(bool), parse_quote! { #expr }),
    };

    parse_quote! {
      #[allow(unused)]
      #[clippy::stainless::#spec_type]
      |#fn_arg_tys#ret_param| -> #return_type {
        #body
      };
    }
  };

  let (pre_specs, other): (Vec<Spec>, Vec<Spec>) = specs
    .into_iter()
    .partition(|spec| spec.typ == SpecType::Pre);

  let (post_specs, measure_specs): (Vec<Spec>, Vec<Spec>) = other
    .into_iter()
    .partition(|spec| spec.typ == SpecType::Post);

  let pre_spec_fns = pre_specs.into_iter().map(make_spec_fn);
  let post_spec_fns = post_specs.into_iter().map(make_spec_fn);
  let measure_spec_fns = measure_specs.into_iter().map(make_spec_fn);

  #[allow(clippy::reversed_empty_ranges)]
  {
    // Post specs need to go first, because they need to wrap the entire tree.
    item_fn.block.stmts.splice(0..0, measure_spec_fns);
    item_fn.block.stmts.splice(0..0, pre_spec_fns);
    item_fn.block.stmts.splice(0..0, post_spec_fns);
  }
  item_fn
}

/// Extract all the specs from a given function and insert spec functions
pub fn extract_specs_and_expand(
  first_spec_type: SpecType,
  first_attr_args: TokenStream,
  item: TokenStream,
) -> TokenStream {
  match try_parse(first_spec_type, first_attr_args, item) {
    Ok((mut item_fn, specs)) => {
      // Remove all remaining spec attributes
      item_fn
        .attrs
        .retain(|attr| SpecType::try_from(attr).is_err());

      // Build the spec function and insert it into the original function
      generate_fn_with_spec(item_fn, specs).to_token_stream()
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

/// Recursively replaces any identifiers with the given string in the entire
/// token stream by new identifiers with the 'replace_with' string.
fn replace_ident(stream: TokenStream, ident: &str, replace_with: &str) -> TokenStream {
  stream
    .into_iter()
    .map(|tt| match tt {
      TokenTree::Ident(ref i) if i.to_string() == ident => {
        TokenTree::Ident(Ident::new(replace_with, i.span()))
      }

      TokenTree::Group(ref g) => TokenTree::Group(Group::new(
        g.delimiter(),
        replace_ident(g.stream(), ident, replace_with),
      )),

      t => t,
    })
    .collect()
}
