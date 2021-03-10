use proc_macro2::{Group, Ident, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use syn::{
  parse2, parse_quote, punctuated::Punctuated, token::Brace, Attribute, Block, Expr, FnArg, ItemFn,
  Receiver, Result, ReturnType, Signature, Stmt, Token, TraitItemMethod, Type, Visibility,
};

use super::spec::*;
use std::convert::TryFrom;
use std::iter;
use syn::parse::Parser;

/// Extract all the specs from a given function and insert spec functions
pub fn extract_specs_and_expand(
  first_spec_type: SpecType,
  first_attr_args: TokenStream,
  item: TokenStream,
) -> TokenStream {
  try_parse(first_spec_type, first_attr_args, item).map_or_else(
    |err| err.to_compile_error(),
    |f| generate_fn_with_spec(f).to_token_stream(),
  )
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

#[derive(Debug)]
struct FnSpecs {
  attrs: Vec<Attribute>,
  sig: Signature,
  vis: Option<Visibility>,
  block: Option<Box<Block>>,
  specs: Vec<Spec>,
}

/// Parse the decorated function and all specs
fn try_parse(
  first_spec_type: SpecType,
  first_attr_args: TokenStream,
  item: TokenStream,
) -> Result<FnSpecs> {
  let first_args: Expr = parse2(first_attr_args)?;

  // Parse function OR abstract trait method.
  let (attrs, sig, block, vis) = parse2::<ItemFn>(item.clone())
    .map(|i| (i.attrs, i.sig, Some(i.block), Some(i.vis)))
    .or_else(|_| parse2::<TraitItemMethod>(item).map(|i| (i.attrs, i.sig, None, None)))?;

  let (specs, attrs): (Vec<_>, Vec<_>) = attrs
    .into_iter()
    .map(|a| (SpecType::try_from(&a), a))
    .partition(|(s, _)| s.is_ok());

  let specs: Vec<_> = iter::once(Ok(Spec {
    typ: first_spec_type,
    expr: first_args,
  }))
  .chain(specs.into_iter().map(|(t, a)| {
    a.parse_args().map(|expr| Spec {
      typ: t.unwrap(),
      expr,
    })
  }))
  // collect and return early if there's an error
  .collect::<Result<Vec<_>>>()?;

  Ok(FnSpecs {
    attrs: attrs.into_iter().map(|(_, a)| a).collect(),
    sig,
    block,
    specs,
    vis,
  })
}

fn generate_fn_with_spec(fn_specs: FnSpecs) -> ItemFn {
  // Take the arguments of the actual function to create the closure's arguments.
  let fn_arg_tys: Vec<_> = fn_specs.sig.inputs.iter().cloned().collect();

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

  let fn_return_ty: Type = match &fn_specs.sig.output {
    ReturnType::Type(_, ty) => *ty.clone(),
    ReturnType::Default => parse_quote! { () },
  };

  let make_spec_fn = |spec: Spec| -> Stmt {
    let spec_type = Ident::new(spec.typ.name(), Span::call_site());

    let ret_param: TokenStream = match spec.typ {
      SpecType::Post => quote! { , ret: #fn_return_ty },
      _ => quote! {},
    };
    let expr = replace_ident(spec.expr.to_token_stream(), "self", "_self");

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

  // Annotate "abstract" functions with a flag
  let mut attrs = fn_specs.attrs;
  if fn_specs.block.is_none() {
    attrs.extend(
      Attribute::parse_outer
        .parse_str("#[clippy::stainless::abstr]")
        .unwrap(),
    )
  }

  let spec_closures = fn_specs.specs.into_iter().map(make_spec_fn);
  let block = Box::new(Block {
    brace_token: Brace::default(),
    stmts: spec_closures
      .chain(
        fn_specs
          .block
          .map_or_else(|| parse_quote! { unimplemented!(); }, |b| b.stmts),
      )
      .collect(),
  });

  ItemFn {
    attrs,
    sig: fn_specs.sig,
    block,
    vis: fn_specs.vis.unwrap_or(Visibility::Inherited),
  }
}

/// Recursively replaces any identifiers with the given string in the entire
/// token stream by new identifiers with the 'replace_with' string.
fn replace_ident(stream: TokenStream, ident: &str, replace_with: &str) -> TokenStream {
  stream
    .into_iter()
    .map(|tt| match tt {
      TokenTree::Ident(ref i) if *i == ident => {
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
