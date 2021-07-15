use proc_macro2::{Group, Ident, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use syn::{
  parse::Parser, parse2, parse_quote, punctuated::Punctuated, token::Brace, token::Semi, Attribute,
  Block, Expr, FnArg, ItemFn, Receiver, Result, ReturnType, Signature, Stmt, Token,
  TraitItemMethod, Type, Visibility,
};

use super::spec::*;
use std::convert::TryFrom;
use std::iter;

/// Extract all the specs from a given function and insert spec functions
pub fn extract_specs_and_expand(
  first_spec_type: SpecType,
  first_attr_args: TokenStream,
  item: TokenStream,
) -> TokenStream {
  try_parse(first_spec_type, first_attr_args, item)
    .map_or_else(|err| err.to_compile_error(), generate_fn_with_spec)
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
  vis: Visibility,
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
    .map(|i| (i.attrs, i.sig, Some(i.block), i.vis))
    .or_else(|_| {
      parse2::<TraitItemMethod>(item).map(|i| (i.attrs, i.sig, None, Visibility::Inherited))
    })?;

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

fn generate_fn_with_spec(fn_specs: FnSpecs) -> TokenStream {
  // Take the arguments of the actual function to create the closure's arguments
  let fn_arg_tys = replace_param(&fn_specs.sig.inputs);

  let fn_return_ty: Type = match &fn_specs.sig.output {
    ReturnType::Type(_, ty) => *ty.clone(),
    ReturnType::Default => parse_quote! { () },
  };

  let make_spec_fn = |spec: Spec| -> Stmt {
    let spec_type = Ident::new(spec.typ.name(), Span::call_site());

    let ret_param: TokenStream = match spec.typ {
      SpecType::Post if !fn_arg_tys.is_empty() => quote! { , ret: #fn_return_ty },
      SpecType::Post => quote! { ret: #fn_return_ty },
      _ => quote! {},
    };
    let expr = replace_ident(spec.expr.to_token_stream(), "self", "_self");

    let (return_type, body): (Type, TokenStream) = match spec.typ {
      SpecType::Measure => (parse_quote!(()), parse_quote! { #expr; }),
      _ => (parse_quote!(bool), parse_quote! { #expr }),
    };

    parse_quote! {
      #[cfg(stainless)]
      #[allow(unused)]
      #[clippy::stainless::#spec_type]
      |#fn_arg_tys#ret_param| -> #return_type {
        #body
      };
    }
  };

  // Remove a warning because spec closures are never called
  let mut attrs = fn_specs.attrs.clone();
  attrs.extend(
    Attribute::parse_outer
      .parse_str("#[allow(unused_must_use)]")
      .unwrap(),
  );

  let spec_closures = fn_specs.specs.into_iter().map(make_spec_fn);
  let block = Box::new(Block {
    brace_token: Brace::default(),
    stmts: spec_closures
      .chain(
        fn_specs
          .block
          .clone()
          .map_or_else(|| parse_quote! { unimplemented!() }, |b| b.stmts),
      )
      .collect(),
  });

  // If the function was abstract, we output two versions of it:
  // - the original ItemFn annotated with `#[cfg(not(stainless))]` for normal build & run
  // - the processed one with the specs and a function body annotated with `#[cfg(stainless)]`
  if fn_specs.block.is_none() {
    attrs.extend(
      Attribute::parse_outer
        .parse_str("#[clippy::stainless::is_abstract]\n#[cfg(stainless)]")
        .unwrap(),
    );

    let mut original_attrs = fn_specs.attrs;
    original_attrs.extend(
      Attribute::parse_outer
        .parse_str("#[cfg(not(stainless))]")
        .unwrap(),
    );

    vec![
      ItemFn {
        attrs,
        sig: fn_specs.sig.clone(),
        block,
        vis: fn_specs.vis,
      }
      .to_token_stream(),
      TraitItemMethod {
        attrs: original_attrs,
        sig: fn_specs.sig,
        default: None,
        semi_token: Some(Semi::default()),
      }
      .to_token_stream(),
    ]
    .into_iter()
    .collect()
  } else {
    ItemFn {
      attrs,
      sig: fn_specs.sig,
      block,
      vis: fn_specs.vis,
    }
    .to_token_stream()
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

/// Replace the [mut|&]self param with a _self: [mut|&]Self for the parameter
/// list of the closure.
fn replace_param(args: &Punctuated<FnArg, Token![,]>) -> Punctuated<FnArg, Token![,]> {
  let fn_arg_tys: Vec<_> = args.iter().cloned().collect();
  (match &fn_arg_tys[..] {
    [FnArg::Receiver(Receiver {
      mutability,
      reference,
      ..
    }), args @ ..] => {
      let new_self: FnArg = match (mutability, reference) {
        (None, None) => parse_quote! { _self: Self },
        (None, Some(_)) => parse_quote! { _self: &Self },
        (Some(_), None) => parse_quote! { mut _self: Self },
        (Some(_), Some(_)) => parse_quote! { _self: &mut Self },
      };
      [&[new_self], args].concat()
    }

    args => args.to_vec(),
  })
  .into_iter()
  .collect()
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_post_no_params() {
    let p = try_parse(
      SpecType::Post,
      quote!(ret),
      quote!(
        fn test() -> bool {
          true
        }
      ),
    );
    assert!(p.is_ok());
    generate_fn_with_spec(p.unwrap());
  }

  #[test]
  fn test_replace_self_param() {
    assert_eq!(replace_param(&parse_quote!()), parse_quote!());

    assert_eq!(
      replace_param(&parse_quote!(self)),
      parse_quote!(_self: Self)
    );

    assert_eq!(
      replace_param(&parse_quote!(self, i: i32, b: bool)),
      parse_quote!(_self: Self, i: i32, b: bool)
    );

    assert_eq!(
      replace_param(&parse_quote!(&self, i: i32, b: bool)),
      parse_quote!(_self: &Self, i: i32, b: bool)
    );

    assert_eq!(
      replace_param(&parse_quote!(mut self, i: i32, b: bool)),
      parse_quote!(mut _self: Self, i: i32, b: bool)
    );
  }
}
