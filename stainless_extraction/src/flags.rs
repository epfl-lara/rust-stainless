use super::*;

use enum_iterator::IntoEnumIterator;
use std::collections::HashSet;

use rustc_ast::ast::{self as ast, AttrKind, Attribute, MacArgs};
use rustc_ast::token::{Lit, LitKind, TokenKind};
use rustc_ast::tokenstream::{TokenStream, TokenTree};
use rustc_hir::HirId;
use rustc_span::symbol::{kw, Symbol};

use stainless_data::ast as st;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, IntoEnumIterator)]
pub(super) enum Flag {
  Extern,
  IsPure,
  IsMutable,
  IsVar,
  Law,
  Pre,
  Post,
  Measure,
}

#[derive(Clone, Debug)]
pub(super) struct Flags {
  set: HashSet<Flag>,
}

impl Flags {
  fn new() -> Self {
    Self {
      set: HashSet::new(),
    }
  }

  pub(super) fn add(&mut self, flag: Flag) {
    self.set.insert(flag);
  }

  pub(super) fn to_stainless<'l>(&self, f: &'l st::Factory) -> Vec<st::Flag<'l>> {
    self
      .set
      .iter()
      .filter_map(|flag| match flag {
        Extern => Some(f.Extern().into()),
        IsPure => Some(f.IsPure().into()),
        IsMutable => Some(f.IsMutable().into()),
        IsVar => Some(f.IsVar().into()),
        Law => Some(f.Law().into()),

        // Or pattern instead of wildcard '_' to keep the compiler checking that
        // all variants are covered.
        Pre | Post | Measure => None,
      })
      .collect()
  }
}

use Flag::*;

impl Flag {
  fn name(&self) -> &'static str {
    match self {
      Extern => "external",
      IsPure => "pure",
      IsMutable => "mutable",
      IsVar => "var",
      Law => "law",
      Pre => "pre",
      Post => "post",
      Measure => "measure",
    }
  }

  fn from_name(name: &str) -> Option<Flag> {
    FLAGS_BY_NAME.get(&name).copied()
  }
}

lazy_static! {
  static ref FLAGS_BY_NAME: HashMap<&'static str, Flag> = {
    let mut flags: HashMap<&'static str, Flag> = HashMap::new();
    for flag in Flag::into_enum_iter() {
      flags.insert(flag.name(), flag);
    }
    flags
  };
}

pub(super) fn extract_flag(
  attr: &Attribute,
) -> Result<(Flag, Option<TokenStream>), Option<String>> {
  match &attr.kind {
    AttrKind::Normal(ast::AttrItem {
      path: ast::Path { ref segments, .. },
      args,
      ..
    }) if segments.len() == 3 && segments[1].ident.to_string() == "stainless" => {
      let name = segments[2].ident.to_string();

      Flag::from_name(name.as_str())
        .ok_or_else(|| Some(format!("Unknown stainless annotation: {}", name)))
        .and_then(|flag| match &args {
          MacArgs::Empty => Ok((flag, None)),
          MacArgs::Delimited(_, _, tokens) => Ok((flag, Some(tokens.clone()))),

          _ => Err(Some(
            "Unsupported target specified on stainless annotation".into(),
          )),
        })
    }
    _ => Err(None),
  }
}

impl<'l, 'tcx> BaseExtractor<'l, 'tcx> {
  pub(super) fn extract_flags(&self, carrier_hid: HirId) -> (Flags, HashMap<Symbol, Flags>) {
    let attrs = self.tcx.hir().attrs(carrier_hid);
    let mut carrier_flags = Flags::new();
    let mut flags_by_symbol: HashMap<Symbol, Flags> = HashMap::new();

    for attr in attrs {
      let (flag, arg_tokens) = match extract_flag(attr) {
        Ok((f, a)) => (f, a),
        Err(Some(mess)) => {
          self.tcx.sess.span_warn(attr.span, &mess);
          continue;
        }
        Err(None) => continue,
      };

      let mut add_by_symbol = |symbol: Symbol| {
        flags_by_symbol
          .entry(symbol)
          .or_insert_with(Flags::new)
          .add(flag)
      };

      if let Some(arg_tokens) = arg_tokens {
        for tt in arg_tokens.into_trees() {
          match tt {
            TokenTree::Token(ref token) => match token.kind {
              TokenKind::Comma => continue,
              TokenKind::Ident(symbol, _) if symbol == kw::Fn => carrier_flags.add(flag),
              TokenKind::Ident(symbol, _) => add_by_symbol(symbol),
              TokenKind::Literal(Lit {
                kind: LitKind::Integer,
                symbol,
                ..
              }) => add_by_symbol(symbol),
              _ => {
                self
                  .tcx
                  .sess
                  .span_warn(tt.span(), "Unknown argument in stainless annotation");
                break;
              }
            },
            _ => {
              self
                .tcx
                .sess
                .span_warn(tt.span(), "Unknown argument in stainless annotation");
              break;
            }
          }
        }
      } else {
        carrier_flags.add(flag);
      }
    }

    (carrier_flags, flags_by_symbol)
  }

  /// Warn about unused flags
  pub(super) fn report_unused_flags(
    &self,
    carrier_hid: HirId,
    flags_by_symbol: &HashMap<Symbol, Flags>,
  ) {
    if !flags_by_symbol.is_empty() {
      let names: String = flags_by_symbol
        .keys()
        .map(|symbol| symbol.to_string())
        .collect::<Vec<String>>()
        .join(", ");
      let attrs = self.tcx.hir().attrs(carrier_hid);
      let span = attrs[0].span.until(attrs.last().unwrap().span);
      let msg = format!(
        "Stainless annotations for non-existent parameters: {}",
        names
      );
      self.tcx.sess.span_warn(span, msg.as_str());
    }
  }
}
