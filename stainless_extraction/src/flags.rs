use super::*;

use std::collections::HashSet;

use rustc_ast::ast::{AttrKind, MacArgs};
use rustc_ast::token::{Lit, LitKind, TokenKind};
use rustc_ast::tokenstream::{TokenStream, TokenTree};
use rustc_hir::HirId;
use rustc_span::symbol::{kw, Symbol};

use stainless_data::ast as st;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum Flag {
  Extern,
  IsPure,
  IsMutable,
  IsVar,
  Law,
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
      .map(|flag| match flag {
        Extern => f.Extern().into(),
        IsPure => f.IsPure().into(),
        IsMutable => f.IsMutable().into(),
        IsVar => f.IsVar().into(),
        Law => f.Law().into(),
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
    }
  }

  fn from_name(name: &str) -> Option<Flag> {
    FLAGS_BY_NAME.get(&name).copied()
  }
}

lazy_static! {
  static ref FLAGS: Vec<Flag> = vec![Extern, IsPure, IsMutable, IsVar, Law];
  static ref FLAGS_BY_NAME: HashMap<&'static str, Flag> = {
    let mut flags: HashMap<&'static str, Flag> = HashMap::new();
    for &flag in FLAGS.iter() {
      flags.insert(flag.name(), flag);
    }
    flags
  };
}

impl<'l, 'tcx> BaseExtractor<'l, 'tcx> {
  pub(super) fn extract_flags(&mut self, carrier_hid: HirId) -> (Flags, HashMap<Symbol, Flags>) {
    let attrs = self.tcx.hir().attrs(carrier_hid);
    let mut carrier_flags = Flags::new();
    let mut flags_by_symbol: HashMap<Symbol, Flags> = HashMap::new();

    for attr in attrs {
      let (flag, arg_tokens) = if let AttrKind::Normal(ref attr) = attr.kind {
        let segments = &attr.path.segments;
        if segments.len() == 3 && segments[1].ident.to_string() == "stainless" {
          let name = segments[2].ident.to_string();
          if let Some(flag) = Flag::from_name(name.as_str()) {
            let arg_tokens: Option<TokenStream> = match &attr.args {
              MacArgs::Empty => None,
              MacArgs::Delimited(_, _, tokens) => Some(tokens.clone()),
              _ => {
                self.tcx.sess.span_warn(
                  attr.span(),
                  "Unsupported target specified on stainless annotation",
                );
                continue;
              }
            };
            (flag, arg_tokens)
          } else {
            self.tcx.sess.span_warn(
              attr.span(),
              format!("Unknown stainless annotation: {}", name).as_str(),
            );
            continue;
          }
        } else {
          continue;
        }
      } else {
        continue;
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
