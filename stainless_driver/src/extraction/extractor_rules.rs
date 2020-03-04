extern crate rustc;
extern crate rustc_hir;
extern crate stainless_data;
extern crate syntax;

use super::extractor::Extractor;

use rustc::span_bug;
use syntax::ast;

use stainless_data::ast as st;

macro_rules! unsupported {
  ($self:expr, $item:expr, $kind_name:expr) => {
    $self.session().span_warn(
      $item.span,
      format!("Unsupported tree: {}", $kind_name).as_str(),
    );
  };
}

macro_rules! unexpected {
  ($sp:expr, $what:expr) => {
    span_bug!(
      $sp,
      concat!("Unexpected ", $what, "encountered during extraction.")
    );
  };
}

/// Extractors for various constructs
impl<'l, 'tcx> Extractor<'l, 'tcx> {
  pub fn extract_fn(
    &mut self,
    item: &'l ast::Item,
    decl: &'l ast::FnDecl,
    ty_params: &'l ast::Generics,
    body: &'l ast::Block,
  ) -> () {
    self.nest_tables(item.id, |xtor| {
      let qualname = format!(
        "::{}",
        xtor
          .tcx
          .def_path_str(xtor.tcx.hir().local_def_id_from_node_id(item.id))
      );
      println!(
        "FN: name: {}, qualname: {}, decl: {:?}, ty_params: {:?}",
        item.ident.to_string(),
        qualname,
        decl,
        ty_params
      );
      xtor.extract_block(body);
    });
  }

  // TODO: Handle last stmt specially / assert it to be an expr without a trailing semi
  fn extract_block(&mut self, block: &'l ast::Block) -> () {
    use ast::{PatKind, StmtKind};

    let mut _exprs: Vec<st::Expr> = vec![];
    for stmt in &block.stmts {
      match stmt.kind {
        StmtKind::Local(ref local) => {
          // TODO: Get HIR tree, check whether simple ident
          // match local.pat.simple_ident() {
          //   None => {},
          //   Some(ident) => {
          //     ident.id
          //   }
          // }
          match local.pat.kind {
            PatKind::Ident(_, ident, _) => {
              let id = self.fetch_id(local.id, &ident);
              println!("FRESH ID FOR LOCAL {:?}", id);
            }
            _ => {}
          }
          unsupported!(self, local, "Let-def statement")
        }
        StmtKind::Item(ref item) => {
          let id = self.fetch_id(item.id, &item.ident);
          println!("FRESH ID FOR ITEM {:?}", id);
          unsupported!(self, item, "Item statement")
        }
        StmtKind::Expr(ref expr) => unsupported!(self, expr, "Expr (result) statement"),
        StmtKind::Semi(ref expr) => unsupported!(self, expr, "Expr (seq) statement"),
        StmtKind::Mac(_) => unexpected!(stmt.span, "macro"),
      }
    }
  }

  // fn extract_stmt(&mut self, stmt: &'l ast::Stmt) -> () {
  // }
}

/// Top-level extraction
/// These handlers simply store extracted constructs in the `Extractor`.
impl<'l, 'tcx> Extractor<'l, 'tcx> {
  pub fn process_mod(&mut self, mod_name: &str, modd: &'l ast::Mod) -> () {
    use ast::ItemKind::*;

    println!(
      "Module {} w/ {:?} items\n=====\n",
      mod_name,
      modd.items.len()
    );
    for item in &modd.items {
      // println!("item: #{:?}  kind: {:?}", item.id, item.kind);
      match item.kind {
        Fn(ref sig, ref generics, ref body) => self.extract_fn(item, &sig.decl, generics, body),
        Use(_) => unsupported!(self, item, "Use"),
        ExternCrate(_) => unsupported!(self, item, "ExternCrate"),
        Mod(_) => unsupported!(self, item, "Mod"),
        Mac(_) => unexpected!(item.span, "macro"),
        _ => unsupported!(self, item, "(Other)"),
      };
    }
  }
}
