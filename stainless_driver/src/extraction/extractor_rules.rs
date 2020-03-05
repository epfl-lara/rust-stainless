extern crate rustc;
extern crate rustc_hir;
extern crate stainless_data;
extern crate syntax;

use super::extractor::Extractor;

use std::collections::HashMap;

use rustc::hir::map::Map;
use rustc::ty::Ty;

use rustc_hir::intravisit::{self, NestedVisitorMap, Visitor};
use rustc_hir::itemlikevisit::ItemLikeVisitor;
use rustc_hir::{self as hir, HirId};

use syntax::ast;

use stainless_data::ast as st;

macro_rules! unsupported {
  ($sess:expr, $item:expr, $kind_name:expr) => {
    $sess.span_warn(
      $item.span,
      format!("Unsupported tree: {}", $kind_name).as_str(),
    );
  };
}

macro_rules! _unexpected {
  ($sp:expr, $what:expr) => {
    span_bug!(
      $sp,
      concat!("Unexpected ", $what, "encountered during extraction.")
    );
  };
}

/// DefContext tracks available bindings
#[derive(Debug)]
struct DefContext<'l> {
  vars: HashMap<HirId, &'l st::Variable<'l>>,
}

impl<'l> DefContext<'l> {
  fn new() -> Self {
    Self {
      vars: HashMap::new(),
    }
  }

  fn add_var(&mut self, hir_id: HirId, var: &'l st::Variable<'l>) -> &mut Self {
    assert!(!self.vars.contains_key(&hir_id));
    self.vars.insert(hir_id, var);
    self
  }
}

/// Top-level extraction

/// These handlers simply store extracted constructs in the `Extractor`.
impl<'l, 'tcx> Extractor<'l, 'tcx> {
  fn extract_ty(&mut self, _ty: Ty<'tcx>) -> st::Type<'l> {
    // TODO: Implement
    self.extraction.factory.UnitType().into()
  }

  fn register_var(&mut self, hir_id: HirId, ident: &ast::Ident, dctx: &mut DefContext<'l>) {
    let id = self.fetch_id(hir_id, ident).id;
    let tpe = self.extract_ty(self.tables.node_type(hir_id));
    let var = self.extraction.factory.Variable(id, tpe, vec![]);
    dctx.add_var(hir_id, var);
  }

  fn extract_fn(
    &mut self,
    item: &'l hir::Item,
    _decl: &'l hir::FnDecl,
    _ty_params: &'l hir::Generics,
    body_id: hir::BodyId,
  ) {
    struct BindingsCollector<'xtor, 'l, 'tcx> {
      xtor: &'xtor mut Extractor<'l, 'tcx>,
      dctx: DefContext<'l>,
    }

    impl<'xtor, 'l, 'tcx> BindingsCollector<'xtor, 'l, 'tcx> {
      fn new(xtor: &'xtor mut Extractor<'l, 'tcx>, dctx: DefContext<'l>) -> Self {
        Self {
          xtor: xtor,
          dctx: dctx,
        }
      }

      fn as_def_context(self) -> DefContext<'l> {
        self.dctx
      }

      fn populate(&mut self, body: &'tcx hir::Body<'tcx>) {
        assert!(body.generator_kind.is_none());
        for param in body.params {
          self.visit_pat(&param.pat);
        }
        self.visit_expr(&body.value);
      }
    }

    impl<'xtor, 'l, 'tcx> Visitor<'tcx> for BindingsCollector<'xtor, 'l, 'tcx> {
      type Map = Map<'tcx>;

      fn nested_visit_map(&mut self) -> NestedVisitorMap<'_, Self::Map> {
        NestedVisitorMap::OnlyBodies(&self.xtor.tcx.hir())
      }

      fn visit_body(&mut self, _b: &'tcx hir::Body<'tcx>) {
        unreachable!();
      }

      fn visit_pat(&mut self, pattern: &'tcx hir::Pat<'tcx>) {
        use hir::PatKind;
        match pattern.kind {
          PatKind::Binding(_, hir_id, ref ident, ref optional_subpattern) => {
            self.xtor.register_var(hir_id, ident, &mut self.dctx);
            syntax::walk_list!(self, visit_pat, optional_subpattern);
          }
          _ => intravisit::walk_pat(self, pattern),
        }
      }
    }

    self.nest_tables(item.hir_id, |xtor| {
      let body = xtor.tcx.hir().body(body_id);
      let mut collector = BindingsCollector::new(xtor, DefContext::new());
      collector.populate(body);
      println!("EXTRACT FN: {:?}  /  {:?}", item.ident, collector.as_def_context());
    });
  }

  pub fn process_crate(&mut self, mod_name: &str) {
    struct ItemVisitor<'xtor, 'l, 'tcx> {
      xtor: &'xtor mut Extractor<'l, 'tcx>,
    }

    impl<'xtor, 'l, 'tcx> ItemLikeVisitor<'tcx> for ItemVisitor<'xtor, 'l, 'tcx> {
      fn visit_item(&mut self, item: &'tcx hir::Item<'tcx>) {
        if let hir::ItemKind::Fn(ref sig, ref generics, body_id) = item.kind {
          let impl_id = self.xtor.tcx.hir().local_def_id(item.hir_id);
          println!("ITEM: {:?} / {:?}", impl_id, item.ident);
          self.xtor.extract_fn(item, &sig.decl, generics, body_id);
        } else {
          // TODO: Ignore certain boilerplate/compiler-generated items
          unsupported!(self.xtor.tcx.sess, item, "Other kind of item");
        }
      }

      // Unsupported
      fn visit_trait_item(&mut self, trait_item: &'tcx hir::TraitItem<'tcx>) {
        unsupported!(self.xtor.tcx.sess, trait_item, "Trait item");
      }

      // Handled above
      fn visit_impl_item(&mut self, _impl_item: &'tcx hir::ImplItem<'tcx>) {
        unreachable!();
      }
    }

    println!("PROCESS CRATE via HIR: {}", mod_name);
    let krate = self.tcx.hir().krate();
    let mut visitor = ItemVisitor { xtor: self };
    krate.visit_all_item_likes(&mut visitor);
  }
}
