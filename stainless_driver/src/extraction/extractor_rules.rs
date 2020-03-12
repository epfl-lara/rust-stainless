extern crate rustc;
extern crate rustc_hir;
extern crate rustc_span;
extern crate stainless_data;
extern crate rustc_ast;

use super::extractor::{Extractor, StainlessSymId};

use std::collections::HashMap;

use rustc::hir::map::Map;
use rustc::ty::{self, Ty, TyKind};

use rustc_hir::intravisit::{self, NestedVisitorMap, Visitor};
use rustc_hir::itemlikevisit::ItemLikeVisitor;
use rustc_hir::{self as hir, HirId};

use rustc_span::Span;

use rustc_ast::ast;

use stainless_data::ast as st;

macro_rules! unsupported {
  ($sess:expr, $item:expr, $kind_name:expr) => {
    $sess.span_warn(
      $item,
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

/// BindingsCollector populates a DefContext
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

  fn populate_from(&mut self, body: &'tcx hir::Body<'tcx>) {
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
    NestedVisitorMap::None
  }

  fn visit_body(&mut self, _b: &'tcx hir::Body<'tcx>) {
    unreachable!();
  }

  fn visit_pat(&mut self, pattern: &'tcx hir::Pat<'tcx>) {
    use hir::PatKind;
    match pattern.kind {
      PatKind::Binding(_, hir_id, ref _ident, ref optional_subpattern) => {
        // Extend DefContext with a new variable
        let xtor = &mut self.xtor;
        let id = xtor.register_var(hir_id).id;
        let tpe = xtor.extract_ty(xtor.tables.node_type(hir_id), &self.dctx, pattern.span);
        let var = xtor.extraction.factory.Variable(id, tpe, vec![]);
        self.dctx.add_var(hir_id, var);

        // Visit potential sub-patterns
        rustc_ast::walk_list!(self, visit_pat, optional_subpattern);
      }
      _ => intravisit::walk_pat(self, pattern),
    }
  }
}

/// Top-level extraction

/// These handlers simply store extracted constructs in the `Extractor`.
impl<'l, 'tcx> Extractor<'l, 'tcx> {
  /// Extends DefContext with a new variable based on a HIR binding node
  fn register_var(&mut self, hir_id: HirId) -> StainlessSymId<'l> {
    // Extract ident from corresponding node, sanity-check binding mode
    let id = {
      let node = self.tcx.hir().find(hir_id).unwrap();
      let sess = self.tcx.sess;
      let ident = if let hir::Node::Binding(pat) = node {
        if let hir::PatKind::Binding(_, _, ident, _) = pat.kind {
          match self.tables.extract_binding_mode(sess, pat.hir_id, pat.span) {
            Some(ty::BindByValue(hir::Mutability::Not)) => {}
            _ => unsupported!(sess, pat.span, "Only immutable by-value bindings are supported"),
          }
          ident
        } else {
          unreachable!()
        }
      } else {
        unreachable!()
      };
      self.register_id_from_ident(hir_id, &ident)
    };
    id
  }

  fn extract_ty(&mut self, ty: Ty<'tcx>, _dctx: &DefContext<'l>, span: Span) -> st::Type<'l> {
    let f = &self.extraction.factory;
    match ty.kind {
      TyKind::Bool => f.BooleanType().into(),
      TyKind::Int(ast::IntTy::I32) => f.BVType(true, 32).into(),
      _ => {
        unsupported!(
          self.tcx.sess,
          span,
          format!("Cannot extract type {:?}", ty.kind)
        );
        f.Untyped().into()
      }
    }
  }

  fn extract_expr(&mut self, _expr: &'tcx hir::Expr<'tcx>, _dctx: &DefContext<'l>) -> st::Expr<'l> {
    // TODO: Implement
    self.extraction.factory.Int32Literal(1).into()
  }

  fn extract_fn(
    &mut self,
    item: &'l hir::Item,
    decl: &'l hir::FnDecl,
    _ty_params: &'l hir::Generics,
    body_id: hir::BodyId,
  ) -> &'l st::FunDef<'l> {
    let fun_hir_id = item.hir_id;
    self.nest_tables(fun_hir_id, |xtor| {
      let fun_id = xtor.fetch_id(fun_hir_id).id;
      let body = xtor.tcx.hir().body(body_id);

      // Build a DefContext that includes all variable bindings
      let mut collector = BindingsCollector::new(xtor, DefContext::new());
      collector.populate_from(body);
      let dctx = collector.as_def_context();

      // Get the function signature and extract the return type
      let sigs = xtor.tables.liberated_fn_sigs();
      let sig = sigs.get(fun_hir_id).unwrap();
      let return_tpe: st::Type<'l> = xtor.extract_ty(sig.output(), &dctx, decl.output.span());

      // Build parameter ValDefs
      let params: Vec<&'l st::ValDef<'l>> = body
        .params
        .iter()
        .zip(sig.inputs().into_iter())
        .enumerate()
        .map(|(index, (param, ty))| {
          let hir_id = param.hir_id;
          let ident_opt = param.pat.simple_ident();
          let id = ident_opt
            .map(|ident| xtor.register_id_from_ident(hir_id, &ident))
            .unwrap_or_else(|| xtor.register_id_from_name(hir_id, format!("param{}", index)));
          let tpe = xtor.extract_ty(ty, &dctx, param.span);
          let var = xtor.extraction.factory.Variable(id.id, tpe, vec![]);
          &*xtor.extraction.factory.ValDef(var)
        })
        .collect();

      // Extract the body
      let body_expr = xtor.extract_expr(&body.value, &dctx);

      println!(
        "EXTRACT FN: {:?} / {:?}\n  dctx: {:?}\n  body_expr: {:?}",
        item.ident, fun_id, dctx, body_expr
      );
      xtor
        .extraction
        .factory
        .FunDef(fun_id, vec![], params, return_tpe, body_expr, vec![])
    })
  }

  pub fn process_crate(&mut self, _mod_name: &str) {
    // TODO: Ignore certain boilerplate/compiler-generated items
    fn should_ignore<'tcx>(item: &'tcx hir::Item<'tcx>) -> bool {
      match item.kind {
        hir::ItemKind::ExternCrate(_) =>
          item.ident.name.to_string() == "std",
        hir::ItemKind::Use(ref path, hir::UseKind::Glob) =>
          path.to_string().starts_with("::std::prelude::v"),
        _ =>
          false
      }
    }

    struct ItemVisitor<'xtor, 'l, 'tcx> {
      xtor: &'xtor mut Extractor<'l, 'tcx>,
      functions: Vec<&'tcx hir::Item<'tcx>>,
    }

    impl<'xtor, 'l, 'tcx> ItemLikeVisitor<'tcx> for ItemVisitor<'xtor, 'l, 'tcx> {
      fn visit_item(&mut self, item: &'tcx hir::Item<'tcx>) {
        if let hir::ItemKind::Fn(..) = item.kind {
          self.xtor.register_id_from_ident(item.hir_id, &item.ident);
          self.functions.push(&item);
        } else if !should_ignore(item) {
          unsupported!(self.xtor.tcx.sess, item.span, "Other kind of item");
        }
      }

      // Unsupported
      fn visit_trait_item(&mut self, trait_item: &'tcx hir::TraitItem<'tcx>) {
        unsupported!(self.xtor.tcx.sess, trait_item.span, "Trait item");
      }

      // Handled above
      fn visit_impl_item(&mut self, _impl_item: &'tcx hir::ImplItem<'tcx>) {
        unreachable!();
      }
    }

    let krate = self.tcx.hir().krate();

    let mut visitor = ItemVisitor {
      xtor: self,
      functions: vec![],
    };
    krate.visit_all_item_likes(&mut visitor);

    for item in visitor.functions {
      if let hir::ItemKind::Fn(ref sig, ref generics, body_id) = item.kind {
        self.extract_fn(item, &sig.decl, generics, body_id);
      }
    }
  }
}
