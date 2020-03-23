extern crate rustc;
extern crate rustc_ast;
extern crate rustc_hir;
extern crate rustc_span;
extern crate stainless_data;

use super::extractor::{Extractor, StainlessSymId};

use std::collections::HashMap;

use rustc::hir::map::Map;
use rustc::span_bug;
use rustc::ty::{self, Ty, TyKind};

use rustc_hir::def::{DefKind, Res};
use rustc_hir::intravisit::{self, NestedVisitorMap, Visitor};
use rustc_hir::itemlikevisit::ItemLikeVisitor;
use rustc_hir::{self as hir, ExprKind, HirId};

use rustc_span::Span;

use rustc_ast::ast;

use stainless_data::ast as st;

macro_rules! unsupported {
  ($sess:expr, $item:expr, $kind_name:expr) => {
    $sess.span_warn($item, format!("Unsupported tree: {}", $kind_name).as_str());
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
            _ => unsupported!(
              sess,
              pat.span,
              "Only immutable by-value bindings are supported"
            ),
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

  /// Extraction helpers

  fn unsupported_expr(&mut self, expr: &'tcx hir::Expr<'tcx>, msg: String) -> st::Expr<'l> {
    unsupported!(self.tcx.sess, expr.span, msg);
    let f = self.factory();
    f.NoTree(f.Untyped().into()).into()
  }

  fn is_bv_type(&self, ty: Ty<'tcx>) -> bool {
    match ty.kind {
      TyKind::Int(_) | TyKind::Uint(_) => true,
      _ => false,
    }
  }

  /// Extraction methods

  fn extract_ty(&mut self, ty: Ty<'tcx>, _dctx: &DefContext<'l>, span: Span) -> st::Type<'l> {
    let f = self.factory();
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

  fn extract_lit(&mut self, expr: &'tcx hir::Expr<'tcx>, lit: &hir::Lit) -> st::Expr<'l> {
    use ast::LitKind;
    let f = self.factory();
    match lit.node {
      LitKind::Bool(value) => f.BooleanLiteral(value).into(),
      LitKind::Int(value, _) => {
        let node_ty = self.tables.node_type(expr.hir_id);
        match node_ty.kind {
          ty::Int(ast::IntTy::I32) => f.Int32Literal(value as i32).into(),
          _ => self.unsupported_expr(
            expr,
            format!("Cannot extract literal kind {:?} (ty {:?})", lit, node_ty),
          ),
        }
      }
      _ => self.unsupported_expr(expr, format!("Cannot extract literal kind {:?}", lit)),
    }
  }

  fn extract_unary(&mut self, expr: &'tcx hir::Expr<'tcx>, dctx: &DefContext<'l>) -> st::Expr<'l> {
    use hir::UnOp;
    let f = self.factory();
    if let hir::ExprKind::Unary(op, arg) = expr.kind {
      let arg_ty = self.tables.node_type(arg.hir_id);
      let arg = self.extract_expr(arg, dctx);
      match op {
        UnOp::UnNot if self.is_bv_type(arg_ty) => f.BVNot(arg).into(),
        UnOp::UnNot if arg_ty.is_bool() => f.Not(arg).into(),
        UnOp::UnNeg if self.is_bv_type(arg_ty) => f.UMinus(arg).into(),
        _ => {
          // TODO: Handle Deref
          // TODO: Handle user-defined operators
          self.unsupported_expr(expr, format!("Cannot extract unary op {:?}", expr.kind))
        }
      }
    } else {
      unreachable!()
    }
  }

  fn extract_binary(&mut self, expr: &'tcx hir::Expr<'tcx>, dctx: &DefContext<'l>) -> st::Expr<'l> {
    use hir::BinOpKind;
    let f = self.factory();
    if let hir::ExprKind::Binary(op, arg1, arg2) = expr.kind {
      let arg1_ty = self.tables.node_type(arg1.hir_id);
      let arg2_ty = self.tables.node_type(arg2.hir_id);
      let args_are_bv = self.is_bv_type(arg1_ty) && self.is_bv_type(arg2_ty);
      let args_are_bool = arg1_ty.is_bool() && arg2_ty.is_bool();
      let arg1 = self.extract_expr(arg1, dctx);
      let arg2 = self.extract_expr(arg2, dctx);
      match op.node {
        BinOpKind::Add if args_are_bv => f.Plus(arg1, arg2).into(),
        BinOpKind::Sub if args_are_bv => f.Minus(arg1, arg2).into(),
        BinOpKind::Mul if args_are_bv => f.Times(arg1, arg2).into(),
        BinOpKind::Div if args_are_bv => f.Division(arg1, arg2).into(),
        BinOpKind::And if args_are_bool => f.And(vec![arg1, arg2]).into(),
        BinOpKind::Or if args_are_bool => f.Or(vec![arg1, arg2]).into(),
        BinOpKind::Eq if args_are_bool => f.Equals(arg1, arg2).into(),
        BinOpKind::Lt if args_are_bv => f.LessThan(arg1, arg2).into(),
        BinOpKind::Le if args_are_bv => f.LessEquals(arg1, arg2).into(),
        BinOpKind::Ne if args_are_bv => f.Not(f.Equals(arg1, arg2).into()).into(),
        BinOpKind::Ge if args_are_bv => f.GreaterEquals(arg1, arg2).into(),
        BinOpKind::Gt if args_are_bv => f.GreaterThan(arg1, arg2).into(),
        _ => {
          // TODO: Handle Rem, BitXor, BitAnd, BitOr, Shl, Shr
          // TODO: Handle arbitrary-precision integers
          // TODO: Handle user-defined operators
          self.unsupported_expr(expr, format!("Cannot extract binary op {:?}", expr.kind))
        }
      }
    } else {
      unreachable!()
    }
  }

  fn extract_call(&mut self, expr: &'tcx hir::Expr<'tcx>, dctx: &DefContext<'l>) -> st::Expr<'l> {
    let f = self.factory();
    if let ExprKind::Call(fun, args) = &expr.kind {
      if let ExprKind::Path(qpath) = &fun.kind {
        let qpath_res = self.tables.qpath_res(qpath, expr.hir_id);
        match qpath_res {
          Res::Def(DefKind::Fn, def_id) => {
            let hir_id = self
              .tcx
              .hir()
              .as_local_hir_id(def_id)
              .unwrap_or_else(|| unexpected!(fun.span, "no local def id"));
            let args: Vec<st::Expr<'l>> = args
              .iter()
              .map(|arg| self.extract_expr(arg, dctx).into())
              .collect();
            let fun_id = self.fetch_id(hir_id).id;
            // TODO: Handle type arguments
            f.FunctionInvocation(fun_id, vec![], args).into()
          }
          Res::Def(DefKind::Ctor(_ctor_of, _ctor_kind), _def_id) => unimplemented!(),
          Res::Def(..) => unexpected!(expr.span, "function of unknown definition kind"),
          res => self.unsupported_expr(
            expr,
            format!(
              "Cannot extract call to function without known definition (resolved to {:?})",
              res
            ),
          ),
        }
      } else {
        self.unsupported_expr(
          expr,
          "Cannot extract call to function given as non-path".into(),
        )
      }
    } else {
      unreachable!()
    }
  }

  fn extract_if(
    &mut self,
    cond: &'tcx hir::Expr<'tcx>,
    then: &'tcx hir::Expr<'tcx>,
    elze_opt: Option<&'tcx hir::Expr<'tcx>>,
    dctx: &DefContext<'l>,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let cond = self.extract_expr(cond, dctx);
    let then = self.extract_expr(then, dctx);
    let elze = elze_opt
      .map(|e| self.extract_expr(e, dctx))
      .unwrap_or_else(|| {
        // TODO: Match the type of the then branch?
        f.UnitLiteral().into()
      });
    f.IfExpr(cond, then, elze).into()
  }

  fn extract_block(
    &mut self,
    block: &'tcx hir::Block<'tcx>,
    dctx: &DefContext<'l>,
  ) -> st::Expr<'l> {
    fn rec<'l, 'tcx>(
      xtor: &mut Extractor,
      stmts: &'tcx [hir::Stmt<'tcx>],
      last_expr: st::Expr<'l>,
    ) -> st::Expr<'l> {
      let mut it = stmts.iter();
      if let Some(_stmt) = it.next() {
        rec(xtor, it.as_slice(), last_expr)
      } else {
        last_expr
      }
    }

    let f = self.factory();
    let last_expr = block
      .expr
      .map(|e| self.extract_expr(e, dctx))
      .unwrap_or_else(|| f.UnitLiteral().into());
    // let exprs = block.stmts.iter().filter_map(|&stmt| match &stmt.kind {});
    // block
    //   .expr
    //   .map(|e| self.extract_expr(e, dctx))
    //   .unwrap_or_else(|| f.UnitLiteral().into())
    rec(self, block.stmts, last_expr)
  }

  fn extract_expr(&mut self, expr: &'tcx hir::Expr<'tcx>, dctx: &DefContext<'l>) -> st::Expr<'l> {
    match &expr.kind {
      ExprKind::Lit(lit) => self.extract_lit(expr, lit),
      ExprKind::Match(
        scrut,
        arms,
        hir::MatchSource::IfDesugar {
          contains_else_clause: has_else,
        },
      ) => {
        assert_eq!(arms.len(), 2);
        let elze = if *has_else { Some(arms[1].body) } else { None };
        self.extract_if(scrut, arms[0].body, elze, dctx)
      }
      ExprKind::Block(block, _) => {
        if let hir::BlockCheckMode::DefaultBlock = block.rules {
          self.extract_block(block, dctx)
        } else {
          self.unsupported_expr(expr, "Cannot extract unsafe block".into())
        }
      }
      ExprKind::Path(qpath) => {
        let qpath_res = self.tables.qpath_res(qpath, expr.hir_id);
        match qpath_res {
          Res::Def(_kind, _def_id) => unimplemented!(),
          Res::Local(ref hir_id) => dctx
            .vars
            .get(hir_id)
            .map(|&v| v.into())
            .unwrap_or_else(|| unexpected!(expr.span, "unregistered variable")),
          _ => unexpected!(expr.span, "non-variable path expression"),
        }
      }
      ExprKind::Call(..) => self.extract_call(expr, dctx),
      ExprKind::Unary(..) => self.extract_unary(expr, dctx),
      ExprKind::Binary(..) => self.extract_binary(expr, dctx),
      ExprKind::DropTemps(expr) => self.extract_expr(expr, dctx),
      _ => self.unsupported_expr(expr, format!("Cannot extract expr kind {:?}", expr.kind)),
    }
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

      // println!(
      //   "EXTRACT FN: {:?} / {:?}\n  dctx: {:?}\n  body_expr: {:?}",
      //   item.ident, fun_id, dctx, body_expr
      // );
      for vd in dctx.vars.values() {
        println!(" - {}: {}", vd.id, vd.tpe);
      }
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
        hir::ItemKind::ExternCrate(_) => item.ident.name.to_string() == "std",
        hir::ItemKind::Use(ref path, hir::UseKind::Glob) => {
          path.to_string().starts_with("::std::prelude::v")
        }
        _ => false,
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
        let impl_id = self.tcx.hir().local_def_id(item.hir_id);
        println!("FUNCTION: {:?} / {:?}", impl_id, item.ident);
        let fd = self.extract_fn(item, &sig.decl, generics, body_id);
        println!(" ==>\n{}\n", fd)
      }
    }
  }
}
