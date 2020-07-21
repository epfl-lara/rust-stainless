use super::extractor::{Extractor, StainlessSymId};

use std::collections::HashMap;

use rustc_middle::hir::map::Map;
use rustc_middle::span_bug;
use rustc_middle::ty::{self, AdtDef, List, Ty, TyKind};

use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::intravisit::{self, NestedVisitorMap, Visitor};
use rustc_hir::itemlikevisit::ItemLikeVisitor;
use rustc_hir::{self as hir, ExprKind, HirId, ItemKind, PatKind, StmtKind};
use rustc_hir_pretty as pretty;

use rustc_span::{Span, DUMMY_SP};

use rustc_ast::ast;

use stainless_data::ast as st;

macro_rules! unsupported {
  ($sess:expr, $item:expr, $kind_name:expr) => {
    $sess.span_err($item, format!("Unsupported tree: {}", $kind_name).as_str());
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
    Self { xtor, dctx }
  }

  fn into_def_context(self) -> DefContext<'l> {
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

  fn nested_visit_map(&mut self) -> NestedVisitorMap<Self::Map> {
    NestedVisitorMap::None
  }

  fn visit_body(&mut self, _b: &'tcx hir::Body<'tcx>) {
    unreachable!();
  }

  fn visit_pat(&mut self, pattern: &'tcx hir::Pat<'tcx>) {
    match pattern.kind {
      PatKind::Binding(_, hir_id, ref _ident, ref optional_subpattern) => {
        // Extend DefContext with a new variable
        self.xtor.extract_binding(hir_id, &mut self.dctx);

        // Visit potential sub-patterns
        rustc_ast::walk_list!(self, visit_pat, optional_subpattern);
      }
      _ => intravisit::walk_pat(self, pattern),
    }
  }
}

/// Top-level extraction

type Result<'l> = std::result::Result<st::Expr<'l>, &'static str>;

/// These handlers simply store extracted constructs in the `Extractor`.
impl<'l, 'tcx> Extractor<'l, 'tcx> {
  /// Extract a binding based on the binding node's HIR id.
  /// Updates `dctx` if the binding hadn't been extacted before.
  fn extract_binding(&mut self, hir_id: HirId, dctx: &mut DefContext<'l>) -> &'l st::Variable<'l> {
    match dctx.vars.get(&hir_id) {
      Some(var) => var,
      None => {
        // Extract ident from corresponding HIR node, sanity-check binding mode
        let (id, span) = {
          let node = self.tcx().hir().find(hir_id).unwrap();
          let sess = self.tcx().sess;
          let ident = if let hir::Node::Binding(pat) = node {
            if let PatKind::Binding(_, _, ident, _) = pat.kind {
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
            let span = node.ident().map(|ident| ident.span).unwrap_or_default();
            unsupported!(
              sess,
              span,
              "Cannot extract complex pattern in binding (cannot recover from this)"
            );
            unreachable!()
          };
          (
            self.register_hir(hir_id, ident.name.to_string()),
            ident.span,
          )
        };

        // Build a Variable node
        // TODO: Extract flags on bindings
        let tpe = self.extract_ty(self.tables.node_type(hir_id), dctx, span);
        let var = self.factory().Variable(id, tpe, vec![]);
        dctx.add_var(hir_id, var);
        var
      }
    }
  }

  /// Extraction helpers

  fn unsupported_expr(&mut self, expr: &'tcx hir::Expr<'tcx>, msg: String) -> st::Expr<'l> {
    unsupported!(self.tcx().sess, expr.span, msg);
    let f = self.factory();
    f.NoTree(f.Untyped().into()).into()
  }

  fn is_bv_type(&self, ty: Ty<'tcx>) -> bool {
    match ty.kind {
      TyKind::Int(_) | TyKind::Uint(_) => true,
      _ => false,
    }
  }

  fn is_bigint(&self, adt_def: &'tcx AdtDef) -> bool {
    // TODO: Add a check for BigInt that avoids generating the string?
    self.tcx().def_path_str(adt_def.did) == "num_bigint::BigInt"
  }

  fn is_bigint_type(&self, ty: Ty<'tcx>) -> bool {
    match ty.kind {
      TyKind::Adt(adt_def, _) => self.is_bigint(adt_def),
      _ => false,
    }
  }

  /// Extraction methods

  fn extract_tys<I>(&mut self, tys: I, dctx: &DefContext<'l>, span: Span) -> Vec<st::Type<'l>>
  where
    I: IntoIterator<Item = Ty<'tcx>>,
  {
    tys
      .into_iter()
      .map(|ty| self.extract_ty(ty, dctx, span))
      .collect()
  }

  fn extract_ty(&mut self, ty: Ty<'tcx>, dctx: &DefContext<'l>, span: Span) -> st::Type<'l> {
    let f = self.factory();
    match ty.kind {
      TyKind::Bool => f.BooleanType().into(),
      TyKind::Adt(adt_def, _) if self.is_bigint(adt_def) => f.IntegerType().into(),
      TyKind::Int(ast::IntTy::I32) => f.BVType(true, 32).into(),
      TyKind::Tuple(..) => {
        let arg_tps = self.extract_tys(ty.tuple_fields(), dctx, span);
        if arg_tps.is_empty() {
          f.UnitType().into()
        } else {
          f.TupleType(arg_tps).into()
        }
      }
      _ => {
        unsupported!(
          self.tcx().sess,
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

  fn try_extract_bigint_lit(&mut self, expr: &'tcx hir::Expr<'tcx>) -> Result<'l> {
    use ast::LitKind;
    let f = self.factory();
    if let ExprKind::Lit(ref lit) = expr.kind {
      match lit.node {
        LitKind::Int(value, _) => {
          let node_ty = self.tables.node_type(expr.hir_id);
          match node_ty.kind {
            ty::Int(_) => Ok(f.IntegerLiteral((value as i128).into()).into()),
            _ => Err("Cannot extract BigInt from non-signed-int literal"),
          }
        }
        _ => Err("Cannot extract BigInt from non-integral literal kind"),
      }
    } else {
      Err("Can only extract BigInt from integer literals")
    }
  }

  fn try_extract_bigint_expr(
    &mut self,
    expr: &'tcx hir::Expr<'tcx>,
    dctx: &DefContext<'l>,
  ) -> Result<'l> {
    self.try_extract_bigint_lit(expr).or_else(|_| {
      let expr_ty = self.tables.node_type(expr.hir_id);
      if self.is_bigint_type(expr_ty) {
        Ok(self.extract_expr(expr, dctx))
      } else {
        Err("Not a BigInt-convertible expr")
      }
    })
  }

  fn extract_unary(&mut self, expr: &'tcx hir::Expr<'tcx>, dctx: &DefContext<'l>) -> st::Expr<'l> {
    use hir::UnOp;
    let f = self.factory();
    if let hir::ExprKind::Unary(op, arg) = expr.kind {
      let arg_ty = self.tables.node_type(arg.hir_id);
      let arg_is_bv = self.is_bv_type(arg_ty);
      let arg_is_int = arg_is_bv || self.is_bigint_type(arg_ty);
      let arg = self.extract_expr(arg, dctx);

      match op {
        UnOp::UnNot if arg_is_bv => f.BVNot(arg).into(),
        UnOp::UnNot if arg_ty.is_bool() => f.Not(arg).into(),
        UnOp::UnNeg if arg_is_int => f.UMinus(arg).into(),
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

      let arg1_bigint_opt = self.try_extract_bigint_expr(arg1, dctx).ok();
      let arg2_bigint_opt = self.try_extract_bigint_expr(arg2, dctx).ok();
      let (arg1, arg2, args_are_int) = match (arg1_bigint_opt, arg2_bigint_opt) {
        (Some(arg1), Some(arg2)) if !args_are_bv => (arg1, arg2, true),
        _ => (
          self.extract_expr(arg1, dctx),
          self.extract_expr(arg2, dctx),
          args_are_bv,
        ),
      };

      match op.node {
        BinOpKind::Add if args_are_int => f.Plus(arg1, arg2).into(),
        BinOpKind::Sub if args_are_int => f.Minus(arg1, arg2).into(),
        BinOpKind::Mul if args_are_int => f.Times(arg1, arg2).into(),
        BinOpKind::Div if args_are_int => f.Division(arg1, arg2).into(),
        BinOpKind::And if args_are_bool => f.And(vec![arg1, arg2]).into(),
        BinOpKind::Or if args_are_bool => f.Or(vec![arg1, arg2]).into(),
        BinOpKind::Eq if args_are_bool => f.Equals(arg1, arg2).into(),
        BinOpKind::Lt if args_are_int => f.LessThan(arg1, arg2).into(),
        BinOpKind::Le if args_are_int => f.LessEquals(arg1, arg2).into(),
        BinOpKind::Ne if args_are_int => f.Not(f.Equals(arg1, arg2).into()).into(),
        BinOpKind::Ge if args_are_int => f.GreaterEquals(arg1, arg2).into(),
        BinOpKind::Gt if args_are_int => f.GreaterThan(arg1, arg2).into(),
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
    if let ExprKind::Call(fun, args) = expr.kind {
      if let ExprKind::Path(ref qpath) = fun.kind {
        let qpath_res = self.tables.qpath_res(qpath, expr.hir_id);
        match qpath_res {
          Res::Def(DefKind::Fn, def_id) => {
            let args = self.extract_exprs(args, dctx);
            let fd_id = self.extract_fn(def_id);
            // TODO: Handle type arguments
            f.FunctionInvocation(fd_id, vec![], args).into()
          }
          Res::Def(DefKind::Ctor(..), def_id) => self.extract_adt_construction(def_id, args, dctx),
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

  // Expressions for which `e.clone()` can be translated simply as `e`.
  // This is sound, in particular, for types for which we don't extract any
  // mutating operations.
  fn can_treat_clone_as_identity(&self, expr: &'tcx hir::Expr<'tcx>) -> bool {
    let expr_ty = self.tables.node_type(expr.hir_id);
    match expr_ty.kind {
      TyKind::Adt(adt_def, _) => self.is_bigint(adt_def),
      _ => false,
    }
  }

  fn extract_conversion_into(
    &mut self,
    outer: &'tcx hir::Expr<'tcx>,
    inner: &'tcx hir::Expr<'tcx>,
  ) -> st::Expr<'l> {
    let from_ty = self.tables.node_type(inner.hir_id);
    let to_ty = self.tables.node_type(outer.hir_id);
    match (&from_ty.kind, &to_ty.kind) {
      (TyKind::Int(_), TyKind::Adt(adt_def, _)) if self.is_bigint(adt_def) => self
        .try_extract_bigint_lit(inner)
        .unwrap_or_else(|reason| self.unsupported_expr(inner, reason.into())),
      _ => self.unsupported_expr(
        outer,
        format!("Cannot extract conversion from {} to {}", from_ty, to_ty),
      ),
    }
  }

  // TODO: Extract type arguments in ADT constructors
  fn extract_adt_construction<I>(
    &mut self,
    def_id: DefId,
    args: I,
    dctx: &DefContext<'l>,
  ) -> st::Expr<'l>
  where
    I: IntoIterator<Item = &'tcx hir::Expr<'tcx>>,
  {
    let f = self.factory();
    let adt_id = self.extract_adt(def_id);
    let args = self.extract_exprs(args, dctx);
    f.ADT(adt_id, vec![], args).into()
  }

  fn extract_method_call(
    &mut self,
    expr: &'tcx hir::Expr<'tcx>,
    dctx: &DefContext<'l>,
  ) -> st::Expr<'l> {
    if let ExprKind::MethodCall(_path_seg, _, args) = expr.kind {
      let def_path = self
        .tables
        .type_dependent_def(expr.hir_id)
        .map(|(_, def_id)| def_id)
        .map(|def_id| self.tcx().def_path_str(def_id))
        .unwrap_or_else(|| "<unknown>".into());
      let arg = &args[0];
      // TODO: Fast check using `path_seg.ident.name == Symbol::intern("into")`?
      match def_path.as_str() {
        "std::convert::Into::into" => self.extract_conversion_into(expr, arg),
        "std::clone::Clone::clone" if self.can_treat_clone_as_identity(expr) => {
          self.extract_expr(arg, dctx)
        }
        _ => self.unsupported_expr(expr, "Cannot extract general method calls".into()),
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

  fn _extract_match(&mut self, _expr: &'tcx hir::Expr<'tcx>) -> st::Expr<'l> {
    unimplemented!()
  }

  fn extract_block_(
    &mut self,
    stmts: &'tcx [hir::Stmt<'tcx>],
    acc_exprs: &mut Vec<st::Expr<'l>>,
    final_expr: st::Expr<'l>,
    dctx: &DefContext<'l>,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let finish = |exprs: Vec<st::Expr<'l>>, final_expr| {
      if exprs.is_empty() {
        final_expr
      } else {
        f.Block(exprs, final_expr).into()
      }
    };

    let mut it = stmts.iter();
    if let Some(stmt) = it.next() {
      match stmt.kind {
        StmtKind::Local(local) => {
          let bail = |msg| -> st::Expr<'l> {
            unsupported!(self.tcx().sess, local.span, msg);
            f.Block(acc_exprs.clone(), f.NoTree(f.Untyped().into()).into())
              .into()
          };
          let has_abnormal_source = match local.source {
            hir::LocalSource::Normal => false,
            _ => true,
          };
          if has_abnormal_source {
            // TODO: Support for loops
            bail("Will not extract let that resulted from desugaring")
          } else if local.pat.simple_ident().is_none() {
            // TODO: Desugar complex patterns
            bail("Cannot extract complex pattern in let")
          } else if local.init.is_none() {
            bail("Cannot extract let without initializer")
          } else {
            let vd = f.ValDef(
              dctx
                .vars
                .get(&local.pat.hir_id)
                .unwrap_or_else(|| unexpected!(local.span, "unregistered variable")),
            );
            let init = self.extract_expr(local.init.unwrap(), dctx);
            let exprs = acc_exprs.clone();
            acc_exprs.clear();
            let body_expr = self.extract_block_(it.as_slice(), acc_exprs, final_expr, dctx);
            let last_expr = f.Let(vd, init, body_expr).into();
            finish(exprs, last_expr)
          }
        }
        StmtKind::Expr(expr) | StmtKind::Semi(expr) => {
          let expr = self.extract_expr(expr, dctx);
          acc_exprs.push(expr);
          self.extract_block_(it.as_slice(), acc_exprs, final_expr, dctx)
        }
        _ => self.extract_block_(it.as_slice(), acc_exprs, final_expr, dctx),
      }
    } else {
      finish(acc_exprs.clone(), final_expr)
    }
  }

  fn extract_block(
    &mut self,
    block: &'tcx hir::Block<'tcx>,
    dctx: &DefContext<'l>,
  ) -> st::Expr<'l> {
    let final_expr = block
      .expr
      .map(|e| self.extract_expr(e, dctx))
      .unwrap_or_else(|| self.factory().UnitLiteral().into());
    self.extract_block_(block.stmts, &mut vec![], final_expr, dctx)
  }

  fn extract_exprs<I>(&mut self, exprs: I, dctx: &DefContext<'l>) -> Vec<st::Expr<'l>>
  where
    I: IntoIterator<Item = &'tcx hir::Expr<'tcx>>,
  {
    exprs
      .into_iter()
      .map(|arg| self.extract_expr(arg, dctx))
      .collect()
  }

  fn extract_expr(&mut self, expr: &'tcx hir::Expr<'tcx>, dctx: &DefContext<'l>) -> st::Expr<'l> {
    let f = self.factory();
    match expr.kind {
      ExprKind::Match(
        scrut,
        arms,
        hir::MatchSource::IfDesugar {
          contains_else_clause: has_else,
        },
      ) => {
        assert_eq!(arms.len(), 2);
        let elze = if has_else { Some(arms[1].body) } else { None };
        self.extract_if(scrut, arms[0].body, elze, dctx)
      }
      ExprKind::Block(block, _) => {
        if let hir::BlockCheckMode::DefaultBlock = block.rules {
          self.extract_block(block, dctx)
        } else {
          self.unsupported_expr(expr, "Cannot extract unsafe block".into())
        }
      }
      ExprKind::Path(ref qpath) => {
        let qpath_res = self.tables.qpath_res(qpath, expr.hir_id);
        match qpath_res {
          Res::Def(DefKind::Ctor(..), def_id) => {
            self.extract_adt_construction(def_id, vec![], dctx)
          }
          Res::Def(kind, _def_id) => self.unsupported_expr(
            expr,
            format!(
              "Cannot extract Path expr of kind {:?} resolving to def kind {:?}",
              expr.kind, kind
            ),
          ),
          Res::Local(ref hir_id) => dctx
            .vars
            .get(hir_id)
            .map(|&v| v.into())
            .unwrap_or_else(|| unexpected!(expr.span, "unregistered variable")),
          _ => unexpected!(expr.span, "non-variable path expression"),
        }
      }
      ExprKind::Tup(args) => {
        let args = self.extract_exprs(args, dctx);
        f.Tuple(args).into()
      }
      ExprKind::Field(recv, ident) => {
        let recv = self.extract_expr(recv, dctx);
        match ident.name.to_ident_string().parse::<i32>() {
          Ok(index) => f.TupleSelect(recv, index + 1).into(),
          _ => unimplemented!(),
        }
      }
      ExprKind::Lit(ref lit) => self.extract_lit(expr, lit),
      ExprKind::Call(..) => self.extract_call(expr, dctx),
      ExprKind::MethodCall(..) => self.extract_method_call(expr, dctx),
      ExprKind::Unary(..) => self.extract_unary(expr, dctx),
      ExprKind::Binary(..) => self.extract_binary(expr, dctx),
      ExprKind::DropTemps(expr) => self.extract_expr(expr, dctx), // TODO: Investigate semantics
      _ => self.unsupported_expr(expr, format!("Cannot extract expr kind {:?}", expr.kind)),
    }
  }

  /// Extract a function declaration (regardless of whether it is local or external)
  // TODO: Extract flags on functions and parameters
  fn extract_fn(&mut self, def_id: DefId) -> StainlessSymId<'l> {
    match self.get_id_from_def(def_id) {
      Some(fun_id) => fun_id,
      None => {
        let fun_id = self.register_def(def_id);

        let f = self.factory();

        let is_external = !def_id.is_local();
        let span = self.tcx().span_of_impl(def_id).unwrap_or(DUMMY_SP);

        let generics = self.tcx().generics_of(def_id);
        if generics.count() > 0 {
          unsupported!(
            self.tcx().sess,
            span,
            "Type parameters on functions are unsupported"
          );
          unreachable!()
        }

        type Params<'l> = Vec<&'l st::ValDef<'l>>;
        type Parts<'l> = (Params<'l>, st::Type<'l>, st::Expr<'l>, Vec<st::Flag<'l>>);

        // Extract the function signature
        let (params, return_tpe, body_expr, flags): Parts<'l> = if is_external {
          let poly_fn_sig = self.tcx().fn_sig(def_id);
          let fn_sig = self.tcx().liberate_late_bound_regions(def_id, &poly_fn_sig);
          let dctx = DefContext::new();
          let params: Params<'l> = fn_sig
            .inputs()
            .iter()
            .enumerate()
            .map(|(i, ty)| {
              let id = self.fresh_param_id(i);
              let tpe = self.extract_ty(ty, &dctx, DUMMY_SP);
              let var = f.Variable(id, tpe, vec![]);
              &*f.ValDef(var)
            })
            .collect();
          let return_tpe = self.extract_ty(fn_sig.output(), &dctx, DUMMY_SP);

          // Attach an empty body
          let body_expr = f.NoTree(return_tpe).into();
          let flags = vec![f.Extern().into()];

          (params, return_tpe, body_expr, flags)
        } else {
          let tcx = self.tcx();
          let hir_id = tcx.hir().as_local_hir_id(def_id.expect_local());
          self.enter_body(hir_id, |bxtor| {
            // Fetch the HIR representation of the function
            let body_id = tcx.hir().body_owned_by(hir_id);
            let body: &rustc_hir::Body<'tcx> = tcx.hir().body(body_id);
            let decl = tcx.hir().fn_decl_by_hir_id(hir_id).unwrap();

            // Build a DefContext that includes all variable bindings
            let mut collector = BindingsCollector::new(bxtor.xtor(), DefContext::new());
            collector.populate_from(body);
            let mut dctx: DefContext = collector.into_def_context();

            // Extract the function signature and extract the signature
            let sigs = bxtor.tables().liberated_fn_sigs();
            let sig = sigs.get(hir_id).unwrap();
            let return_tpe = bxtor
              .xtor()
              .extract_ty(sig.output(), &dctx, decl.output.span());

            // let param = body.params.iter().next().unwrap();
            // let var = bxtor.xtor().extract_binding(param.pat.hir_id, &mut dctx);
            // let vd: &'l st::ValDef<'l> = &*f.ValDef(var);
            // let params: Params<'l> = vec![vd];

            let params: Params<'l> = body
              .params
              .iter()
              .map(|param| {
                let var = bxtor.xtor().extract_binding(param.pat.hir_id, &mut dctx);
                let vd: &'l st::ValDef<'l> = &*f.ValDef(var);
                vd
              })
              .collect();

            // Extract the body
            // let body_expr = bxtor.xtor().extract_expr(bxtor.hcx().mirror(&body.value), &dctx);
            let body_expr = bxtor.xtor().extract_expr(&body.value, &dctx);
            let flags = vec![];

            (params, return_tpe, body_expr, flags)
          })
        };

        let fd = f.FunDef(fun_id, vec![], params, return_tpe, body_expr, flags);
        self.add_function(fun_id, fd);
        fun_id
      }
    }
  }

  /// Extract an ADT (regardless of whether it is local or external)
  fn extract_adt(&mut self, def_id: DefId) -> StainlessSymId<'l> {
    match self.get_id_from_def(def_id) {
      Some(adt_id) => adt_id,
      None => {
        let adt_id = self.register_def(def_id);

        let f = self.factory();

        let adt_def = self.tcx().adt_def(def_id);
        let span = self.tcx().span_of_impl(def_id).unwrap_or(DUMMY_SP);

        // TODO: Support type parameters on ADTs
        // TODO: Extract flags on ADTs
        let generics = self.tcx().generics_of(def_id);
        if generics.count() > 0 {
          unsupported!(
            self.tcx().sess,
            span,
            "Type parameters on ADT are unsupported"
          );
          unreachable!()
        }

        let constructors = adt_def
          .variants
          .iter()
          .map(|variant| {
            let cons_id = self.register_def(variant.def_id);
            let fields = variant
              .fields
              .iter()
              .map(|field| {
                let field_id = self.register_def(field.did);
                let field_ty = field.ty(self.tcx(), List::empty());
                let field_ty = self.extract_ty(field_ty, &DefContext::new(), field.ident.span);
                // TODO: Extract flags on ADT fields
                let field = f.Variable(field_id, field_ty, vec![]);
                &*f.ValDef(field)
              })
              .collect();
            &*f.ADTConstructor(cons_id, adt_id, fields)
          })
          .collect();

        let adt = f.ADTSort(adt_id, vec![], constructors, vec![]);
        self.add_adt(adt_id, adt);
        adt_id
      }
    }
  }

  /// Entrypoint and output helpers

  pub fn process_crate(&mut self, _crate_name: String) {
    fn pretty_path(path: &hir::Path<'_>) -> String {
      pretty::to_string(pretty::NO_ANN, |s| s.print_path(path, false))
    }

    // TODO: Ignore certain boilerplate/compiler-generated items
    fn should_ignore<'tcx>(item: &'tcx hir::Item<'tcx>) -> bool {
      match item.kind {
        ItemKind::ExternCrate(_) => {
          let name = item.ident.name.to_string();
          name == "std" || name == "num_bigint"
        }
        ItemKind::Use(ref path, _) => {
          let path_str = pretty_path(path);
          path_str.starts_with("::std::prelude::v") || path_str.starts_with("num_bigint::")
        }
        // TODO: Quick fix to filter our synthetic functions
        ItemKind::Fn(..) if !item.attrs.is_empty() => true,
        _ => false,
      }
    }

    struct ItemVisitor<'xtor, 'l, 'tcx> {
      xtor: &'xtor mut Extractor<'l, 'tcx>,
      adts: Vec<&'tcx hir::Item<'tcx>>,
      functions: Vec<&'tcx hir::Item<'tcx>>,
    }

    impl<'xtor, 'l, 'tcx> ItemLikeVisitor<'tcx> for ItemVisitor<'xtor, 'l, 'tcx> {
      fn visit_item(&mut self, item: &'tcx hir::Item<'tcx>) {
        match item.kind {
          _ if should_ignore(item) => {}
          ItemKind::Enum(..) | ItemKind::Struct(..) | ItemKind::Fn(..) => {
            let def_id = self.xtor.tcx().hir().local_def_id(item.hir_id).to_def_id();
            let def_path_str = self.xtor.tcx().def_path_str(def_id);
            if let ItemKind::Fn(..) = item.kind {
              eprintln!("  - Fun {}", def_path_str);
              self.functions.push(&item);
            } else {
              eprintln!("  - ADT {}", def_path_str);
              self.adts.push(&item);
            }
          }
          _ => {
            unsupported!(self.xtor.tcx().sess, item.span, "Other kind of item");
          }
        }
      }

      // Unsupported
      fn visit_trait_item(&mut self, trait_item: &'tcx hir::TraitItem<'tcx>) {
        unsupported!(self.xtor.tcx().sess, trait_item.span, "Trait item");
      }

      // Handled above
      fn visit_impl_item(&mut self, _impl_item: &'tcx hir::ImplItem<'tcx>) {
        unreachable!();
      }
    }

    let krate = self.tcx().hir().krate();

    // Discover items in the local crate
    let mut visitor = ItemVisitor {
      xtor: self,
      adts: vec![],
      functions: vec![],
    };
    eprintln!("[ Discovering local definitions ]");
    krate.visit_all_item_likes(&mut visitor);
    eprintln!("");

    // Extract items
    let (adts, functions) = (visitor.adts, visitor.functions);
    for item in adts {
      let def_id = self.tcx().hir().local_def_id(item.hir_id).to_def_id();
      self.extract_adt(def_id);
    }
    for item in functions {
      let def_id = self.tcx().hir().local_def_id(item.hir_id).to_def_id();
      self.extract_fn(def_id);
    }
  }
}
