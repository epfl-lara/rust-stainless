use super::*;

use rustc_ast::ast;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::{self as hir, ExprKind, StmtKind};
use rustc_middle::ty::{self, TyKind};

use stainless_data::ast as st;

type Result<'l> = std::result::Result<st::Expr<'l>, &'static str>;

/// Extraction of bodies (i.e., expressions, for the most part)
impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_expr(&mut self, expr: &'tcx hir::Expr<'tcx>) -> st::Expr<'l> {
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
        self.extract_if(scrut, arms[0].body, elze)
      }
      ExprKind::Block(block, _) => {
        if let hir::BlockCheckMode::DefaultBlock = block.rules {
          self.extract_block(block)
        } else {
          self.unsupported_expr(expr, "Cannot extract unsafe block")
        }
      }
      ExprKind::Path(ref qpath) => {
        let qpath_res = self.tables.qpath_res(qpath, expr.hir_id);
        match qpath_res {
          Res::Def(DefKind::Ctor(..), def_id) => self.extract_adt_construction(def_id, vec![]),
          Res::Def(kind, _def_id) => self.unsupported_expr(
            expr,
            format!(
              "Cannot extract Path expr of kind {:?} resolving to def kind {:?}",
              expr.kind, kind
            ),
          ),
          Res::Local(hir_id) => self.fetch_var(hir_id).into(),
          _ => unexpected(expr.span, "non-variable path expression"),
        }
      }
      ExprKind::Tup(args) => {
        let args = self.extract_exprs(args);
        f.Tuple(args).into()
      }
      ExprKind::Field(recv, ident) => {
        let recv = self.extract_expr(recv);
        match ident.name.to_ident_string().parse::<i32>() {
          Ok(index) => f.TupleSelect(recv, index + 1).into(),
          _ => unimplemented!(),
        }
      }
      ExprKind::Lit(ref lit) => self.extract_lit(expr, lit),
      ExprKind::Call(..) => self.extract_call(expr),
      ExprKind::MethodCall(..) => self.extract_method_call(expr),
      ExprKind::Unary(..) => self.extract_unary(expr),
      ExprKind::Binary(..) => self.extract_binary(expr),
      ExprKind::DropTemps(expr) => self.extract_expr(expr), // TODO: Investigate semantics
      _ => self.unsupported_expr(expr, format!("Cannot extract expr kind {:?}", expr.kind)),
    }
  }

  fn extract_exprs<I>(&mut self, exprs: I) -> Vec<st::Expr<'l>>
  where
    I: IntoIterator<Item = &'tcx hir::Expr<'tcx>>,
  {
    exprs
      .into_iter()
      .map(|arg| self.extract_expr(arg))
      .collect()
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

  fn try_extract_bigint_expr(&mut self, expr: &'tcx hir::Expr<'tcx>) -> Result<'l> {
    self.try_extract_bigint_lit(expr).or_else(|_| {
      let expr_ty = self.tables.node_type(expr.hir_id);
      if self.base.is_bigint_type(expr_ty) {
        Ok(self.extract_expr(expr))
      } else {
        Err("Not a BigInt-convertible expr")
      }
    })
  }

  fn extract_unary(&mut self, expr: &'tcx hir::Expr<'tcx>) -> st::Expr<'l> {
    use hir::UnOp;
    let f = self.factory();
    if let hir::ExprKind::Unary(op, arg) = expr.kind {
      let arg_ty = self.tables.node_type(arg.hir_id);
      let arg_is_bv = self.base.is_bv_type(arg_ty);
      let arg_is_int = arg_is_bv || self.base.is_bigint_type(arg_ty);
      let arg = self.extract_expr(arg);

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

  fn extract_binary(&mut self, expr: &'tcx hir::Expr<'tcx>) -> st::Expr<'l> {
    use hir::BinOpKind;
    let f = self.factory();
    if let hir::ExprKind::Binary(op, arg1, arg2) = expr.kind {
      let arg1_ty = self.tables.node_type(arg1.hir_id);
      let arg2_ty = self.tables.node_type(arg2.hir_id);
      let args_are_bv = self.base.is_bv_type(arg1_ty) && self.base.is_bv_type(arg2_ty);
      let args_are_bool = arg1_ty.is_bool() && arg2_ty.is_bool();

      let arg1_bigint_opt = self.try_extract_bigint_expr(arg1).ok();
      let arg2_bigint_opt = self.try_extract_bigint_expr(arg2).ok();
      let (arg1, arg2, args_are_int) = match (arg1_bigint_opt, arg2_bigint_opt) {
        (Some(arg1), Some(arg2)) if !args_are_bv => (arg1, arg2, true),
        _ => (
          self.extract_expr(arg1),
          self.extract_expr(arg2),
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

  fn extract_call(&mut self, expr: &'tcx hir::Expr<'tcx>) -> st::Expr<'l> {
    let f = self.factory();
    if let ExprKind::Call(fun, args) = expr.kind {
      if let ExprKind::Path(ref qpath) = fun.kind {
        let qpath_res = self.tables.qpath_res(qpath, expr.hir_id);
        match qpath_res {
          Res::Def(DefKind::Fn, def_id) => {
            let args = self.extract_exprs(args);
            let fd_id = self.base.extract_fn_ref(def_id);
            // TODO: Handle type arguments
            f.FunctionInvocation(fd_id, vec![], args).into()
          }
          Res::Def(DefKind::Ctor(..), def_id) => self.extract_adt_construction(def_id, args),
          Res::Def(..) => unexpected(expr.span, "function of unknown definition kind"),
          res => self.unsupported_expr(
            expr,
            format!(
              "Cannot extract call to function without known definition (resolved to {:?})",
              res
            ),
          ),
        }
      } else {
        self.unsupported_expr(expr, "Cannot extract call to function given as non-path")
      }
    } else {
      unreachable!()
    }
  }

  // Expressions for which `e.clone()` can be translated simply as `e`.
  // This is sound, in particular, for types for which we don't extract any
  // mutating operations.
  fn can_treat_clone_as_identity(&mut self, expr: &'tcx hir::Expr<'tcx>) -> bool {
    let expr_ty = self.tables.node_type(expr.hir_id);
    match expr_ty.kind {
      TyKind::Adt(adt_def, _) => self.base.is_bigint(adt_def),
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
      (TyKind::Int(_), TyKind::Adt(adt_def, _)) if self.base.is_bigint(adt_def) => self
        .try_extract_bigint_lit(inner)
        .unwrap_or_else(|reason| self.unsupported_expr(inner, reason)),
      _ => self.unsupported_expr(
        outer,
        format!("Cannot extract conversion from {} to {}", from_ty, to_ty),
      ),
    }
  }

  // TODO: Extract type arguments in ADT constructors
  fn extract_adt_construction<I>(&mut self, def_id: DefId, args: I) -> st::Expr<'l>
  where
    I: IntoIterator<Item = &'tcx hir::Expr<'tcx>>,
  {
    let f = self.factory();
    let adt_id = self.base.extract_adt(def_id);
    let args = self.extract_exprs(args);
    f.ADT(adt_id, vec![], args).into()
  }

  fn extract_method_call(&mut self, expr: &'tcx hir::Expr<'tcx>) -> st::Expr<'l> {
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
          self.extract_expr(arg)
        }
        _ => self.unsupported_expr(expr, "Cannot extract general method calls"),
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
  ) -> st::Expr<'l> {
    let f = self.factory();
    let cond = self.extract_expr(cond);
    let then = self.extract_expr(then);
    let elze = elze_opt.map(|e| self.extract_expr(e)).unwrap_or_else(|| {
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
            self.base.unsupported(local.span, msg);
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
            let vd = f.ValDef(self.fetch_var(local.pat.hir_id));
            let init = self.extract_expr(local.init.unwrap());
            let exprs = acc_exprs.clone();
            acc_exprs.clear();
            let body_expr = self.extract_block_(it.as_slice(), acc_exprs, final_expr);
            let last_expr = f.Let(vd, init, body_expr).into();
            finish(exprs, last_expr)
          }
        }
        StmtKind::Expr(expr) | StmtKind::Semi(expr) => {
          let expr = self.extract_expr(expr);
          acc_exprs.push(expr);
          self.extract_block_(it.as_slice(), acc_exprs, final_expr)
        }
        _ => self.extract_block_(it.as_slice(), acc_exprs, final_expr),
      }
    } else {
      finish(acc_exprs.clone(), final_expr)
    }
  }

  fn extract_block(&mut self, block: &'tcx hir::Block<'tcx>) -> st::Expr<'l> {
    let final_expr = block
      .expr
      .map(|e| self.extract_expr(e))
      .unwrap_or_else(|| self.factory().UnitLiteral().into());
    self.extract_block_(block.stmts, &mut vec![], final_expr)
  }

  fn unsupported_expr<M: Into<String>>(
    &mut self,
    expr: &'tcx hir::Expr<'tcx>,
    msg: M,
  ) -> st::Expr<'l> {
    self.base.unsupported(expr.span, msg);
    let f = self.factory();
    f.NoTree(f.Untyped().into()).into()
  }
}
