use super::extractor::{Extractor, StainlessSymId};

use std::collections::HashMap;

use rustc_middle::hir::map::Map;
use rustc_middle::span_bug;
use rustc_middle::ty::{self, AdtDef, Ty, TyKind};

use rustc_hir::def::{DefKind, Res};
use rustc_hir::intravisit::{self, NestedVisitorMap, Visitor};
use rustc_hir::itemlikevisit::ItemLikeVisitor;
use rustc_hir::{self as hir, ExprKind, HirId, StmtKind};
use rustc_hir_pretty as pretty;

use rustc_span::Span;

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
    use hir::PatKind;
    match pattern.kind {
      PatKind::Binding(_, hir_id, ref _ident, ref optional_subpattern) => {
        // Extend DefContext with a new variable
        let xtor = &mut self.xtor;
        let id = xtor.register_var(hir_id);
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

type Result<'l> = std::result::Result<st::Expr<'l>, &'static str>;

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

  fn is_bigint(&self, adt_def: &'tcx AdtDef) -> bool {
    // TODO: Add a check for BigInt that avoids generating the string?
    self.tcx.def_path_str(adt_def.did) == "num_bigint::BigInt"
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
            let hir_id = self.tcx.hir().as_local_hir_id(
              def_id
                .as_local()
                .unwrap_or_else(|| unexpected!(fun.span, "no local def id")),
            );
            let args = self.extract_exprs(args, dctx);
            let fun_id = self.fetch_id(hir_id);
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

  // Expressions for which `e.clone()` can be translated simply as `e`.
  // This is sound, in particular, for types for which we don't extract any
  // mutating operations.
  fn can_treat_clone_as_identity(&self, expr: &'tcx hir::Expr<'tcx>) -> bool {
    let expr_ty = self.tables.node_type(expr.hir_id);
    match expr_ty.kind {
      TyKind::Adt(adt_ref, _) => self.is_bigint(adt_ref),
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
        .map(|def_id| self.tcx.def_path_str(def_id))
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
            unsupported!(self.tcx.sess, local.span, msg);
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
          Res::Def(_kind, _def_id) => unimplemented!(),
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
    let f = self.factory();
    let fun_hir_id = item.hir_id;
    self.nest_tables(fun_hir_id, |xtor| {
      let fun_id = xtor.fetch_id(fun_hir_id);
      let body = xtor.tcx.hir().body(body_id);

      // Build a DefContext that includes all variable bindings
      let mut collector = BindingsCollector::new(xtor, DefContext::new());
      collector.populate_from(body);
      let dctx = collector.into_def_context();

      // Get the function signature and extract the return type
      let sigs = xtor.tables.liberated_fn_sigs();
      let sig = sigs.get(fun_hir_id).unwrap();
      let return_tpe: st::Type<'l> = xtor.extract_ty(sig.output(), &dctx, decl.output.span());

      // Build parameter ValDefs
      let params: Vec<&'l st::ValDef<'l>> = body
        .params
        .iter()
        .zip(sig.inputs().iter())
        .enumerate()
        .map(|(index, (param, ty))| {
          let hir_id = param.pat.hir_id;
          let id = xtor
            .get_id(hir_id) // already registered via BindingsCollector
            .or_else(|| {
              param
                .pat
                .simple_ident()
                .map(|ident| xtor.register_id_from_ident(hir_id, &ident))
            })
            .unwrap_or_else(|| xtor.register_id_from_name(hir_id, format!("param{}", index)));
          // TODO: Desugar to match in case of non-simple-ident bindings (e.g. `(a, b): (i32, i32)`)
          if param.pat.simple_ident().is_none() {
            unsupported!(
              xtor.tcx.sess,
              param.pat.span,
              "Cannot extract complex pattern in parameter"
            );
          }
          let tpe = xtor.extract_ty(ty, &dctx, param.span);
          let var = f.Variable(id, tpe, vec![]);
          &*f.ValDef(var)
        })
        .collect();

      // Extract the body
      let body_expr = xtor.extract_expr(&body.value, &dctx);
      f.FunDef(fun_id, vec![], params, return_tpe, body_expr, vec![])
    })
  }

  fn output_program<P: AsRef<std::path::Path>>(&mut self, path: P, symbols: st::Symbols<'l>) -> () {
    use stainless_data::ser::{BufferSerializer, Serializable};
    let mut ser = BufferSerializer::new();
    symbols
      .serialize(&mut ser)
      .expect("Unable to serialize stainless program");
    std::fs::write(path, ser.as_slice()).expect("Unable to write serialized stainless program");
  }

  pub fn process_crate(&mut self, _mod_name: &str) {
    fn pretty_path(path: &hir::Path<'_>) -> String {
      pretty::to_string(pretty::NO_ANN, |s| s.print_path(path, false))
    }

    // TODO: Ignore certain boilerplate/compiler-generated items
    fn should_ignore<'tcx>(item: &'tcx hir::Item<'tcx>) -> bool {
      match item.kind {
        hir::ItemKind::ExternCrate(_) => {
          let name = item.ident.name.to_string();
          name == "std" || name == "num_bigint"
        }
        hir::ItemKind::Use(ref path, _) => {
          let path_str = pretty_path(path);
          path_str.starts_with("::std::prelude::v") || path_str.starts_with("num_bigint::")
        }
        // TODO: Quick fix to filter our synthetic functions
        hir::ItemKind::Fn(..) if !item.attrs.is_empty() => true,
        _ => false,
      }
    }

    struct ItemVisitor<'xtor, 'l, 'tcx> {
      xtor: &'xtor mut Extractor<'l, 'tcx>,
      functions: Vec<&'tcx hir::Item<'tcx>>,
    }

    impl<'xtor, 'l, 'tcx> ItemLikeVisitor<'tcx> for ItemVisitor<'xtor, 'l, 'tcx> {
      fn visit_item(&mut self, item: &'tcx hir::Item<'tcx>) {
        if !should_ignore(item) {
          if let hir::ItemKind::Fn(..) = item.kind {
            self.xtor.register_id_from_ident(item.hir_id, &item.ident);
            self.functions.push(&item);
          } else {
            unsupported!(self.xtor.tcx.sess, item.span, "Other kind of item");
          }
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

    let mut functions: Vec<&st::FunDef<'l>> = vec![];

    for item in visitor.functions {
      if let hir::ItemKind::Fn(ref sig, ref generics, body_id) = item.kind {
        eprintln!("== FUNCTION: {:?} ==", item.ident);
        let fd = self.extract_fn(item, &sig.decl, generics, body_id);
        eprintln!("{}", fd);
        functions.push(fd);
      }
    }

    let output_path = std::path::Path::new("./output.inoxser");
    self.output_program(output_path, st::Symbols::new(vec![], functions));
  }
}
