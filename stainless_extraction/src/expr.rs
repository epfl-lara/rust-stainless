use super::literal::Literal;
use super::std_items::StdItem;
use super::std_items::StdItem::*;
use super::*;

use std::convert::TryFrom;

use rustc_middle::mir::{BinOp, BorrowKind, Mutability, UnOp};
use rustc_middle::ty::{subst::SubstsRef, Ty, TyKind};

use rustc_hair::hair::{
  Arm, BindingMode, Block, BlockSafety, Expr, ExprKind, ExprRef, FieldPat, Guard, LogicalOp,
  Mirror, Pat, PatKind, StmtKind, StmtRef,
};

use stainless_data::ast as st;

type Result<T> = std::result::Result<T, &'static str>;

/// Extraction of bodies (i.e., expressions, for the most part)
impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_expr(&mut self, expr: Expr<'tcx>) -> st::Expr<'l> {
    match expr.kind {
      ExprKind::Literal { literal: konst, .. } => match Literal::try_from(konst) {
        Ok(lit) => lit.as_st_literal(self.factory()),
        _ => self.unsupported_expr(expr.span, "Unsupported kind of literal"),
      },
      ExprKind::Unary { .. } => self.extract_unary(expr),
      ExprKind::Binary { .. } => self.extract_binary(expr),
      ExprKind::LogicalOp { .. } => self.extract_logical_op(expr),
      ExprKind::Tuple { .. } => self.extract_tuple(expr),
      ExprKind::Field { .. } => self.extract_field(expr),
      ExprKind::VarRef { id } => self.fetch_var(id).into(),
      ExprKind::Call { ty, ref args, .. } => self.extract_call_like(ty, args, expr.span),
      ExprKind::Adt { .. } => self.extract_adt_construction(expr),
      ExprKind::Block { body: ast_block } => {
        let block = self.mirror(ast_block);
        match block.safety_mode {
          BlockSafety::Safe => self.extract_block(block),
          _ => self.unsupported_expr(expr.span, "Cannot extract unsafe block"),
        }
      }
      ExprKind::Match {
        scrutinee,
        mut arms,
      } => {
        // TODO: Avoid this clone by just looking up the type of scrutinee for looks_like_if
        let scrutinee_ = scrutinee.clone();
        match self.looks_like_if(scrutinee, &arms) {
          Some(has_elze) => {
            let elze = arms.pop().unwrap().body;
            let then = arms.pop().unwrap().body;
            let elze_opt = if has_elze { Some(elze) } else { None };
            self.extract_if(scrutinee_, then, elze_opt)
          }
          None => self.extract_match(scrutinee_, arms),
        }
      }

      // TODO: Handle arbitrary-precision integers
      ExprKind::Scope { value, .. } => self.extract_expr_ref(value),
      ExprKind::Use { source } => self.extract_expr_ref(source),
      ExprKind::NeverToAny { source } => self.extract_expr_ref(source),

      ExprKind::Deref { arg } => self.extract_expr_ref(arg),

      // Borrow an immutable and aliasable value (i.e. the meaning of
      // BorrowKind::Shared). Handle this safe case with erasure.
      ExprKind::Borrow {
        borrow_kind: BorrowKind::Shared,
        arg,
      } => self.extract_expr_ref(arg),

      _ => self.unsupported_expr(
        expr.span,
        format!("Cannot extract expr kind {:?}", expr.kind),
      ),
    }
  }

  fn extract_expr_ref(&mut self, expr: ExprRef<'tcx>) -> st::Expr<'l> {
    let expr = self.mirror(expr);
    self.extract_expr(expr)
  }

  fn extract_expr_refs<I>(&mut self, exprs: I) -> Vec<st::Expr<'l>>
  where
    I: IntoIterator<Item = ExprRef<'tcx>>,
  {
    exprs
      .into_iter()
      .map(|arg| self.extract_expr_ref(arg))
      .collect()
  }

  fn extract_unary(&mut self, expr: Expr<'tcx>) -> st::Expr<'l> {
    let f = self.factory();
    if let ExprKind::Unary { op, arg } = expr.kind {
      let arg = self.mirror(arg);
      let arg_ty = arg.ty;
      let arg_is_bv = self.base.is_bv_type(arg_ty);
      let arg_is_int = arg_is_bv || self.base.is_bigint_type(arg_ty);
      let arg = self.extract_expr(arg);

      match op {
        UnOp::Not if arg_is_bv => f.BVNot(arg).into(),
        UnOp::Not if arg_ty.is_bool() => f.Not(arg).into(),
        UnOp::Neg if arg_is_int => f.UMinus(arg).into(),
        _ => unexpected(expr.span, format!("Cannot extract unary op {:?}", op)),
      }
    } else {
      unreachable!()
    }
  }

  fn extract_binary(&mut self, expr: Expr<'tcx>) -> st::Expr<'l> {
    let f = self.factory();
    if let ExprKind::Binary {
      op,
      lhs: arg1,
      rhs: arg2,
    } = expr.kind
    {
      let (arg1, arg2) = (self.mirror(arg1), self.mirror(arg2));
      let (arg1_ty, arg2_ty) = (arg1.ty, arg2.ty);
      let args_are_bv = self.base.is_bv_type(arg1_ty) && self.base.is_bv_type(arg2_ty);
      let args_are_bool = arg1_ty.is_bool() && arg2_ty.is_bool();

      let (arg1, arg2) = (self.extract_expr(arg1), self.extract_expr(arg2));
      match op {
        _ if !args_are_bv && !args_are_bool => {
          self.unsupported_expr(expr.span, format!("Cannot extract binary op {:?}", op))
        }

        BinOp::Eq => f.Equals(arg1, arg2).into(),
        BinOp::Ne => f.Not(f.Equals(arg1, arg2).into()).into(),
        BinOp::Add if args_are_bv => f.Plus(arg1, arg2).into(),
        BinOp::Sub if args_are_bv => f.Minus(arg1, arg2).into(),
        BinOp::Mul if args_are_bv => f.Times(arg1, arg2).into(),
        BinOp::Div if args_are_bv => f.Division(arg1, arg2).into(),
        BinOp::Rem if args_are_bv => f.Remainder(arg1, arg2).into(),
        BinOp::Lt if args_are_bv => f.LessThan(arg1, arg2).into(),
        BinOp::Le if args_are_bv => f.LessEquals(arg1, arg2).into(),
        BinOp::Ge if args_are_bv => f.GreaterEquals(arg1, arg2).into(),
        BinOp::Gt if args_are_bv => f.GreaterThan(arg1, arg2).into(),
        BinOp::BitXor if args_are_bv => f.BVXor(arg1, arg2).into(),
        BinOp::BitAnd if args_are_bv => f.BVAnd(arg1, arg2).into(),
        BinOp::BitOr if args_are_bv => f.BVOr(arg1, arg2).into(),
        BinOp::Shl | BinOp::Shr if args_are_bv => {
          self.extract_shift(arg1, arg2, arg1_ty, arg2_ty, op == BinOp::Shl, expr.span)
        }
        _ => {
          // TODO: Support pointer offset BinOp?
          self.unsupported_expr(expr.span, format!("Cannot extract binary op {:?}", op))
        }
      }
    } else {
      unreachable!()
    }
  }

  /// Stainless requires matching bitvector types on all operations, so we insert appropriate
  /// widenings, where possible. Stainless currently doesn't support mixing operands of
  /// different signedness, however, so we reject such cases.
  fn extract_shift(
    &mut self,
    arg1: st::Expr<'l>,
    mut arg2: st::Expr<'l>,
    arg1_ty: Ty<'tcx>,
    arg2_ty: Ty<'tcx>,
    is_shl: bool,
    span: Span,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let bail = |bxtor: &mut BodyExtractor<'_, 'l, 'tcx>, msg| bxtor.unsupported_expr(span, msg);
    match (&arg1_ty.kind, &arg2_ty.kind) {
      (kind1, kind2) if kind1 == kind2 => {} // No need to adapt anything,
      (TyKind::Int(int_ty1), TyKind::Int(int_ty2)) => {
        match (int_ty1.bit_width(), int_ty2.bit_width()) {
          (Some(width1), Some(width2)) => {
            if width1 > width2 {
              arg2 = f.BVWideningCast(arg2, f.BVType(true, width1 as i32)).into();
            } else {
              return bail(self, "Adapting lhs shift argument would change result type");
            }
          }
          _ => {
            return bail(self, "Cannot adapt shift arguments of unknown bit widths");
          }
        }
      }
      (TyKind::Uint(uint_ty1), TyKind::Uint(uint_ty2)) => {
        match (uint_ty1.bit_width(), uint_ty2.bit_width()) {
          (Some(width1), Some(width2)) => {
            if width1 > width2 {
              arg2 = f
                .BVWideningCast(arg2, f.BVType(false, width1 as i32))
                .into();
            } else {
              return bail(self, "Adapting lhs shift argument would change result type");
            }
          }
          _ => {
            return bail(self, "Cannot adapt shift arguments of unknown bit widths");
          }
        }
      }
      _ => {
        return bail(
          self,
          "Cannot extract shift mixing signed and unsigned operands",
        );
      }
    };
    if is_shl {
      f.BVShiftLeft(arg1, arg2).into()
    } else if self.base.is_signed_bv_type(arg1_ty) {
      f.BVAShiftRight(arg1, arg2).into()
    } else {
      f.BVLShiftRight(arg1, arg2).into()
    }
  }

  fn extract_logical_op(&mut self, expr: Expr<'tcx>) -> st::Expr<'l> {
    let f = self.factory();
    if let ExprKind::LogicalOp {
      op,
      lhs: arg1,
      rhs: arg2,
    } = expr.kind
    {
      let (arg1, arg2) = (self.mirror(arg1), self.mirror(arg2));
      assert!(arg1.ty.is_bool() && arg2.ty.is_bool());
      let (arg1, arg2) = (self.extract_expr(arg1), self.extract_expr(arg2));
      match op {
        LogicalOp::And => f.And(vec![arg1, arg2]).into(),
        LogicalOp::Or => f.Or(vec![arg1, arg2]).into(),
      }
    } else {
      unreachable!()
    }
  }

  fn extract_tuple(&mut self, expr: Expr<'tcx>) -> st::Expr<'l> {
    let f = self.factory();
    if let ExprKind::Tuple { fields } = expr.kind {
      match fields.len() {
        0 => f.UnitLiteral().into(),
        1 => self.unsupported_expr(expr.span, "Cannot extract one-tuples"),
        _ => f.Tuple(self.extract_expr_refs(fields)).into(),
      }
    } else {
      unreachable!()
    }
  }

  fn extract_field(&mut self, expr: Expr<'tcx>) -> st::Expr<'l> {
    let f = self.factory();
    if let ExprKind::Field { lhs, name } = expr.kind {
      let lhs = self.mirror(lhs);
      let lhs_ty = lhs.ty;
      let index = name.index();
      match lhs_ty.kind {
        TyKind::Tuple(_) => {
          let lhs = self.extract_expr(lhs);
          f.TupleSelect(lhs, (index as i32) + 1).into()
        }
        TyKind::Adt(adt_def, _) => {
          let sort = self.base.extract_adt(adt_def.did);
          assert_eq!(sort.constructors.len(), 1);
          let constructor = sort.constructors[0];
          assert!(index < constructor.fields.len());
          let lhs = self.extract_expr(lhs);
          f.ADTSelector(lhs, constructor.fields[index].v.id).into()
        }
        ref kind => unexpected(
          expr.span,
          format!("Unexpected kind of field selection: {:?}", kind),
        ),
      }
    } else {
      unreachable!()
    }
  }

  fn extract_call_like(
    &mut self,
    ty: Ty<'tcx>,
    args: &Vec<ExprRef<'tcx>>,
    span: Span,
  ) -> st::Expr<'l> {
    if let TyKind::FnDef(def_id, substs_ref) = ty.kind {
      // If the call is a std item, extract it specially
      match self.base.std_items.def_to_item_opt(def_id) {
        Some(BeginPanicFn) => self.extract_panic(args, span, false),
        Some(BeginPanicFmtFn) => self.extract_panic(args, span, true),

        Some(SetEmptyFn) | Some(SetSingletonFn) => {
          self.extract_set_creation(args, substs_ref, span)
        }
        Some(std_item)
          if std_item == SetAddFn
            || std_item == SetDifferenceFn
            || std_item == SetIntersectionFn
            || std_item == SetUnionFn
            || std_item == SubsetOfFn =>
        {
          self.extract_set_op(std_item, args, span)
        }

        // Otherwise, extract a normal call
        _ => self.extract_call(def_id, substs_ref, args, span),
      }
    } else {
      self.unsupported_expr(span, "Cannot extract call without statically known target")
    }
  }

  fn extract_set_op(
    &mut self,
    std_item: StdItem,
    args: &Vec<ExprRef<'tcx>>,
    span: Span,
  ) -> st::Expr<'l> {
    if let [set, arg, ..] = &self.extract_expr_refs(args.to_vec())[0..2] {
      return match std_item {
        SetAddFn => self.factory().SetAdd(*set, *arg).into(),
        SetDifferenceFn => self.factory().SetDifference(*set, *arg).into(),
        SetIntersectionFn => self.factory().SetIntersection(*set, *arg).into(),
        SetUnionFn => self.factory().SetUnion(*set, *arg).into(),
        SubsetOfFn => self.factory().SubsetOf(*set, *arg).into(),
        _ => unreachable!(),
      };
    }
    self.unsupported_expr(
      span,
      "Cannot extract set operation with less than two arguments.",
    )
  }

  fn extract_set_creation(
    &mut self,
    args: &Vec<ExprRef<'tcx>>,
    substs: SubstsRef<'tcx>,
    span: Span,
  ) -> st::Expr<'l> {
    let args = self.extract_expr_refs(args.to_vec());
    let ty = self.base.extract_ty(substs.type_at(0), &self.txtcx, span);
    self.factory().FiniteSet(args, ty).into()
  }

  fn extract_call(
    &mut self,
    def_id: DefId,
    substs_ref: SubstsRef<'tcx>,
    args: &Vec<ExprRef<'tcx>>,
    span: Span,
  ) -> st::Expr<'l> {
    let fd_id = self.base.extract_fn_ref(def_id);
    let class_def = self.base.get_class_of_method(fd_id);

    // Special case for Box::new, erase it and return the argument directly.
    // TODO: turn Box::new to a StdItem and use that. Tracked here:
    //  https://github.com/epfl-lara/rust-stainless/issues/34
    if fd_id.symbol_path == ["std", "boxed", "Box", "T", "new"] {
      return self.extract_expr_ref(args.first().cloned().unwrap());
    }

    // FIXME: Filter out as many type params of the function as the classdef
    //   already provides. This clearly fails when there is more than one
    //   parent etc. => improve
    let arg_tps_without_parents = self.extract_arg_types(
      substs_ref
        .types()
        .skip(class_def.map_or(0, |cd| cd.tparams.len())),
      span,
    );
    let args = self.extract_expr_refs(args.to_vec());

    // If this function is a method, then we may need to extract it as a method call.
    // To do so, we need a type class instance as receiver.
    match class_def.and_then(|cd| {
      // The receiver type is the type of the &self of the method call. This
      // is the first argument type. We have to extract it because we filtered
      // the self type above.
      let recv_type = self
        .base
        .extract_ty(substs_ref.type_at(0), &self.txtcx, span);

      // Retrieve the needed type class instance
      self.tc_insts.get(&(cd.id, recv_type, vec![]))
    }) {
      Some(&recv) => self
        .factory()
        .MethodInvocation(recv, fd_id, arg_tps_without_parents, args)
        .into(),
      None => self
        .factory()
        .FunctionInvocation(fd_id, arg_tps_without_parents, args)
        .into(),
    }
  }

  fn extract_arg_types<I>(&mut self, types: I, span: Span) -> Vec<st::Type<'l>>
  where
    I: IntoIterator<Item = Ty<'tcx>>,
  {
    // Remove closure type parameters (they were already replaced by FunctionTypes)
    let arg_tys = types.into_iter().filter(|ty| match ty.kind {
      TyKind::Closure(..) => false,
      _ => true,
    });
    self.base.extract_tys(arg_tys, &self.txtcx, span)
  }

  fn extract_panic(&mut self, args: &Vec<ExprRef<'tcx>>, span: Span, is_fmt: bool) -> st::Expr<'l> {
    let f = self.factory();
    let mut args = args.to_vec();

    assert_eq!(args.len(), 1);
    let arg = args.pop().unwrap();
    // FIXME: It seems that for expressions encoding panics, `expr.ty` always gives us the
    // `never` type, rather than the expected one. We currently just use the Unit type here,
    // because this is correct for panics resulting from asserts, but we should really recover,
    // or -- if necessary -- infer the correct type instead. (Stainless will reject any ill-typed
    // programs.)
    let tpe = f.UnitType().into();
    let error_expr = f.Error(tpe, "Panic".into());

    if is_fmt {
      // TODO: Implement panic! with formatted message
      self.unsupported_expr(span, "Cannot extract panic with formatted message")
    } else {
      let arg = self.extract_expr_ref(arg);
      self.keep_for_effects(error_expr.into(), vec![arg])
    }
  }

  /*
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

  fn try_extract_bigint_expr(&mut self, expr: Expr<'tcx>) -> Result<'l> {
    self.try_extract_bigint_lit(expr).or_else(|_| {
      let expr_ty = self.tables.node_type(expr.hir_id);
      if self.base.is_bigint_type(expr_ty) {
        Ok(self.extract_expr(expr))
      } else {
        Err("Not a BigInt-convertible expr")
      }
    })
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
  */

  fn extract_adt_construction(&mut self, expr: Expr<'tcx>) -> st::Expr<'l> {
    let f = self.factory();
    if let ExprKind::Adt {
      adt_def,
      variant_index,
      substs,
      mut fields,
      base,
      ..
    } = expr.kind
    {
      if base.is_some() {
        self.unsupported_expr(expr.span, "Cannot extract ADT constructions with bases")
      } else {
        // TODO: Also consider type arguments
        let sort = self.base.extract_adt(adt_def.did);
        let constructor = sort.constructors[variant_index.index()];
        let arg_tps = self.extract_arg_types(substs.types(), expr.span);
        fields.sort_by_key(|field| field.name.index());
        let args = fields
          .into_iter()
          .map(|field| self.extract_expr_ref(field.expr))
          .collect();
        f.ADT(constructor.id, arg_tps, args).into()
      }
    } else {
      unreachable!()
    }
  }

  fn extract_if(
    &mut self,
    cond: ExprRef<'tcx>,
    then: ExprRef<'tcx>,
    elze_opt: Option<ExprRef<'tcx>>,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let cond = self.extract_expr_ref(cond);
    let then = self.extract_expr_ref(then);
    let elze = elze_opt
      .map(|e| self.extract_expr_ref(e))
      .unwrap_or_else(|| {
        // TODO: Match the type of the then branch?
        f.UnitLiteral().into()
      });
    f.IfExpr(cond, then, elze).into()
  }

  fn extract_match(&mut self, scrutinee: ExprRef<'tcx>, arms: Vec<Arm<'tcx>>) -> st::Expr<'l> {
    let scrutinee = self.extract_expr_ref(scrutinee);
    let cases = arms.into_iter().map(|arm| self.extract_arm(arm)).collect();
    self.factory().MatchExpr(scrutinee, cases).into()
  }

  fn extract_arm(&mut self, arm: Arm<'tcx>) -> &'l st::MatchCase<'l> {
    let Arm {
      pattern,
      guard,
      body,
      ..
    } = arm;
    let pattern = self.extract_pattern(pattern, None);
    let guard = guard.map(|Guard::If(expr)| self.extract_expr_ref(expr));
    let body = self.extract_expr_ref(body);
    self.factory().MatchCase(pattern, guard, body)
  }

  fn extract_pattern(
    &mut self,
    pattern: Pat<'tcx>,
    binder: Option<&'l st::ValDef<'l>>,
  ) -> st::Pattern<'l> {
    let f = self.factory();
    match pattern.kind {
      box PatKind::Wild => f.WildcardPattern(binder).into(),

      box kind @ PatKind::Binding { .. } => {
        assert!(binder.is_none());
        match self.try_pattern_to_var(&kind, true) {
          Ok(binder) => {
            let binder = f.ValDef(binder);
            match kind {
              PatKind::Binding {
                subpattern: Some(subpattern),
                ..
              } => self.extract_pattern(subpattern, Some(binder)),
              PatKind::Binding {
                subpattern: None, ..
              } => f.WildcardPattern(Some(binder)).into(),
              _ => unreachable!(),
            }
          }
          Err(reason) => self.unsupported_pattern(
            pattern.span,
            format!("Unsupported pattern binding: {}", reason),
          ),
        }
      }

      // From rustc_hair docs:  `Foo(...)` or `Foo{...}` or `Foo`, where `Foo`
      // is a variant name from an ADT with multiple variants.
      box PatKind::Variant {
        adt_def,
        variant_index,
        subpatterns,
        substs,
      } => {
        let sort = self.base.extract_adt(adt_def.did);
        let constructor = sort.constructors[variant_index.index()];
        let arg_tps = self.extract_arg_types(substs.types(), pattern.span);
        let subpatterns = self.extract_subpatterns(subpatterns, constructor.fields.len());
        f.ADTPattern(binder, constructor.id, arg_tps, subpatterns)
          .into()
      }

      // From rustc_hair docs:  `(...)`, `Foo(...)`, `Foo{...}`, or `Foo`, where
      // `Foo` is a variant name from an ADT with a single variant.
      box PatKind::Leaf { subpatterns } => match pattern.ty.kind {
        TyKind::Adt(adt_def, substs) => {
          let sort = self.base.extract_adt(adt_def.did);
          assert_eq!(sort.constructors.len(), 1);
          let constructor = sort.constructors[0];
          let arg_tps = self.extract_arg_types(substs.types(), pattern.span);
          let subpatterns = self.extract_subpatterns(subpatterns, constructor.fields.len());
          f.ADTPattern(binder, constructor.id, arg_tps, subpatterns)
            .into()
        }
        TyKind::Tuple(substs) => f
          .TuplePattern(binder, self.extract_subpatterns(subpatterns, substs.len()))
          .into(),

        _ => self.unsupported_pattern(
          pattern.span,
          "Encountered Leaf pattern, but type is not an ADT",
        ),
      },

      box PatKind::Constant { value: konst } => match Literal::try_from(konst) {
        Ok(lit) => f.LiteralPattern(binder, lit.as_st_literal(f)).into(),
        _ => self.unsupported_pattern(pattern.span, "Unsupported kind of literal in pattern"),
      },

      // TODO: Confirm that rustc introduces this pattern only for primitive derefs
      box PatKind::Deref { subpattern } => self.extract_pattern(subpattern, binder),

      _ => self.unsupported_pattern(pattern.span, "Unsupported kind of pattern"),
    }
  }

  fn extract_subpatterns(
    &mut self,
    mut field_pats: Vec<FieldPat<'tcx>>,
    num_fields: usize,
  ) -> Vec<st::Pattern<'l>> {
    let f = self.factory();
    field_pats.sort_by_key(|field| field.field.index());
    field_pats.reverse();
    let mut subpatterns = Vec::with_capacity(num_fields);
    for i in 0..num_fields {
      let next = if let Some(FieldPat { field, .. }) = field_pats.last() {
        if field.index() == i {
          let FieldPat { pattern, .. } = field_pats.pop().unwrap();
          self.extract_pattern(pattern, None)
        } else {
          f.WildcardPattern(None).into()
        }
      } else {
        f.WildcardPattern(None).into()
      };
      subpatterns.push(next);
    }
    subpatterns
  }

  #[allow(clippy::unnecessary_unwrap)]
  fn extract_block_(
    &mut self,
    stmts: &mut Vec<StmtRef<'tcx>>,
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

    if let Some(stmt) = stmts.pop() {
      let stmt = self.mirror(stmt);
      match stmt.kind {
        StmtKind::Let {
          pattern,
          initializer,
          ..
        } => {
          let span = pattern.span;
          let bail = |msg| -> st::Expr<'l> {
            self.base.unsupported(span, msg);
            f.Block(acc_exprs.clone(), f.NoTree(f.Untyped().into()).into())
              .into()
          };

          // FIXME: Detect desugared `let`s
          let has_abnormal_source = false;
          let var_result = self.try_pattern_to_var(&pattern.kind, false);

          if has_abnormal_source {
            // TODO: Support for loops
            bail("Cannot extract let that resulted from desugaring")
          } else if let Err(reason) = var_result {
            // TODO: Desugar complex patterns
            bail(format!("Cannot extract complex pattern in let: {}", reason).as_str())
          } else if initializer.is_none() {
            bail("Cannot extract let without initializer")
          } else {
            let vd = f.ValDef(var_result.unwrap());
            let init = self.extract_expr_ref(initializer.unwrap());
            let exprs = acc_exprs.clone();
            acc_exprs.clear();
            let body_expr = self.extract_block_(stmts, acc_exprs, final_expr);
            let last_expr = f.Let(vd, init, body_expr).into();
            finish(exprs, last_expr)
          }
        }

        StmtKind::Expr { expr, .. } => {
          let expr = self.extract_expr_ref(expr);
          acc_exprs.push(expr);
          self.extract_block_(stmts, acc_exprs, final_expr)
        }
      }
    } else {
      finish(acc_exprs.clone(), final_expr)
    }
  }

  fn extract_block(&mut self, block: Block<'tcx>) -> st::Expr<'l> {
    let Block {
      mut stmts,
      expr: final_expr,
      ..
    } = block;
    let final_expr = final_expr
      .map(|e| self.extract_expr_ref(e))
      .unwrap_or_else(|| self.factory().UnitLiteral().into());
    stmts.reverse();
    self.extract_block_(&mut stmts, &mut vec![], final_expr)
  }

  /// Factory helpers

  fn keep_for_effects(&mut self, expr: st::Expr<'l>, exprs: Vec<st::Expr<'l>>) -> st::Expr<'l> {
    self.factory().Block(exprs, expr).into()
  }

  /// Various helpers

  fn mirror<M: Mirror<'tcx>>(&mut self, m: M) -> M::Output {
    m.make_mirror(&mut self.hcx)
  }

  fn strip_scopes(&mut self, expr: Expr<'tcx>) -> Expr<'tcx> {
    match expr.kind {
      ExprKind::Scope { value, .. } => {
        let expr = self.mirror(value);
        self.strip_scopes(expr)
      }
      _ => expr,
    }
  }

  /// Try to detect whether the given match corresponds to an if expression.
  /// Returns None if it is not an if expression and Some(has_elze) otherwise.
  fn looks_like_if(&mut self, scrutinee: ExprRef<'tcx>, arms: &[Arm<'tcx>]) -> Option<bool> {
    let cond = self.mirror(scrutinee);
    let is_if = arms.len() == 2
      && cond.ty.is_bool()
      && match (&arms[0].pattern.kind, &arms[1].pattern.kind) {
        (box PatKind::Constant { value: konst }, box PatKind::Wild) => {
          match Literal::try_from(*konst) {
            Ok(Literal::Bool(true)) => true,
            _ => false,
          }
        }
        _ => false,
      };

    if is_if {
      let elze = self.mirror(arms[1].body.clone());
      match self.strip_scopes(elze).kind {
        ExprKind::Block { body: ast_block } => {
          let Block { stmts, expr, .. } = self.mirror(ast_block);
          let elze_missing = stmts.is_empty() && expr.is_none();
          Some(!elze_missing)
        }
        _ => unreachable!(),
      }
    } else {
      None
    }
  }

  fn try_pattern_to_var(
    &self,
    pat_kind: &PatKind<'tcx>,
    allow_subpattern: bool,
  ) -> Result<&'l st::Variable<'l>> {
    match pat_kind {
      PatKind::Binding {
        mutability: Mutability::Mut,
        ..
      } => Err("Mutable bindings are not supported"),

      PatKind::Binding {
        subpattern: Some(_),
        ..
      } if !allow_subpattern => Err("Subpatterns are not supported here"),

      PatKind::Binding {
        mutability: Mutability::Not,
        mode,
        var: hir_id,
        ..
      } => match mode {
        BindingMode::ByValue | BindingMode::ByRef(BorrowKind::Shared) => {
          Ok(self.fetch_var(*hir_id))
        }
        _ => Err("Binding mode not allowed"),
      },

      // This encodes a user-written type ascription: let a: u32 = ...
      // Rustc needs these for borrow-checking but stainless doesn't, therefore
      // we erase them here by recursing once, and passing down the
      // 'allow_subpattern' argument.
      PatKind::AscribeUserType { subpattern, .. } => {
        self.try_pattern_to_var(&subpattern.kind, allow_subpattern)
      }

      _ => Err("Expected a top-level binding"),
    }
  }

  fn unsupported_expr<M: Into<String>>(&mut self, span: Span, msg: M) -> st::Expr<'l> {
    self.base.unsupported(span, msg);
    let f = self.factory();
    f.NoTree(f.Untyped().into()).into()
  }

  fn unsupported_pattern<M: Into<String>>(&mut self, span: Span, msg: M) -> st::Pattern<'l> {
    self.base.unsupported(span, msg);
    self.factory().WildcardPattern(None).into()
  }
}
