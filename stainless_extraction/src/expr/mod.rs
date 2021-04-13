mod ops;
mod set;

use super::*;

use crate::literal::Literal;
use crate::spec::SpecType;
use crate::std_items::{CrateItem::*, LangItem};
use crate::ty::{int_bit_width, uint_bit_width};

use std::convert::TryFrom;

use rustc_middle::mir::{BorrowKind, Field, Mutability};
use rustc_middle::ty::{subst::SubstsRef, Ty, TyKind};
use rustc_mir_build::thir::{
  Arm, BindingMode, Block, BlockSafety, Expr, ExprKind, FieldPat, FruInfo, Guard, Pat, PatKind,
  Stmt, StmtKind,
};

type Result<T> = std::result::Result<T, &'static str>;

/// Extraction of bodies (i.e., expressions, for the most part)
impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_expr(&mut self, expr: &'a Expr<'a, 'tcx>) -> st::Expr<'l> {
    match &expr.kind {
      ExprKind::Literal { literal: konst, .. } => match Literal::from_const(konst, self.tcx()) {
        Some(lit) => lit.as_st_literal(self.factory()),
        _ => self.unsupported_expr(expr.span, "Unsupported kind of literal"),
      },
      ExprKind::Unary { op, arg } => self.extract_unary(op, arg, expr.span),
      ExprKind::Binary { op, lhs, rhs } => self.extract_binary(*op, lhs, rhs, expr.span),
      ExprKind::LogicalOp { op, lhs, rhs } => self.extract_logical_op(*op, lhs, rhs),

      ExprKind::Tuple { .. } => self.extract_tuple(expr),
      ExprKind::Field { lhs, name } => self.extract_field(lhs, *name),
      ExprKind::VarRef { id } => self.fetch_var(*id).into(),

      ExprKind::Call { ty, ref args, .. } => match ty.kind() {
        TyKind::FnDef(def_id, substs_ref) => {
          self.extract_call_like(*def_id, substs_ref, args, expr.span)
        }
        _ => self.unsupported_expr(
          expr.span,
          "Cannot extract call without statically known target",
        ),
      },

      ExprKind::Adt { .. } => self.extract_adt_construction(expr),

      ExprKind::Block { body: block } => match block.safety_mode {
        BlockSafety::Safe => self.extract_block(block),
        _ => self.unsupported_expr(expr.span, "Cannot extract unsafe block"),
      },

      ExprKind::If {
        cond,
        then,
        else_opt,
      } => self.extract_if(cond, then, *else_opt),
      ExprKind::Match { scrutinee, arms } => self.extract_match(scrutinee, arms),

      // TODO: Handle arbitrary-precision integers
      ExprKind::Scope { value, .. } => self.extract_expr(value),
      ExprKind::Use { source } => self.extract_expr(source),
      ExprKind::NeverToAny { source } => self.extract_expr(source),

      ExprKind::Deref { arg } => self.extract_expr(arg),

      // Borrow an immutable and aliasable value (i.e. the meaning of
      // BorrowKind::Shared). Handle this safe case with erasure.
      ExprKind::Borrow {
        borrow_kind: BorrowKind::Shared,
        arg,
      } => self.extract_expr(arg),

      ExprKind::Assign { lhs, rhs } => self.extract_assignment(lhs, rhs),

      ExprKind::Return { value } => self.extract_return(*value),

      _ => self.unsupported_expr(
        expr.span,
        format!("Cannot extract expr kind {:?}", expr.kind),
      ),
    }
  }

  fn extract_exprs<I>(&mut self, exprs: I) -> Vec<st::Expr<'l>>
  where
    I: IntoIterator<Item = &'a Expr<'a, 'tcx>>,
  {
    exprs
      .into_iter()
      .map(|arg| self.extract_expr(arg))
      .collect()
  }

  fn extract_tuple(&mut self, expr: &Expr<'a, 'tcx>) -> st::Expr<'l> {
    let f = self.factory();
    if let ExprKind::Tuple { fields } = expr.kind {
      match fields.len() {
        0 => f.UnitLiteral().into(),
        1 => self.unsupported_expr(expr.span, "Cannot extract one-tuples"),
        _ => f.Tuple(self.extract_exprs(fields)).into(),
      }
    } else {
      unreachable!()
    }
  }

  fn extract_field(&mut self, lhs: &'a Expr<'a, 'tcx>, field: Field) -> st::Expr<'l> {
    let f = self.factory();
    match lhs.ty.kind() {
      TyKind::Tuple(_) => {
        let lhs = self.extract_expr(lhs);
        f.TupleSelect(lhs, (field.index() as i32) + 1).into()
      }
      TyKind::Adt(adt_def, _) => {
        let selector = self.extract_field_selector(adt_def.did, field);
        let lhs = self.extract_expr(lhs);
        f.ADTSelector(lhs, selector).into()
      }
      ref kind => unexpected(
        lhs.span,
        format!("Unexpected kind of field selection: {:?}", kind),
      ),
    }
  }

  fn extract_field_selector(&mut self, adt_def_id: DefId, field: Field) -> StainlessSymId<'l> {
    let sort = self.base.get_or_extract_adt(adt_def_id);
    assert_eq!(sort.constructors.len(), 1);
    let constructor = sort.constructors[0];
    assert!(field.index() < constructor.fields.len());
    constructor.fields[field.index()].v.id
  }

  fn extract_call_like(
    &mut self,
    def_id: DefId,
    substs_ref: SubstsRef<'tcx>,
    args: &'a [Expr<'a, 'tcx>],
    span: Span,
  ) -> st::Expr<'l> {
    // If the call is a std item, extract it specially
    self
      .base
      .std_items
      .def_to_item_opt(def_id)
      .and_then(|sti| match sti {
        // Panics
        StdItem::LangItem(LangItem::BeginPanicFn) | StdItem::LangItem(LangItem::PanicFn) => {
          Some(self.extract_panic(args, span, false))
        }
        StdItem::CrateItem(BeginPanicFmtFn) => Some(self.extract_panic(args, span, true)),

        StdItem::CrateItem(item) if item.is_set_related() => {
          Some(self.extract_set_expr(item, args, substs_ref, span))
        }

        StdItem::CrateItem(CrateItem::ImpliesFn) => self.extract_implies(args),

        // Box::new, erase it and return the argument directly.
        StdItem::CrateItem(BoxNewFn) => Some(self.extract_expr(args.first().unwrap())),

        StdItem::CrateItem(ToStringFn) => self.extract_str_to_string(args.first().unwrap()),
        StdItem::CrateItem(PartialEqFn) => self.extract_partial_eq(args),
        StdItem::CrateItem(CloneFn) => self.extract_clone(args.first().unwrap()),

        _ => None,
      })
      // Otherwise, extract a normal call
      .unwrap_or_else(|| self.extract_call(def_id, substs_ref, args, span))
  }

  fn extract_implies(&mut self, args: &'a [Expr<'a, 'tcx>]) -> Option<st::Expr<'l>> {
    match args {
      [lhs, rhs] => Some(
        self
          .factory()
          .Implies(self.extract_expr(lhs), self.extract_expr(rhs))
          .into(),
      ),
      _ => None,
    }
  }

  fn extract_call(
    &mut self,
    def_id: DefId,
    substs_ref: SubstsRef<'tcx>,
    args: &'a [Expr<'a, 'tcx>],
    span: Span,
  ) -> st::Expr<'l> {
    let fd_id = self.base.get_or_extract_fn_ref(def_id);
    let class_def = self.base.get_class_of_method(fd_id);
    let Generics {
      tparams,
      trait_bounds,
      ..
    } = self.base.get_or_extract_generics(def_id);

    // FIXME: Filter out as many type params of the function as the classdef
    //   already provides. This clearly fails when there is more than one
    //   parent etc. => improve
    let arg_tps_without_parents = self.extract_arg_types(
      substs_ref
        .types()
        .skip(class_def.map_or(0, |cd| cd.tparams.len())),
      span,
    );
    let mut args = self.extract_exprs(args);

    // If the function has trait bounds but is not on a class, we must find the
    // corresponding evidence arguments.
    if class_def.is_none() && !trait_bounds.is_empty() {
      let type_substs = tparams
        .iter()
        .map(|st::TypeParameterDef { tp }| (*tp).into())
        .zip(arg_tps_without_parents.as_slice().iter().copied())
        .collect();

      if let Some(evidence_args) = self
        .base
        .evidence_params(trait_bounds)
        .iter()
        .map(|vd| self.find_evidence_arg(vd, &type_substs))
        .collect::<Option<Vec<_>>>()
      {
        args.extend(evidence_args)
      }
    }

    // If this function is a method, then we may need to extract it as a method call.
    // To do so, we need a type class instance as receiver.
    match class_def.and_then(|st::ClassDef { id, .. }| {
      // The receiver type is the type of the &self of the method call. This
      // is the first argument type. We have to extract it because we filtered
      // the self type above.
      let recv_tps = self.base.extract_tys(substs_ref.types(), &self.txtcx, span);
      self.extract_method_receiver(&TypeClassKey { id, recv_tps })
    }) {
      Some(recv) => self
        .factory()
        .MethodInvocation(recv, fd_id, arg_tps_without_parents, args)
        .into(),

      None => self
        .factory()
        .FunctionInvocation(fd_id, arg_tps_without_parents, args)
        .into(),
    }
  }

  fn extract_return(&mut self, value: Option<&'a Expr<'a, 'tcx>>) -> st::Expr<'l> {
    let expr = value
      .map(|v| self.extract_expr(v))
      .unwrap_or_else(|| self.factory().UnitLiteral().into());
    self.factory().Return(expr).into()
  }

  fn extract_str_to_string(&mut self, expr: &'a Expr<'a, 'tcx>) -> Option<st::Expr<'l>> {
    self.is_str_type(expr).then(|| self.extract_expr(expr))
  }

  /// Extracts a call to `PartialEq::eq`. Currently, this only works for strings
  /// and otherwise returns None.
  fn extract_partial_eq(&mut self, args: &'a [Expr<'a, 'tcx>]) -> Option<st::Expr<'l>> {
    match args {
      [lhs, rhs] if self.is_str_type(lhs) && self.is_str_type(rhs) => Some(
        self
          .factory()
          .Equals(self.extract_expr(lhs), self.extract_expr(rhs))
          .into(),
      ),
      _ => None,
    }
  }

  /// Extracts a call to `Clone::clone` by erasure.
  ///
  /// FIXME: this is a quick-fix/hack to make use of `PartialEq::eq` for strings
  ///   across clones, see [extract_partial_eq]. As soon, as strings can be mutated,
  ///   this becomes potentially unsafe and has to be revisited.
  ///   The way to correctly solve this use-case is by attaching a spec to the real
  ///   `Clone::clone` that preserves equality.
  fn extract_clone(&mut self, expr: &'a Expr<'a, 'tcx>) -> Option<st::Expr<'l>> {
    self.is_str_type(expr).then(|| self.extract_expr(expr))
  }

  fn is_str_type(&mut self, expr: &'a Expr<'a, 'tcx>) -> bool {
    let ty = self.base.extract_ty(expr.ty, &self.txtcx, expr.span);
    matches!(ty, st::Type::StringType(_))
  }

  fn extract_arg_types<I>(&mut self, types: I, span: Span) -> Vec<st::Type<'l>>
  where
    I: IntoIterator<Item = Ty<'tcx>>,
  {
    // Remove closure type parameters (they were already replaced by FunctionTypes)
    let arg_tys = types
      .into_iter()
      .filter(|ty| !matches!(ty.kind(), TyKind::Closure(..)));
    self.base.extract_tys(arg_tys, &self.txtcx, span)
  }

  fn extract_panic(
    &mut self,
    args: &'a [Expr<'a, 'tcx>],
    span: Span,
    is_fmt: bool,
  ) -> st::Expr<'l> {
    match &self.extract_exprs(args)[..] {
      // TODO: Implement panic! with formatted message
      _ if is_fmt => self.unsupported_expr(span, "Cannot extract panic with formatted message"),

      [st::Expr::StringLiteral(st::StringLiteral { value: message })] => {
        let f = self.factory();
        // Using the nothing type to be subtype of everything.
        let tpe = f.NothingType().into();
        f.Error(tpe, message.into()).into()
      }
      _ => self.unsupported_expr(
        span,
        "Cannot extract panic without a single literal string argument",
      ),
    }
  }

  fn extract_adt_construction(&mut self, expr: &Expr<'a, 'tcx>) -> st::Expr<'l> {
    let f = self.factory();
    if let ExprKind::Adt {
      adt_def,
      variant_index,
      substs,
      fields,
      base,
      ..
    } = &expr.kind
    {
      let sort = self.base.get_or_extract_adt(adt_def.did);
      let constructor = sort.constructors[variant_index.index()];
      let arg_tps = self.extract_arg_types(substs.types(), expr.span);

      // If the ADT is constructed with "struct update syntax"
      let args = if let Some(FruInfo { base, .. }) = base {
        // we take the explicit fields
        let fields_by_index = fields
          .iter()
          .map(|f| (f.name.index(), self.extract_expr(f.expr)))
          .collect::<HashMap<_, _>>();
        let adt = self.extract_expr(base);
        // and fill the rest in from the base ADT
        constructor
          .fields
          .iter()
          .enumerate()
          .map(|(index, fi)| {
            fields_by_index
              .get(&index)
              .copied()
              .unwrap_or_else(|| f.ADTSelector(adt, fi.v.id).into())
          })
          .collect()
      }
      // Otherwise, just normally extract the field expressions.
      else {
        let mut fields: Vec<_> = fields.iter().collect();
        fields.sort_by_key(|field| field.name.index());
        fields
          .into_iter()
          .map(|field| self.extract_expr(field.expr))
          .collect()
      };
      f.ADT(constructor.id, arg_tps, args).into()
    } else {
      unreachable!()
    }
  }

  fn extract_if(
    &mut self,
    cond: &'a Expr<'a, 'tcx>,
    then: &'a Expr<'a, 'tcx>,
    else_opt: Option<&'a Expr<'a, 'tcx>>,
  ) -> st::Expr<'l> {
    let cond = self.extract_expr(cond);
    let then = self.extract_expr(then);
    let elze = else_opt
      .map(|e| self.extract_expr(e))
      .unwrap_or_else(|| self.factory().UnitLiteral().into());

    self.factory().IfExpr(cond, then, elze).into()
  }

  fn extract_match(
    &mut self,
    scrutinee: &'a Expr<'a, 'tcx>,
    arms: &[Arm<'a, 'tcx>],
  ) -> st::Expr<'l> {
    let scrutinee = self.extract_expr(scrutinee);
    let cases = arms.iter().map(|arm| self.extract_arm(arm)).collect();
    self.factory().MatchExpr(scrutinee, cases).into()
  }

  fn extract_arm(&mut self, arm: &Arm<'a, 'tcx>) -> &'l st::MatchCase<'l> {
    let Arm {
      pattern,
      guard,
      body,
      ..
    } = arm;
    let pattern = self.extract_pattern(pattern, None);
    let guard = guard.as_ref().and_then(|g| match g {
      Guard::If(expr) => Some(self.extract_expr(expr)),
      _ => None,
    });
    let body = self.extract_expr(body);
    self.factory().MatchCase(pattern, guard, body)
  }

  fn extract_pattern(
    &mut self,
    pattern: &Pat<'tcx>,
    binder: Option<&'l st::ValDef<'l>>,
  ) -> st::Pattern<'l> {
    let f = self.factory();
    match &pattern.kind {
      box PatKind::Wild => f.WildcardPattern(binder).into(),

      box kind @ PatKind::Binding { .. } => {
        assert!(binder.is_none());
        match self.try_pattern_to_var(&kind, true) {
          Ok(binder) => match kind {
            PatKind::Binding {
              subpattern: Some(subpattern),
              ..
            } => self.extract_pattern(&subpattern, Some(binder)),
            PatKind::Binding {
              subpattern: None, ..
            } => f.WildcardPattern(Some(binder)).into(),
            _ => unreachable!(),
          },
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
        let sort = self.base.get_or_extract_adt(adt_def.did);
        let constructor = sort.constructors[variant_index.index()];
        let arg_tps = self.extract_arg_types(substs.types(), pattern.span);
        let subpatterns = self.extract_subpatterns(subpatterns.to_vec(), constructor.fields.len());
        f.ADTPattern(binder, constructor.id, arg_tps, subpatterns)
          .into()
      }

      // From rustc_hair docs:  `(...)`, `Foo(...)`, `Foo{...}`, or `Foo`, where
      // `Foo` is a variant name from an ADT with a single variant.
      box PatKind::Leaf { subpatterns } => match pattern.ty.kind() {
        TyKind::Adt(adt_def, substs) => {
          let sort = self.base.get_or_extract_adt(adt_def.did);
          assert_eq!(sort.constructors.len(), 1);
          let constructor = sort.constructors[0];
          let arg_tps = self.extract_arg_types(substs.types(), pattern.span);
          let subpatterns =
            self.extract_subpatterns(subpatterns.to_vec(), constructor.fields.len());
          f.ADTPattern(binder, constructor.id, arg_tps, subpatterns)
            .into()
        }
        TyKind::Tuple(substs) => f
          .TuplePattern(
            binder,
            self.extract_subpatterns(subpatterns.to_vec(), substs.len()),
          )
          .into(),

        _ => self.unsupported_pattern(
          pattern.span,
          "Encountered Leaf pattern, but type is not an ADT",
        ),
      },

      box PatKind::Constant { value: konst } => match Literal::from_const(konst, self.tcx()) {
        Some(lit) => f.LiteralPattern(binder, lit.as_st_literal(f)).into(),
        _ => self.unsupported_pattern(pattern.span, "Unsupported kind of literal in pattern"),
      },

      // TODO: Confirm that rustc introduces this pattern only for primitive derefs
      box PatKind::Deref { ref subpattern } => self.extract_pattern(subpattern, binder),

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
          self.extract_pattern(&pattern, None)
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

  fn extract_assignment(
    &mut self,
    lhs: &'a Expr<'a, 'tcx>,
    rhs: &'a Expr<'a, 'tcx>,
  ) -> st::Expr<'l> {
    let value = self.extract_expr(rhs);
    let lhs = self.strip_scopes(lhs);
    match &lhs.kind {
      ExprKind::VarRef { id } => self.factory().Assignment(self.fetch_var(*id), value).into(),

      ExprKind::Field { lhs, name } => match lhs.ty.kind() {
        TyKind::Adt(adt_def, _) => {
          let adt = self.extract_expr(lhs);
          let selector = self.extract_field_selector(adt_def.did, *name);
          self.factory().FieldAssignment(adt, selector, value).into()
        }
        ref t => self.unsupported_expr(
          lhs.span,
          format!("Cannot extract assignment to type {:?}", t),
        ),
      },

      e => self.unsupported_expr(
        lhs.span,
        format!("Cannot extract assignment to kind {:?}", e),
      ),
    }
  }

  fn strip_scopes(&mut self, expr: &'a Expr<'a, 'tcx>) -> &'a Expr<'a, 'tcx> {
    match expr.kind {
      ExprKind::Scope { value, .. } => self.strip_scopes(value),
      _ => expr,
    }
  }

  fn extract_block_(
    &mut self,
    stmts: &mut Vec<&Stmt<'a, 'tcx>>,
    acc_exprs: &mut Vec<st::Expr<'l>>,
    // Accumulates the HirId's of all spec closures for later extraction
    acc_specs: &mut HashMap<SpecType, Vec<HirId>>,
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
      let bail = |msg, span| -> st::Expr<'l> {
        self.base.unsupported(span, msg);
        f.Block(acc_exprs.clone(), f.NoTree(f.Untyped().into()).into())
          .into()
      };

      match &stmt.kind {
        // Spec expressions are recognized by their specific closure shape and
        // their attributes (stainless::) flags.
        StmtKind::Expr {
          expr:
            Expr {
              kind:
                ExprKind::Scope {
                  lint_level: thir::LintLevel::Explicit(hir_id),
                  value: expr,
                  ..
                },
              ..
            },
          ..
        } if matches!(expr.kind, ExprKind::Closure { .. }) => {
          if let Ok(spec_type) = SpecType::try_from(self.tcx().hir().attrs(*hir_id)) {
            acc_specs
              .entry(spec_type)
              .or_insert_with(Vec::new)
              .push(*hir_id);
            self.extract_block_(stmts, acc_exprs, acc_specs, final_expr)
          } else {
            bail("Cannot extract closure that is not a spec.", expr.span)
          }
        }

        StmtKind::Let {
          pattern,
          initializer: None,
          ..
        } => bail("Cannot extract let without initializer", pattern.span),

        StmtKind::Let {
          pattern,
          initializer: Some(init),
          ..
        } => {
          // FIXME: Detect desugared `let`s
          let has_abnormal_source = false;
          if has_abnormal_source {
            // TODO: Support for loops
            bail(
              "Cannot extract let that resulted from desugaring",
              pattern.span,
            )
          } else {
            match self.try_pattern_to_var(&pattern.kind, false) {
              // TODO: Desugar complex patterns
              Err(reason) => bail(
                &format!("Cannot extract complex pattern in let: {}", reason),
                pattern.span,
              ),
              Ok(vd) => {
                // recurse the extract all the following statements
                let exprs = acc_exprs.clone();
                acc_exprs.clear();
                let body_expr = self.extract_block_(stmts, acc_exprs, acc_specs, final_expr);
                // wrap that body expression into the Let
                let init = self.extract_expr(init);
                let last_expr = if vd.is_mutable() {
                  f.LetVar(vd, init, body_expr).into()
                } else {
                  f.Let(vd, init, body_expr).into()
                };
                finish(exprs, last_expr)
              }
            }
          }
        }

        StmtKind::Expr { expr, .. } => {
          let expr = self.extract_expr(expr);
          acc_exprs.push(expr);
          self.extract_block_(stmts, acc_exprs, acc_specs, final_expr)
        }
      }
    } else {
      finish(acc_exprs.clone(), final_expr)
    }
  }

  fn extract_block(
    &mut self,
    Block {
      stmts,
      expr: final_expr,
      ..
    }: &Block<'a, 'tcx>,
  ) -> st::Expr<'l> {
    let mut stmts: Vec<_> = stmts.iter().collect();
    let final_expr = final_expr
      .map(|e| self.extract_expr(e))
      // If there's no final expression, we need to check whether the last
      // statement is a return. If yes, we take the return as final expression.
      .or_else(|| {
        stmts.last().cloned().and_then(|s| match s.kind {
          StmtKind::Expr { expr, .. } => match self.strip_scopes(expr).kind {
            ExprKind::Return { value } => {
              stmts.pop();
              Some(self.extract_return(value))
            }
            _ => None,
          },
          _ => None,
        })
      })
      .unwrap_or_else(|| self.factory().UnitLiteral().into());

    stmts.reverse();
    let mut spec_ids = HashMap::new();
    let body_expr = self.extract_block_(&mut stmts, &mut vec![], &mut spec_ids, final_expr);
    self.extract_specs(&spec_ids, body_expr)
  }

  fn try_pattern_to_var(
    &self,
    pat_kind: &PatKind<'tcx>,
    allow_subpattern: bool,
  ) -> Result<&'l st::ValDef<'l>> {
    match pat_kind {
      PatKind::Binding {
        subpattern: Some(_),
        ..
      } if !allow_subpattern => Err("Subpatterns are not supported here"),

      PatKind::Binding {
        mutability,
        mode: BindingMode::ByValue,
        var: hir_id,
        ..
      }
      | PatKind::Binding {
        mutability,
        mode: BindingMode::ByRef(BorrowKind::Shared),
        var: hir_id,
        ..
      } => {
        let var = self.fetch_var(*hir_id);
        if *mutability == Mutability::Not || var.is_mutable() {
          Ok(self.factory().ValDef(var))
        } else {
          Err("Binding mode not allowed")
        }
      }

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
