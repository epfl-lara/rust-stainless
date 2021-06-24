use std::convert::TryFrom;

use literal::Literal;
use rustc_middle::ty::{subst::Subst, subst::SubstsRef, Ty, TyKind};
use rustc_mir_build::thir::{BlockSafety, Expr, ExprKind, FruInfo, Stmt, StmtKind};

use crate::std_items::{CrateItem::*, LangItem};

use super::*;

mod block;
mod field;
mod literal;
mod map;
mod ops;
mod pattern;
mod refs;
mod set;
mod spec;
mod tuple;

type Result<T> = std::result::Result<T, &'static str>;

/// Extraction of bodies (i.e., expressions, for the most part)
impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_expr(&mut self, expr: &'a Expr<'a, 'tcx>) -> st::Expr<'l> {
    match &expr.kind {
      // TODO: Handle arbitrary-precision integers?
      ExprKind::Literal { literal, .. } => match Literal::from_const(literal, self.tcx()) {
        Some(lit) => lit.as_st_literal(self.factory()),
        _ => self.unsupported_expr(expr.span, "Unsupported kind of literal"),
      },
      ExprKind::Unary { op, arg } => self.extract_unary(op, arg, expr.span),
      ExprKind::Binary { op, lhs, rhs } => self.extract_binary(*op, lhs, rhs, expr.span),
      ExprKind::LogicalOp { op, lhs, rhs } => self.extract_logical_op(*op, lhs, rhs),

      ExprKind::Tuple { fields } => self.extract_tuple(fields, expr.span),
      ExprKind::Field { lhs, name } => self.extract_field(lhs, *name, false),
      ExprKind::VarRef { id } => self.extract_var_ref(*id),

      ExprKind::Call { ty, ref args, .. } => match ty.kind() {
        TyKind::FnDef(def_id, substs) => self.extract_call_like(*def_id, substs, args, expr.span),
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

      ExprKind::Scope { value, .. } => self.extract_expr(value),
      ExprKind::Use { source } => self.extract_expr(source),
      ExprKind::NeverToAny { source } => self.extract_expr(source),

      ExprKind::Deref { arg } => self.extract_deref(arg),
      ExprKind::Borrow { borrow_kind, arg } => self.extract_borrow(borrow_kind, arg),

      ExprKind::Assign { lhs, rhs } => self.extract_assignment(lhs, rhs),

      ExprKind::Return { value } => self.extract_return(*value),

      _ => self.unsupported_expr(
        expr.span,
        format!("Cannot extract expr kind {:?}", expr.kind),
      ),
    }
  }

  pub(super) fn extract_exprs<I>(&mut self, exprs: I) -> Vec<st::Expr<'l>>
  where
    I: IntoIterator<Item = &'a Expr<'a, 'tcx>>,
  {
    exprs
      .into_iter()
      .map(|arg| self.extract_expr(arg))
      .collect()
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
        StdItem::CrateItem(item) if item.is_map_related() => {
          Some(self.extract_map_expr(item, args, substs_ref, span))
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
    substs: SubstsRef<'tcx>,
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
    let arg_tps_without_parents = self.base.extract_arg_tys(
      substs
        .types()
        .skip(class_def.map_or(0, |cd| cd.tparams.len())),
      &self.txtcx,
      span,
    );
    let fn_sig = self.base.ty_fn_sig(def_id).subst(self.tcx(), substs);

    // If the callee has a HIR body, then we use the HIR body to see which
    // arguments are locally mutable and hence need to be wrapped in a MutCell.
    let are_params_mutable: Vec<bool> = self
      .base
      .def_to_hir_id(def_id)
      .and_then(|hir_id| self.base.hir_body(hir_id))
      .map(|body| {
        body
          .params
          .iter()
          .map(|p| is_mutable_binding(p.pat))
          .collect()
      })
      .unwrap_or_else(|| vec![false; args.len()]);

    // Wrap locally mutable params in a MutCell
    let mut args: Vec<_> = self
      .extract_exprs(args)
      .into_iter()
      .zip(
        self
          .base
          .extract_arg_tys(fn_sig.inputs().iter().copied(), &self.txtcx, span),
      )
      .zip(are_params_mutable)
      .map(|((arg, tpe), is_mutable)| {
        if is_mutable {
          self.synth().mut_cell(tpe, arg)
        } else {
          arg
        }
      })
      .collect();

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
      let recv_tps = self.base.extract_tys(substs.types(), &self.txtcx, span);
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
    let f = self.factory();
    f.Return(
      value
        .map(|v| self.extract_move_copy(v))
        .unwrap_or_else(|| f.UnitLiteral().into()),
    )
    .into()
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
  /// FIXME: This is a work-around for the problems encountered while extracting
  ///   Clone and PartialEq as type classes. See the issue for details. The way
  ///   to correctly solve this use-case is by attaching a spec to the real
  ///   `Clone::clone` that preserves equality.
  ///   https://github.com/epfl-lara/rust-stainless/issues/136
  fn extract_clone(&mut self, arg: &'a Expr<'a, 'tcx>) -> Option<st::Expr<'l>> {
    // Extract with fresh copy to be sure to have distinct objects. Rustc
    // doesn't automatically derive Clone for types that contain mutable
    // references, therefore we can't accidentally freshCopy MutCells that we
    // shouldn't copy here. (Once we test that the clone is derived => FIXME)
    Some(self.extract_move_copy(arg))
  }

  fn is_str_type(&mut self, expr: &'a Expr<'a, 'tcx>) -> bool {
    let ty = self.base.extract_ty(expr.ty, &self.txtcx, expr.span);
    matches!(ty, st::Type::StringType(_))
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

      let arg_tps = self
        .base
        .extract_arg_tys(substs.types(), &self.txtcx, expr.span);

      // If the ADT is constructed with "struct update syntax"
      let args: Vec<_> = if let Some(FruInfo { base, .. }) = base {
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
            fields_by_index.get(&index).copied().unwrap_or_else(|| {
              self
                .synth()
                .mut_cell_value(f.ADTSelector(adt, fi.v.id).into())
            })
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

      // Wrap args in MutCell
      let args = args
        .into_iter()
        .zip(
          self
            .base
            .adt_field_types(adt_def, *variant_index, &self.txtcx, substs),
        )
        .map(|(a, t)| {
          if self.base.is_mut_cell(t) {
            a
          } else {
            self.synth().mut_cell(t, a)
          }
        })
        .collect();

      f.ADT(constructor.id, arg_tps, args).into()
    } else {
      unreachable!()
    }
  }

  fn strip_scopes(&mut self, expr: &'a Expr<'a, 'tcx>) -> &'a Expr<'a, 'tcx> {
    match expr.kind {
      ExprKind::Scope { value, .. } => self.strip_scopes(value),
      _ => expr,
    }
  }

  /// Extract a move or copy of the expression. By default everything is
  /// freshCopy'd because this is safe except for mutable types (see thesis).
  ///
  /// A mutable type is a type that is or contains mutable references
  /// [is_mutable]. For mutable types, we return the identity so that changes to
  /// the mutable data will be propagated.
  ///
  /// Additionally, we don't insert freshCopy around some control structures for
  /// which we know by design that a freshCopy will be inserted inside the
  /// structure (like ifs, matches, return). We also omit freshCopy for common
  /// Rust & JVM primitive types.
  fn extract_move_copy(&mut self, expr: &'a Expr<'a, 'tcx>) -> st::Expr<'l> {
    let e = self.extract_expr(expr);
    if is_mutable(expr.ty) {
      e
    } else {
      let tpe = self.base.extract_ty(expr.ty, &self.txtcx, expr.span);
      match e {
        // don't nest freshCopy
        st::Expr::FreshCopy(_)
        // don't fresh copy entire matches/ifs, we leave that to each block
        | st::Expr::MatchExpr(_)
        | st::Expr::IfExpr(_)
        // don't fresh copy "around" the return, we do it inside
        | st::Expr::Return(_) => e,
        _ => match tpe {
          st::Type::BooleanType(_)
          | st::Type::NothingType(_)
          | st::Type::BVType(_)
          | st::Type::CharType(_)
          | st::Type::UnitType(_)
          | st::Type::RealType(_)
          | st::Type::IntegerType(_) => e,
          _ => self.factory().FreshCopy(e).into(),
        },
      }
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
