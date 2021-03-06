use super::*;

use rustc_middle::mir::Mutability;
use rustc_middle::ty::AdtDef;
use rustc_mir_build::thir::{Arm, BindingMode, FieldPat, Guard, Pat, PatKind};
use rustc_target::abi::VariantIdx;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_if(
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

  pub(super) fn try_pattern_to_var(
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

  pub(super) fn extract_match(
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
      } => self
        .extract_adt_pattern(
          adt_def,
          *variant_index,
          binder,
          subpatterns,
          substs,
          pattern.span,
        )
        .into(),

      // From rustc_hair docs:  `(...)`, `Foo(...)`, `Foo{...}`, or `Foo`, where
      // `Foo` is a variant name from an ADT with a single variant.
      box PatKind::Leaf { subpatterns } => match pattern.ty.kind() {
        TyKind::Adt(adt_def, substs) => self
          .extract_adt_pattern(
            adt_def,
            VariantIdx::from_u32(0),
            binder,
            subpatterns,
            substs,
            pattern.span,
          )
          .into(),

        // Empty tuple is more like Unit
        TyKind::Tuple(substs) if substs.is_empty() => {
          f.LiteralPattern(binder, f.UnitLiteral().into()).into()
        }

        TyKind::Tuple(substs) => {
          let id = self.synth().tuple_id(substs.len());
          self
            .adt_pattern(binder, id, subpatterns, substs, substs.len(), pattern.span)
            .into()
        }

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

  fn extract_adt_pattern(
    &mut self,
    adt_def: &AdtDef,
    variant_index: VariantIdx,
    binder: Option<&'l st::ValDef<'l>>,
    subpatterns: &[FieldPat<'tcx>],
    substs: SubstsRef<'tcx>,
    span: Span,
  ) -> &'l st::ADTPattern<'l> {
    let sort = self.base.get_or_extract_adt(adt_def.did);
    let constructor = sort.constructors[variant_index.index()];
    self.adt_pattern(
      binder,
      constructor.id,
      subpatterns,
      substs,
      constructor.fields.len(),
      span,
    )
  }

  fn adt_pattern(
    &mut self,
    binder: Option<&'l st::ValDef<'l>>,
    id: StainlessSymId<'l>,
    subpatterns: &[FieldPat<'tcx>],
    substs: SubstsRef<'tcx>,
    fields_len: usize,
    span: Span,
  ) -> &'l st::ADTPattern<'l> {
    let arg_tps = self.extract_arg_types(substs.types(), span);
    let subpatterns = self.extract_subpatterns(subpatterns.to_vec(), fields_len);
    self.factory().ADTPattern(binder, id, arg_tps, subpatterns)
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
}
