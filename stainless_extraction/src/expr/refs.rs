use rustc_middle::mir::BorrowKind;

use super::*;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_deref(&mut self, arg: &'a Expr<'a, 'tcx>) -> st::Expr<'l> {
    let expr = self.extract_expr(arg);
    if is_mut_ref(arg.ty) {
      self.synth().mut_cell_value(expr)
    } else {
      expr
    }
  }

  pub(super) fn extract_borrow(
    &mut self,
    borrow_kind: &BorrowKind,
    arg: &'a Expr<'a, 'tcx>,
  ) -> st::Expr<'l> {
    match borrow_kind {
      // Borrow an immutable and aliasable value (i.e. the meaning of
      // BorrowKind::Shared). Handle this safe case with erasure of the
      // reference augmented with fresh copy to instruct Stainless that this
      // aliasing is safe.
      BorrowKind::Shared => {
        let arg = self.strip_scopes(arg);
        match arg.kind {
          // Re-borrows a dereferenced mutable borrow, we take the original ref
          ExprKind::Deref { arg: inner } if is_mut_ref(inner.ty) => self.extract_expr(inner),
          _ => self.extract_expr(arg),
        }
      }

      BorrowKind::Mut { .. } => self.extract_mut_borrow_expr(arg),

      _ => self.unsupported_expr(arg.span, format!("borrow kind {:?}", borrow_kind)),
    }
  }

  /// Extracts an expression and avoids wrapping mutable cell values into field
  /// selectors because a mutable borrow wants to access the cell not its value.
  fn extract_mut_borrow_expr(&mut self, arg: &'a Expr<'a, 'tcx>) -> st::Expr<'l> {
    let arg = self.strip_scopes(arg);
    match arg.kind {
      ExprKind::Deref { arg: inner } if is_mut_ref(inner.ty) => self.extract_mut_borrow_expr(inner),
      ExprKind::Field { lhs, name } => self.extract_field(lhs, name, true),
      ExprKind::VarRef { id } => self.fetch_var(id).into(),
      _ => self.extract_expr(arg),
    }
  }

  pub(super) fn extract_var_ref(&mut self, id: HirId) -> st::Expr<'l> {
    let var = self.fetch_var(id);
    if var.is_mutable() {
      self.synth().mut_cell_value(var.into())
    } else {
      var.into()
    }
  }
}
