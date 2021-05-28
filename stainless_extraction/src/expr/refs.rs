use rustc_middle::mir::BorrowKind;

use super::*;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_deref(&mut self, arg: &'a Expr<'a, 'tcx>) -> st::Expr<'l> {
    match arg.ty.ref_mutability() {
      Some(Mutability::Mut) => {
        let arg = self.extract_expr(arg);
        self.synth().mut_ref_value(arg)
      }
      _ => self.extract_expr(arg),
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
      BorrowKind::Shared => self.extract_aliasable_expr(arg),

      BorrowKind::Mut { .. } => match arg.kind {
        // Re-borrows a dereferenced mutable borrow, we take the original ref
        ExprKind::Deref { arg } if matches!(arg.ty.ref_mutability(), Some(Mutability::Mut)) => {
          self.extract_expr(arg)
        }
        _ => {
          let tpe = self.base.extract_ty(arg.ty, &self.txtcx, arg.span);
          let a = self.extract_expr(arg);
          self.synth().mut_ref(tpe, a)
        }
      },
      _ => self.unsupported_expr(arg.span, format!("borrow kind {:?}", borrow_kind)),
    }
  }
}
