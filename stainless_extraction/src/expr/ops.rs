use super::*;

use rustc_middle::mir::{BinOp, UnOp};
use rustc_mir_build::thir::LogicalOp;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_unary(
    &mut self,
    op: &UnOp,
    arg: &'a Expr<'a, 'tcx>,
    span: Span,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let arg_ty = arg.ty;
    let arg_is_bv = self.base.is_bv_type(arg_ty);
    let arg_is_int = arg_is_bv || self.base.is_bigint_type(arg_ty);
    let arg = self.extract_expr(arg);

    match op {
      UnOp::Not if arg_is_bv => f.BVNot(arg).into(),
      UnOp::Not if arg_ty.is_bool() => f.Not(arg).into(),
      UnOp::Neg if arg_is_int => f.UMinus(arg).into(),
      _ => unexpected(span, format!("Cannot extract unary op {:?}", op)),
    }
  }

  pub(super) fn extract_binary(
    &mut self,
    op: BinOp,
    arg1: &'a Expr<'a, 'tcx>,
    arg2: &'a Expr<'a, 'tcx>,
    span: Span,
  ) -> st::Expr<'l> {
    let f = self.factory();

    let (arg1_ty, arg2_ty) = (arg1.ty, arg2.ty);
    let args_are_bv = self.base.is_bv_type(arg1_ty) && self.base.is_bv_type(arg2_ty);
    let args_are_bool = arg1_ty.is_bool() && arg2_ty.is_bool();

    let (arg1, arg2) = (self.extract_expr(arg1), self.extract_expr(arg2));
    match op {
      _ if !args_are_bv && !args_are_bool => {
        self.unsupported_expr(span, format!("Cannot extract binary op {:?}", op))
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
        self.extract_shift(arg1, arg2, arg1_ty, arg2_ty, op == BinOp::Shl, span)
      }
      _ => {
        // TODO: Support pointer offset BinOp?
        self.unsupported_expr(span, format!("Cannot extract binary op {:?}", op))
      }
    }
  }

  pub(super) fn extract_logical_op(
    &mut self,
    op: LogicalOp,
    arg1: &'a Expr<'a, 'tcx>,
    arg2: &'a Expr<'a, 'tcx>,
  ) -> st::Expr<'l> {
    let f = self.factory();
    assert!(arg1.ty.is_bool() && arg2.ty.is_bool());
    let (arg1, arg2) = (self.extract_expr(arg1), self.extract_expr(arg2));
    match op {
      LogicalOp::And => f.And(vec![arg1, arg2]).into(),
      LogicalOp::Or => f.Or(vec![arg1, arg2]).into(),
    }
  }

  /// Stainless requires matching bitvector types on all operations, so we insert appropriate
  /// widenings, where possible. Stainless currently doesn't support mixing operands of
  /// different signedness, however, so we reject such cases.
  fn extract_shift(
    &mut self,
    arg1: st::Expr<'l>,
    arg2: st::Expr<'l>,
    arg1_ty: Ty<'tcx>,
    arg2_ty: Ty<'tcx>,
    is_shl: bool,
    span: Span,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let bail = |bxtor: &mut BodyExtractor<'_, 'l, 'tcx>, msg| bxtor.unsupported_expr(span, msg);

    // No need to adapt anything,
    let arg2 = if arg1_ty.kind() == arg2_ty.kind() {
      arg2
    } else {
      let (width1, width2, signed) = match (&arg1_ty.kind(), &arg2_ty.kind()) {
        (TyKind::Int(int_ty1), TyKind::Int(int_ty2)) => (
          int_bit_width(int_ty1, self.tcx()),
          int_bit_width(int_ty2, self.tcx()),
          true,
        ),
        (TyKind::Uint(uint_ty1), TyKind::Uint(uint_ty2)) => (
          uint_bit_width(uint_ty1, self.tcx()),
          uint_bit_width(uint_ty2, self.tcx()),
          false,
        ),
        _ => {
          return bail(
            self,
            "Cannot extract shift mixing signed and unsigned operands",
          );
        }
      };
      if width1 > width2 {
        f.BVWideningCast(arg2, f.BVType(signed, width1 as i32))
          .into()
      } else {
        return bail(self, "Adapting lhs shift argument would change result type");
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
}
