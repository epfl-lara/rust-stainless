use super::*;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_set_expr(
    &mut self,
    item: CrateItem,
    args: &'a [Expr<'a, 'tcx>],
    substs_ref: SubstsRef<'tcx>,
    span: Span,
  ) -> st::Expr<'l> {
    match item {
      SetEmptyFn | SetSingletonFn => self.extract_set_creation(args, substs_ref, span),
      i if i.is_set_related() => self.extract_set_op(i, args, span),
      _ => panic!("Cannot be called on non set-related item, was {:?}", item),
    }
  }

  fn extract_set_op(
    &mut self,
    std_item: CrateItem,
    args: &'a [Expr<'a, 'tcx>],
    span: Span,
  ) -> st::Expr<'l> {
    let f = self.factory();
    match &self.extract_exprs(args)[..] {
      [set, arg] => match std_item {
        SetAddFn => f.SetAdd(*set, *arg).into(),
        SetDifferenceFn => f.SetDifference(*set, *arg).into(),
        SetIntersectionFn => f.SetIntersection(*set, *arg).into(),
        SetUnionFn => f.SetUnion(*set, *arg).into(),
        SubsetOfFn => f.SubsetOf(*set, *arg).into(),
        SetContainsFn => f.ElementOfSet(*arg, *set).into(),
        _ => unreachable!(),
      },

      _ => self.unsupported_expr(
        span,
        format!(
          "Cannot extract set operation with {} arguments.",
          args.len()
        ),
      ),
    }
  }

  fn extract_set_creation(
    &mut self,
    args: &'a [Expr<'a, 'tcx>],
    substs: SubstsRef<'tcx>,
    span: Span,
  ) -> st::Expr<'l> {
    let args = self.extract_exprs(args);
    let ty = self.base.extract_ty(substs.type_at(0), &self.txtcx, span);
    self.factory().FiniteSet(args, ty).into()
  }
}
