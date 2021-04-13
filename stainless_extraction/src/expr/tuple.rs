use super::*;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_tuple(&mut self, fields: &'a [Expr<'a, 'tcx>], span: Span) -> st::Expr<'l> {
    let f = self.factory();
    match fields.len() {
      0 => f.UnitLiteral().into(),
      1 => self.unsupported_expr(span, "Cannot extract one-tuples"),
      _ => f.Tuple(self.extract_exprs(fields)).into(),
    }
  }
}
