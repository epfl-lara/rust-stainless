use super::*;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_tuple(&mut self, fields: &'a [Expr<'a, 'tcx>], span: Span) -> st::Expr<'l> {
    if fields.is_empty() {
      self.factory().UnitLiteral().into()
    } else {
      let tps = self
        .base
        .extract_tys(fields.iter().map(|f| f.ty), &self.txtcx, span);
      let args = self.extract_exprs(fields);
      self.synth().tuple(tps, args)
    }
  }
}
