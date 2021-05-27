use rustc_middle::mir::Field;

use super::*;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_assignment(
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

      ExprKind::Deref { arg } => {
        let arg = self.extract_expr(arg);
        self
          .factory()
          .FieldAssignment(arg, self.synth().mut_ref_value_id(), value)
          .into()
      }

      e => self.unsupported_expr(
        lhs.span,
        format!("Cannot extract assignment to kind {:?}", e),
      ),
    }
  }

  pub(super) fn extract_field(&mut self, lhs: &'a Expr<'a, 'tcx>, field: Field) -> st::Expr<'l> {
    match lhs.ty.kind() {
      TyKind::Tuple(substs) => {
        let lhs = self.extract_expr(lhs);
        self.synth().tuple_select(substs.len(), lhs, field.index())
      }
      TyKind::Adt(adt_def, _) => {
        let selector = self.extract_field_selector(adt_def.did, field);
        let lhs = self.extract_expr(lhs);
        self.factory().ADTSelector(lhs, selector).into()
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
}
