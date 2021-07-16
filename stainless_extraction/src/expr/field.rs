use rustc_middle::mir::Field;

use super::*;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_assignment(
    &mut self,
    lhs: &'a Expr<'a, 'tcx>,
    rhs: &'a Expr<'a, 'tcx>,
  ) -> st::Expr<'l> {
    let f = self.factory();

    // Assigning creates an alias of the rhs, therefore the rhs needs to be
    // potentially fresh copied. Fresh copies are not inserted for MutCells i.e.
    // mutable references. For other types it's safe to fresh copy because rustc
    // ensures that we own the value we assign.
    let value = self.extract_move_copy(rhs);
    let lhs = self.strip_scopes(lhs);
    match &lhs.kind {
      ExprKind::VarRef { id } => f
        .FieldAssignment(
          self.fetch_var(*id).into(),
          self.synth().mut_cell_value_id(),
          value,
        )
        .into(),

      ExprKind::Field { lhs, name } => match lhs.ty.kind() {
        TyKind::Adt(adt_def, _) => {
          let adt = self.extract_expr(lhs);
          let selector = self.extract_field_selector(adt_def.did, *name);
          f.FieldAssignment(
            f.ADTSelector(adt, selector).into(),
            self.synth().mut_cell_value_id(),
            value,
          )
          .into()
        }
        TyKind::Tuple(substs) => {
          let lhs = self.extract_expr(lhs);
          f.FieldAssignment(
            self.synth().tuple_select(substs.len(), lhs, name.index()),
            self.synth().mut_cell_value_id(),
            value,
          )
          .into()
        }
        ref t => self.unsupported_expr(
          lhs.span,
          format!("Cannot extract assignment to type {:?}", t),
        ),
      },

      ExprKind::Deref { arg } => {
        let arg = self.strip_scopes(arg);
        match arg.kind {
          ExprKind::VarRef { id } if is_mut_ref(arg.ty) => f
            .FieldAssignment(
              self.extract_var_ref(id),
              self.synth().mut_cell_value_id(),
              value,
            )
            .into(),
          _ => {
            let arg = self.extract_expr(arg);
            f.FieldAssignment(arg, self.synth().mut_cell_value_id(), value)
              .into()
          }
        }
      }

      e => self.unsupported_expr(
        lhs.span,
        format!("Cannot extract assignment to kind {:?}", e),
      ),
    }
  }

  pub(super) fn extract_field(
    &mut self,
    lhs: &'a Expr<'a, 'tcx>,
    field: Field,
    mutable_borrow: bool,
  ) -> st::Expr<'l> {
    let f = self.factory();
    let field = match lhs.ty.kind() {
      TyKind::Tuple(substs) => {
        let lhs = self.extract_expr(lhs);
        self.synth().tuple_select(substs.len(), lhs, field.index())
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
    };
    if mutable_borrow {
      field
    } else {
      self.synth().mut_cell_value(field)
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
