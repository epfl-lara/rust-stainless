use super::spec::SpecType;
use super::*;

use rustc_mir_build::thir::Block;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_block(
    &mut self,
    Block {
      stmts,
      expr: final_expr,
      ..
    }: &Block<'a, 'tcx>,
  ) -> st::Expr<'l> {
    let f = self.factory();

    let mut stmts: Vec<_> = stmts.iter().collect();
    let final_expr = final_expr
      // The return value is necessarily aliasable because we don't support
      // returning `&mut` references.
      .map(|e| self.extract_aliasable_expr(e))
      // If there's no final expression, we need to check whether the last
      // statement is a return. If yes, we take the return as final expression.
      .or_else(|| {
        stmts.last().cloned().and_then(|s| match s.kind {
          StmtKind::Expr { expr, .. } => match self.strip_scopes(expr).kind {
            ExprKind::Return { value } => {
              stmts.pop();
              Some(self.extract_return(value))
            }
            _ => None,
          },
          _ => None,
        })
      })
      .unwrap_or_else(|| f.UnitLiteral().into());

    stmts.reverse();
    let mut spec_ids = HashMap::new();
    let body_expr = self.extract_block_(&mut stmts, &mut vec![], &mut spec_ids, final_expr);
    self.extract_specs(&spec_ids, body_expr)
  }

  fn extract_block_(
    &mut self,
    stmts: &mut Vec<&Stmt<'a, 'tcx>>,
    acc_exprs: &mut Vec<st::Expr<'l>>,
    // Accumulates the HirId's of all spec closures for later extraction
    acc_specs: &mut HashMap<SpecType, Vec<HirId>>,
    final_expr: st::Expr<'l>,
  ) -> st::Expr<'l> {
    let f = self.factory();

    let finish = |exprs: Vec<st::Expr<'l>>, final_expr| {
      if exprs.is_empty() {
        final_expr
      } else {
        f.Block(exprs, final_expr).into()
      }
    };

    if let Some(stmt) = stmts.pop() {
      let bail = |msg, span| -> st::Expr<'l> {
        self.base.unsupported(span, msg);
        f.Block(acc_exprs.clone(), f.NoTree(f.Untyped().into()).into())
          .into()
      };

      match &stmt.kind {
        // Spec expressions are recognized by their specific closure shape and
        // their attributes (stainless::) flags.
        StmtKind::Expr {
          expr:
            Expr {
              kind:
                ExprKind::Scope {
                  lint_level: thir::LintLevel::Explicit(hir_id),
                  value: expr,
                  ..
                },
              ..
            },
          ..
        } if matches!(expr.kind, ExprKind::Closure { .. }) => {
          if let Ok(spec_type) = SpecType::try_from(self.tcx().hir().attrs(*hir_id)) {
            acc_specs
              .entry(spec_type)
              .or_insert_with(Vec::new)
              .push(*hir_id);
            self.extract_block_(stmts, acc_exprs, acc_specs, final_expr)
          } else {
            bail("Cannot extract closure that is not a spec.", expr.span)
          }
        }

        StmtKind::Let {
          pattern,
          initializer: None,
          ..
        } => bail("Cannot extract let without initializer", pattern.span),

        StmtKind::Let {
          pattern,
          initializer: Some(init),
          ..
        } => {
          // FIXME: Detect desugared `let`s
          let has_abnormal_source = false;
          if has_abnormal_source {
            // TODO: Support for loops
            bail(
              "Cannot extract let that resulted from desugaring",
              pattern.span,
            )
          } else {
            match self.try_pattern_to_var(&pattern.kind, false) {
              // TODO: Desugar complex patterns
              Err(reason) => bail(
                &format!("Cannot extract complex pattern in let: {}", reason),
                pattern.span,
              ),
              Ok(vd) => {
                let init_expr = self.extract_aliasable_expr(init);
                let init_expr = if vd.is_wrapped() {
                  let tpe = self.base.extract_ty(init.ty, &self.txtcx, init.span);
                  self.synth().mut_cell(tpe, init_expr)
                } else {
                  init_expr
                };

                // recurse the extract all the following statements
                let exprs = acc_exprs.clone();
                acc_exprs.clear();
                let body_expr = self.extract_block_(stmts, acc_exprs, acc_specs, final_expr);

                // wrap that body expression into the Let
                let last_expr = if vd.is_mutable() {
                  f.LetVar(vd, init_expr, body_expr).into()
                } else {
                  f.Let(vd, init_expr, body_expr).into()
                };
                finish(exprs, last_expr)
              }
            }
          }
        }

        StmtKind::Expr { expr, .. } => {
          let expr = self.extract_expr(expr);
          acc_exprs.push(expr);
          self.extract_block_(stmts, acc_exprs, acc_specs, final_expr)
        }
      }
    } else {
      finish(acc_exprs.clone(), final_expr)
    }
  }
}
