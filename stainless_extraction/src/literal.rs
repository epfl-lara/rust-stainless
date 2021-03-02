use rustc_middle::mir::interpret::ConstValue;
use rustc_middle::ty::{self, ConstKind, TyCtxt, TyKind};
use rustc_target::abi;

use crate::ty::{int_bit_width, uint_bit_width};
use stainless_data::ast as st;
use stainless_data::ser::types as st_types;

pub(super) enum Literal {
  Unit,
  Bool(bool),
  Int { value: i128, size: u64 },
  Uint { value: u128, size: u64 },
  String(String),
}

impl Literal {
  pub(super) fn as_st_literal<'l>(&self, f: &'l st::Factory) -> st::Expr<'l> {
    match self {
      Literal::Unit => f.UnitLiteral().into(),
      Literal::Bool(value) => f.BooleanLiteral(*value).into(),
      // TODO: Double-check that this corresponds to Stainless' decoding of values
      Literal::Int { value, size } => f
        .BVLiteral(true, (*value).into(), *size as st_types::Int)
        .into(),
      Literal::Uint { value, size } => f
        .BVLiteral(false, (*value).into(), *size as st_types::Int)
        .into(),
      Literal::String(value) => f.StringLiteral(value.clone()).into(),
    }
  }

  pub fn from_const(konst: &ty::Const<'_>, tcx: TyCtxt<'_>) -> Option<Self> {
    match konst.ty.kind {
      _ if konst.ty.is_unit() => Some(Literal::Unit),

      TyKind::Bool => {
        let value = konst.val.try_to_bits(abi::Size::from_bits(1)).unwrap() == 1;
        Some(Literal::Bool(value))
      }

      TyKind::Int(int_ty) => {
        let size = int_bit_width(int_ty, tcx);
        let value = konst.val.try_to_bits(abi::Size::from_bits(size));
        value.map(|value| Literal::Int {
          value: value as i128,
          size,
        })
      }
      TyKind::Uint(uint_ty) => {
        let size = uint_bit_width(uint_ty, tcx);
        let value = konst.val.try_to_bits(abi::Size::from_bits(size));
        value.map(|value| Literal::Uint { value, size })
      }

      TyKind::Ref(
        _,
        ty::TyS {
          kind: TyKind::Str, ..
        },
        _,
      ) => match konst.val {
        ConstKind::Value(ConstValue::Slice { data, start, end }) => {
          let slice = data.inspect_with_undef_and_ptr_outside_interpreter(start..end);
          let s = ::std::str::from_utf8(slice).expect("Expected UTF8 str in ConstValue");
          Some(Literal::String(s.into()))
        }
        _ => None,
      },
      _ => None,
    }
  }
}
