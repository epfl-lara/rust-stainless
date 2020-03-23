use std::fmt;

use super::*;

macro_rules! writeSeq {
  ($f:expr, $xs:expr, $sep:expr) => {{
    let mut it = $xs.iter();
    if let Some(x) = it.next() {
      write!($f, "{}", x)?
    }
    for x in it {
      write!($f, concat!($sep, "{}"), x)?;
    }
    fmt::Result::Ok(())
  }};
}

impl fmt::Display for Identifier {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}${}", self.name, self.globalId)
  }
}

impl<'a> fmt::Display for SymbolIdentifier<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}<{}>", self.id, self.symbol_path.join("::"))
  }
}

impl<'a> fmt::Display for FunDef<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for flag in &self.flags {
      writeln!(f, "{}", flag)?;
    }
    write!(f, "fn {}", self.id)?;
    if !self.tparams.is_empty() {
      let tparams: Vec<&TypeParameter<'a>> = self.tparams.iter().map(|td| td.tp).collect();
      write!(f, "<")?;
      writeSeq!(f, tparams, ", ")?;
      write!(f, ">")?;
    }
    write!(f, "(")?;
    writeSeq!(f, self.params, ", ")?;
    writeln!(f, ") -> {} {{", self.returnType)?;
    writeln!(f, "    {}", self.fullBody)?;
    writeln!(f, "}}")
  }
}

impl<'a> fmt::Display for ValDef<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for flag in &self.v.flags {
      write!(f, "{} ", flag)?;
    }
    write!(f, "{}: {}", self.v.id, self.v.tpe)
  }
}

impl<'a> fmt::Display for TypeParameter<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for flag in &self.flags {
      write!(f, "{} ", flag)?;
    }
    write!(f, "{}", self.id)
  }
}

impl<'a> fmt::Display for Expr<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expr::Variable(e) => e.fmt(f),
      Expr::BooleanLiteral(e) => e.fmt(f),
      Expr::BVLiteral(e) => e.fmt(f),
      Expr::ADT(e) => e.fmt(f),
      Expr::Plus(e) => e.fmt(f),
      Expr::Minus(e) => e.fmt(f),
      Expr::Times(e) => e.fmt(f),
      Expr::Division(e) => e.fmt(f),
      Expr::Not(e) => e.fmt(f),
      Expr::Equals(e) => e.fmt(f),
      Expr::And(e) => e.fmt(f),
      Expr::Or(e) => e.fmt(f),
      Expr::LessThan(e) => e.fmt(f),
      Expr::LessEquals(e) => e.fmt(f),
      Expr::GreaterThan(e) => e.fmt(f),
      Expr::GreaterEquals(e) => e.fmt(f),
      Expr::FunctionInvocation(e) => e.fmt(f),
      Expr::Tuple(e) => e.fmt(f),
      Expr::TupleSelect(e) => e.fmt(f),
      Expr::IfExpr(e) => e.fmt(f),
      _ => unimplemented!("No formatter for {:?}", self),
    }
  }
}

impl<'a> fmt::Display for Type<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Type::BooleanType(t) => t.fmt(f),
      Type::BVType(t) => t.fmt(f),
      Type::TupleType(t) => t.fmt(f),
      _ => unimplemented!("No formatter for {:?}", self),
    }
  }
}

impl<'a> fmt::Display for Flag<'a> {
  fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      _ => unimplemented!("No formatter for {:?}", self),
    }
  }
}

impl fmt::Display for BooleanType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "bool")
  }
}

impl fmt::Display for BVType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.signed {
      write!(f, "i")?;
    } else {
      write!(f, "u")?;
    }
    write!(f, "{}", self.size)
  }
}

impl<'a> fmt::Display for TupleType<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(")?;
    writeSeq!(f, self.bases, ", ")?;
    write!(f, ")")
  }
}

impl<'a> fmt::Display for Variable<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.id)
  }
}

impl fmt::Display for BooleanLiteral {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.value {
      write!(f, "true")
    } else {
      write!(f, "false")
    }
  }
}

impl fmt::Display for BVLiteral {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value.to_str_radix(10))?;
    if self.signed {
      write!(f, "i")?;
    } else {
      write!(f, "u")?;
    }
    write!(f, "{}", self.size)
  }
}

impl<'a> fmt::Display for ADT<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.id)?;
    if !self.tps.is_empty() {
      write!(f, "<")?;
      writeSeq!(f, self.tps, ", ")?;
      write!(f, ">")?;
    }
    write!(f, "(")?;
    writeSeq!(f, self.args, ", ")?;
    write!(f, ")")
  }
}

impl<'a> fmt::Display for Plus<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({} + {})", self.lhs, self.rhs)
  }
}

impl<'a> fmt::Display for Minus<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({} - {})", self.lhs, self.rhs)
  }
}

impl<'a> fmt::Display for Times<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({} * {})", self.lhs, self.rhs)
  }
}

impl<'a> fmt::Display for Division<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({} / {})", self.lhs, self.rhs)
  }
}

impl<'a> fmt::Display for Not<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(!{})", self.expr)
  }
}

impl<'a> fmt::Display for Equals<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({} == {})", self.lhs, self.rhs)
  }
}

impl<'a> fmt::Display for And<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(")?;
    writeSeq!(f, self.exprs, " && ")?;
    write!(f, ")")
  }
}

impl<'a> fmt::Display for Or<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(")?;
    writeSeq!(f, self.exprs, " || ")?;
    write!(f, ")")
  }
}

impl<'a> fmt::Display for LessThan<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({} < {})", self.lhs, self.rhs)
  }
}

impl<'a> fmt::Display for LessEquals<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({} <= {})", self.lhs, self.rhs)
  }
}

impl<'a> fmt::Display for GreaterThan<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({} > {})", self.lhs, self.rhs)
  }
}

impl<'a> fmt::Display for GreaterEquals<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({} >= {})", self.lhs, self.rhs)
  }
}

impl<'a> fmt::Display for FunctionInvocation<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.id)?;
    if !self.tps.is_empty() {
      write!(f, "<")?;
      writeSeq!(f, self.tps, ", ")?;
      write!(f, ">")?;
    }
    write!(f, "(")?;
    writeSeq!(f, self.args, ", ")?;
    write!(f, ")")
  }
}

impl<'a> fmt::Display for Tuple<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(")?;
    writeSeq!(f, self.exprs, ", ")?;
    write!(f, ")")
  }
}

impl<'a> fmt::Display for TupleSelect<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}.{}", self.tuple, self.index)
  }
}

impl<'a> fmt::Display for IfExpr<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(
      f,
      "if {} {{\n    {}\n}} else {{\n    {}\n}}",
      self.cond, self.thenn, self.elze
    )
  }
}
