use std::fmt;

use super::*;

macro_rules! writeSeq {
  ($f:expr, $xs:expr) => {{
    let mut it = $xs.iter();
    if let Some(x) = it.next() {
      write!($f, "{}", x)?
    }
    for x in it {
      write!($f, ", {}", x)?;
    }
    fmt::Result::Ok(())
  }}
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


impl<'a> fmt::Display for Expr<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expr::BooleanLiteral(e) => e.fmt(f),
      Expr::BVLiteral(e) => e.fmt(f),
      Expr::ADT(e) => e.fmt(f),
      Expr::Plus(e) => e.fmt(f),
      Expr::Minus(e) => e.fmt(f),
      Expr::Times(e) => e.fmt(f),
      Expr::Division(e) => e.fmt(f),
      Expr::FunctionInvocation(e) => e.fmt(f),
      _ => unimplemented!()
    }
  }
}

impl<'a> fmt::Display for Type<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Type::BooleanType(t) => t.fmt(f),
      Type::BVType(t) => t.fmt(f),
      _ => unimplemented!()
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
    if self.signed { write!(f, "i")?; } else { write!(f, "u")?; }
    write!(f, "{}", self.size)
  }
}


impl fmt::Display for BooleanLiteral {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.value { write!(f, "true") } else { write!(f, "false") }
  }
}

impl fmt::Display for BVLiteral {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value.to_str_radix(10))?;
    if self.signed { write!(f, "i")?; } else { write!(f, "u")?; }
    write!(f, "{}", self.size)
  }
}


impl<'a> fmt::Display for ADT<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}[", self.id)?;
    writeSeq!(f, self.tps)?;
    write!(f, "](")?;
    writeSeq!(f, self.args)?;
    write!(f, ")")
  }
}


impl<'a> fmt::Display for Equals<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({} == {})", self.lhs, self.rhs)
  }
}

impl<'a> fmt::Display for Not<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(!{})", self.expr)
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


impl<'a> fmt::Display for FunctionInvocation<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}[", self.id)?;
    writeSeq!(f, self.tps)?;
    write!(f, "](")?;
    writeSeq!(f, self.args)?;
    write!(f, ")")
  }
}