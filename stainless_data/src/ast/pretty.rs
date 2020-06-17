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
    write!(f, "{}", self.id)?;
    // if !self.symbol_path.is_empty() {
    //   write!(f, "<{}>", self.symbol_path.join("::"))?;
    // }
    Ok(())
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
    match self.fullBody {
      Expr::Ensuring(e) => {
        writeln!(f, "}} ensuring ({})", e.pred)
      },
      _ => writeln!(f, "}}")
    }
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

impl<'a> fmt::Display for ADTSort<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "enum {} {{", self.id)?;
    writeSeq!(f, self.constructors, ",\n")?;
    writeln!(f, "\n}}")
  }
}

impl<'a> fmt::Display for ADTConstructor<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "    {}", self.id)?;
    if !&self.fields.is_empty() {
      write!(f, "(")?;
      writeSeq!(f, self.fields, ", ")?;
      write!(f, ")")?;
    }
    Ok(())
  }
}

impl<'a> fmt::Display for Expr<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expr::Variable(e) => e.fmt(f),
      Expr::UnitLiteral(e) => e.fmt(f),
      Expr::BooleanLiteral(e) => e.fmt(f),
      Expr::IntegerLiteral(e) => e.fmt(f),
      Expr::BVLiteral(e) => e.fmt(f),
      Expr::ADT(e) => e.fmt(f),
      Expr::UMinus(e) => e.fmt(f),
      Expr::BVNot(e) => e.fmt(f),
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
      Expr::Let(e) => e.fmt(f),
      Expr::Block(e) => e.fmt(f),
      Expr::Lambda(e) => e.fmt(f),
      Expr::LetVar(e) => e.fmt(f),
      Expr::Ensuring(e) => e.fmt(f),
      Expr::MatchExpr(e) => e.fmt(f),
      Expr::NoTree(e) => e.fmt(f),
      _ => unimplemented!("No formatter for {:?}", self),
    }
  }
}

impl<'a> fmt::Display for Type<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Type::UnitType(t) => t.fmt(f),
      Type::BooleanType(t) => t.fmt(f),
      Type::IntegerType(t) => t.fmt(f),
      Type::BVType(t) => t.fmt(f),
      Type::TupleType(t) => t.fmt(f),
      Type::FunctionType(t) => t.fmt(f),
      Type::AnyType(t) => t.fmt(f),
      Type::ADTType(t) => t.fmt(f),
      _ => unimplemented!("No formatter for {:?}", self),
    }
  }
}

impl<'a> fmt::Display for Pattern<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Pattern::ADTPattern(t) => t.fmt(f),
      Pattern::WildcardPattern(t) => t.fmt(f),
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

impl fmt::Display for UnitType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "()")
  }
}

impl fmt::Display for BooleanType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "bool")
  }
}

impl fmt::Display for IntegerType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "BigInt")
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

impl<'a> fmt::Display for FunctionType<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(")?;
    writeSeq!(f, self.from, ", ")?;
    write!(f, ") -> ")?;
    write!(f, "{}", self.to)
  }
}

impl fmt::Display for AnyType {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Any")
  }
}

impl<'a> fmt::Display for ADTType<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.id)
  }
}


impl<'a> fmt::Display for Lambda<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "|")?;
    writeSeq!(f, self.params, ", ")?;
    write!(f, "| ")?;
    write!(f, "{}", self.body)
  }
}

impl<'a> fmt::Display for Ensuring<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.body)
  }
}

impl<'a> fmt::Display for Variable<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.id)
  }
}

impl fmt::Display for UnitLiteral {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "()")
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

impl fmt::Display for IntegerLiteral {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
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

impl<'a> fmt::Display for UMinus<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(-{})", self.expr)
  }
}

impl<'a> fmt::Display for BVNot<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(~{})", self.e)
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

impl<'a> fmt::Display for NoTree<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "???")
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

impl<'a> fmt::Display for MatchExpr<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "match {} {{", self.scrutinee)?;
    for case in &self.cases {
      writeln!(f, "    {}", case)?;
    }
    write!(f, "}}")
  }
}

impl<'a> fmt::Display for MatchCase<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} => {}", self.pattern, self.rhs)
  }
}

impl<'a> fmt::Display for Let<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "let {} = {};\n    {}", self.vd, self.value, self.body)
  }
}

impl<'a> fmt::Display for LetVar<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "let mut {} = {};\n    {}", self.vd, self.value, self.body)
  }
}

impl<'a> fmt::Display for Block<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "{{\n")?;
    for expr in &self.exprs {
      writeln!(f, "    {};", expr)?;
    }
    writeln!(f, "    {}", self.last)?;
    write!(f, "}}")
  }
}

impl<'a> fmt::Display for ADTPattern<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(b) = self.binder {
      write!(f, "{} @ ", b)?;
    }
    write!(f, "{}", self.id)?;
    if !self.subPatterns.is_empty() {
      write!(f, "(")?;
      writeSeq!(f, self.subPatterns, ", ")?;
      write!(f, ")")?;
    }
    Ok(())
  }
}

impl<'a> fmt::Display for WildcardPattern<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(b) = self.binder {
      write!(f, "{}", b.v.id)
    } else {
      write!(f, "_")
    }
  }
}
