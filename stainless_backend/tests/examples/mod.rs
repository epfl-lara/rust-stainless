use std::io::Write;
use tempfile::NamedTempFile;

use stainless_data::ast::*;
use stainless_data::ser::*;
use types::*;

macro_rules! test_gen {
  ($name:ident, $s:ident => $body:block) => {
    pub fn $name() -> NamedTempFile {
      let mut file = NamedTempFile::new().expect("Unable to create temporary example file");
      let mut s = BufferSerializer::new();
      let $s = &mut s;
      $body
      file.write_all(s.as_slice()).expect("Unable to write example file");
      file
    }
  };
}

macro_rules! ser {
  ($v:expr, $s:ident) => {
    $v.serialize($s).unwrap()
  };
}

fn make_sym<'a, N: Into<String>>(f: &'a Factory, gid: i32, name: N) -> &'a SymbolIdentifier<'a> {
  let name = name.into();
  let id = f.Identifier(name.clone(), gid, 1);
  f.SymbolIdentifier(id, vec![name])
}

fn make_ensuring<'a, F: FnOnce(&'a Variable<'a>) -> Expr<'a>>(
  f: &'a Factory,
  body: Expr<'a>,
  make_cond: F,
) -> Expr<'a> {
  let id = make_sym(f, 999, "res");
  let v_res = f.Variable(id, f.Int32Type().into(), vec![]);
  f.Ensuring(body, f.Lambda(vec![f.ValDef(v_res)], make_cond(v_res)))
    .into()
}

fn make_identity_fundef<'a>(f: &'a Factory) -> &'a FunDef<'a> {
  let id_x = make_sym(f, 1, "x");
  let id_f = make_sym(f, 2, "f");
  let tpe_int: Type = f.Int32Type().into();
  let v_x: &'a _ = f.Variable(id_x, tpe_int, vec![]);
  let param: &'a _ = f.ValDef(v_x);
  let body: Expr = v_x.into();
  let body = make_ensuring(f, body, |v_res| f.Equals(v_res.into(), v_x.into()).into());
  f.FunDef(id_f, vec![], vec![&param], tpe_int, body, vec![])
}

test_gen!(identity_symbols, s => {
  let f = Factory::new();
  let fd = make_identity_fundef(&f);
  let mut functions = Map::new();
  functions.insert(fd.id, fd);
  let symbols = Symbols {
    sorts: Map::new(),
    functions
  };
  ser!(symbols, s)
});
