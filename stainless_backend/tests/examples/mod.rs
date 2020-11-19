use stainless_data::ast::*;
use stainless_data::ser::*;
use types::*;

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

pub fn identity_symbols<'a>(f: &'a Factory) -> Symbols<'a> {
  let fd = make_identity_fundef(&f);
  let mut functions = Map::new();
  functions.insert(fd.id, fd);
  Symbols {
    sorts: Map::new(),
    classes: Map::new(),
    functions,
  }
}
