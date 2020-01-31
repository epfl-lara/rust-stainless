extern crate rustc;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate syntax;

use rustc::ty::TyCtxt;
use syntax::ast;

fn dump_mod<'tcx>(_tcx: TyCtxt<'tcx>, mod_name: String, modd: &ast::Mod) -> () {
  use ast::ItemKind::*;

  println!("Module {} w/ {:?} items", &mod_name, modd.items.len());
  for item in &modd.items {
    println!("item: #{:?}  kind: {:?}\n", item.id, item.kind);
    match &item.kind {
      Mod(modd) => println!("MOD: {:?}", modd),
      _ => println!("OTHER"),
    };
  };
}

pub fn playground<'tcx>(tcx: TyCtxt<'tcx>, crate_name: String, krate: &ast::Crate) -> () {
  dump_mod(tcx, crate_name, &krate.module);
}
