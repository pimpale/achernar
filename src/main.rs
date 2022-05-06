#![feature(allocator_api)]
mod ast;
mod astbuilder;
mod codereader;
mod dlogger;
mod find_free_vars;
mod hir;
mod hirbuilder;
mod mir;
mod mirbuilder;
mod token;
mod tokenize;
mod utils;

use bumpalo::Bump;
use std::io::stdin;
use std::io::Read;

use astbuilder::construct_ast;
use dlogger::DiagnosticLog;
use hirbuilder::construct_hir;
use tokenize::tokenize;

fn main() {
  let mut log = DiagnosticLog::new();
  let charstream = stdin().bytes().map_while(|x| x.ok());
  let tokenstream = tokenize(charstream, log.get_logger(Some(String::from("acnc-lex"))));
  let ast = construct_ast(tokenstream, log.get_logger(Some(String::from("acnc-ast"))));

  let allocator = Bump::new();
  let hir = construct_hir(
    &ast,
    &allocator,
    log.get_logger(Some(String::from("acnc-hir"))),
  );

  dbg!(hir);


  // let thir = construct_thir(
  //   &hir,
  //   &allocator,
  //   log.get_logger(Some(String::from("acnc-thir"))),
  // );

  // dbg!(thir);
}
