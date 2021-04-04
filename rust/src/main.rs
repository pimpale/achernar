#![feature(or_patterns)]
#![feature(iter_map_while)]
#![feature(destructuring_assignment)]
mod ast;
mod astbuilder;
mod codereader;
mod dlogger;
mod hir;
mod token;
mod tokenize;
mod hirbuilder;

use std::io::stdin;
use std::io::Read;

use dlogger::DiagnosticLog;
use tokenize::tokenize;
use astbuilder::construct_ast;
use hirbuilder::construct_hir;

fn main() {
  let mut log = DiagnosticLog::new();
  let charstream = stdin().bytes().map_while(|x| x.ok());
  let tokenstream = tokenize(charstream, log.get_logger(Some(String::from("acnc-lex"))));
  let ast = construct_ast(
    tokenstream,
    log.get_logger(Some(String::from("acnc-construct-ast"))),
  );

  let hir = construct_hir(
    ast,
    log.get_logger(Some(String::from("acnc-construct-hir"))),
  );
  dbg!(ast);
}
