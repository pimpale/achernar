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

use std::io::stdin;
use std::io::Read;

use astbuilder::construct_ast;
use dlogger::DiagnosticLog;
use tokenize::tokenize;

fn main() {
  let mut log = DiagnosticLog::new();
  let charstream = stdin().bytes().map_while(|x| x.ok());
  let tokenstream = tokenize(charstream, log.get_logger(Some(String::from("acnc-lex"))));
  let ast = construct_ast(
    tokenstream,
    log.get_logger(Some(String::from("acnc-parse"))),
  );
  dbg!(ast);
}
