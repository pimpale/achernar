#![feature(or_patterns)]
#![feature(iter_map_while)]
mod ast;
mod codereader;
mod dlogger;
mod hir;
mod token;
mod tokenize;
mod astbuilder;

use std::io::stdin;
use std::io::Read;

use dlogger::DiagnosticLog;
use tokenize::tokenize;
use astbuilder::construct_ast;

pub static COMPILER_NAME: &str = "acnc";

fn main() {
  let mut log = DiagnosticLog::new();
  let charstream = stdin().bytes().map_while(|x| x.ok());
  let tokenstream = tokenize(charstream, log.get_logger()) ;
  let ast = construct_ast(tokenstream, log.get_logger());
}
