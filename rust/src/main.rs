#![feature(or_patterns)]
#![feature(iter_map_while)]
mod ast;
mod codereader;
mod dlogger;
mod hir;
mod token;
mod tokenize;

use std::io::stdin;
use std::io::Read;

use dlogger::DiagnosticLog;
use tokenize::tokenize;

pub static COMPILER_NAME: &str = "acnc";

fn main() {
  let mut log = DiagnosticLog::new();

  for t in tokenize(stdin().bytes().map_while(|x| x.ok()), log.getLogger()) {
      dbg!(t);
  }
}
