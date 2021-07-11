use super::dlogger::DiagnosticLogger;
use super::hir;
use bumpalo::Bump;
use std::alloc::Allocator;
use hashbrown::HashMap;
use num_bigint::BigInt;
use num_rational::BigRational;

use super::mir;

pub fn eval<'ast, 'hir, 'mir, HirAllocator: Allocator>(
  hir: &'mir mir::Expr<'hir, 'ast, HirAllocator>,
  allocator: &'mir Bump,
  mut dlogger: DiagnosticLogger,
) {
  
}

pub fn construct_mir_pat<'ast, 'hir, 'mir, HirAllocator: Allocator>(
  hir: &'hir hir::Pat<'hir, 'ast, HirAllocator>,
  allocator: &'mir Bump,
  mut dlogger: DiagnosticLogger,
) {
  
}


pub fn construct_mir<'ast, 'hir, 'mir, HirAllocator: Allocator>(
  hir: &'hir hir::Expr<'hir, 'ast, HirAllocator>,
  allocator: &'mir Bump,
  mut dlogger: DiagnosticLogger,
) {
  
}
