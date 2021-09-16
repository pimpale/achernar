use super::dlogger::DiagnosticLogger;
use super::hir;
use bumpalo::Bump;
use hashbrown::HashMap;
use num_bigint::BigInt;
use num_rational::BigRational;
use std::alloc::Allocator;

use super::mir;

struct Environment<'ast, 'hir, 'mir, A: Allocator, HA: Allocator> {
  types: Environment<Vec<u8, A>, mir::Ty<'ast, 'hir, 'mir, A, HA>>,
}

pub fn eval<'ast, 'hir, 'mir, A: Allocator, HA: Allocator>(
  hir: &'mir mir::Expr<'ast, 'hir, 'mir, A, HA>,
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
) -> BasicBlock{
  match (hir) {
    None => No
  }
}
