use super::dlogger::DiagnosticLogger;
use super::hir;
use bumpalo::Bump;
use hashbrown::HashMap;
use num_bigint::BigInt;
use num_rational::BigRational;
use std::alloc::Allocator;

use super::mir;

pub fn construct_mir<'ast, 'hir, 'mir, HirAllocator: Clone + Allocator>(
  hir: &'hir hir::ValExpr<'hir, 'ast, HirAllocator>,
  allocator: &'mir Bump,
  mut dlogger: DiagnosticLogger,
) -> mir::BasicBlock<'ast, 'mir, 'hir, &'mir Bump, HirAllocator> {
    todo!();
}
