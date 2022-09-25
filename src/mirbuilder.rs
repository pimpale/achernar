use super::dlogger::DiagnosticLogger;
use super::hir;
use std::alloc::Allocator;

use super::mir;

pub fn construct_mir_fn<'ast, 'hir, HirAllocator: Clone + Allocator>(
  hir: &'hir hir::ValExpr<'hir, 'ast, HirAllocator>,
) -> mir::MirFunc<'ast, 'hir, HirAllocator> {
    todo!();
}

pub fn construct_mir_module<'ast, 'hir, HirAllocator: Clone + Allocator>(
  hir: &'hir hir::ValExpr<'hir, 'ast, HirAllocator>,
  mut dlogger: DiagnosticLogger,
) -> mir::BasicBlock<'ast, 'hir, HirAllocator> {
  todo!();
}
