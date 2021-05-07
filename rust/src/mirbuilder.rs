use super::dlogger::DiagnosticLogger;
use std::alloc::Allocator;
use super::hir;
use bumpalo::Bump;
use std::collections::HashMap;

struct Context<'hir> {
    vars: HashMap<&'hir Vec<u8>,
}









pub fn construct_mir<'ast, 'hir, 'mir, HirAllocator:Allocator>(
  hir: &'hir hir::Expr<'hir, 'ast, HirAllocator>,
  allocator: &'mir Bump,
  mut dlogger: DiagnosticLogger,
) {

}
