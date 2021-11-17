use super::ast;
use super::hir;

use std::collections::HashMap;
use std::collections::hash_map::Entry;

// Returns a list of free vars present in source
pub fn find_free_vars<'hir, 'ast>(
    source: &[&'ast ast::Expr]
) -> HashMap<&'ast [u8], (&'ast ast::Expr, hir::UseKind)> {
 todo!();
}
