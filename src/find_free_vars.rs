use super::ast;
use super::hir;

use std::collections::hash_map::Entry;
use std::collections::HashMap;

// Source is a list of AST nodes.
// We will return the list of free variables
// Free variables are any variables used by this expression that are not defined within it
// Free variables are ordered by their order of appearance
pub fn find_free_vars<'hir, 'ast>(
  source: &[&'ast ast::Expr],
) -> Vec<(&'ast [u8], hir::UseKind)> {
  todo!();
}
