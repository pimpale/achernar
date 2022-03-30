use std::alloc::Allocator;

use super::ast;
use super::hir;

fn exists(
  env: &Vec<Vec<&[u8]>>,
  name: &[u8],
) -> bool {
  for level in env.iter().rev() {
    for var in level.iter().rev() {
      if &name == var {
        return true;
      }
    }
  }
  return false;
}

// Source is hir, assumed to be valueExpr
// We will return the list of free variables
// Free variables are any variables used by this expression that are not defined within it
// Free variables are ordered by their order of appearance
pub fn find_free_vars<'hir, 'ast, HA: Allocator + Clone>(
    source: &'hir hir::ValExpr<'hir, 'ast, HA>,
) -> Vec<(&'ast Vec<u8>, hir::UseKind)> {
  let vars = vec![];
  parse_val_expr(source, vars)
}

fn parse_val_expr<'hir, 'ast, HA: Allocator + Clone>(
    source: &'hir hir::ValExpr<'hir, 'ast, HA>,
    vars: &mut Vec<Vec<&[u8]>>,
) -> Vec<(&'ast Vec<u8>, hir::UseKind)> {
  match source.kind {
    hir::ValExprKind::Error => todo!(),
    hir::ValExprKind::Loop(_) => todo!(),
    hir::ValExprKind::App { fun, arg } => todo!(),
    hir::ValExprKind::Struct(_) => todo!(),
    hir::ValExprKind::Use(_, _) => todo!(),
    hir::ValExprKind::Annotate { val_expr, ty_expr } => todo!(),
    hir::ValExprKind::CaseOf { expr, case_options, source } => todo!(),
    hir::ValExprKind::Universe(_) => todo!(),
    hir::ValExprKind::Nil => todo!(),
    hir::ValExprKind::Bool(_) => todo!(),
    hir::ValExprKind::Char(_) => todo!(),
    hir::ValExprKind::Int(_) => todo!(),
    hir::ValExprKind::Float(_) => todo!(),
    hir::ValExprKind::StructTy(_) => todo!(),
    hir::ValExprKind::EnumTy(_) => todo!(),
    hir::ValExprKind::Pair { fst, snd } => todo!(),
    hir::ValExprKind::Lam { arg, body } => todo!(),
    hir::ValExprKind::LamTy { arg_ty, body_dep_ty, use_kind } => todo!(),
    hir::ValExprKind::Sequence { fst, snd } => todo!(),
    hir::ValExprKind::LetIn { pat, val, body } => todo!(),
}
}
