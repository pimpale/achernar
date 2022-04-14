use std::alloc::Allocator;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

use crate::hir::PlaceExprOpKind;

use super::hir;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UseKind {
  Take,
  Ref,
  UniqRef,
}

fn exists(env: &Vec<&[u8]>, name: &[u8]) -> bool {
  for var in env.iter().rev() {
    if name == *var {
      return true;
    }
  }
  return false;
}

fn add_entry<'ast>(env: &mut HashMap<&'ast [u8], UseKind>, var: &'ast [u8], varuse: UseKind) {
  match env.entry(var) {
    Entry::Vacant(ve) => {
      ve.insert(varuse);
    }
    Entry::Occupied(mut oe) => match varuse {
      UseKind::Take => {
        oe.insert(varuse);
      }
      UseKind::UniqRef => {
        if oe.get() == &UseKind::Ref {
          oe.insert(varuse);
        }
      }
      UseKind::Ref => (),
    },
  }
}

// Source is hir, assumed to be valueExpr
// We will return the list of free variables
// Free variables are any variables used by this expression that are not defined within it
// Free variables are ordered by their order of appearance
pub fn find_free_vars<'hir, 'ast, HA: Allocator + Clone>(
  source: &'hir hir::ValExpr<'hir, 'ast, HA>,
) -> HashMap<&'ast [u8], UseKind> {
  let mut free_vars = HashMap::new();
  let mut bound_vars = vec![];
  parse_val_expr(source, &mut bound_vars, &mut free_vars);
  return free_vars;
}

fn parse_val_expr<'hir, 'ast, HA: Allocator + Clone>(
  source: &'hir hir::ValExpr<'hir, 'ast, HA>,
  bound_vars: &mut Vec<&'ast [u8]>,
  free_vars: &mut HashMap<&'ast [u8], UseKind>,
) {
  match &source.kind {
    hir::ValExprKind::Error => (),
    hir::ValExprKind::Loop(body) => parse_val_expr(body, bound_vars, free_vars),
    hir::ValExprKind::App { fun, arg } => {
      parse_val_expr(fun, bound_vars, free_vars);
      parse_val_expr(arg, bound_vars, free_vars);
    }
    hir::ValExprKind::Struct(fields) => {
      for (_, (_, ref field_initializer)) in fields {
        parse_val_expr(field_initializer, bound_vars, free_vars);
      }
    }
    hir::ValExprKind::Use(place) => {
      if let Some((var, varuse)) = parse_place_expr(place, bound_vars) {
        add_entry(free_vars, var, varuse);
      }
    }
    hir::ValExprKind::Unit => (),
    hir::ValExprKind::Annotate { val_expr, ty_expr } => {
      parse_val_expr(val_expr, bound_vars, free_vars);
      parse_val_expr(ty_expr, bound_vars, free_vars);
    }
    hir::ValExprKind::CaseOf {
      expr,
      ref case_options,
      ..
    } => {
      // parse scrutinee
      if let Some((var, varuse)) = parse_place_expr(expr, bound_vars) {
        add_entry(free_vars, var, varuse);
      }
      // parse each branch of the case
      let original_len = bound_vars.len();
      for (case_pat, case_body) in case_options {
        let new_bound_vars = parse_refutable_expr(case_pat, bound_vars, free_vars);
        bound_vars.extend(new_bound_vars);
        parse_val_expr(case_body, bound_vars, free_vars);
        bound_vars.truncate(original_len);
      }
    }
    hir::ValExprKind::Bool(_) => (),
    hir::ValExprKind::Char(_) => (),
    hir::ValExprKind::Int(_) => (),
    hir::ValExprKind::Float(_) => (),
    hir::ValExprKind::StructTy(ref fields) => {
      for (_, (_, field_initializer)) in fields {
        parse_val_expr(field_initializer, bound_vars, free_vars);
      }
    }
    hir::ValExprKind::EnumTy(ref fields) => {
      for (_, (_, field_initializer)) in fields {
        parse_val_expr(field_initializer, bound_vars, free_vars);
      }
    }
    hir::ValExprKind::Pair { fst, snd } => {
      parse_val_expr(fst, bound_vars, free_vars);
      parse_val_expr(snd, bound_vars, free_vars);
    }
    hir::ValExprKind::Lam { arg, body } => {
      let original_len = bound_vars.len();
      let new_bound_vars = parse_irrefutable_expr(arg, bound_vars);
      bound_vars.extend(new_bound_vars);
      parse_val_expr(body, bound_vars, free_vars);
      bound_vars.truncate(original_len);
    }
    hir::ValExprKind::LamTy {
      arg_ty,
      body_dep_ty,
    } => {
      let original_len = bound_vars.len();
      let new_bound_vars = parse_refutable_expr(arg_ty, bound_vars, free_vars);
      bound_vars.extend(new_bound_vars);
      parse_val_expr(body_dep_ty, bound_vars, free_vars);
      bound_vars.truncate(original_len);
    }
    hir::ValExprKind::Sequence { fst, snd } => {
      parse_val_expr(fst, bound_vars, free_vars);
      parse_val_expr(snd, bound_vars, free_vars);
    }
    hir::ValExprKind::LetIn { pat, val, body } => {
      parse_val_expr(val, bound_vars, free_vars);
      let original_len = bound_vars.len();
      let new_bound_vars = parse_irrefutable_expr(pat, bound_vars);
      bound_vars.extend(new_bound_vars);
      parse_val_expr(body, bound_vars, free_vars);
      bound_vars.truncate(original_len);
    }
    hir::ValExprKind::Assign { val, .. } => parse_val_expr(val, bound_vars, free_vars),
  }
}

fn parse_refutable_expr<'hir, 'ast, HA: Allocator + Clone>(
  source: &'hir hir::RefutablePatExpr<'hir, 'ast, HA>,
  bound_vars: &mut Vec<&'ast [u8]>,
  free_vars: &mut HashMap<&'ast [u8], UseKind>,
) -> Vec<&'ast [u8]> {
  match source.kind {
    hir::RefutablePatExprKind::Error => vec![],
    hir::RefutablePatExprKind::IrrefutablePat(pat) => parse_irrefutable_expr(pat, bound_vars),
    hir::RefutablePatExprKind::ValPat(pat) => {
      parse_valpat_expr(pat, bound_vars, free_vars);
      vec![]
    }
    hir::RefutablePatExprKind::Pair { fst, snd } => {
      let fst_vars = parse_refutable_expr(fst, bound_vars, free_vars);
      let snd_vars = parse_refutable_expr(snd, bound_vars, free_vars);
      let mut ret = vec![];
      ret.extend(fst_vars);
      ret.extend(snd_vars);
      ret
    }
    hir::RefutablePatExprKind::Struct(ref fields) => {
      let mut ret = vec![];
      for (_, (_, field_pat)) in fields {
        ret.extend(parse_refutable_expr(field_pat, bound_vars, free_vars));
      }
      ret
    }
    hir::RefutablePatExprKind::And { fst, snd } => {
      let fst_vars = parse_irrefutable_expr(fst, bound_vars);
      parse_valpat_expr(snd, bound_vars, free_vars);
      fst_vars
    }
  }
}

fn parse_irrefutable_expr<'hir, 'ast, HA: Allocator + Clone>(
  source: &'hir hir::IrrefutablePatExpr<'hir, 'ast, HA>,
  bound_vars: &mut Vec<&'ast [u8]>,
) -> Vec<&'ast [u8]> {
  match source.kind {
    hir::IrrefutablePatExprKind::Error => vec![],
    hir::IrrefutablePatExprKind::Unit => vec![],
    hir::IrrefutablePatExprKind::BindVariable(var) => vec![var],
    hir::IrrefutablePatExprKind::Pair { fst, snd } => {
      let fst_vars = parse_irrefutable_expr(fst, bound_vars);
      let snd_vars = parse_irrefutable_expr(snd, bound_vars);
      let mut ret = vec![];
      ret.extend(fst_vars);
      ret.extend(snd_vars);
      ret
    }
    hir::IrrefutablePatExprKind::Struct(ref fields) => {
      let mut ret = vec![];
      for (_, (_, field_pat)) in fields {
        ret.extend(parse_irrefutable_expr(field_pat, bound_vars));
      }
      ret
    }
  }
}

fn parse_valpat_expr<'hir, 'ast, HA: Allocator + Clone>(
  source: &'hir hir::ValPatExpr<'hir, 'ast, HA>,
  bound_vars: &mut Vec<&'ast [u8]>,
  free_vars: &mut HashMap<&'ast [u8], UseKind>,
) {
  match source.kind {
    hir::ValPatExprKind::Error => (),
    hir::ValPatExprKind::Ignore => (),
    hir::ValPatExprKind::Range { fst, snd, .. } => {
      parse_val_expr(fst, bound_vars, free_vars);
      parse_val_expr(snd, bound_vars, free_vars);
    }
    hir::ValPatExprKind::Constructor { fun, arg } => {
      parse_val_expr(fun, bound_vars, free_vars);
      parse_valpat_expr(arg, bound_vars, free_vars);
    }
    hir::ValPatExprKind::Pair { fst, snd } => {
      parse_valpat_expr(fst, bound_vars, free_vars);
      parse_valpat_expr(snd, bound_vars, free_vars);
    }
    hir::ValPatExprKind::Struct(ref fields) => {
      for (_, (_, field_pat)) in fields {
        parse_valpat_expr(field_pat, bound_vars, free_vars);
      }
    }
    hir::ValPatExprKind::And { fst, snd } => {
      parse_valpat_expr(fst, bound_vars, free_vars);
      parse_valpat_expr(snd, bound_vars, free_vars);
    }
    hir::ValPatExprKind::Or { fst, snd } => {
      parse_valpat_expr(fst, bound_vars, free_vars);
      parse_valpat_expr(snd, bound_vars, free_vars);
    }
    hir::ValPatExprKind::Value(val) => parse_val_expr(val, bound_vars, free_vars),
  }
}

fn parse_place_expr<'hir, 'ast>(
  source: &'hir hir::PlaceExpr<'hir, 'ast>,
  bound_vars: &mut Vec<&'ast [u8]>,
) -> Option<(&'ast [u8], UseKind)> {
  let mut parts = vec![];

  let mut current_root = source;

  let var_name = loop {
    match &current_root.kind {
      hir::PlaceExprKind::Error => return None,
      hir::PlaceExprKind::StructField { root, field, .. } => {
        current_root = root;
      }
      hir::PlaceExprKind::Op(root, kind) => {
        parts.push(kind);
        current_root = root;
      }
      // if its a bound variable, we don't need to do anything
      // its a free variable, we need to calculate its use level
      hir::PlaceExprKind::Var(var_name) => {
        if exists(bound_vars, var_name) {
          return None;
        } else {
          break var_name;
        }
      }
      // no need to do any calculation, builtins are not variables
      hir::PlaceExprKind::Builtin(root) => return None,
    }
  };

  // TODO

  let mut uniq = false;
  let mut layers_of_indirection = 0;
  for x in parts.into_iter().rev() {
    match x {
      PlaceExprOpKind::Deref => {
        layers_of_indirection += -1;
        uniq = false;
      }
      PlaceExprOpKind::Ref => {
        layers_of_indirection += 1;
      }
      PlaceExprOpKind::UniqRef => {
        layers_of_indirection += 1;
        uniq = true;
      }
    }
    if layers_of_indirection < 0 {
      return Some((var_name, UseKind::Take));
    }
  }

  if layers_of_indirection == 0 {
    return Some((var_name, UseKind::Take));
  } else if uniq {
    return Some((var_name, UseKind::UniqRef));
  } else {
    return Some((var_name, UseKind::Ref));
  }
}
