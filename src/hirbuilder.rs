use super::ast;
use super::dlogger::DiagnosticLogger;
use super::find_free_vars::find_free_vars;
use super::hir;
use super::utils::new_vec_from;
use bumpalo::Bump;
use num_bigint::BigUint;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

struct VarScope<'ast> {
  declaration: &'ast ast::Expr,
}

fn lookup_maybe<'env, Scope>(
  env: &'env Vec<(&[u8], Scope)>,
  label: &[u8],
) -> Option<(&'env Scope, usize)> {
  let mut count: usize = 1;
  for (scopelabel, scope) in env.iter().rev() {
    if &label == scopelabel {
      return Some((scope, count));
    } else {
      count += 1;
    }
  }
  return None;
}

fn lookup<'env, Scope>(env: &'env Vec<(&[u8], Scope)>, label: &[u8]) -> &'env Scope {
  lookup_maybe(env, label).unwrap().0
}

// foo
fn lookup_count_up<'env, Scope>(env: &'env Vec<(&[u8], Scope)>, label: &[u8]) -> usize {
  lookup_maybe(env, label).unwrap().1
}

fn update<'env, Scope, F>(env: &'env mut Vec<(&[u8], Scope)>, label: &[u8], update: F) -> bool
where
  F: FnOnce(&'env mut Scope),
{
  for (scopelabel, scope) in env.iter_mut().rev() {
    if &label == scopelabel {
      update(scope);
      return true;
    }
  }
  return false;
}

fn next_id(idgen: &RefCell<u64>) -> u64 {
  idgen.replace_with(|x| *x + 1)
}

fn gen_app<'hir, 'ast>(
  idgen: &RefCell<u64>,
  ha: &'hir Bump,
  source: &'ast ast::Expr,
  fun: hir::ValExpr<'hir, 'ast, &'hir Bump>,
  args: Vec<&'hir hir::ValExpr<'hir, 'ast, &'hir Bump>>,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  args.iter().fold(fun, |acc, x| hir::ValExpr {
    source,
    id: Some(next_id(idgen)),
    kind: hir::ValExprKind::App {
      fun: ha.alloc(acc),
      arg: x,
    },
  })
}

fn gen_bool<'hir, 'ast>(
  idgen: &RefCell<u64>,
  source: &'ast ast::Expr,
  b: bool,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  hir::ValExpr {
    source: source,
    id: Some(next_id(idgen)),
    kind: hir::ValExprKind::Bool(b),
  }
}

fn gen_nil<'hir, 'ast>(
  idgen: &RefCell<u64>,
  source: &'ast ast::Expr,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  hir::ValExpr {
    source: source,
    id: Some(next_id(idgen)),
    kind: hir::ValExprKind::Nil,
  }
}

fn gen_nil_ty<'hir, 'ast>(
  idgen: &RefCell<u64>,
  source: &'ast ast::Expr,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  hir::ValExpr {
    source: source,
    id: Some(next_id(idgen)),
    kind: hir::ValExprKind::NilTy,
  }
}

// Generates a place referencing a var
// It can handle if the var is captured
fn gen_place_from_identifier<'hir, 'ast>(
  source: &'ast ast::Expr,
  dlogger: &mut DiagnosticLogger,
  identifier: &[u8],
  var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
  captured_var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
) -> hir::PlaceExpr<'hir, 'ast, &'hir Bump> {
  let lkup_val = lookup_maybe(var_env, &identifier);

  if let Some((_, debruijn_index)) = lkup_val {
    hir::PlaceExpr {
      source,
      kind: hir::PlaceExprKind::Var(debruijn_index),
    }
  } else {
    dlogger.log_variable_not_found(source.range, &identifier);
    // return error if not exist
    hir::PlaceExpr {
      source,
      kind: hir::PlaceExprKind::Error,
    }
  }
}

// Generates an expression using the variable identifier and the use kind from an identifier
fn gen_valexpr_identifier_use<'hir, 'ast>(
  idgen: &RefCell<u64>,
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  identifier: &[u8],
  use_kind: hir::UseKind,
  var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
  captured_var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  hir::ValExpr {
    source,
    id: Some(next_id(idgen)),
    kind: hir::ValExprKind::Use(
      ha.alloc(gen_place_from_identifier(
        source,
        dlogger,
        identifier,
        var_env,
        captured_var_env,
      )),
      use_kind,
    ),
  }
}

// Generates an expression using the place and the use kind from a place expression
fn gen_valexpr_place_use<'hir, 'ast>(
  idgen: &RefCell<u64>,
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  use_kind: hir::UseKind,
  var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
  captured_var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
}

fn parse_struct_fields<'hir, 'ast>(
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
) -> HashMap<&'ast [u8], &'ast ast::Expr> {
  // this is what we return:
  // a hashmap between identifier and whatever pattern
  // create a struct of literals
  let mut patterns = HashMap::new();

  // depth first search of binary tree
  let mut sequences = vec![source];

  while let Some(current) = sequences.pop() {
    match current.kind {
      ast::ExprKind::BinaryOp {
        ref left_operand,
        ref right_operand,
        ref op,
      } => match op {
        ast::BinaryOpKind::Sequence => {
          sequences.push(left_operand);
          sequences.push(right_operand);
        }
        ast::BinaryOpKind::Assign => match left_operand.as_ref() {
          // match field
          ast::Expr {
            kind: ast::ExprKind::Bind(ref identifier),
            range,
            ..
          } => match patterns.entry(identifier.as_slice()) {
            Entry::Vacant(ve) => {
              ve.insert(&**right_operand);
            }
            Entry::Occupied(oe) => {
              // identifier not unique, log error for using duplicate identifier in struct
              dlogger.log_duplicate_field_name(*range, &oe.key(), oe.get().range);
            }
          },
          // error handle
          ast::Expr {
            range, ref kind, ..
          } => {
            // means that there was no single bind as a target of the assign
            dlogger.log_unsupported_target_in_struct_assign(*range, kind);
          }
        },
        _ => {
          dlogger.log_unexpected_binop_in_struct(current.range, op);
        }
      },
      ref kind => {
        dlogger.log_unexpected_element_in_struct(current.range, kind);
      }
    }
  }
  patterns
}

fn tr_irrefutable_pat_expr<'hir, 'ast>(
  idgen: &RefCell<u64>,
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  var_env: &mut Vec<(&'ast [u8], VarScope<'ast>)>,
  captured_var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
) -> (
  hir::IrrefutablePatExpr<'hir, 'ast, &'hir Bump>,
  Vec<(&'ast [u8], VarScope<'ast>)>,
) {
  match source.kind {
    // propagate error
    ast::ExprKind::Error => (
      hir::IrrefutablePatExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::IrrefutablePatExprKind::Error,
      },
      vec![],
    ),
    // as long as it typechecks, nil will always match
    ast::ExprKind::Nil => (
      hir::IrrefutablePatExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::IrrefutablePatExprKind::Nil,
      },
      vec![],
    ),
    // elide groups
    ast::ExprKind::Group(ref body) => {
      tr_irrefutable_pat_expr(idgen, ha, dlogger, body, var_env, captured_var_env)
    }
    ast::ExprKind::StructLiteral(ref body) => {
      let patterns = parse_struct_fields(ha, dlogger, source);

      let mut bindings = vec![];
      let mut fields = Vec::new_in(ha);

      for (id, pat_src) in patterns.into_iter() {
        let (field_pat, field_bindings) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, pat_src, var_env, captured_var_env);
        fields.push((id, (pat_src, field_pat)));
        bindings.extend(field_bindings);
      }

      (
        hir::IrrefutablePatExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::IrrefutablePatExprKind::Struct(fields),
        },
        bindings,
      )
    }
    ast::ExprKind::Bind(ref identifier) => (
      hir::IrrefutablePatExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::IrrefutablePatExprKind::BindVariable,
      },
      vec![(
        identifier,
        VarScope {
          declaration: source,
        },
      )],
    ),
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::Apply => {
        let (arg_pat, arg_bindings) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, right_operand, var_env, captured_var_env);
        (
          hir::IrrefutablePatExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::IrrefutablePatExprKind::ActivePattern {
              fun: ha.alloc(tr_val_expr(
                idgen,
                ha,
                dlogger,
                left_operand,
                var_env,
                captured_var_env,
              )),
              arg: ha.alloc(arg_pat),
            },
          },
          arg_bindings,
        )
      }
      ast::BinaryOpKind::Cons => {
        let (fst_pat, fst_bindings) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env, captured_var_env);
        let (snd_pat, snd_bindings) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, right_operand, var_env, captured_var_env);

        (
          hir::IrrefutablePatExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::IrrefutablePatExprKind::Pair {
              fst: ha.alloc(fst_pat),
              snd: ha.alloc(snd_pat),
            },
          },
          // join them together
          fst_bindings
            .into_iter()
            .chain(snd_bindings.into_iter())
            .collect(),
        )
      }
      // Error
      c => {
        dlogger.log_unexpected_binop_in_irrefutable_pattern(source.range, c);
        (
          hir::IrrefutablePatExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::IrrefutablePatExprKind::Error,
          },
          vec![],
        )
      }
    },
    // pattern is an error
    ref c => {
      dlogger.log_unexpected_in_irrefutable_pattern(source.range, c);
      (
        hir::IrrefutablePatExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::IrrefutablePatExprKind::Error,
        },
        vec![],
      )
    }
  }
}

fn tr_refutable_pat_expr<'hir, 'ast>(
  idgen: &RefCell<u64>,
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  var_env: &mut Vec<(&'ast [u8], VarScope<'ast>)>,
  captured_var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
) -> (
  hir::RefutablePatExpr<'hir, 'ast, &'hir Bump>,
  Vec<(&'ast [u8], VarScope<'ast>)>,
) {
  match source.kind {
    // propagate error
    ast::ExprKind::Error => (
      hir::RefutablePatExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::RefutablePatExprKind::Error,
      },
      vec![],
    ),
    // send irrefutable patterns here
    ast::ExprKind::Nil | ast::ExprKind::Bind(_) => {
      let (pat, bindings) =
        tr_irrefutable_pat_expr(idgen, ha, dlogger, source, var_env, captured_var_env);
      (
        hir::RefutablePatExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::RefutablePatExprKind::IrrefutablePat(ha.alloc(pat)),
        },
        bindings,
      )
    }
    // send refutable patterns here
    ast::ExprKind::Bool(_)
    | ast::ExprKind::Int(_)
    | ast::ExprKind::Float(_)
    | ast::ExprKind::String { .. } => {
      let pat = tr_val_pat_expr(idgen, ha, dlogger, source, var_env, captured_var_env);
      (
        hir::RefutablePatExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::RefutablePatExprKind::ValPat(ha.alloc(pat)),
        },
        vec![],
      )
    }
    // elide groups
    ast::ExprKind::Group(ref body) => {
      tr_refutable_pat_expr(idgen, ha, dlogger, body, var_env, captured_var_env)
    }
    ast::ExprKind::StructLiteral(ref body) => {
      let patterns = parse_struct_fields(ha, dlogger, source);

      let mut bindings = vec![];
      let mut fields = Vec::new_in(ha);

      for (id, pat_src) in patterns.into_iter() {
        let (field_pat, field_bindings) =
          tr_refutable_pat_expr(idgen, ha, dlogger, pat_src, var_env, captured_var_env);
        fields.push((id, (pat_src, field_pat)));
        bindings.extend(field_bindings);
      }

      (
        hir::RefutablePatExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::RefutablePatExprKind::Struct(fields),
        },
        bindings,
      )
    }
    // handle refutable ops
    ast::ExprKind::Val(ref body) => (
      hir::RefutablePatExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::RefutablePatExprKind::ValPat(ha.alloc(tr_val_pat_expr(
          idgen,
          ha,
          dlogger,
          body,
          var_env,
          captured_var_env,
        ))),
      },
      vec![],
    ),
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      // handle refutable ops
      ast::BinaryOpKind::Range | ast::BinaryOpKind::RangeInclusive | ast::BinaryOpKind::Or => (
        hir::RefutablePatExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::RefutablePatExprKind::ValPat(ha.alloc(tr_val_pat_expr(
            idgen,
            ha,
            dlogger,
            source,
            var_env,
            captured_var_env,
          ))),
        },
        vec![],
      ),
      ast::BinaryOpKind::Apply => {
        let (arg_pat, arg_bindings) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, right_operand, var_env, captured_var_env);
        (
          hir::RefutablePatExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::RefutablePatExprKind::ActivePattern {
              fun: ha.alloc(tr_val_expr(
                idgen,
                ha,
                dlogger,
                left_operand,
                var_env,
                captured_var_env,
              )),
              arg: ha.alloc(arg_pat),
            },
          },
          arg_bindings,
        )
      }
      ast::BinaryOpKind::Cons => {
        let (fst_pat, fst_bindings) =
          tr_refutable_pat_expr(idgen, ha, dlogger, left_operand, var_env, captured_var_env);
        let (snd_pat, snd_bindings) =
          tr_refutable_pat_expr(idgen, ha, dlogger, right_operand, var_env, captured_var_env);

        (
          hir::RefutablePatExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::RefutablePatExprKind::Pair {
              fst: ha.alloc(fst_pat),
              snd: ha.alloc(snd_pat),
            },
          },
          // join them together
          fst_bindings
            .into_iter()
            .chain(snd_bindings.into_iter())
            .collect(),
        )
      }
      ast::BinaryOpKind::And => {
        let (fst_pat, fst_bindings) =
          tr_refutable_pat_expr(idgen, ha, dlogger, left_operand, var_env, captured_var_env);
        let (snd_pat, snd_bindings) =
          tr_refutable_pat_expr(idgen, ha, dlogger, right_operand, var_env, captured_var_env);
        (
          hir::RefutablePatExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::RefutablePatExprKind::And {
              fst: ha.alloc(fst_pat),
              snd: ha.alloc(snd_pat),
            },
          },
          // join them together
          fst_bindings
            .into_iter()
            .chain(snd_bindings.into_iter())
            .collect(),
        )
      }
      // Error
      c => {
        dlogger.log_unexpected_binop_in_refutable_pattern(source.range, c);
        (
          hir::RefutablePatExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::RefutablePatExprKind::Error,
          },
          vec![],
        )
      }
    },
    // pattern is an error
    ref c => {
      dlogger.log_unexpected_in_refutable_pattern(source.range, c);
      (
        hir::RefutablePatExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::RefutablePatExprKind::Error,
        },
        vec![],
      )
    }
  }
}

fn tr_val_pat_expr<'hir, 'ast>(
  idgen: &RefCell<u64>,
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  var_env: &mut Vec<(&'ast [u8], VarScope<'ast>)>,
  captured_var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
) -> hir::ValPatExpr<'hir, 'ast, &'hir Bump> {
  match source.kind {
    // propagate error
    ast::ExprKind::Error => hir::ValPatExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValPatExprKind::Error,
    },
    // match against values automatically
    ast::ExprKind::Nil
    | ast::ExprKind::Bool(_)
    | ast::ExprKind::Int(_)
    | ast::ExprKind::Float(_)
    | ast::ExprKind::String { .. } => hir::ValPatExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValPatExprKind::Value(ha.alloc(tr_val_expr(
        idgen,
        ha,
        dlogger,
        source,
        var_env,
        captured_var_env,
      ))),
    },
    // elide groups
    ast::ExprKind::Group(ref body) => {
      tr_val_pat_expr(idgen, ha, dlogger, body, var_env, captured_var_env)
    }
    ast::ExprKind::StructLiteral(ref body) => {
      let mut fields = Vec::new_in(ha);

      for (id, pat_src) in parse_struct_fields(ha, dlogger, source).into_iter() {
        let field_pat = tr_val_pat_expr(idgen, ha, dlogger, pat_src, var_env, captured_var_env);
        fields.push((id, (pat_src, field_pat)));
      }

      hir::ValPatExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValPatExprKind::Struct(fields),
      }
    }
    ast::ExprKind::Val(ref body) => hir::ValPatExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValPatExprKind::Value(ha.alloc(tr_val_expr(
        idgen,
        ha,
        dlogger,
        body,
        var_env,
        captured_var_env,
      ))),
    },
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::Range => hir::ValPatExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValPatExprKind::Range {
          fst: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          snd: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
          inclusive: false,
        },
      },
      ast::BinaryOpKind::RangeInclusive => hir::ValPatExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValPatExprKind::Range {
          fst: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          snd: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
          inclusive: true,
        },
      },
      ast::BinaryOpKind::Apply => hir::ValPatExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValPatExprKind::ActivePattern {
          fun: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          arg: ha.alloc(tr_val_pat_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        },
      },
      ast::BinaryOpKind::Cons => hir::ValPatExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValPatExprKind::Pair {
          fst: ha.alloc(tr_val_pat_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          snd: ha.alloc(tr_val_pat_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        },
      },
      ast::BinaryOpKind::And => hir::ValPatExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValPatExprKind::And {
          fst: ha.alloc(tr_val_pat_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          snd: ha.alloc(tr_val_pat_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        },
      },
      ast::BinaryOpKind::Or => hir::ValPatExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValPatExprKind::Or {
          fst: ha.alloc(tr_val_pat_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          snd: ha.alloc(tr_val_pat_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        },
      },
      // Error
      c => {
        dlogger.log_unexpected_binop_in_val_pattern(source.range, c);
        hir::ValPatExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValPatExprKind::Error,
        }
      }
    },
    // pattern is an error
    ref c => {
      dlogger.log_unexpected_in_val_pattern(source.range, c);
      hir::ValPatExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValPatExprKind::Error,
      }
    }
  }
}
fn tr_val_expr<'hir, 'ast>(
  idgen: &RefCell<u64>,
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  var_env: &mut Vec<(&'ast [u8], VarScope<'ast>)>,
  captured_var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  match source.kind {
    ast::ExprKind::Error => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::Error,
    },
    ast::ExprKind::NilTy => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::NilTy,
    },
    ast::ExprKind::NeverTy => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::NeverTy,
    },
    ast::ExprKind::BoolTy => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::BoolTy,
    },
    ast::ExprKind::U8Ty => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::U8Ty,
    },
    ast::ExprKind::U16Ty => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::U16Ty,
    },
    ast::ExprKind::U32Ty => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::U32Ty,
    },
    ast::ExprKind::U64Ty => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::U64Ty,
    },
    ast::ExprKind::I8Ty => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::I8Ty,
    },
    ast::ExprKind::I16Ty => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::I16Ty,
    },
    ast::ExprKind::I32Ty => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::I32Ty,
    },
    ast::ExprKind::I64Ty => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::I64Ty,
    },
    ast::ExprKind::F32Ty => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::F32Ty,
    },
    ast::ExprKind::F64Ty => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::F64Ty,
    },
    ast::ExprKind::LifetimeTy => todo!(),
    ast::ExprKind::Nil => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::Nil,
    },
    ast::ExprKind::Bool(b) => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::Bool(b),
    },
    ast::ExprKind::Int(ref i) => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::Int(i),
    },
    ast::ExprKind::Float(ref i) => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::Float(i),
    },
    ast::ExprKind::Type(ref maybe_signed_int) => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::Universe(if let Some(signed_int) = maybe_signed_int {
        if let Some(unsigned_int) = signed_int.to_biguint() {
          unsigned_int
        } else {
          dlogger.log_universe_level_negative(source.range);
          BigUint::from(0u32)
        }
      } else {
        BigUint::from(0u32)
      }),
    },
    ast::ExprKind::String { ref value, .. } => value.iter().rev().fold(
      // start with a null at the end of the list
      hir::ValExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValExprKind::Nil,
      },
      // as we work our way backwards, we prepend the current char as an int to our list
      |acc, x| {
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Pair {
            // first arg is the new expr for the int
            fst: ha.alloc(hir::ValExpr {
              source,
              id: Some(next_id(idgen)),
              kind: hir::ValExprKind::Char(*x as u32), // TODO: parse strings by pure unicode
            }),
            // second arg is the current tail of the list
            snd: ha.alloc(acc),
          },
        }
      },
    ),
    ast::ExprKind::Lifetime(_) => todo!(),
    ast::ExprKind::Group(ref body) => {
      tr_val_expr(idgen, ha, dlogger, body, var_env, captured_var_env)
    }
    ast::ExprKind::StructLiteral(ref body) => {
      let mut fields = Vec::new_in(ha);

      for (id, pat) in parse_struct_fields(ha, dlogger, source).into_iter() {
        let field_pat = tr_val_expr(idgen, ha, dlogger, pat, var_env, captured_var_env);
        fields.push((id, (pat, field_pat)));
      }

      hir::ValExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValExprKind::Struct(fields),
      }
    }
    ast::ExprKind::Identifier(ref identifier) => gen_var_use(
      idgen,
      ha,
      identifier,
      hir::UseKind::Take,
      dlogger,
      source,
      var_env,
      captured_var_env,
    ),
    ast::ExprKind::CaseOf {
      ref expr,
      ref cases,
    } => {
      let mut case_options = Vec::new_in(ha);

      // depth first search of binary tree
      let mut optstack = vec![cases];
      while let Some(current) = optstack.pop() {
        match current.kind {
          ast::ExprKind::BinaryOp {
            ref left_operand,
            ref right_operand,
            ref op,
          } => match op {
            ast::BinaryOpKind::CaseOption => {
              optstack.push(left_operand);
              optstack.push(right_operand);
            }
            ast::BinaryOpKind::Defun => {
              // translate pattern
              let (pat, bindings) =
                tr_refutable_pat_expr(idgen, ha, dlogger, left_operand, var_env, captured_var_env);

              // find old len
              let old_len = var_env.len();

              // add new variables to var env
              var_env.extend(bindings);

              // translate body
              let expr = tr_val_expr(idgen, ha, dlogger, right_operand, var_env, captured_var_env);

              // get rid of old variables
              var_env.truncate(old_len);

              // now add pair to case options
              case_options.push((pat, expr));
            }
            bok => {
              dlogger.log_expected_case_option_binop(current.range, bok);
            }
          },
          ref kind => {
            dlogger.log_expected_case_option_expr(current.range, kind);
          }
        }
      }

      // return case option
      hir::ValExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValExprKind::CaseOf {
          source: hir::CaseSource::Case,
          expr: ha.alloc(tr_place_expr(
            idgen,
            ha,
            dlogger,
            expr,
            var_env,
            captured_var_env,
          )),
          case_options,
        },
      }
    }
    ast::ExprKind::Struct(body) => match body.kind {
      ast::ExprKind::StructLiteral(fields_src) => {
        let mut fields = Vec::new_in(ha);

        for (id, pat) in parse_struct_fields(ha, dlogger, source).into_iter() {
          let field_pat = tr_val_expr(idgen, ha, dlogger, pat, var_env, captured_var_env);
          fields.push((id, (pat, field_pat)));
        }

        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::StructTy(fields),
        }
      }
      kind => {
        dlogger.log_expected_struct_literal_struct(body.range, &kind);
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Error,
        }
      }
    },
    ast::ExprKind::Enum(body) => match body.kind {
      ast::ExprKind::StructLiteral(fields_src) => {
        let mut fields = Vec::new_in(ha);

        for (id, pat) in parse_struct_fields(ha, dlogger, source).into_iter() {
          let field_pat = tr_val_expr(idgen, ha, dlogger, pat, var_env, captured_var_env);
          fields.push((id, (pat, field_pat)));
        }

        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::EnumTy(fields),
        }
      }
      kind => {
        dlogger.log_expected_struct_literal_enum(body.range, &kind);
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Error,
        }
      }
    },
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::Constrain => hir::ValExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValExprKind::Annotate {
          val_expr: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          ty_expr: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        },
      },
      ast::BinaryOpKind::RevConstrain => hir::ValExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValExprKind::Annotate {
          ty_expr: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          val_expr: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        },
      },
      ast::BinaryOpKind::Defun => {
        // Find free variables and their borrow levels from the body and arg expr
        let free_vars = find_free_vars(&[left_operand, right_operand]);

        // we will now try to construct the captured variable scope that helps
        let new_captured_vars = Vec::new();
        for (&identifier, (field_source, _)) in free_vars.iter() {
          new_captured_vars.push((
            identifier,
            VarScope {
              declaration: field_source,
            },
          ));
        }

        // we will now attempt to construct the captured var struct
        let captured_var_fields = Vec::new_in(ha);
        for (identifier, (field_source, use_kind)) in free_vars.into_iter() {
          let place = gen_var_place(field_source, identifier, dlogger, var_env, captured_var_env);
          let rhs = hir::ValExpr {
            source: field_source,
            id: Some(next_id(idgen)),
            kind: hir::ValExprKind::Use(ha.alloc(place), use_kind),
          };
          captured_var_fields.push((identifier, (field_source, rhs)));
        }

        let captured_vars = hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::StructLiteral(captured_var_fields),
        };

        // translate binding pattern. there's no var_env yet,
        // but we can still get variables from captured_var_env
        let (pat, bindings) = tr_irrefutable_pat_expr(
          idgen,
          ha,
          dlogger,
          left_operand,
          &mut vec![],
          &new_captured_vars,
        );

        // translate body. use the bound variables from the pat as the var_env
        // use the captured var fields as the
        let body = tr_val_expr(
          idgen,
          ha,
          dlogger,
          right_operand,
          &mut bindings,
          &new_captured_vars,
        );

        // return expr
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Lam {
            captured_vars: ha.alloc(captured_vars),
            arg: ha.alloc(pat),
            body: ha.alloc(body),
          },
        }
      }
      ast::BinaryOpKind::CaseOption => {
        dlogger.log_only_in_case(source.range);
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Error,
        }
      }
      ast::BinaryOpKind::Apply => hir::ValExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValExprKind::App {
          fun: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          arg: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        },
      },
      ast::BinaryOpKind::Compose => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(
          idgen,
          b"_compose",
          dlogger,
          source,
          var_env,
          captured_var_env,
          ha,
        ),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Pipe => hir::ValExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValExprKind::App {
          fun: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
          arg: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
        },
      },
      ast::BinaryOpKind::Add => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(
          idgen,
          b"_add",
          dlogger,
          source,
          var_env,
          captured_var_env,
          ha,
        ),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Sub => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(
          idgen,
          b"_sub",
          dlogger,
          source,
          var_env,
          captured_var_env,
          ha,
        ),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Mul => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(
          idgen,
          b"_mul",
          dlogger,
          source,
          var_env,
          captured_var_env,
          ha,
        ),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Div => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(
          idgen,
          b"_div",
          dlogger,
          source,
          var_env,
          captured_var_env,
          ha,
        ),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Rem => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(
          idgen,
          b"_rem",
          dlogger,
          source,
          var_env,
          captured_var_env,
          ha,
        ),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        ],
      ),
      ast::BinaryOpKind::And => hir::ValExpr {
        // a && b
        // case a
        // of true  = > b
        // || false = > false
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValExprKind::CaseOf {
          expr: ha.alloc(tr_place_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          source: hir::CaseSource::And,
          case_options: {
            let mut v = Vec::new_in(ha);
            // true path
            v.push((
              hir::RefutablePatExpr {
                source,
                id: Some(next_id(idgen)),
                kind: hir::RefutablePatExprKind::ValPat(ha.alloc(hir::ValPatExpr {
                  source,
                  id: Some(next_id(idgen)),
                  kind: hir::ValPatExprKind::Value(ha.alloc(gen_bool(idgen, source, true))),
                })),
              },
              tr_val_expr(idgen, ha, dlogger, right_operand, var_env, captured_var_env),
            ));

            // false path
            v.push((
              hir::RefutablePatExpr {
                source,
                id: Some(next_id(idgen)),
                kind: hir::RefutablePatExprKind::ValPat(ha.alloc(hir::ValPatExpr {
                  source,
                  id: Some(next_id(idgen)),
                  kind: hir::ValPatExprKind::Value(ha.alloc(gen_bool(idgen, source, false))),
                })),
              },
              gen_bool(idgen, source, false),
            ));
            v
          },
        },
      },
      ast::BinaryOpKind::Or => hir::ValExpr {
        // a || b
        // case a
        // of true => true
        // of false => b
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValExprKind::CaseOf {
          expr: ha.alloc(tr_place_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          source: hir::CaseSource::Or,
          case_options: {
            let mut v = Vec::new_in(ha);
            // true path
            v.push((
              hir::RefutablePatExpr {
                source,
                id: Some(next_id(idgen)),
                kind: hir::RefutablePatExprKind::ValPat(ha.alloc(hir::ValPatExpr {
                  source,
                  id: Some(next_id(idgen)),
                  kind: hir::ValPatExprKind::Value(ha.alloc(gen_bool(idgen, source, true))),
                })),
              },
              gen_bool(idgen, source, true),
            ));

            // false path
            v.push((
              hir::RefutablePatExpr {
                source,
                id: Some(next_id(idgen)),
                kind: hir::RefutablePatExprKind::ValPat(ha.alloc(hir::ValPatExpr {
                  source,
                  id: Some(next_id(idgen)),
                  kind: hir::ValPatExprKind::Value(ha.alloc(gen_bool(idgen, source, false))),
                })),
              },
              tr_val_expr(idgen, ha, dlogger, right_operand, var_env, captured_var_env),
            ));
            v
          },
        },
      },
      ast::BinaryOpKind::Equal => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(
          idgen,
          b"_eq",
          dlogger,
          source,
          var_env,
          captured_var_env,
          ha,
        ),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        ],
      ),
      ast::BinaryOpKind::NotEqual => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(
          idgen,
          b"_neq",
          dlogger,
          source,
          var_env,
          captured_var_env,
          ha,
        ),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Less => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_l", dlogger, source, var_env, captured_var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        ],
      ),
      ast::BinaryOpKind::LessEqual => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(
          idgen,
          b"_le",
          dlogger,
          source,
          var_env,
          captured_var_env,
          ha,
        ),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Greater => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_g", dlogger, source, var_env, captured_var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        ],
      ),
      ast::BinaryOpKind::GreaterEqual => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(
          idgen,
          b"_ge",
          dlogger,
          source,
          var_env,
          captured_var_env,
          ha,
        ),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Cons => hir::ValExpr {
        id: Some(next_id(idgen)),
        source,
        kind: hir::ValExprKind::Pair {
          fst: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          )),
          snd: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            captured_var_env,
          )),
        },
      },
      ast::BinaryOpKind::PlusAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let (pat, _) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env, captured_var_env);

        // create val by adding together
        let val = gen_app(
          idgen,
          ha,
          source,
          gen_var_take(
            idgen,
            b"_add",
            dlogger,
            source,
            var_env,
            captured_var_env,
            ha,
          ),
          vec![
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              left_operand,
              var_env,
              captured_var_env,
            )),
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              right_operand,
              var_env,
              captured_var_env,
            )),
          ],
        );

        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              id: Some(next_id(idgen)),
              kind: hir::ValExprKind::Nil,
            }),
          },
        }
      }
      ast::BinaryOpKind::MinusAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let (pat, _) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env, captured_var_env);

        // create val by adding together
        let val = gen_app(
          idgen,
          ha,
          source,
          gen_var_take(
            idgen,
            b"_sub",
            dlogger,
            source,
            var_env,
            captured_var_env,
            ha,
          ),
          vec![
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              left_operand,
              var_env,
              captured_var_env,
            )),
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              right_operand,
              var_env,
              captured_var_env,
            )),
          ],
        );

        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              id: Some(next_id(idgen)),
              kind: hir::ValExprKind::Nil,
            }),
          },
        }
      }
      ast::BinaryOpKind::MulAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let (pat, _) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env, captured_var_env);

        // create val by adding together
        let val = gen_app(
          idgen,
          ha,
          source,
          gen_var_take(
            idgen,
            b"_mul",
            dlogger,
            source,
            var_env,
            captured_var_env,
            ha,
          ),
          vec![
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              left_operand,
              var_env,
              captured_var_env,
            )),
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              right_operand,
              var_env,
              captured_var_env,
            )),
          ],
        );

        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              id: Some(next_id(idgen)),
              kind: hir::ValExprKind::Nil,
            }),
          },
        }
      }
      ast::BinaryOpKind::DivAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let (pat, _) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env, captured_var_env);

        // create val by adding together
        let val = gen_app(
          idgen,
          ha,
          source,
          gen_var_take(
            idgen,
            b"_div",
            dlogger,
            source,
            var_env,
            captured_var_env,
            ha,
          ),
          vec![
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              left_operand,
              var_env,
              captured_var_env,
            )),
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              right_operand,
              var_env,
              captured_var_env,
            )),
          ],
        );

        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              id: Some(next_id(idgen)),
              kind: hir::ValExprKind::Nil,
            }),
          },
        }
      }
      ast::BinaryOpKind::RemAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let (pat, _) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env, captured_var_env);

        // create val by adding together
        let val = gen_app(
          idgen,
          ha,
          source,
          gen_var_take(
            idgen,
            b"_rem",
            dlogger,
            source,
            var_env,
            captured_var_env,
            ha,
          ),
          vec![
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              left_operand,
              var_env,
              captured_var_env,
            )),
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              right_operand,
              var_env,
              captured_var_env,
            )),
          ],
        );

        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              id: Some(next_id(idgen)),
              kind: hir::ValExprKind::Nil,
            }),
          },
        }
      }
      ast::BinaryOpKind::Assign => {
        // this represents an assign at the tail end of an expression.
        // variables created in such assigns will not be available outside, and should warn about unused vars

        // translate the rhs
        let val = tr_val_expr(idgen, ha, dlogger, right_operand, var_env, captured_var_env);

        // translate pattern (dropping vars, since they won't be used)
        // later, a warning will be sent in liveness checking
        let (pat, _) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env, captured_var_env);

        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              id: Some(next_id(idgen)),
              kind: hir::ValExprKind::Nil,
            }),
          },
        }
      }
      ast::BinaryOpKind::Sequence => {
        // if the lhs is an assign
        if let ast::Expr {
          kind:
            ast::ExprKind::BinaryOp {
              op: ast::BinaryOpKind::Assign,
              left_operand: ref assign_pat,
              right_operand: ref assign_value,
            },
          ..
        } = **left_operand
        {
          // this represents an assign with a scope after it

          // translate rhs
          let val = tr_val_expr(idgen, ha, dlogger, assign_value, var_env, captured_var_env);

          // translate pattern and get bindings
          let (pat, bindings) =
            tr_irrefutable_pat_expr(idgen, ha, dlogger, assign_pat, var_env, captured_var_env);

          // get old length
          let old_len = var_env.len();

          // extend env with new bindings
          var_env.extend(bindings);

          // parse body in the amended environment
          let body = tr_val_expr(idgen, ha, dlogger, right_operand, var_env, captured_var_env);

          // now drop bindings
          var_env.truncate(old_len);

          hir::ValExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::ValExprKind::LetIn {
              pat: ha.alloc(pat),
              val: ha.alloc(val),
              body: ha.alloc(body),
            },
          }
        } else {
          // continue
          hir::ValExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::ValExprKind::Sequence {
              fst: ha.alloc(tr_val_expr(
                idgen,
                ha,
                dlogger,
                left_operand,
                var_env,
                captured_var_env,
              )),
              snd: ha.alloc(tr_val_expr(
                idgen,
                ha,
                dlogger,
                right_operand,
                var_env,
                captured_var_env,
              )),
            },
          }
        }
      }
      // TODO: this should go to tr_place
      ast::BinaryOpKind::ModuleAccess => gen_take(
        idgen,
        source,
        tr_place_expr(idgen, ha, dlogger, source, var_env, captured_var_env),
        ha,
      ),
      ast::BinaryOpKind::Range => todo!(),
      ast::BinaryOpKind::RangeInclusive => todo!(),
    },
  }
}

fn tr_place_expr<'hir, 'ast>(
  idgen: &RefCell<u64>,
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  var_env: &mut Vec<(&'ast [u8], VarScope<'ast>)>,
  captured_var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
) -> hir::PlaceExpr<'hir, 'ast, &'hir Bump> {
  match &source.kind {
    ast::ExprKind::Identifier(ref identifier) => {
      gen_var_place(source, identifier, dlogger, var_env, captured_var_env)
    }
    ast::ExprKind::Group(ref expr) => {
      tr_place_expr(idgen, ha, dlogger, expr, var_env, captured_var_env)
    }
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::ModuleAccess => match **right_operand {
        ast::Expr {
          kind: ast::ExprKind::Identifier(ref field),
          ..
        } => hir::PlaceExpr {
          source,
          kind: hir::PlaceExprKind::StructField {
            root: ha.alloc(tr_place_expr(
              idgen,
              ha,
              dlogger,
              left_operand,
              var_env,
              captured_var_env,
            )),
            field,
            field_source: right_operand,
          },
        },

        _ => {
          dlogger.log_field_not_valid(right_operand.range, &right_operand.kind);
          hir::PlaceExpr {
            source,
            kind: hir::PlaceExprKind::Error,
          }
        }
      },
      e => {
        // throw error about unrecognized binary operation
        dlogger.log_unexpected_binop_in_place_expression(source.range, e);
        // then return error struct
        hir::PlaceExpr {
          source,
          kind: hir::PlaceExprKind::Error,
        }
      }
    },
    e => {
      dlogger.log_invalid_place_expression(source.range, e);
      // throw error then return error
      hir::PlaceExpr {
        source,
        kind: hir::PlaceExprKind::Error,
      }
    }
  }
}

pub fn construct_hir<'hir, 'ast>(
  ast: &'ast ast::Expr,
  ha: &'hir Bump,
  mut dlogger: DiagnosticLogger,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  tr_val_expr(
    &RefCell::new(0),
    ha,
    &mut dlogger,
    ast,
    &mut vec![],
    &vec![],
  )
}
