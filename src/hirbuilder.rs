use super::ast;
use super::dlogger::DiagnosticLogger;
use super::hir;
use super::utils::new_vec_from;
use bumpalo::Bump;
use num_bigint::BigUint;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

struct LabelScope<'ast> {
  declaration: &'ast ast::Expr,
  defers: Vec<&'ast ast::Expr>,
}

struct VarScope<'ast> {
  declaration: Option<&'ast ast::Expr>,
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
  let s = String::from("hi");

  std::mem::forget(s);

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
    id: next_id(idgen),
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
    id: next_id(idgen),
    kind: hir::ValExprKind::Bool(b),
  }
}

fn gen_nil<'hir, 'ast>(
  idgen: &RefCell<u64>,

  source: &'ast ast::Expr,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  hir::ValExpr {
    source: source,
    id: next_id(idgen),
    kind: hir::ValExprKind::Nil,
  }
}

fn gen_nil_ty<'hir, 'ast>(
  idgen: &RefCell<u64>,

  source: &'ast ast::Expr,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  hir::ValExpr {
    source: source,
    id: next_id(idgen),
    kind: hir::ValExprKind::NilTy,
  }
}

fn gen_var_place<'hir, 'ast>(
  source: &'ast ast::Expr,
  identifier: &[u8],
  dlogger: &mut DiagnosticLogger,
  var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
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

fn gen_take<'hir, 'ast>(
  idgen: &RefCell<u64>,
  source: &'ast ast::Expr,
  place: hir::PlaceExpr<'hir, 'ast, &'hir Bump>,
  ha: &'hir Bump,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  hir::ValExpr {
    source,
    id: next_id(idgen),
    kind: hir::ValExprKind::Take(ha.alloc(place)),
  }
}

fn gen_var_take<'hir, 'ast>(
  idgen: &RefCell<u64>,
  identifier: &[u8],
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
  ha: &'hir Bump,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  gen_take(
    idgen,
    source,
    gen_var_place(source, identifier, dlogger, var_env),
    ha,
  )
}

fn struct_pattern_to_pat_vec<'hir, 'ast>(
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
) -> HashMap<&'ast Vec<u8>, &'ast ast::Expr> {
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
            kind:
              ast::ExprKind::UnaryOp {
                op: ast::UnaryOpKind::Bind,
                operand,
              },
            ..
          } => match operand.as_ref() {
            // this means that a bind was the target of the assign
            ast::Expr {
              kind: ast::ExprKind::Identifier(ref identifier),
              range,
              ..
            } => match patterns.entry(identifier) {
              Entry::Vacant(ve) => {
                ve.insert(&**right_operand);
              }
              Entry::Occupied(oe) => {
                // identifier not unique, log error for using duplicate identifier in struct
                dlogger.log_duplicate_field_name(*range, &oe.key(), oe.get().range);
              }
            },
            // means that something other than a identifier was the target of the bind
            ast::Expr {
              range, ref kind, ..
            } => dlogger.log_unsupported_bind_target_in_pattern_struct_assign(*range, kind),
          },
          // error handle
          ast::Expr {
            range, ref kind, ..
          } => {
            // means that there was no single bind as a target of the assign
            dlogger.log_unsupported_target_in_pattern_struct_assign(*range, kind);
          }
        },
        _ => {
          dlogger.log_unexpected_binop_in_pattern_struct(current.range, op);
        }
      },
      ref kind => {
        dlogger.log_unexpected_element_in_pattern_struct(current.range, kind);
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
) -> (
  hir::IrrefutablePatExpr<'hir, 'ast, &'hir Bump>,
  Vec<(&'ast [u8], VarScope<'ast>)>,
) {
  match source.kind {
    // propagate error
    ast::ExprKind::Error => (
      hir::IrrefutablePatExpr {
        source,
        id: next_id(idgen),
        kind: hir::IrrefutablePatExprKind::Error,
      },
      vec![],
    ),
    // as long as it typechecks, nil will always match
    ast::ExprKind::Nil => (
      hir::IrrefutablePatExpr {
        source,
        id: next_id(idgen),
        kind: hir::IrrefutablePatExprKind::Nil,
      },
      vec![],
    ),
    // elide groups
    ast::ExprKind::Group(ref body) => tr_irrefutable_pat_expr(idgen, ha, dlogger, body, var_env),
    ast::ExprKind::StructLiteral(ref body) => {
      let patterns = struct_pattern_to_pat_vec(ha, dlogger, source);

      let mut bindings = vec![];
      let mut fields = Vec::new_in(ha);

      for (id, pat) in patterns.into_iter() {
        let (field_pat, field_bindings) = tr_irrefutable_pat_expr(idgen, ha, dlogger, pat, var_env);
        fields.push((id, field_pat));
        bindings.extend(field_bindings);
      }

      (
        hir::IrrefutablePatExpr {
          source,
          id: next_id(idgen),
          kind: hir::IrrefutablePatExprKind::StructLiteral(fields),
        },
        bindings,
      )
    }
    ast::ExprKind::UnaryOp {
      ref op,
      ref operand,
    } => match op {
      ast::UnaryOpKind::Bind => match **operand {
        // binds a variable to an identifier
        ast::Expr {
          kind: ast::ExprKind::Identifier(ref identifier),
          ..
        } => (
          hir::IrrefutablePatExpr {
            source,
            id: next_id(idgen),
            kind: hir::IrrefutablePatExprKind::BindVariable,
          },
          vec![(
            identifier,
            VarScope {
              declaration: Some(operand),
            },
          )],
        ),
        // handle error
        ast::Expr {
          range, ref kind, ..
        } => {
          dlogger.log_unexpected_bind_target(range, kind);
          (
            hir::IrrefutablePatExpr {
              source,
              id: next_id(idgen),
              kind: hir::IrrefutablePatExprKind::Error,
            },
            vec![],
          )
        }
      },
      ast::UnaryOpKind::Mutate => (
        hir::IrrefutablePatExpr {
          source,
          id: next_id(idgen),
          kind: hir::IrrefutablePatExprKind::BindPlace(tr_place_expr(
            idgen,
            ha,
            dlogger,
            operand,
            var_env,
            &mut vec![],
          )),
        },
        vec![],
      ),
      // the remaining operators
      c => {
        dlogger.log_unexpected_unop_in_irrefutable_pattern(source.range, c);
        (
          hir::IrrefutablePatExpr {
            source,
            id: next_id(idgen),
            kind: hir::IrrefutablePatExprKind::Error,
          },
          vec![],
        )
      }
    },
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::Apply => {
        let (arg_pat, arg_bindings) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, right_operand, var_env);
        (
          hir::IrrefutablePatExpr {
            source,
            id: next_id(idgen),
            kind: hir::IrrefutablePatExprKind::ActivePattern {
              fun: ha.alloc(tr_val_expr(
                idgen,
                ha,
                dlogger,
                left_operand,
                var_env,
                &mut vec![],
              )),
              arg: ha.alloc(arg_pat),
            },
          },
          arg_bindings,
        )
      }
      ast::BinaryOpKind::Cons => {
        let (fst_pat, fst_bindings) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env);
        let (snd_pat, snd_bindings) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, right_operand, var_env);

        (
          hir::IrrefutablePatExpr {
            source,
            id: next_id(idgen),
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
            id: next_id(idgen),
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
          id: next_id(idgen),
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
) -> (
  hir::RefutablePatExpr<'hir, 'ast, &'hir Bump>,
  Vec<(&'ast [u8], VarScope<'ast>)>,
) {
  match source.kind {
    // propagate error
    ast::ExprKind::Error => (
      hir::RefutablePatExpr {
        source,
        id: next_id(idgen),
        kind: hir::RefutablePatExprKind::Error,
      },
      vec![],
    ),
    // send irrefutable patterns here
    ast::ExprKind::Nil => {
      let (pat, bindings) = tr_irrefutable_pat_expr(idgen, ha, dlogger, source, var_env);
      (
        hir::RefutablePatExpr {
          source,
          id: next_id(idgen),
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
      let pat = tr_val_pat_expr(idgen, ha, dlogger, source, var_env);
      (
        hir::RefutablePatExpr {
          source,
          id: next_id(idgen),
          kind: hir::RefutablePatExprKind::ValPat(ha.alloc(pat)),
        },
        vec![],
      )
    }
    // elide groups
    ast::ExprKind::Group(ref body) => tr_refutable_pat_expr(idgen, ha, dlogger, body, var_env),
    ast::ExprKind::StructLiteral(ref body) => {
      let patterns = struct_pattern_to_pat_vec(ha, dlogger, source);

      let mut bindings = vec![];
      let mut fields = Vec::new_in(ha);

      for (id, pat) in patterns.into_iter() {
        let (field_pat, field_bindings) = tr_refutable_pat_expr(idgen, ha, dlogger, pat, var_env);
        fields.push((id, field_pat));
        bindings.extend(field_bindings);
      }

      (
        hir::RefutablePatExpr {
          source,
          id: next_id(idgen),
          kind: hir::RefutablePatExprKind::StructLiteral(fields),
        },
        bindings,
      )
    }
    ast::ExprKind::UnaryOp { ref op, .. } => match op {
      // handle irrefutable ops
      ast::UnaryOpKind::Bind | ast::UnaryOpKind::Mutate => {
        let (pat, bindings) = tr_irrefutable_pat_expr(idgen, ha, dlogger, source, var_env);
        (
          hir::RefutablePatExpr {
            source,
            id: next_id(idgen),
            kind: hir::RefutablePatExprKind::IrrefutablePat(ha.alloc(pat)),
          },
          bindings,
        )
      }
      // handle refutable ops
      ast::UnaryOpKind::Val => (
        hir::RefutablePatExpr {
          source,
          id: next_id(idgen),
          kind: hir::RefutablePatExprKind::ValPat(
            ha.alloc(tr_val_pat_expr(idgen, ha, dlogger, source, var_env)),
          ),
        },
        vec![],
      ),
      // the remaining operators
      c => {
        dlogger.log_unexpected_unop_in_refutable_pattern(source.range, c);
        (
          hir::RefutablePatExpr {
            source,
            id: next_id(idgen),
            kind: hir::RefutablePatExprKind::Error,
          },
          vec![],
        )
      }
    },
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      // handle refutable ops
      ast::BinaryOpKind::Range | ast::BinaryOpKind::RangeInclusive | ast::BinaryOpKind::Or => (
        hir::RefutablePatExpr {
          source,
          id: next_id(idgen),
          kind: hir::RefutablePatExprKind::ValPat(
            ha.alloc(tr_val_pat_expr(idgen, ha, dlogger, source, var_env)),
          ),
        },
        vec![],
      ),
      ast::BinaryOpKind::Apply => {
        let (arg_pat, arg_bindings) =
          tr_irrefutable_pat_expr(idgen, ha, dlogger, right_operand, var_env);
        (
          hir::RefutablePatExpr {
            source,
            id: next_id(idgen),
            kind: hir::RefutablePatExprKind::ActivePattern {
              fun: ha.alloc(tr_val_expr(
                idgen,
                ha,
                dlogger,
                left_operand,
                var_env,
                &mut vec![],
              )),
              arg: ha.alloc(arg_pat),
            },
          },
          arg_bindings,
        )
      }
      ast::BinaryOpKind::Cons => {
        let (fst_pat, fst_bindings) =
          tr_refutable_pat_expr(idgen, ha, dlogger, left_operand, var_env);
        let (snd_pat, snd_bindings) =
          tr_refutable_pat_expr(idgen, ha, dlogger, right_operand, var_env);

        (
          hir::RefutablePatExpr {
            source,
            id: next_id(idgen),
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
          tr_refutable_pat_expr(idgen, ha, dlogger, left_operand, var_env);
        let (snd_pat, snd_bindings) =
          tr_refutable_pat_expr(idgen, ha, dlogger, right_operand, var_env);
        (
          hir::RefutablePatExpr {
            source,
            id: next_id(idgen),
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
            id: next_id(idgen),
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
          id: next_id(idgen),
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
) -> hir::ValPatExpr<'hir, 'ast, &'hir Bump> {
  match source.kind {
    // propagate error
    ast::ExprKind::Error => hir::ValPatExpr {
      source,
      id: next_id(idgen),
      kind: hir::ValPatExprKind::Error,
    },
    // match against values automatically
    ast::ExprKind::Nil
    | ast::ExprKind::Bool(_)
    | ast::ExprKind::Int(_)
    | ast::ExprKind::Float(_)
    | ast::ExprKind::String { .. } => hir::ValPatExpr {
      source,
      id: next_id(idgen),
      kind: hir::ValPatExprKind::Value(ha.alloc(tr_val_expr(
        idgen,
        ha,
        dlogger,
        source,
        var_env,
        &mut vec![],
      ))),
    },
    // elide groups
    ast::ExprKind::Group(ref body) => tr_val_pat_expr(idgen, ha, dlogger, body, var_env),
    ast::ExprKind::StructLiteral(ref body) => {
      let mut fields = Vec::new_in(ha);

      for (id, pat) in struct_pattern_to_pat_vec(ha, dlogger, source).into_iter() {
        let field_pat = tr_val_pat_expr(idgen, ha, dlogger, pat, var_env);
        fields.push((id, field_pat));
      }

      hir::ValPatExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValPatExprKind::StructLiteral(fields),
      }
    }
    ast::ExprKind::UnaryOp {
      ref op,
      ref operand,
    } => match op {
      ast::UnaryOpKind::Val => hir::ValPatExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValPatExprKind::Value(ha.alloc(tr_val_expr(
          idgen,
          ha,
          dlogger,
          source,
          var_env,
          &mut vec![],
        ))),
      },
      // the remaining operators
      c => {
        dlogger.log_unexpected_unop_in_val_pattern(source.range, c);
        hir::ValPatExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValPatExprKind::Error,
        }
      }
    },
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::Range => hir::ValPatExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValPatExprKind::Range {
          fst: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            &mut vec![],
          )),
          snd: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            &mut vec![],
          )),
          inclusive: false,
        },
      },
      ast::BinaryOpKind::RangeInclusive => hir::ValPatExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValPatExprKind::Range {
          fst: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            &mut vec![],
          )),
          snd: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            &mut vec![],
          )),
          inclusive: true,
        },
      },
      ast::BinaryOpKind::Apply => hir::ValPatExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValPatExprKind::ActivePattern {
          fun: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            &mut vec![],
          )),
          arg: ha.alloc(tr_val_pat_expr(idgen, ha, dlogger, right_operand, var_env)),
        },
      },
      ast::BinaryOpKind::Cons => hir::ValPatExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValPatExprKind::Pair {
          fst: ha.alloc(tr_val_pat_expr(idgen, ha, dlogger, left_operand, var_env)),
          snd: ha.alloc(tr_val_pat_expr(idgen, ha, dlogger, right_operand, var_env)),
        },
      },
      ast::BinaryOpKind::And => hir::ValPatExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValPatExprKind::And {
          fst: ha.alloc(tr_val_pat_expr(idgen, ha, dlogger, left_operand, var_env)),
          snd: ha.alloc(tr_val_pat_expr(idgen, ha, dlogger, right_operand, var_env)),
        },
      },
      ast::BinaryOpKind::Or => hir::ValPatExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValPatExprKind::Or {
          fst: ha.alloc(tr_val_pat_expr(idgen, ha, dlogger, left_operand, var_env)),
          snd: ha.alloc(tr_val_pat_expr(idgen, ha, dlogger, right_operand, var_env)),
        },
      },
      // Error
      c => {
        dlogger.log_unexpected_binop_in_val_pattern(source.range, c);
        hir::ValPatExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValPatExprKind::Error,
        }
      }
    },
    // pattern is an error
    ref c => {
      dlogger.log_unexpected_in_val_pattern(source.range, c);
      hir::ValPatExpr {
        source,
        id: next_id(idgen),
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
  label_env: &mut Vec<(&'ast [u8], LabelScope<'ast>)>,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  match source.kind {
    ast::ExprKind::Error => hir::ValExpr {
      source,
      id: next_id(idgen),
      kind: hir::ValExprKind::Error,
    },
    ast::ExprKind::Nil => hir::ValExpr {
      source,
      id: next_id(idgen),
      kind: hir::ValExprKind::Nil,
    },
    ast::ExprKind::Bool(b) => hir::ValExpr {
      source,
      id: next_id(idgen),
      kind: hir::ValExprKind::Bool(b),
    },
    ast::ExprKind::Int(ref i) => hir::ValExpr {
      source,
      id: next_id(idgen),
      kind: hir::ValExprKind::Int(i),
    },
    ast::ExprKind::Float(ref i) => hir::ValExpr {
      source,
      id: next_id(idgen),
      kind: hir::ValExprKind::Float(i),
    },
    ast::ExprKind::Type(ref maybe_signed_int) => hir::ValExpr {
      source,
      id: next_id(idgen),
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
        id: next_id(idgen),
        kind: hir::ValExprKind::Nil,
      },
      // as we work our way backwards, we prepend the current char as an int to our list
      |acc, x| {
        hir::ValExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValExprKind::Pair {
            // first arg is the new expr for the int
            fst: ha.alloc(hir::ValExpr {
              source,
              id: next_id(idgen),
              kind: hir::ValExprKind::Char(*x as u32), // TODO: parse strings by pure unicode
            }),
            // second arg is the current tail of the list
            snd: ha.alloc(acc),
          },
        }
      },
    ),
    ast::ExprKind::Label {
      ref label,
      ref body,
    } => {
      // introduce label into the label environment
      label_env.push((
        &label,
        LabelScope {
          declaration: source,
          defers: vec![],
        },
      ));

      // translate body
      let body_tr = tr_val_expr(idgen, ha, dlogger, body, var_env, label_env);

      // pop label
      let _ = label_env.pop().unwrap();

      // return label
      hir::ValExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValExprKind::Label(ha.alloc(body_tr)),
      }
    }
    ast::ExprKind::Group(ref body) => tr_val_expr(idgen, ha, dlogger, body, var_env, label_env),
    ast::ExprKind::Defer {
      label: ref maybe_label,
      ref body,
    } => {
      // fail if label wasn't specified none
      if let Some(label) = maybe_label {
        // try to push the syntax to
        let updated = update(label_env, &label, |scope| scope.defers.push(body));

        if !updated {
          dlogger.log_cannot_find_label_in_scope(source.range, &label);
        }

        // all defers are replaced with a nil
        hir::ValExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValExprKind::Nil,
        }
      } else {
        // a label was never properly provided
        // an error should already have been thrown, so don't double report
        hir::ValExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValExprKind::Error,
        }
      }
    }

    ast::ExprKind::Ret {
      label: ref maybe_label,
      ref body,
    } => {
      // fail if label wasn't specified none
      if let Some(label) = maybe_label {
        // translate body
        let body_tr = tr_val_expr(idgen, ha, dlogger, body, var_env, label_env);

        // now make the defer-ret construct
        // ret 'x 0
        // to
        // let toret = 0;
        // (run defers);
        // ret 'x toret

        hir::ValExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValExprKind::LetIn {
            // introduce variable with value of ret
            pat: ha.alloc(hir::IrrefutablePatExpr {
              source,
              id: next_id(idgen),
              kind: hir::IrrefutablePatExprKind::BindVariable,
            }),
            val: ha.alloc(body_tr),

            // write body
            body: lookup(&label_env, &label).defers.clone().iter().fold(
              // the last thing to execute is the actual ret
              ha.alloc(hir::ValExpr {
                source,
                id: next_id(idgen),
                kind: hir::ValExprKind::Ret {
                  labels_up: lookup_count_up(label_env, &label),
                  // lookup variable introduced earlier
                  value: ha.alloc(gen_take(
                    idgen,
                    source,
                    // place expr
                    hir::PlaceExpr {
                      source,
                      kind: hir::PlaceExprKind::Var(0),
                    },
                    ha,
                  )),
                },
              }),
              // closure
              |acc, x| {
                // translate defer
                let tr_x = tr_val_expr(idgen, ha, dlogger, x, var_env, label_env);
                // return sequenced defer
                ha.alloc(hir::ValExpr {
                  source: x,
                  id: next_id(idgen),
                  // we want to sequence it in the reverse order we saw them
                  // so, later ones will be executed first
                  kind: hir::ValExprKind::Sequence {
                    fst: ha.alloc(tr_x),
                    snd: acc,
                  },
                })
              },
            ),
          },
        }
      } else {
        // a label was never properly provided
        // an error should already have been thrown, so don't double report
        hir::ValExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValExprKind::Error,
        }
      }
    }
    ast::ExprKind::StructLiteral(ref body) => {
      // create a struct of literals
      let mut fields = HashMap::new();

      // depth first search of binary tree
      let mut sequences = vec![body];

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
                kind:
                  ast::ExprKind::UnaryOp {
                    op: ast::UnaryOpKind::Bind,
                    operand,
                  },
                ..
              } => match operand.as_ref() {
                // this means that a bind was the target of the assign
                ast::Expr {
                  kind: ast::ExprKind::Identifier(ref identifier),
                  range,
                  ..
                } => match fields.entry(identifier) {
                  Entry::Vacant(ve) => {
                    // identifier unique, translate rhs and insert into map
                    ve.insert((
                      operand.as_ref(),
                      tr_val_expr(idgen, ha, dlogger, right_operand, var_env, label_env),
                    ));
                  }
                  Entry::Occupied(oe) => {
                    // identifier not unique, log error for using duplicate identifier in struct
                    dlogger.log_duplicate_field_name(*range, &oe.key(), oe.get().0.range);
                  }
                },

                // means that something other than a reference was the target of the bind
                ast::Expr {
                  range, ref kind, ..
                } => dlogger.log_unsupported_target_in_struct_assign(*range, kind),
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
      // return struct
      hir::ValExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValExprKind::StructLiteral(new_vec_from(ha, fields.drain())),
      }
    }
    ast::ExprKind::Identifier(ref identifier) => {
      gen_var_take(idgen, identifier, dlogger, source, var_env, ha)
    }
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
                tr_refutable_pat_expr(idgen, ha, dlogger, left_operand, var_env);

              // find old len
              let old_len = var_env.len();

              // add new variables to var env
              var_env.extend(bindings);

              // translate body
              let expr = tr_val_expr(idgen, ha, dlogger, right_operand, var_env, label_env);

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
        id: next_id(idgen),
        kind: hir::ValExprKind::CaseOf {
          source: hir::CaseSource::Case,
          expr: ha.alloc(tr_place_expr(idgen, ha, dlogger, expr, var_env, label_env)),
          case_options,
        },
      }
    }
    ast::ExprKind::UnaryOp {
      ref op,
      ref operand,
    } => match op {
      ast::UnaryOpKind::Ref => hir::ValExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValExprKind::Borrow(ha.alloc(tr_place_expr(
          idgen, ha, dlogger, operand, var_env, label_env,
        ))),
      },
      ast::UnaryOpKind::MutRef => hir::ValExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValExprKind::MutBorrow(ha.alloc(tr_place_expr(
          idgen, ha, dlogger, operand, var_env, label_env,
        ))),
      },
      ast::UnaryOpKind::Deref => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_deref", dlogger, source, var_env, ha),
        vec![ha.alloc(tr_val_expr(idgen, ha, dlogger, operand, var_env, label_env))],
      ),
      // TODO: decompose
      ast::UnaryOpKind::ReturnOnError => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_return_on_error", dlogger, source, var_env, ha),
        vec![ha.alloc(tr_val_expr(idgen, ha, dlogger, operand, var_env, label_env))],
      ),
      ast::UnaryOpKind::Struct => hir::ValExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValExprKind::Struct(
          ha.alloc(tr_val_expr(idgen, ha, dlogger, operand, var_env, label_env)),
        ),
      },
      ast::UnaryOpKind::Enum => hir::ValExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValExprKind::Enum(
          ha.alloc(tr_val_expr(idgen, ha, dlogger, operand, var_env, label_env)),
        ),
      },
      ast::UnaryOpKind::Loop => hir::ValExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValExprKind::Loop(
          ha.alloc(tr_val_expr(idgen, ha, dlogger, operand, var_env, label_env)),
        ),
      },
      c => {
        dlogger.log_unexpected_unop_in_expr(source.range, c);
        hir::ValExpr {
          source,
          id: next_id(idgen),
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
        id: next_id(idgen),
        kind: hir::ValExprKind::Annotate {
          val_expr: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ty_expr: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        },
      },
      ast::BinaryOpKind::RevConstrain => hir::ValExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValExprKind::Annotate {
          ty_expr: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          val_expr: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        },
      },
      ast::BinaryOpKind::Defun => {
        // translate pattern
        let (pat, bindings) = tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env);
        // capture old length of var_env
        let old_len = var_env.len();
        // push bindings into the new equation
        var_env.extend(bindings);
        // translate the body in the new context
        let val = tr_val_expr(idgen, ha, dlogger, right_operand, var_env, label_env);
        // get rid of the new variable bindings
        var_env.truncate(old_len);
        // return expr
        hir::ValExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValExprKind::Lam {
            arg: ha.alloc(pat),
            body: ha.alloc(val),
          },
        }
      }
      ast::BinaryOpKind::CaseOption => {
        dlogger.log_only_in_case(source.range);
        hir::ValExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValExprKind::Error,
        }
      }
      ast::BinaryOpKind::Apply => hir::ValExpr {
        source,
        id: next_id(idgen),
        kind: hir::ValExprKind::App {
          fun: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          arg: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        },
      },
      ast::BinaryOpKind::Compose => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_compose", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::PipeForward => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_pipe_forward", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::PipeBackward => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_pipe_backward", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Add => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_add", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Sub => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_sub", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Mul => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_mul", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Div => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_div", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Rem => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_rem", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::And => hir::ValExpr {
        // a && b
        // case a
        // of true  = > b
        // || false = > false
        source,
        id: next_id(idgen),
        kind: hir::ValExprKind::CaseOf {
          expr: ha.alloc(tr_place_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          source: hir::CaseSource::And,
          case_options: {
            let mut v = Vec::new_in(ha);
            // true path
            v.push((
              hir::RefutablePatExpr {
                source,
                id: next_id(idgen),
                kind: hir::RefutablePatExprKind::ValPat(ha.alloc(hir::ValPatExpr {
                  source,
                  id: next_id(idgen),
                  kind: hir::ValPatExprKind::Value(ha.alloc(gen_bool(idgen, source, true))),
                })),
              },
              tr_val_expr(idgen, ha, dlogger, right_operand, var_env, label_env),
            ));

            // false path
            v.push((
              hir::RefutablePatExpr {
                source,
                id: next_id(idgen),
                kind: hir::RefutablePatExprKind::ValPat(ha.alloc(hir::ValPatExpr {
                  source,
                  id: next_id(idgen),
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
        id: next_id(idgen),
        kind: hir::ValExprKind::CaseOf {
          expr: ha.alloc(tr_place_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          source: hir::CaseSource::Or,
          case_options: {
            let mut v = Vec::new_in(ha);
            // true path
            v.push((
              hir::RefutablePatExpr {
                source,
                id: next_id(idgen),
                kind: hir::RefutablePatExprKind::ValPat(ha.alloc(hir::ValPatExpr {
                  source,
                  id: next_id(idgen),
                  kind: hir::ValPatExprKind::Value(ha.alloc(gen_bool(idgen, source, true))),
                })),
              },
              gen_bool(idgen, source, true),
            ));

            // false path
            v.push((
              hir::RefutablePatExpr {
                source,
                id: next_id(idgen),
                kind: hir::RefutablePatExprKind::ValPat(ha.alloc(hir::ValPatExpr {
                  source,
                  id: next_id(idgen),
                  kind: hir::ValPatExprKind::Value(ha.alloc(gen_bool(idgen, source, false))),
                })),
              },
              tr_val_expr(idgen, ha, dlogger, right_operand, var_env, label_env),
            ));
            v
          },
        },
      },
      ast::BinaryOpKind::Equal => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_eq", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::NotEqual => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_neq", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Less => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_l", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::LessEqual => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_le", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Greater => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_g", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::GreaterEqual => gen_app(
        idgen,
        ha,
        source,
        gen_var_take(idgen, b"_ge", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        ],
      ),
      ast::BinaryOpKind::Cons => hir::ValExpr {
        id: next_id(idgen),
        source,
        kind: hir::ValExprKind::Pair {
          fst: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            label_env,
          )),
          snd: ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            right_operand,
            var_env,
            label_env,
          )),
        },
      },
      ast::BinaryOpKind::PlusAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let (pat, _) = tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env);

        // create val by adding together
        let val = gen_app(
          idgen,
          ha,
          source,
          gen_var_take(idgen, b"_add", dlogger, source, var_env, ha),
          vec![
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              left_operand,
              var_env,
              label_env,
            )),
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              right_operand,
              var_env,
              label_env,
            )),
          ],
        );

        hir::ValExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              id: next_id(idgen),
              kind: hir::ValExprKind::Nil,
            }),
          },
        }
      }
      ast::BinaryOpKind::MinusAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let (pat, _) = tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env);

        // create val by adding together
        let val = gen_app(
          idgen,
          ha,
          source,
          gen_var_take(idgen, b"_sub", dlogger, source, var_env, ha),
          vec![
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              left_operand,
              var_env,
              label_env,
            )),
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              right_operand,
              var_env,
              label_env,
            )),
          ],
        );

        hir::ValExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              id: next_id(idgen),
              kind: hir::ValExprKind::Nil,
            }),
          },
        }
      }
      ast::BinaryOpKind::MulAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let (pat, _) = tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env);

        // create val by adding together
        let val = gen_app(
          idgen,
          ha,
          source,
          gen_var_take(idgen, b"_mul", dlogger, source, var_env, ha),
          vec![
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              left_operand,
              var_env,
              label_env,
            )),
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              right_operand,
              var_env,
              label_env,
            )),
          ],
        );

        hir::ValExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              id: next_id(idgen),
              kind: hir::ValExprKind::Nil,
            }),
          },
        }
      }
      ast::BinaryOpKind::DivAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let (pat, _) = tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env);

        // create val by adding together
        let val = gen_app(
          idgen,
          ha,
          source,
          gen_var_take(idgen, b"_div", dlogger, source, var_env, ha),
          vec![
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              left_operand,
              var_env,
              label_env,
            )),
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              right_operand,
              var_env,
              label_env,
            )),
          ],
        );

        hir::ValExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              id: next_id(idgen),
              kind: hir::ValExprKind::Nil,
            }),
          },
        }
      }
      ast::BinaryOpKind::RemAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let (pat, _) = tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env);

        // create val by adding together
        let val = gen_app(
          idgen,
          ha,
          source,
          gen_var_take(idgen, b"_rem", dlogger, source, var_env, ha),
          vec![
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              left_operand,
              var_env,
              label_env,
            )),
            ha.alloc(tr_val_expr(
              idgen,
              ha,
              dlogger,
              right_operand,
              var_env,
              label_env,
            )),
          ],
        );

        hir::ValExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              id: next_id(idgen),
              kind: hir::ValExprKind::Nil,
            }),
          },
        }
      }
      ast::BinaryOpKind::Assign => {
        // this represents an assign at the tail end of an expression.
        // variables created in such assigns will not be available outside, and should warn about unused vars

        // translate the rhs
        let val = tr_val_expr(idgen, ha, dlogger, right_operand, var_env, label_env);

        // translate pattern (dropping vars, since they won't be used)
        // later, a warning will be sent in liveness checking
        let (pat, _) = tr_irrefutable_pat_expr(idgen, ha, dlogger, left_operand, var_env);

        hir::ValExpr {
          source,
          id: next_id(idgen),
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              id: next_id(idgen),
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
          let val = tr_val_expr(idgen, ha, dlogger, assign_value, var_env, label_env);

          // translate pattern and get bindings
          let (pat, bindings) = tr_irrefutable_pat_expr(idgen, ha, dlogger, assign_pat, var_env);

          // get old length
          let old_len = var_env.len();

          // extend env with new bindings
          var_env.extend(bindings);

          // parse body in the amended environment
          let body = tr_val_expr(idgen, ha, dlogger, right_operand, var_env, label_env);

          // now drop bindings
          var_env.truncate(old_len);

          hir::ValExpr {
            source,
            id: next_id(idgen),
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
            id: next_id(idgen),
            kind: hir::ValExprKind::Sequence {
              fst: ha.alloc(tr_val_expr(
                idgen,
                ha,
                dlogger,
                left_operand,
                var_env,
                label_env,
              )),
              snd: ha.alloc(tr_val_expr(
                idgen,
                ha,
                dlogger,
                right_operand,
                var_env,
                label_env,
              )),
            },
          }
        }
      }
      ast::BinaryOpKind::ModuleAccess => gen_take(
        idgen,
        source,
        tr_place_expr(idgen, ha, dlogger, source, var_env, label_env),
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
  label_env: &mut Vec<(&'ast [u8], LabelScope<'ast>)>,
) -> hir::PlaceExpr<'hir, 'ast, &'hir Bump> {
  match &source.kind {
    ast::ExprKind::Identifier(ref identifier) => {
      gen_var_place(source, identifier, dlogger, var_env)
    }
    ast::ExprKind::Group(ref expr) => tr_place_expr(idgen, ha, dlogger, expr, var_env, label_env),
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::ModuleAccess => {
        if let ast::Expr {
          kind: ast::ExprKind::Identifier(ref field),
          ..
        } = **right_operand
        {
          hir::PlaceExpr {
            source,
            kind: hir::PlaceExprKind::StructField {
              root: ha.alloc(tr_place_expr(
                idgen,
                ha,
                dlogger,
                left_operand,
                var_env,
                label_env,
              )),
              field,
              field_source: right_operand,
            },
          }
        } else {
          dlogger.log_field_not_identifier(right_operand.range, &right_operand.kind);
          hir::PlaceExpr {
            source,
            kind: hir::PlaceExprKind::Error,
          }
        }
      }
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
    ast::ExprKind::UnaryOp {
      ref op,
      ref operand,
    } => match op {
      ast::UnaryOpKind::Deref => hir::PlaceExpr {
        source: operand,
        kind: hir::PlaceExprKind::Deref(
          ha.alloc(tr_val_expr(idgen, ha, dlogger, operand, var_env, label_env)),
        ),
      },
      e => {
        // throw error about unrecognized unaryoperation
        dlogger.log_unexpected_unop_in_place_expression(source.range, e);
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
    &mut vec![],
  )
}
