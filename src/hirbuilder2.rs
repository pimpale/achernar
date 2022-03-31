use super::ast;
use super::dlogger::DiagnosticLogger;
use super::find_free_vars::find_free_vars;
use super::hir;
use bumpalo::Bump;
use num_bigint::BigUint;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

struct VarScope<'ast> {
  declaration: &'ast ast::Expr,
}

struct CapturedVarScope<'ast> {
  original_declaration: &'ast ast::Expr,
  captured_at: &'ast ast::Expr,
  capture_kind: hir::UseKind,
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

fn try_get_declaration_place_from_identifier<'hir, 'ast>(
  source: &'ast ast::Expr,
  identifier: &[u8],
  var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
  captured_var_env: &Vec<(&'ast [u8], CapturedVarScope<'ast>)>,
) -> Option<(&'ast ast::Expr, hir::PlaceExpr<'hir, 'ast, &'hir Bump>)> {
  // first try to lookup the var in the local enviroment
  if let Some((VarScope { declaration }, debruijn_index)) = lookup_maybe(var_env, &identifier) {
    return Some((
      declaration,
      hir::PlaceExpr {
        source,
        kind: hir::PlaceExprKind::Var(debruijn_index),
      },
    ));
  }

  // if that failed then we try to look up inside the captured vars
  // Captured vars are in a struct that gets passed along with the data
  if let Some((
    CapturedVarScope {
      original_declaration,
      ..
    },
    debruijn_index,
  )) = lookup_maybe(captured_var_env, &identifier)
  {
    // we return a variable access to a field of the captured env struct
    return Some((
      original_declaration,
      hir::PlaceExpr {
        source,
        kind: hir::PlaceExprKind::CapturedVar(debruijn_index),
      },
    ));
  }

  return None;
}

// Generates a place referencing a var
// It can handle if the var is captured
fn gen_place_from_identifier<'hir, 'ast>(
  source: &'ast ast::Expr,
  dlogger: &mut DiagnosticLogger,
  identifier: &[u8],
  var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
  captured_var_env: &Vec<(&'ast [u8], CapturedVarScope<'ast>)>,
) -> hir::PlaceExpr<'hir, 'ast, &'hir Bump> {
  match try_get_declaration_place_from_identifier(source, identifier, var_env, captured_var_env) {
    Some((_, place)) => place,
    None => {
      // return error if the identifier doesn't exist in either the variable or the captured variable
      dlogger.log_variable_not_found(source.range, &identifier);
      hir::PlaceExpr {
        source,
        kind: hir::PlaceExprKind::Error,
      }
    }
  }
}

fn parse_struct_fields<'ast>(
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
            kind: ast::ExprKind::Bind(ref identifier),
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
  captured_var_env: &Vec<(&'ast [u8], CapturedVarScope<'ast>)>,
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
      let patterns = parse_struct_fields(dlogger, body);

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
  captured_var_env: &Vec<(&'ast [u8], CapturedVarScope<'ast>)>,
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
    // dont need groups
    ast::ExprKind::Group(ref body) => {
      tr_refutable_pat_expr(idgen, ha, dlogger, body, var_env, captured_var_env)
    }
    ast::ExprKind::StructLiteral(ref body) => {
      let patterns = parse_struct_fields(dlogger, body);

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
  captured_var_env: &Vec<(&'ast [u8], CapturedVarScope<'ast>)>,
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

      for (id, pat_src) in parse_struct_fields(dlogger, body).into_iter() {
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
  captured_var_env: &Vec<(&'ast [u8], CapturedVarScope<'ast>)>,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  match source.kind {
    ast::ExprKind::Error => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::Error,
    },
    ref c @ (ast::ExprKind::Ref | ast::ExprKind::UniqRef | ast::ExprKind::Deref) => {
      dlogger.log_only_in_field(source.range, c);

      hir::ValExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValExprKind::Error,
      }
    }
    ref c @ (ast::ExprKind::Val(_) | ast::ExprKind::Bind(_)) => {
      dlogger.log_only_in_pattern(source.range, c);

      hir::ValExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValExprKind::Error,
      }
    }
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

      for (id, pat) in parse_struct_fields(dlogger, body).into_iter() {
        let field_pat = tr_val_expr(idgen, ha, dlogger, pat, var_env, captured_var_env);
        fields.push((id, (pat, field_pat)));
      }

      hir::ValExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValExprKind::Struct(fields),
      }
    }
    // this option will only ever be invoked if we have a
    // bare identifier without a reference
    // In this case we are using it
    ast::ExprKind::Identifier(_) => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::Use(
        // generate the place
        ha.alloc(tr_place_expr(
          idgen,
          ha,
          dlogger,
          source,
          var_env,
          captured_var_env,
        )),
        // we take ownership of the value at this place
        hir::UseKind::Take,
      ),
    },
    ast::ExprKind::Builtin(_) => hir::ValExpr {
      source,
      id: Some(next_id(idgen)),
      kind: hir::ValExprKind::Use(
        // generate the place
        ha.alloc(tr_place_expr(
          idgen,
          ha,
          dlogger,
          source,
          var_env,
          captured_var_env,
        )),
        // we take ownership of the value at this place
        hir::UseKind::Take,
      ),
    },
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
    ast::ExprKind::Struct(ref body) => match body.kind {
      ast::ExprKind::StructLiteral(ref fields_src) => {
        let mut fields = Vec::new_in(ha);

        for (id, pat) in parse_struct_fields(dlogger, fields_src).into_iter() {
          let field_pat = tr_val_expr(idgen, ha, dlogger, pat, var_env, captured_var_env);
          fields.push((id, (pat, field_pat)));
        }

        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::StructTy(fields),
        }
      }
      ref kind => {
        dlogger.log_expected_struct_literal_struct(body.range, kind);
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Error,
        }
      }
    },
    ast::ExprKind::Enum(ref body) => match body.kind {
      ast::ExprKind::StructLiteral(ref fields_src) => {
        let mut fields = Vec::new_in(ha);

        for (id, pat) in parse_struct_fields(dlogger, fields_src).into_iter() {
          let field_pat = tr_val_expr(idgen, ha, dlogger, pat, var_env, captured_var_env);
          fields.push((id, (pat, field_pat)));
        }

        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::EnumTy(fields),
        }
      }
      ref kind => {
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
        // Find free variables inside the the body of the function we have encountered
        let free_vars = find_free_vars(right_operand);

        // the variables we will provide to the lambda
        let mut lam_captured_vars = Vec::new_in(ha);
        // the captured variables enviroment we will provide to the lower structs
        let mut new_captured_vars_env = Vec::new();

        // For every free variable found, see if we can capture it from our enviroment
        for (identifier, max_use_kind) in free_vars.into_iter() {
          if let Some((original_declaration, place)) =
            try_get_declaration_place_from_identifier(source, identifier, var_env, captured_var_env)
          {
            let rhs = hir::ValExpr {
              source: source,
              id: Some(next_id(idgen)),
              kind: hir::ValExprKind::Use(ha.alloc(place), max_use_kind),
            };

            lam_captured_vars.push((identifier, (source, rhs)));

            new_captured_vars_env.push((
              identifier,
              CapturedVarScope {
                original_declaration,
                captured_at: source,
                capture_kind: max_use_kind,
              },
            ));
          }
          // if we end up not being able to capture it, it'll cause an error later
        }

        // For all fields we could capture, we create a declaration for it so that the function's body knows which variables are in scope

        // translate binding pattern. there's no var_env yet,
        // but we can still get variables from captured_var_env
        let (pat, mut bindings) = tr_irrefutable_pat_expr(
          idgen,
          ha,
          dlogger,
          left_operand,
          &mut vec![],
          &new_captured_vars_env,
        );

        // translate body. use the bound variables from the pat as the var_env
        // use the captured var fields as the
        let body = tr_val_expr(
          idgen,
          ha,
          dlogger,
          right_operand,
          &mut bindings,
          &new_captured_vars_env,
        );

        // return expr
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Lam {
            captured_vars: lam_captured_vars,
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
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Use(
            // generate the place
            ha.alloc(gen_place_from_identifier(
              source,
              dlogger,
              b"_compose",
              var_env,
              captured_var_env,
            )),
            // we take ownership of the value at this place
            hir::UseKind::Take,
          ),
        },
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
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Use(
            // generate the place
            ha.alloc(gen_place_from_identifier(
              source,
              dlogger,
              b"_add",
              var_env,
              captured_var_env,
            )),
            // we take ownership of the value at this place
            hir::UseKind::Take,
          ),
        },
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
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(
              source,
              dlogger,
              b"_sub",
              var_env,
              captured_var_env,
            )),
            hir::UseKind::Take,
          ),
        },
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
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(
              source,
              dlogger,
              b"_mul",
              var_env,
              captured_var_env,
            )),
            hir::UseKind::Take,
          ),
        },
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
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(
              source,
              dlogger,
              b"_div",
              var_env,
              captured_var_env,
            )),
            hir::UseKind::Take,
          ),
        },
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
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(
              source,
              dlogger,
              b"_rem",
              var_env,
              captured_var_env,
            )),
            hir::UseKind::Take,
          ),
        },
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
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(
              source,
              dlogger,
              b"_eq",
              var_env,
              captured_var_env,
            )),
            hir::UseKind::Take,
          ),
        },
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
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(
              source,
              dlogger,
              // TODO: apply logical law here
              b"_neq",
              var_env,
              captured_var_env,
            )),
            hir::UseKind::Take,
          ),
        },
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
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(
              source,
              dlogger,
              b"_l",
              var_env,
              captured_var_env,
            )),
            hir::UseKind::Take,
          ),
        },
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
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(
              source,
              dlogger,
              b"_le",
              var_env,
              captured_var_env,
            )),
            hir::UseKind::Take,
          ),
        },
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
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(
              source,
              dlogger,
              b"_g",
              var_env,
              captured_var_env,
            )),
            hir::UseKind::Take,
          ),
        },
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
        hir::ValExpr {
          source,
          id: Some(next_id(idgen)),
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(
              source,
              dlogger,
              b"_ge",
              var_env,
              captured_var_env,
            )),
            hir::UseKind::Take,
          ),
        },
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
          hir::ValExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::ValExprKind::Use(
              ha.alloc(gen_place_from_identifier(
                source,
                dlogger,
                b"_add",
                var_env,
                captured_var_env,
              )),
              hir::UseKind::Take,
            ),
          },
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
          hir::ValExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::ValExprKind::Use(
              ha.alloc(gen_place_from_identifier(
                source,
                dlogger,
                b"_sub",
                var_env,
                captured_var_env,
              )),
              hir::UseKind::Take,
            ),
          },
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
          hir::ValExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::ValExprKind::Use(
              ha.alloc(gen_place_from_identifier(
                source,
                dlogger,
                b"_mul",
                var_env,
                captured_var_env,
              )),
              hir::UseKind::Take,
            ),
          },
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
          hir::ValExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::ValExprKind::Use(
              ha.alloc(gen_place_from_identifier(
                source,
                dlogger,
                b"_div",
                var_env,
                captured_var_env,
              )),
              hir::UseKind::Take,
            ),
          },
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
          hir::ValExpr {
            source,
            id: Some(next_id(idgen)),
            kind: hir::ValExprKind::Use(
              ha.alloc(gen_place_from_identifier(
                source,
                dlogger,
                b"_rem",
                var_env,
                captured_var_env,
              )),
              hir::UseKind::Take,
            ),
          },
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
      ast::BinaryOpKind::ModuleAccess => hir::ValExpr {
        source,
        id: Some(next_id(idgen)),
        kind: hir::ValExprKind::Use(
          ha.alloc(tr_place_expr(
            idgen,
            ha,
            dlogger,
            source,
            var_env,
            captured_var_env,
          )),
          hir::UseKind::Take,
        ),
      },
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
  captured_var_env: &Vec<(&'ast [u8], CapturedVarScope<'ast>)>,
) -> hir::PlaceExpr<'hir, 'ast, &'hir Bump> {
  match &source.kind {
    ast::ExprKind::Identifier(ref identifier) => {
      gen_place_from_identifier(source, dlogger, identifier, var_env, captured_var_env)
    }
    ast::ExprKind::Builtin(ref builtin) => {

      fn is_valid_int_ty(s: &[u8]) -> bool {
        match s {
          b"U8" => true,
          b"U16" => true,
          b"U32" => true,
          b"U64" => true,
          b"I8" => true,
          b"I16" => true,
          b"I32" => true,
          b"I64" => true,
          _ => false,
        }
      }

      fn is_valid_float_ty(s: &[u8]) -> bool {
        match s {
          b"F32" => true,
          b"F64" => true,
          _ => false,
        }
      }

      fn is_valid_int_op(s: &[u8]) -> bool {
        match s {
          b"add" => true,
          b"add_overflow" => true,
          b"sub" => true,
          b"sub_overflow" => true,
          b"mul" => true,
          b"mul_overflow" => true,
          b"div" => true,
          b"rem" => true,
          b"div_rem" => true,
          b"shl_l" => true,
          b"shr_l" => true,
          b"shr_a" => true,
          b"rol" => true,
          b"ror" => true,
          b"and" => true,
          b"or" => true,
          b"xor" => true,
          b"inv" => true,
          b"neg" => true,
          _ => false,
        }
      }

      fn is_valid_float_op(s: &[u8]) -> bool {
        match s {
          b"add" => true,
          b"sub" => true,
          b"mul" => true,
          b"div" => true,
          b"rem" => true,
          b"div_rem" => true,
          b"neg" => true,
          _ => false,
        }
      }

      fn parse_valid_int_ty(s: &[u8]) -> hir::IntTy {
        match s {
          b"U8" => hir::IntTy {
            signed: false,
            size: 8,
          },
          b"U16" => hir::IntTy {
            signed: false,
            size: 16,
          },
          b"U32" => hir::IntTy {
            signed: false,
            size: 32,
          },
          b"U64" => hir::IntTy {
            signed: false,
            size: 64,
          },
          b"I8" => hir::IntTy {
            signed: false,
            size: 8,
          },
          b"I16" => hir::IntTy {
            signed: false,
            size: 16,
          },
          b"I32" => hir::IntTy {
            signed: false,
            size: 32,
          },
          b"I64" => hir::IntTy {
            signed: false,
            size: 64,
          },
          _ => unreachable!(),
        }
      }

      fn parse_valid_float_ty(s: &[u8]) -> hir::FloatTy {
        match s {
          b"F32" => hir::FloatTy { size: 32 },
          b"F64" => hir::FloatTy { size: 64 },
          _ => unreachable!(),
        }
      }

      fn parse_valid_int_op(s: &[u8]) -> hir::IntOp {
        match s {
          b"add" => hir::IntOp::Add,
          b"add_overflow" => hir::IntOp::AddOverflow,
          b"sub" => hir::IntOp::Sub,
          b"sub_overflow" => hir::IntOp::SubOverflow,
          b"mul" => hir::IntOp::Mul,
          b"mul_overflow" => hir::IntOp::MulOverflow,
          b"div" => hir::IntOp::Div,
          b"rem" => hir::IntOp::Rem,
          b"div_rem" => hir::IntOp::DivRem,
          b"shl_l" => hir::IntOp::ShlL,
          b"shr_l" => hir::IntOp::ShrL,
          b"shr_a" => hir::IntOp::ShrA,
          b"rol" => hir::IntOp::Rol,
          b"ror" => hir::IntOp::Ror,
          b"and" => hir::IntOp::And,
          b"or" => hir::IntOp::Or,
          b"xor" => hir::IntOp::Xor,
          b"inv" => hir::IntOp::Inv,
          b"neg" => hir::IntOp::Neg,
          _ => unreachable!(),
        }
      }

      fn parse_valid_float_op(s: &[u8]) -> hir::FloatOp {
        match s {
          b"add" => hir::FloatOp::Add,
          b"sub" => hir::FloatOp::Sub,
          b"mul" => hir::FloatOp::Mul,
          b"div" => hir::FloatOp::Div,
          b"rem" => hir::FloatOp::Rem,
          b"div_rem" => hir::FloatOp::DivRem,
          b"neg" => hir::FloatOp::Neg,
          _ => unreachable!(),
        }
      }

      let maybe_builtin = match builtin.split(|&x| x == b'_').collect::<Vec<_>>().as_slice() {
        [b"Unit"] => Some(hir::Builtin::UnitTy),
        [b"Never"] => Some(hir::Builtin::NeverTy),
        [b"Bool"] => Some(hir::Builtin::BoolTy),
        [b"not"] => Some(hir::Builtin::BoolNot),
        [s] if is_valid_int_ty(s) => Some(hir::Builtin::IntTy(parse_valid_int_ty(s))),
        [s] if is_valid_float_ty(s) => Some(hir::Builtin::FloatTy(parse_valid_float_ty(s))),
        [os, ts] if is_valid_int_op(os) && is_valid_int_ty(ts) => Some(hir::Builtin::IntOp {
          ty: parse_valid_int_ty(ts),
          op: parse_valid_int_op(os),
        }),
        [os, ts] if is_valid_float_op(os) && is_valid_float_ty(ts) => Some(hir::Builtin::FloatOp {
          ty: parse_valid_float_ty(ts),
          op: parse_valid_float_op(os),
        }),
        [b"conv", t1, t2] if is_valid_int_ty(t1) && is_valid_float_ty(t2) => {
          Some(hir::Builtin::ConvIntFloatOp {
            src: parse_valid_int_ty(t1),
            dest: parse_valid_float_ty(t2),
          })
        }
        [b"conv", t1, t2] if is_valid_float_ty(t1) && is_valid_int_ty(t2) => {
          Some(hir::Builtin::ConvFloatIntOp {
            src: parse_valid_float_ty(t1),
            dest: parse_valid_int_ty(t2),
          })
        }
        [b"conv", t1, t2] if is_valid_int_ty(t1) && is_valid_int_ty(t2) => {
          Some(hir::Builtin::ConvIntIntOp {
            src: parse_valid_int_ty(t1),
            dest: parse_valid_int_ty(t2),
          })
        }
        [b"conv", t1, t2] if is_valid_float_ty(t1) && is_valid_float_ty(t2) => {
          Some(hir::Builtin::ConvFloatFloatOp {
            src: parse_valid_float_ty(t1),
            dest: parse_valid_float_ty(t2),
          })
        }
        _ => None,
      };

      // now return builtin if found
      match maybe_builtin {
        Some(x) => hir::PlaceExpr {
          source,
          kind: hir::PlaceExprKind::Builtin(x),
        },
        None => hir::PlaceExpr {
          source,
          kind: hir::PlaceExprKind::Error,
        },
      }
    }
    ast::ExprKind::Group(ref expr) => {
      tr_place_expr(idgen, ha, dlogger, expr, var_env, captured_var_env)
    }
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::ModuleAccess => match right_operand.kind {
        ast::ExprKind::Identifier(ref field) => hir::PlaceExpr {
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
        ast::ExprKind::Deref => hir::PlaceExpr {
          source,
          // A place can be the dereference of a pointer.
          // In that case we evaluate the struct_root and then dereference it
          kind: hir::PlaceExprKind::Deref(ha.alloc(tr_val_expr(
            idgen,
            ha,
            dlogger,
            left_operand,
            var_env,
            captured_var_env,
          ))),
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
  // first insert the necessary_builtins into the prelude
  tr_val_expr(
    &RefCell::new(0),
    ha,
    &mut dlogger,
    ast,
    &mut vec![],
    &vec![],
  )
}
