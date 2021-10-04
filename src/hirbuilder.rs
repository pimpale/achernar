use super::ast;
use super::dlogger::DiagnosticLogger;
use super::hir;
use super::utils::new_vec_from;
use bumpalo::Bump;
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
  lookup_maybe(env, label).unwrap().0
}

fn lookup_exists<'env, Scope>(env: &'env Vec<(&[u8], Scope)>, label: &[u8]) -> bool {
  lookup_maybe(env, label).is_some()
}

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

fn gen_apply_fn<'hir, 'ast>(
  ha: &'hir Bump,
  source: &'ast ast::Expr,
  fun: hir::Expr<'hir, 'ast, &'hir Bump>,
  args: Vec<&'hir hir::Expr<'hir, 'ast, &'hir Bump>>,
) -> hir::Expr<'hir, 'ast, &'hir Bump> {
  args.iter().fold(fun, |acc, x| hir::Expr {
    source,
    kind: hir::ExprKind::Apply {
      fun: ha.alloc(acc),
      arg: x,
    },
  })
}

fn gen_bool<'hir, 'ast>(source: &'ast ast::Expr, b: bool) -> hir::Expr<'hir, 'ast, &'hir Bump> {
  hir::Expr {
    source: source,
    kind: hir::ExprKind::Bool(b),
  }
}

fn gen_nil<'hir, 'ast>(source: &'ast ast::Expr) -> hir::Expr<'hir, 'ast, &'hir Bump> {
  hir::Expr {
    source: source,
    kind: hir::ExprKind::Nil,
  }
}

fn gen_var<'hir, 'ast>(
  identifier: &[u8],
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
) -> hir::Expr<'hir, 'ast, &'hir Bump> {
  let lkup_val = lookup_maybe(var_env, &identifier);

  if let Some((_, debruijn_index)) = lkup_val {
    hir::Expr {
      source,
      kind: hir::ExprKind::TakeVar(debruijn_index),
    }
  } else {
    dlogger.log_variable_not_found(source.range, &identifier);
    // return error if not exist
    hir::Expr {
      source,
      kind: hir::ExprKind::Error,
    }
  }
}

fn tr_pat<'hir, 'ast>(
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  var_env: &mut Vec<(&'ast [u8], VarScope<'ast>)>,
) -> hir::Pat<'hir, 'ast, &'hir Bump> {
  match source.kind {
    // propagate error
    ast::ExprKind::Error => hir::Pat {
      source,
      kind: hir::PatKind::Error,
    },
    // transparent valueification
    ast::ExprKind::Nil
    | ast::ExprKind::Bool(_)
    | ast::ExprKind::Int(_)
    | ast::ExprKind::Float(_)
    | ast::ExprKind::String { .. } => hir::Pat {
      source,
      kind: hir::PatKind::Value(ha.alloc(tr_expr(ha, dlogger, source, var_env, &mut vec![]))),
    },
    // reject use in pattern without explicit `val`
    ref
    c
    @
    (ast::ExprKind::Label { .. }
    | ast::ExprKind::Reference(_)
    | ast::ExprKind::Defer { .. }
    | ast::ExprKind::Ret { .. }
    | ast::ExprKind::CaseOf { .. }) => {
      dlogger.log_unexpected_in_pattern(source.range, c);
      hir::Pat {
        source,
        kind: hir::PatKind::Error,
      }
    }
    // elide groups
    ast::ExprKind::Group(ref body) => tr_pat(ha, dlogger, body, var_env),
    ast::ExprKind::StructLiteral(ref body) => {
      // create a struct of literals
      let mut patterns = HashMap::new();

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
                  kind: ast::ExprKind::Reference(ref identifier),
                  range,
                  ..
                } => match patterns.entry(identifier) {
                  Entry::Vacant(ve) => {
                    // identifier unique, translate rhs and insert into map
                    ve.insert(tr_pat(ha, dlogger, right_operand, var_env));
                  }
                  Entry::Occupied(oe) => {
                    // identifier not unique, log error for using duplicate identifier in struct
                    dlogger.log_duplicate_field_name(*range, &oe.key(), oe.get().source.range);
                  }
                },

                // means that something other than a reference was the target of the bind
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
      // return struct
      hir::Pat {
        source,
        kind: hir::PatKind::StructLiteral(new_vec_from(ha, patterns.drain())),
      }
    }
    ast::ExprKind::UnaryOp {
      ref op,
      ref operand,
    } => match op {
      ast::UnaryOpKind::Posit => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          fun: ha.alloc(gen_var(b"_posit", dlogger, source, var_env)),
          arg: ha.alloc(tr_pat(ha, dlogger, operand, var_env)),
        },
      },
      ast::UnaryOpKind::Negate => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          fun: ha.alloc(gen_var(b"_negate", dlogger, source, var_env)),
          arg: ha.alloc(tr_pat(ha, dlogger, operand, var_env)),
        },
      },
      ast::UnaryOpKind::Ref => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          fun: ha.alloc(gen_var(b"_ref", dlogger, source, var_env)),
          arg: ha.alloc(tr_pat(ha, dlogger, operand, var_env)),
        },
      },
      ast::UnaryOpKind::Deref => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          fun: ha.alloc(gen_var(b"_ref", dlogger, source, var_env)),
          arg: ha.alloc(tr_pat(ha, dlogger, operand, var_env)),
        },
      },
      ast::UnaryOpKind::Not => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          fun: ha.alloc(gen_var(b"_not", dlogger, source, var_env)),
          arg: ha.alloc(tr_pat(ha, dlogger, operand, var_env)),
        },
      },
      ast::UnaryOpKind::Complement => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          fun: ha.alloc(gen_var(b"_complement", dlogger, source, var_env)),
          arg: ha.alloc(tr_pat(ha, dlogger, operand, var_env)),
        },
      },
      ast::UnaryOpKind::Val => hir::Pat {
        source,
        kind: hir::PatKind::Value(ha.alloc(tr_expr(ha, dlogger, operand, var_env, &mut vec![]))),
      },
      ast::UnaryOpKind::Bind => match **operand {
        // binds a variable to an identifier
        ast::Expr {
          kind: ast::ExprKind::Reference(ref identifier), //TODO: we need to bind here
          ..
        } => hir::Pat {
          source,
          kind: hir::PatKind::BindVariable,
        },
        // handle error
        ast::Expr {
          range, ref kind, ..
        } => {
          dlogger.log_unexpected_bind_target(range, kind);
          hir::Pat {
            source,
            kind: hir::PatKind::Error,
          }
        }
      },
      // these operators must be valified
      c
      @
      (ast::UnaryOpKind::ReturnOnError
      | ast::UnaryOpKind::Struct
      | ast::UnaryOpKind::Enum
      | ast::UnaryOpKind::Loop) => {
        dlogger.log_unexpected_unop_in_pattern(source.range, c);
        hir::Pat {
          source,
          kind: hir::PatKind::Error,
        }
      }
    },
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      c
      @
      (ast::BinaryOpKind::Defun
      | ast::BinaryOpKind::CaseOption
      | ast::BinaryOpKind::Compose
      | ast::BinaryOpKind::PipeForward
      | ast::BinaryOpKind::PipeBackward
      | ast::BinaryOpKind::Add
      | ast::BinaryOpKind::Sub
      | ast::BinaryOpKind::Mul
      | ast::BinaryOpKind::Div
      | ast::BinaryOpKind::Rem
      | ast::BinaryOpKind::Pow
      | ast::BinaryOpKind::Equal
      | ast::BinaryOpKind::NotEqual
      | ast::BinaryOpKind::Less
      | ast::BinaryOpKind::LessEqual
      | ast::BinaryOpKind::Greater
      | ast::BinaryOpKind::GreaterEqual
      | ast::BinaryOpKind::RelativeComplement
      | ast::BinaryOpKind::Union
      | ast::BinaryOpKind::Intersection
      | ast::BinaryOpKind::SymmetricDifference
      | ast::BinaryOpKind::In
      | ast::BinaryOpKind::Assign
      | ast::BinaryOpKind::Sequence
      | ast::BinaryOpKind::As
      | ast::BinaryOpKind::Append
      | ast::BinaryOpKind::SuchThat
      | ast::BinaryOpKind::ModuleAccess) => {
        dlogger.log_unexpected_binop_in_pattern(source.range, c);
        hir::Pat {
          source,
          kind: hir::PatKind::Error,
        }
      }
      ast::BinaryOpKind::Constrain => hir::Pat {
        source,
        kind: hir::PatKind::Annotate {
          pattern: ha.alloc(tr_pat(ha, dlogger, left_operand, var_env)),
          ty: ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, &mut vec![])),
        },
      },
      // TODO warn style
      ast::BinaryOpKind::RevConstrain => hir::Pat {
        source,
        kind: hir::PatKind::Annotate {
          // swap the expressions
          ty: ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, &mut vec![])),
          pattern: ha.alloc(tr_pat(ha, dlogger, right_operand, var_env)),
        },
      },
      ast::BinaryOpKind::Apply => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          fun: ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, &mut vec![])),
          arg: ha.alloc(tr_pat(ha, dlogger, right_operand, var_env)),
        },
      },
      ast::BinaryOpKind::RevApply => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          // swap the expressions
          arg: ha.alloc(tr_pat(ha, dlogger, left_operand, var_env)),
          fun: ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, &mut vec![])),
        },
      },
      ast::BinaryOpKind::And => hir::Pat {
        source,
        kind: hir::PatKind::And {
          left_operand: ha.alloc(tr_pat(ha, dlogger, left_operand, var_env)),
          right_operand: ha.alloc(tr_pat(ha, dlogger, right_operand, var_env)),
        },
      },
      ast::BinaryOpKind::Or => hir::Pat {
        source,
        kind: hir::PatKind::Or {
          left_operand: ha.alloc(tr_pat(ha, dlogger, left_operand, var_env)),
          right_operand: ha.alloc(tr_pat(ha, dlogger, right_operand, var_env)),
        },
      },
      ast::BinaryOpKind::Cons => hir::Pat {
        source,
        kind: hir::PatKind::Cons {
          fst: ha.alloc(tr_pat(ha, dlogger, left_operand, var_env)),
          snd: ha.alloc(tr_pat(ha, dlogger, right_operand, var_env)),
        },
      },
      ast::BinaryOpKind::Range => hir::Pat {
        source,
        kind: hir::PatKind::Range {
          inclusive: false,
          left_operand: ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, &mut vec![])),
          right_operand: ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, &mut vec![])),
        },
      },
      ast::BinaryOpKind::RangeInclusive => hir::Pat {
        source,
        kind: hir::PatKind::Range {
          inclusive: true,
          left_operand: ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, &mut vec![])),
          right_operand: ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, &mut vec![])),
        },
      },
    },
  }
}

fn tr_expr<'hir, 'ast>(
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  var_env: &mut Vec<(&'ast [u8], VarScope<'ast>)>,
  label_env: &mut Vec<(&'ast [u8], LabelScope<'ast>)>,
) -> hir::Expr<'hir, 'ast, &'hir Bump> {
  match source.kind {
    ast::ExprKind::Error => hir::Expr {
      source,
      kind: hir::ExprKind::Error,
    },
    ast::ExprKind::Nil => hir::Expr {
      source,
      kind: hir::ExprKind::Nil,
    },
    ast::ExprKind::Bool(b) => hir::Expr {
      source,
      kind: hir::ExprKind::Bool(b),
    },
    ast::ExprKind::Int(ref i) => hir::Expr {
      source,
      kind: hir::ExprKind::Int(i),
    },
    ast::ExprKind::Float(ref i) => hir::Expr {
      source,
      kind: hir::ExprKind::Float(i),
    },
    ast::ExprKind::String { ref value, .. } => value.iter().rev().fold(
      // start with a null at the end of the list
      hir::Expr {
        source,
        kind: hir::ExprKind::Nil,
      },
      // as we work our way backwards, we prepend the current char as an int to our list
      |acc, x| {
        hir::Expr {
          source,
          kind: hir::ExprKind::Cons {
            // first arg is the new expr for the int
            fst: ha.alloc(hir::Expr {
              source,
              kind: hir::ExprKind::Char(*x as u32), // TODO: parse strings by pure unicode
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
      let body_tr = tr_expr(ha, dlogger, body, var_env, label_env);

      // pop label
      let _ = label_env.pop().unwrap();

      // return label
      hir::Expr {
        source,
        kind: hir::ExprKind::Label(ha.alloc(body_tr)),
      }
    }
    ast::ExprKind::Group(ref body) => tr_expr(ha, dlogger, body, var_env, label_env),
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
        hir::Expr {
          source,
          kind: hir::ExprKind::Nil,
        }
      } else {
        // a label was never properly provided
        // an error should already have been thrown, so don't double report
        hir::Expr {
          source,
          kind: hir::ExprKind::Error,
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
        let body_tr = tr_expr(ha, dlogger, body, var_env, label_env);

        // now make the defer-ret construct
        // ret 'x 0
        // to
        // let toret = 0;
        // (run defers);
        // ret 'x toret

        hir::Expr {
          source,
          kind: hir::ExprKind::LetIn {
            // introduce variable with value of ret
            pat: ha.alloc(hir::Pat {
              source,
              kind: hir::PatKind::BindVariable,
            }),
            val: ha.alloc(body_tr),

            // write body
            body: lookup(&label_env, &label).defers.clone().iter().fold(
              // the last thing to execute is the actual ret
              ha.alloc(hir::Expr {
                source,
                kind: hir::ExprKind::Ret {
                  labels_up: lookup_count_up(label_env, &label),
                  // lookup variable introduced earlier
                  value: ha.alloc(hir::Expr {
                    source,
                    kind: hir::ExprKind::TakeVar(0), // we will grab the most recent bound one
                  }),
                },
              }),
              // closure
              |acc, x| {
                // translate defer
                let tr_x = tr_expr(ha, dlogger, x, var_env, label_env);
                // return sequenced defer
                ha.alloc(hir::Expr {
                  source: x,
                  // we want to sequence it in the reverse order we saw them
                  // so, later ones will be executed first
                  kind: hir::ExprKind::Sequence {
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
        hir::Expr {
          source,
          kind: hir::ExprKind::Error,
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
                  kind: ast::ExprKind::Reference(ref identifier),
                  range,
                  ..
                } => match fields.entry(identifier) {
                  Entry::Vacant(ve) => {
                    // identifier unique, translate rhs and insert into map
                    ve.insert((
                      operand.as_ref(),
                      tr_expr(ha, dlogger, right_operand, var_env, label_env),
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
      hir::Expr {
        source,
        kind: hir::ExprKind::StructLiteral(new_vec_from(ha, fields.drain())),
      }
    }
    ast::ExprKind::Reference(ref identifier) => gen_var(identifier, dlogger, source, var_env),
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
            ast::BinaryOpKind::Defun => case_options.push((
              tr_pat(ha, dlogger, left_operand, var_env),
              tr_expr(ha, dlogger, right_operand, var_env, label_env),
            )),
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
      hir::Expr {
        source,
        kind: hir::ExprKind::CaseOf {
          source: hir::CaseSource::Case,
          expr: ha.alloc(tr_expr(ha, dlogger, expr, var_env, label_env)),
          case_options,
        },
      }
    }
    ast::ExprKind::UnaryOp {
      ref op,
      ref operand,
    } => match op {
      ast::UnaryOpKind::Posit => gen_apply_fn(
        ha,
        source,
        gen_var(b"_posit", dlogger, source, var_env),
        vec![ha.alloc(tr_expr(ha, dlogger, operand, var_env, label_env))],
      ),
      ast::UnaryOpKind::Negate => gen_apply_fn(
        ha,
        source,
        gen_var(b"_negate", dlogger, source, var_env),
        vec![ha.alloc(tr_expr(ha, dlogger, operand, var_env, label_env))],
      ),
      ast::UnaryOpKind::Ref => gen_apply_fn(
        ha,
        source,
        gen_var(b"_ref", dlogger, source, var_env),
        vec![ha.alloc(tr_expr(ha, dlogger, operand, var_env, label_env))],
      ),
      ast::UnaryOpKind::Deref => gen_apply_fn(
        ha,
        source,
        gen_var(b"_deref", dlogger, source, var_env),
        vec![ha.alloc(tr_expr(ha, dlogger, operand, var_env, label_env))],
      ),
      ast::UnaryOpKind::Not => gen_apply_fn(
        ha,
        source,
        gen_var(b"_not", dlogger, source, var_env),
        vec![ha.alloc(tr_expr(ha, dlogger, operand, var_env, label_env))],
      ),
      ast::UnaryOpKind::Complement => gen_apply_fn(
        ha,
        source,
        gen_var(b"_complement", dlogger, source, var_env),
        vec![ha.alloc(tr_expr(ha, dlogger, operand, var_env, label_env))],
      ),
      // TODO decompose
      ast::UnaryOpKind::ReturnOnError => gen_apply_fn(
        ha,
        source,
        gen_var(b"_return_on_error", dlogger, source, var_env),
        vec![ha.alloc(tr_expr(ha, dlogger, operand, var_env, label_env))],
      ),
      ast::UnaryOpKind::Struct => hir::Expr {
        source,
        kind: hir::ExprKind::Struct(ha.alloc(tr_expr(ha, dlogger, operand, var_env, label_env))),
      },
      ast::UnaryOpKind::Enum => hir::Expr {
        source,
        kind: hir::ExprKind::Enum(ha.alloc(tr_expr(ha, dlogger, operand, var_env, label_env))),
      },
      ast::UnaryOpKind::Loop => hir::Expr {
        source,
        kind: hir::ExprKind::Loop(ha.alloc(tr_expr(ha, dlogger, operand, var_env, label_env))),
      },
      c @ ast::UnaryOpKind::Val => {
        dlogger.log_unexpected_unop_in_expr(source.range, c);
        hir::Expr {
          source,
          kind: hir::ExprKind::Error,
        }
      }
      c @ ast::UnaryOpKind::Bind => {
        dlogger.log_unexpected_unop_in_expr(source.range, c);
        hir::Expr {
          source,
          kind: hir::ExprKind::Error,
        }
      }
    },
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::Constrain => hir::Expr {
        source,
        kind: hir::ExprKind::Annotate {
          expr: ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ty: ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        },
      },
      ast::BinaryOpKind::RevConstrain => hir::Expr {
        source,
        kind: hir::ExprKind::Annotate {
          ty: ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          expr: ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        },
      },
      ast::BinaryOpKind::Defun => hir::Expr {
        source,
        kind: hir::ExprKind::Defun {
          pattern: ha.alloc(tr_pat(ha, dlogger, left_operand, var_env)),
          result: ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
          infer_pattern: false,
        },
      },
      ast::BinaryOpKind::CaseOption => {
        dlogger.log_only_in_case(source.range);
        hir::Expr {
          source,
          kind: hir::ExprKind::Error,
        }
      }
      ast::BinaryOpKind::Apply => hir::Expr {
        source,
        kind: hir::ExprKind::Apply {
          fun: ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          arg: ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        },
      },
      ast::BinaryOpKind::RevApply => hir::Expr {
        source,
        kind: hir::ExprKind::Apply {
          // swap the expressions
          fun: ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
          arg: ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
        },
      },
      ast::BinaryOpKind::Compose => gen_apply_fn(
        ha,
        source,
        gen_var(b"_compose", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::PipeForward => gen_apply_fn(
        ha,
        source,
        gen_var(b"_pipe_forward", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::PipeBackward => gen_apply_fn(
        ha,
        source,
        gen_var(b"_pipe_backward", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Add => gen_apply_fn(
        ha,
        source,
        gen_var(b"_add", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Sub => gen_apply_fn(
        ha,
        source,
        gen_var(b"_sub", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Mul => gen_apply_fn(
        ha,
        source,
        gen_var(b"_mul", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Div => gen_apply_fn(
        ha,
        source,
        gen_var(b"_div", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Rem => gen_apply_fn(
        ha,
        source,
        gen_var(b"_rem", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Pow => gen_apply_fn(
        ha,
        source,
        gen_var(b"_pow", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::And => hir::Expr {
        // a && b
        // case a
        // of true  = > b
        // || false = > false
        source,
        kind: hir::ExprKind::CaseOf {
          expr: ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          source: hir::CaseSource::And,
          case_options: {
            let mut v = Vec::new_in(ha);
            // true path
            v.push((
              hir::Pat {
                source,
                kind: hir::PatKind::Value(ha.alloc(gen_bool(source, true))),
              },
              tr_expr(ha, dlogger, right_operand, var_env, label_env),
            ));

            // false path
            v.push((
              hir::Pat {
                source,
                kind: hir::PatKind::Value(ha.alloc(gen_bool(source, false))),
              },
              gen_bool(source, false),
            ));
            v
          },
        },
      },
      ast::BinaryOpKind::Or => hir::Expr {
        // a || b
        // case a
        // of true => true
        // of false => b
        source,
        kind: hir::ExprKind::CaseOf {
          expr: ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          source: hir::CaseSource::Or,
          case_options: {
            let mut v = Vec::new_in(ha);
            // true path
            v.push((
              hir::Pat {
                source,
                kind: hir::PatKind::Value(ha.alloc(gen_bool(source, true))),
              },
              gen_bool(source, true),
            ));

            // false path
            v.push((
              hir::Pat {
                source,
                kind: hir::PatKind::Value(ha.alloc(gen_bool(source, false))),
              },
              tr_expr(ha, dlogger, right_operand, var_env, label_env),
            ));
            v
          },
        },
      },
      ast::BinaryOpKind::Equal => gen_apply_fn(
        ha,
        source,
        gen_var(b"_eq", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::NotEqual => gen_apply_fn(
        ha,
        source,
        gen_var(b"_neq", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Less => gen_apply_fn(
        ha,
        source,
        gen_var(b"_l", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::LessEqual => gen_apply_fn(
        ha,
        source,
        gen_var(b"_le", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Greater => gen_apply_fn(
        ha,
        source,
        gen_var(b"_g", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::GreaterEqual => gen_apply_fn(
        ha,
        source,
        gen_var(b"_ge", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::RelativeComplement => gen_apply_fn(
        ha,
        source,
        gen_var(b"_relative_complement", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Union => gen_apply_fn(
        ha,
        source,
        gen_var(b"_union", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Intersection => gen_apply_fn(
        ha,
        source,
        gen_var(b"_intersection", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::SymmetricDifference => gen_apply_fn(
        ha,
        source,
        gen_var(b"_symmetric_difference", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::In => gen_apply_fn(
        ha,
        source,
        gen_var(b"_in", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Cons => hir::Expr {
        source,
        kind: hir::ExprKind::Cons {
          fst: ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          snd: ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        },
      },
      ast::BinaryOpKind::SuchThat => hir::Expr {
        // TODO what the heck is a refinement
        source,
        kind: hir::ExprKind::Error,
      },
      c @ (ast::BinaryOpKind::RangeInclusive | ast::BinaryOpKind::Range) => {
        dlogger.log_unexpected_binop_in_expr(source.range, c);

        hir::Expr {
          source,
          kind: hir::ExprKind::Error,
        }
      }
      ast::BinaryOpKind::Assign => hir::Expr {
        source,
        kind: hir::ExprKind::LetIn {
          pat: ha.alloc(tr_pat(ha, dlogger, left_operand, var_env)),
          val: ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
          // this represents an assign at the tail end of an expression.
          // such assigns will not be available outside, and should warn about unused vars
          body: ha.alloc(hir::Expr {
            source,
            kind: hir::ExprKind::Nil,
          }),
        },
      },
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
          hir::Expr {
            source,
            kind: hir::ExprKind::LetIn {
              pat: ha.alloc(tr_pat(ha, dlogger, assign_pat, var_env)),
              val: ha.alloc(tr_expr(ha, dlogger, assign_value, var_env, label_env)),
              // this represents an assign with a scope after it
              body: ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
            },
          }
        } else {
          // continue
          hir::Expr {
            source,
            kind: hir::ExprKind::Sequence {
              fst: ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
              snd: ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
            },
          }
        }
      }
      ast::BinaryOpKind::As => gen_apply_fn(
        ha,
        source,
        gen_var(b"_as", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Append => gen_apply_fn(
        ha,
        source,
        gen_var(b"_append", dlogger, source, var_env),
        vec![
          ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::ModuleAccess => {
        if let ast::Expr {
          kind: ast::ExprKind::Reference(ref field),
          ..
        } = **right_operand
        {
          hir::Expr {
            source,
            kind: hir::ExprKind::StructFieldTake {
              root: ha.alloc(tr_expr(ha, dlogger, left_operand, var_env, label_env)),
              field: (field, right_operand),
            },
          }
        } else {
          dlogger.log_field_not_identifier(right_operand.range, &right_operand.kind);
          hir::Expr {
            source,
            kind: hir::ExprKind::Error,
          }
        }
      }
      ast::BinaryOpKind::Range => todo!(),
      ast::BinaryOpKind::RangeInclusive => todo!(),
    },
  }
}

pub fn construct_hir<'hir, 'ast>(
  ast: &'ast ast::Expr,
  ha: &'hir Bump,
  mut dlogger: DiagnosticLogger,
) -> hir::Expr<'hir, 'ast, &'hir Bump> {
  tr_expr(ha, &mut dlogger, ast, &mut vec![], &mut vec![])
}
