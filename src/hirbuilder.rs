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
  fun: hir::ValExpr<'hir, 'ast, &'hir Bump>,
  args: Vec<&'hir hir::ValExpr<'hir, 'ast, &'hir Bump>>,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  args.iter().fold(fun, |acc, x| hir::ValExpr {
    source,
    kind: hir::ValExprKind::Apply {
      fun: ha.alloc(acc),
      arg: x,
    },
  })
}

fn gen_bool<'hir, 'ast>(source: &'ast ast::Expr, b: bool) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  hir::ValExpr {
    source: source,
    kind: hir::ValExprKind::Bool(b),
  }
}

fn gen_nil<'hir, 'ast>(source: &'ast ast::Expr) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  hir::ValExpr {
    source: source,
    kind: hir::ValExprKind::Nil,
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
  source: &'ast ast::Expr,
  place: hir::PlaceExpr<'hir, 'ast, &'hir Bump>,
  ha: &'hir Bump,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  hir::ValExpr {
    source,
    kind: hir::ValExprKind::Take(ha.alloc(place)),
  }
}

fn gen_var_take<'hir, 'ast>(
  identifier: &[u8],
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  var_env: &Vec<(&'ast [u8], VarScope<'ast>)>,
  ha: &'hir Bump,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  gen_take(
    source,
    gen_var_place(source, identifier, dlogger, var_env),
    ha,
  )
}

fn tr_pat<'hir, 'ast>(
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  var_env: &mut Vec<(&'ast [u8], VarScope<'ast>)>,
) -> (
  hir::Pat<'hir, 'ast, &'hir Bump>,
  Vec<(&'ast [u8], VarScope<'ast>)>,
) {
  match source.kind {
    // propagate error
    ast::ExprKind::Error =>
        (
        hir::Pat {
      source,
      kind: hir::PatKind::Error,
    },
    vec![]
    )
    ,
    // transparent valueification
    ast::ExprKind::Nil
    | ast::ExprKind::Bool(_)
    | ast::ExprKind::Int(_)
    | ast::ExprKind::Float(_)
    | ast::ExprKind::String { .. } => (
    hir::Pat {
      source,
      kind: hir::PatKind::Value(ha.alloc(tr_val_expr(ha, dlogger, source, var_env, &mut vec![]))),
    }, vec![])
    ,
    // reject use in pattern without explicit `val`
    ref
    c
    @
    (ast::ExprKind::Label { .. }
    | ast::ExprKind::Identifier(_)
    | ast::ExprKind::Defer { .. }
    | ast::ExprKind::Ret { .. }
    | ast::ExprKind::CaseOf { .. }) => {
      dlogger.log_unexpected_in_pattern(source.range, c);
      (
          hir::Pat {
        source,
        kind: hir::PatKind::Error,
      },
      vec![]
      )
    }
    // elide groups
    ast::ExprKind::Group(ref body) => tr_pat(ha, dlogger, body, var_env),
    ast::ExprKind::StructLiteral(ref body) => {
      // create a struct of literals
      let mut patterns = HashMap::new();

      // depth first search of binary tree
      let mut sequences = vec![body];
      let mut bindings = vec![];

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
                    // identifier unique, translate rhs and insert into map
                    let (field_pat, field_bindings) =tr_pat(ha, dlogger, right_operand, var_env);
                    ve.insert(field_pat);
                    bindings.extend(field_bindings);
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
      (
          hir::Pat {
        source,
        kind: hir::PatKind::StructLiteral(new_vec_from(ha, patterns.into_iter())),
      },
      bindings
      )
    }
    ast::ExprKind::UnaryOp {
      ref op,
      ref operand,
    } => match op {
      // TODO: use the ref / deref laws to optimize this
      ast::UnaryOpKind::Ref => {
          let (arg_pat, arg_bindings) =tr_pat(ha, dlogger, operand, var_env);
          (
              hir::Pat {
                source,
                kind: hir::PatKind::ActivePattern {
                  fun: ha.alloc(gen_take(
                    source,
                    gen_var_place(source, b"_ref", dlogger, var_env),
                    ha,
                  )),
                  arg: ha.alloc(arg_pat),
                },
              },
                arg_bindings
          )
      },
      ast::UnaryOpKind::MutRef => {
          let (arg_pat, arg_bindings) =tr_pat(ha, dlogger, operand, var_env);

        (
            hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          fun: ha.alloc(gen_take(
            source,
            gen_var_place(source, b"_mut_ref", dlogger, var_env),
            ha,
          )),
          arg: ha.alloc(arg_pat),
        },
      },
      arg_bindings
        )
      },
      ast::UnaryOpKind::Deref => {
          let (arg_pat, arg_bindings) =tr_pat(ha, dlogger, operand, var_env);

        (
            hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          fun: ha.alloc(gen_take(
            source,
            gen_var_place(source, b"_deref", dlogger, var_env),
            ha,
          )),
          arg: ha.alloc(arg_pat),
        },
      },
      arg_bindings
        )
      },
      ast::UnaryOpKind::Val => (
          hir::Pat {
        source,
        kind: hir::PatKind::Value(ha.alloc(tr_val_expr(
          ha,
          dlogger,
          operand,
          var_env,
          &mut vec![],
        ))),
      }, vec![]
      ),
      ast::UnaryOpKind::Bind => match **operand {
        // binds a variable to an identifier
        ast::Expr {
          kind: ast::ExprKind::Identifier(ref identifier),
          ..
        } => ( hir::Pat { source, kind: hir::PatKind::BindVariable, }, vec![(identifier, VarScope { declaration: Some(operand) })]),
        // handle error
        ast::Expr {
          range, ref kind, ..
        } => {
          dlogger.log_unexpected_bind_target(range, kind);
        (hir::Pat { source, kind: hir::PatKind::Error, }, vec![])
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
        (hir::Pat { source, kind: hir::PatKind::Error, }, vec![])
      }
    },
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::Apply => {
          let (arg_pat, arg_bindings) = tr_pat(ha, dlogger, right_operand, var_env);
          (hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          fun: ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, &mut vec![])),
          arg: ha.alloc(arg_pat),
        }
      }, arg_bindings)
      },
      ast::BinaryOpKind::And => {
          let (left_pat, left_bindings) =tr_pat(ha, dlogger, left_operand, var_env);
          let (right_pat, right_bindings) =tr_pat(ha, dlogger, right_operand, var_env);

          (
              hir::Pat {
           source,
           kind: hir::PatKind::And{
             left_operand: ha.alloc(left_pat),
             right_operand: ha.alloc(right_pat),
           }
              },

              // join them together
              left_bindings.into_iter().chain(right_bindings.into_iter()).collect()

           )
      },
      ast::BinaryOpKind::Or => {
          let (left_pat, left_bindings) =tr_pat(ha, dlogger, left_operand, var_env);
          let (right_pat, right_bindings) =tr_pat(ha, dlogger, right_operand, var_env);

          (
              hir::Pat {
           source,
           kind: hir::PatKind::Or {
             left_operand: ha.alloc(left_pat),
             right_operand: ha.alloc(right_pat),
           }
              },

              // join them together
              left_bindings.into_iter().chain(right_bindings.into_iter()).collect()

           )
      },
      ast::BinaryOpKind::Cons => {
          let (fst_pat, fst_bindings) =tr_pat(ha, dlogger, left_operand, var_env);
          let (snd_pat, snd_bindings) =tr_pat(ha, dlogger, right_operand, var_env);

          (
              hir::Pat {
           source,
           kind: hir::PatKind::Cons {
             fst: ha.alloc(fst_pat),
             snd: ha.alloc(snd_pat),
           }
              },

              // join them together
              fst_bindings.into_iter().chain(snd_bindings.into_iter()).collect()

           )
      },
      ast::BinaryOpKind::Range => (
          hir::Pat {
        source,
        kind: hir::PatKind::Range {
          inclusive: false,
          left_operand: ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, &mut vec![])),
          right_operand: ha.alloc(tr_val_expr(
            ha,
            dlogger,
            right_operand,
            var_env,
            &mut vec![],
          )),
        },
      },vec![]),
      ast::BinaryOpKind::RangeInclusive =>
          (hir::Pat {
        source,
        kind: hir::PatKind::Range {
          inclusive: true,
          left_operand: ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, &mut vec![])),
          right_operand: ha.alloc(tr_val_expr(
            ha,
            dlogger,
            right_operand,
            var_env,
            &mut vec![],
          )),
        },
      }, vec![]
      ),
      // Error
      c => {
        dlogger.log_unexpected_binop_in_pattern(source.range, c);
        (
            hir::Pat {
          source,
          kind: hir::PatKind::Error,
        },
         vec![]
        )
      }
    },
  }
}

fn tr_val_expr<'hir, 'ast>(
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  var_env: &mut Vec<(&'ast [u8], VarScope<'ast>)>,
  label_env: &mut Vec<(&'ast [u8], LabelScope<'ast>)>,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  match source.kind {
    ast::ExprKind::Error => hir::ValExpr {
      source,
      kind: hir::ValExprKind::Error,
    },
    ast::ExprKind::Nil => hir::ValExpr {
      source,
      kind: hir::ValExprKind::Nil,
    },
    ast::ExprKind::Bool(b) => hir::ValExpr {
      source,
      kind: hir::ValExprKind::Bool(b),
    },
    ast::ExprKind::Int(ref i) => hir::ValExpr {
      source,
      kind: hir::ValExprKind::Int(i),
    },
    ast::ExprKind::Float(ref i) => hir::ValExpr {
      source,
      kind: hir::ValExprKind::Float(i),
    },
    ast::ExprKind::String { ref value, .. } => value.iter().rev().fold(
      // start with a null at the end of the list
      hir::ValExpr {
        source,
        kind: hir::ValExprKind::Nil,
      },
      // as we work our way backwards, we prepend the current char as an int to our list
      |acc, x| {
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Cons {
            // first arg is the new expr for the int
            fst: ha.alloc(hir::ValExpr {
              source,
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
      let body_tr = tr_val_expr(ha, dlogger, body, var_env, label_env);

      // pop label
      let _ = label_env.pop().unwrap();

      // return label
      hir::ValExpr {
        source,
        kind: hir::ValExprKind::Label(ha.alloc(body_tr)),
      }
    }
    ast::ExprKind::Group(ref body) => tr_val_expr(ha, dlogger, body, var_env, label_env),
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
          kind: hir::ValExprKind::Nil,
        }
      } else {
        // a label was never properly provided
        // an error should already have been thrown, so don't double report
        hir::ValExpr {
          source,
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
        let body_tr = tr_val_expr(ha, dlogger, body, var_env, label_env);

        // now make the defer-ret construct
        // ret 'x 0
        // to
        // let toret = 0;
        // (run defers);
        // ret 'x toret

        hir::ValExpr {
          source,
          kind: hir::ValExprKind::LetIn {
            // introduce variable with value of ret
            pat: ha.alloc(hir::Pat {
              source,
              kind: hir::PatKind::BindVariable,
            }),
            val: ha.alloc(body_tr),

            // write body
            body: lookup(&label_env, &label).defers.clone().iter().fold(
              // the last thing to execute is the actual ret
              ha.alloc(hir::ValExpr {
                source,
                kind: hir::ValExprKind::Ret {
                  labels_up: lookup_count_up(label_env, &label),
                  // lookup variable introduced earlier
                  value: ha.alloc(gen_take(
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
                let tr_x = tr_val_expr(ha, dlogger, x, var_env, label_env);
                // return sequenced defer
                ha.alloc(hir::ValExpr {
                  source: x,
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
                      tr_val_expr(ha, dlogger, right_operand, var_env, label_env),
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
        kind: hir::ValExprKind::StructLiteral(new_vec_from(ha, fields.drain())),
      }
    }
    ast::ExprKind::Identifier(ref identifier) => {
      gen_var_take(identifier, dlogger, source, var_env, ha)
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
                let (pat, bindings) = tr_pat(ha, dlogger, left_operand, var_env);
                var_env.ex
                case_options.push((pat
              tr_val_expr(ha, dlogger, right_operand, var_env, label_env),
            ))
            },
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
        kind: hir::ValExprKind::CaseOf {
          source: hir::CaseSource::Case,
          expr: ha.alloc(tr_place_expr(ha, dlogger, expr, var_env, label_env)),
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
        kind: hir::ValExprKind::Borrow(
          ha.alloc(tr_place_expr(ha, dlogger, operand, var_env, label_env)),
        ),
      },
      ast::UnaryOpKind::MutRef => hir::ValExpr {
        source,
        kind: hir::ValExprKind::MutBorrow(
          ha.alloc(tr_place_expr(ha, dlogger, operand, var_env, label_env)),
        ),
      },
      ast::UnaryOpKind::Deref => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_deref", dlogger, source, var_env, ha),
        vec![ha.alloc(tr_val_expr(ha, dlogger, operand, var_env, label_env))],
      ),
      // TODO decompose
      ast::UnaryOpKind::ReturnOnError => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_return_on_error", dlogger, source, var_env, ha),
        vec![ha.alloc(tr_val_expr(ha, dlogger, operand, var_env, label_env))],
      ),
      ast::UnaryOpKind::Struct => hir::ValExpr {
        source,
        kind: hir::ValExprKind::Struct(
          ha.alloc(tr_val_expr(ha, dlogger, operand, var_env, label_env)),
        ),
      },
      ast::UnaryOpKind::Enum => hir::ValExpr {
        source,
        kind: hir::ValExprKind::Enum(
          ha.alloc(tr_val_expr(ha, dlogger, operand, var_env, label_env)),
        ),
      },
      ast::UnaryOpKind::Loop => hir::ValExpr {
        source,
        kind: hir::ValExprKind::Loop(
          ha.alloc(tr_val_expr(ha, dlogger, operand, var_env, label_env)),
        ),
      },
      c @ ast::UnaryOpKind::Val => {
        dlogger.log_unexpected_unop_in_expr(source.range, c);
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Error,
        }
      }
      c @ ast::UnaryOpKind::Bind => {
        dlogger.log_unexpected_unop_in_expr(source.range, c);
        hir::ValExpr {
          source,
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
        kind: hir::ValExprKind::Annotate {
          val_expr: ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ty_expr: ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        },
      },
      ast::BinaryOpKind::RevConstrain => hir::ValExpr {
        source,
        kind: hir::ValExprKind::Annotate {
          ty_expr: ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          val_expr: ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        },
      },
      ast::BinaryOpKind::Defun => hir::ValExpr {
        source,
        kind: hir::ValExprKind::Defun {
          pattern: ha.alloc(tr_pat(ha, dlogger, left_operand, var_env)),
          result: ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
          infer_pattern: false,
        },
      },
      ast::BinaryOpKind::CaseOption => {
        dlogger.log_only_in_case(source.range);
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Error,
        }
      }
      ast::BinaryOpKind::Apply => hir::ValExpr {
        source,
        kind: hir::ValExprKind::Apply {
          fun: ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          arg: ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        },
      },
      ast::BinaryOpKind::Compose => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_compose", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::PipeForward => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_pipe_forward", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::PipeBackward => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_pipe_backward", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Add => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_add", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Sub => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_sub", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Mul => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_mul", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Div => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_div", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Rem => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_rem", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::And => hir::ValExpr {
        // a && b
        // case a
        // of true  = > b
        // || false = > false
        source,
        kind: hir::ValExprKind::CaseOf {
          expr: ha.alloc(tr_place_expr(ha, dlogger, left_operand, var_env, label_env)),
          source: hir::CaseSource::And,
          case_options: {
            let mut v = Vec::new_in(ha);
            // true path
            v.push((
              hir::Pat {
                source,
                kind: hir::PatKind::Value(ha.alloc(gen_bool(source, true))),
              },
              tr_val_expr(ha, dlogger, right_operand, var_env, label_env),
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
      ast::BinaryOpKind::Or => hir::ValExpr {
        // a || b
        // case a
        // of true => true
        // of false => b
        source,
        kind: hir::ValExprKind::CaseOf {
          expr: ha.alloc(tr_place_expr(ha, dlogger, left_operand, var_env, label_env)),
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
              tr_val_expr(ha, dlogger, right_operand, var_env, label_env),
            ));
            v
          },
        },
      },
      ast::BinaryOpKind::Equal => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_eq", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::NotEqual => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_neq", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Less => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_l", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::LessEqual => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_le", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Greater => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_g", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::GreaterEqual => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_ge", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::Cons => hir::ValExpr {
        source,
        kind: hir::ValExprKind::Cons {
          fst: ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          snd: ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        },
      },
      ast::BinaryOpKind::Assign => hir::ValExpr {
        source,
        kind: hir::ValExprKind::LetIn {
          pat: ha.alloc(tr_pat(ha, dlogger, left_operand, var_env)),
          val: ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
          // this represents an assign at the tail end of an expression.
          // such assigns will not be available outside, and should warn about unused vars
          body: ha.alloc(hir::ValExpr {
            source,
            kind: hir::ValExprKind::Nil,
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
          hir::ValExpr {
            source,
            kind: hir::ValExprKind::LetIn {
              pat: ha.alloc(tr_pat(ha, dlogger, assign_pat, var_env)),
              val: ha.alloc(tr_val_expr(ha, dlogger, assign_value, var_env, label_env)),
              // this represents an assign with a scope after it
              body: ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
            },
          }
        } else {
          // continue
          hir::ValExpr {
            source,
            kind: hir::ValExprKind::Sequence {
              fst: ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
              snd: ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
            },
          }
        }
      }
      ast::BinaryOpKind::As => gen_apply_fn(
        ha,
        source,
        gen_var_take(b"_as", dlogger, source, var_env, ha),
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand, var_env, label_env)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand, var_env, label_env)),
        ],
      ),
      ast::BinaryOpKind::ModuleAccess => gen_take(
        source,
        tr_place_expr(ha, dlogger, source, var_env, label_env),
        ha,
      ),
      ast::BinaryOpKind::Range => todo!(),
      ast::BinaryOpKind::RangeInclusive => todo!(),
    },
  }
}

fn tr_place_expr<'hir, 'ast>(
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
              root: ha.alloc(tr_place_expr(ha, dlogger, left_operand, var_env, label_env)),
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
          ha.alloc(tr_val_expr(ha, dlogger, operand, var_env, label_env)),
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
  tr_val_expr(ha, &mut dlogger, ast, &mut vec![], &mut vec![])
}
