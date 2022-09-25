use super::ast;
use super::dlogger::DiagnosticLogger;
use super::hir;
use bumpalo::Bump;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

fn gen_app<'hir, 'ast>(
  ha: &'hir Bump,
  source: &'ast ast::Expr,
  fun: hir::ValExpr<'hir, 'ast, &'hir Bump>,
  args: Vec<&'hir hir::ValExpr<'hir, 'ast, &'hir Bump>>,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  args.iter().fold(fun, |acc, x| hir::ValExpr {
    source,
    kind: hir::ValExprKind::App {
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

// Generates a place referencing a var
// It can handle if the var is captured
fn gen_place_from_identifier<'hir, 'ast>(
  source: &'ast ast::Expr,
  identifier: &'ast [u8],
) -> hir::PlaceExpr<'hir, 'ast> {
  hir::PlaceExpr {
    source,
    kind: hir::PlaceExprKind::Var(identifier),
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
        ast::BinaryOpKind::Constrain => match left_operand.as_ref() {
          // match field
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
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
) -> hir::IrrefutablePatExpr<'hir, 'ast, &'hir Bump> {
  match source.kind {
    // propagate error
    ast::ExprKind::Error => hir::IrrefutablePatExpr {
      source,
      kind: hir::IrrefutablePatExprKind::Error,
    },
    // as long as it typechecks, unit will always match
    ast::ExprKind::Unit => hir::IrrefutablePatExpr {
      source,
      kind: hir::IrrefutablePatExprKind::Unit,
    },
    // elide groups
    ast::ExprKind::Group(ref body) => tr_irrefutable_pat_expr(ha, dlogger, body),
    ast::ExprKind::StructLiteral(ref body) => {
      let patterns = parse_struct_fields(dlogger, body);

      let mut fields = Vec::new_in(ha);

      for (id, pat_src) in patterns.into_iter() {
        let field_pat = tr_irrefutable_pat_expr(ha, dlogger, pat_src);
        fields.push((id, (pat_src, field_pat)));
      }

      hir::IrrefutablePatExpr {
        source,
        kind: hir::IrrefutablePatExprKind::Struct(fields),
      }
    }
    ast::ExprKind::Identifier(ref identifier) => hir::IrrefutablePatExpr {
      source,
      kind: hir::IrrefutablePatExprKind::BindVariable(identifier),
    },
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::Cons => hir::IrrefutablePatExpr {
        source,
        kind: hir::IrrefutablePatExprKind::Pair {
          fst: ha.alloc(tr_irrefutable_pat_expr(ha, dlogger, left_operand)),
          snd: ha.alloc(tr_irrefutable_pat_expr(ha, dlogger, right_operand)),
        },
      },
      // Error
      c => {
        dlogger.log_unexpected_binop_in_irrefutable_pattern(source.range, c);
        hir::IrrefutablePatExpr {
          source,
          kind: hir::IrrefutablePatExprKind::Error,
        }
      }
    },
    // pattern is an error
    ref c => {
      dlogger.log_unexpected_in_irrefutable_pattern(source.range, c);
      hir::IrrefutablePatExpr {
        source,
        kind: hir::IrrefutablePatExprKind::Error,
      }
    }
  }
}

fn tr_refutable_pat_expr<'hir, 'ast>(
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
) -> hir::RefutablePatExpr<'hir, 'ast, &'hir Bump> {
  match source.kind {
    // propagate error
    ast::ExprKind::Error => hir::RefutablePatExpr {
      source,
      kind: hir::RefutablePatExprKind::Error,
    },
    // send irrefutable patterns here
    ast::ExprKind::Unit | ast::ExprKind::Identifier(_) => hir::RefutablePatExpr {
      source,
      kind: hir::RefutablePatExprKind::IrrefutablePat(
        ha.alloc(tr_irrefutable_pat_expr(ha, dlogger, source)),
      ),
    },
    // send refutable patterns here
    ast::ExprKind::Bool(_)
    | ast::ExprKind::Int(_)
    | ast::ExprKind::Float(_)
    | ast::ExprKind::String { .. } => hir::RefutablePatExpr {
      source,
      kind: hir::RefutablePatExprKind::ValPat(ha.alloc(tr_val_pat_expr(ha, dlogger, source))),
    },
    // dont need groups
    ast::ExprKind::Group(ref body) => tr_refutable_pat_expr(ha, dlogger, body),
    ast::ExprKind::StructLiteral(ref body) => {
      let patterns = parse_struct_fields(dlogger, body);

      let mut fields = Vec::new_in(ha);

      for (id, pat_src) in patterns.into_iter() {
        let field_pat = tr_refutable_pat_expr(ha, dlogger, pat_src);
        fields.push((pat_src, (id, field_pat)));
      }

      hir::RefutablePatExpr {
        source,
        kind: hir::RefutablePatExprKind::Struct(fields),
      }
    }
    // handle refutable ops
    ast::ExprKind::Val(ref body) => hir::RefutablePatExpr {
      source,
      kind: hir::RefutablePatExprKind::ValPat(ha.alloc(tr_val_pat_expr(ha, dlogger, body))),
    },

    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      // handle valuelike ops
      ast::BinaryOpKind::Range | ast::BinaryOpKind::RangeInclusive | ast::BinaryOpKind::Or => {
        hir::RefutablePatExpr {
          source,
          kind: hir::RefutablePatExprKind::ValPat(ha.alloc(tr_val_pat_expr(ha, dlogger, source))),
        }
      }
      ast::BinaryOpKind::Cons => hir::RefutablePatExpr {
        source,
        kind: hir::RefutablePatExprKind::Pair {
          fst: ha.alloc(tr_refutable_pat_expr(ha, dlogger, left_operand)),
          snd: ha.alloc(tr_refutable_pat_expr(ha, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::And => hir::RefutablePatExpr {
        source,
        kind: hir::RefutablePatExprKind::And {
          fst: ha.alloc(tr_refutable_pat_expr(ha, dlogger, left_operand)),
          snd: ha.alloc(tr_refutable_pat_expr(ha, dlogger, right_operand)),
        },
      },
      // Error
      c => {
        dlogger.log_unexpected_binop_in_refutable_pattern(source.range, c);
        hir::RefutablePatExpr {
          source,
          kind: hir::RefutablePatExprKind::Error,
        }
      }
    },
    // pattern is an error
    ref c => {
      dlogger.log_unexpected_in_refutable_pattern(source.range, c);
      hir::RefutablePatExpr {
        source,
        kind: hir::RefutablePatExprKind::Error,
      }
    }
  }
}

fn tr_val_pat_expr<'hir, 'ast>(
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
) -> hir::ValPatExpr<'hir, 'ast, &'hir Bump> {
  match source.kind {
    // propagate error
    ast::ExprKind::Error => hir::ValPatExpr {
      source,
      kind: hir::ValPatExprKind::Error,
    },
    // match against values automatically
    ast::ExprKind::Unit
    | ast::ExprKind::Bool(_)
    | ast::ExprKind::Int(_)
    | ast::ExprKind::Float(_)
    | ast::ExprKind::String { .. } => hir::ValPatExpr {
      source,
      kind: hir::ValPatExprKind::Value(ha.alloc(tr_val_expr(ha, dlogger, source))),
    },
    // elide groups
    ast::ExprKind::Group(ref body) => tr_val_pat_expr(ha, dlogger, body),
    ast::ExprKind::StructLiteral(ref body) => {
      let mut fields = Vec::new_in(ha);

      for (id, pat_src) in parse_struct_fields(dlogger, body).into_iter() {
        let field_pat = tr_val_pat_expr(ha, dlogger, pat_src);
        fields.push((id, (pat_src, field_pat)));
      }

      hir::ValPatExpr {
        source,
        kind: hir::ValPatExprKind::Struct(fields),
      }
    }
    ast::ExprKind::Val(ref body) => hir::ValPatExpr {
      source,
      kind: hir::ValPatExprKind::Value(ha.alloc(tr_val_expr(ha, dlogger, body))),
    },
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::Range => hir::ValPatExpr {
        source,
        kind: hir::ValPatExprKind::Range {
          fst: ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          snd: ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
          inclusive: false,
        },
      },
      ast::BinaryOpKind::RangeInclusive => hir::ValPatExpr {
        source,
        kind: hir::ValPatExprKind::Range {
          fst: ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          snd: ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
          inclusive: true,
        },
      },
      ast::BinaryOpKind::Apply => hir::ValPatExpr {
        source,
        kind: hir::ValPatExprKind::Constructor {
          fun: ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          arg: ha.alloc(tr_val_pat_expr(ha, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::Cons => hir::ValPatExpr {
        source,
        kind: hir::ValPatExprKind::Pair {
          fst: ha.alloc(tr_val_pat_expr(ha, dlogger, left_operand)),
          snd: ha.alloc(tr_val_pat_expr(ha, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::And => hir::ValPatExpr {
        source,
        kind: hir::ValPatExprKind::And {
          fst: ha.alloc(tr_val_pat_expr(ha, dlogger, left_operand)),
          snd: ha.alloc(tr_val_pat_expr(ha, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::Or => hir::ValPatExpr {
        source,
        kind: hir::ValPatExprKind::Or {
          fst: ha.alloc(tr_val_pat_expr(ha, dlogger, left_operand)),
          snd: ha.alloc(tr_val_pat_expr(ha, dlogger, right_operand)),
        },
      },
      // Error
      c => {
        dlogger.log_unexpected_binop_in_val_pattern(source.range, c);
        hir::ValPatExpr {
          source,
          kind: hir::ValPatExprKind::Error,
        }
      }
    },
    // pattern is an error
    ref c => {
      dlogger.log_unexpected_in_val_pattern(source.range, c);
      hir::ValPatExpr {
        source,
        kind: hir::ValPatExprKind::Error,
      }
    }
  }
}

fn tr_val_expr<'hir, 'ast>(
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
) -> hir::ValExpr<'hir, 'ast, &'hir Bump> {
  match source.kind {
    ast::ExprKind::Error => hir::ValExpr {
      source,
      kind: hir::ValExprKind::Error,
    },
    ref c @ (ast::ExprKind::Ref | ast::ExprKind::UniqRef | ast::ExprKind::Deref) => {
      dlogger.log_only_in_field(source.range, c);

      hir::ValExpr {
        source,
        kind: hir::ValExprKind::Error,
      }
    },
    ref c @ ast::ExprKind::Val(_) => {
      dlogger.log_only_in_pattern(source.range, c);

      hir::ValExpr {
        source,
        kind: hir::ValExprKind::Error,
      }
    }
    ast::ExprKind::Unit => hir::ValExpr {
      source,
      kind: hir::ValExprKind::Unit,
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
        kind: hir::ValExprKind::Unit,
      },
      // as we work our way backwards, we prepend the current char as an int to our list
      |acc, x| {
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Pair {
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
    ast::ExprKind::Lifetime(_) => todo!(),
    ast::ExprKind::Group(ref body) => tr_val_expr(ha, dlogger, body),
    ast::ExprKind::StructLiteral(ref body) => {
      let mut fields = Vec::new_in(ha);

      for (id, pat) in parse_struct_fields(dlogger, body).into_iter() {
        let field_pat = tr_val_expr(ha, dlogger, pat);
        fields.push((id, (pat, field_pat)));
      }

      hir::ValExpr {
        source,
        kind: hir::ValExprKind::Struct(fields),
      }
    }
    ast::ExprKind::Identifier(_) | ast::ExprKind::Builtin(_) => hir::ValExpr {
      source,
      kind: hir::ValExprKind::Use(
        // generate the place
        ha.alloc(tr_place_expr(ha, dlogger, source)),
        // we take ownership of the value at this place
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
              let pat = tr_refutable_pat_expr(ha, dlogger, left_operand);
              let expr = tr_val_expr(ha, dlogger, right_operand);
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
        kind: hir::ValExprKind::CaseOf {
          source: hir::CaseSource::Case,
          expr: ha.alloc(tr_place_expr(ha, dlogger, expr)),
          case_options,
        },
      }
    }
    ast::ExprKind::Struct(ref body) => match body.kind {
      ast::ExprKind::StructLiteral(ref fields_src) => {
        let mut fields = Vec::new_in(ha);

        for (id, pat) in parse_struct_fields(dlogger, fields_src).into_iter() {
          let field_pat = tr_val_expr(ha, dlogger, pat);
          fields.push((id, (pat, field_pat)));
        }

        hir::ValExpr {
          source,
          kind: hir::ValExprKind::StructTy(fields),
        }
      }
      ref kind => {
        dlogger.log_expected_struct_literal_struct(body.range, kind);
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Error,
        }
      }
    },
    ast::ExprKind::Enum(ref body) => match body.kind {
      ast::ExprKind::StructLiteral(ref fields_src) => {
        let mut fields = Vec::new_in(ha);

        for (id, pat) in parse_struct_fields(dlogger, fields_src).into_iter() {
          let field_pat = tr_val_expr(ha, dlogger, pat);
          fields.push((id, (pat, field_pat)));
        }

        hir::ValExpr {
          source,
          kind: hir::ValExprKind::EnumTy(fields),
        }
      }
      ref kind => {
        dlogger.log_expected_struct_literal_enum(body.range, &kind);
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
          val_expr: ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          ty_expr: ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::RevConstrain => hir::ValExpr {
        source,
        kind: hir::ValExprKind::Annotate {
          ty_expr: ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          val_expr: ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::Defun => hir::ValExpr {
        source,
        kind: hir::ValExprKind::Lam {
          arg: ha.alloc({dbg!(left_operand.clone()); tr_irrefutable_pat_expr(ha, dlogger, left_operand)}),
          body: ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
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
        kind: hir::ValExprKind::App {
          fun: ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          arg: ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::Compose => gen_app(
        ha,
        source,
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Use(
            // generate the place
            ha.alloc(gen_place_from_identifier(source, b"_compose")),
          ),
        },
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Pipe => hir::ValExpr {
        source,
        kind: hir::ValExprKind::App {
          fun: ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
          arg: ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
        },
      },
      ast::BinaryOpKind::Add => gen_app(
        ha,
        source,
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Use(
            // generate the place
            ha.alloc(gen_place_from_identifier(source, b"_add")),
          ),
        },
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Sub => gen_app(
        ha,
        source,
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Use(ha.alloc(gen_place_from_identifier(source, b"_sub"))),
        },
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Mul => gen_app(
        ha,
        source,
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Use(ha.alloc(gen_place_from_identifier(source, b"_mul"))),
        },
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Div => gen_app(
        ha,
        source,
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Use(ha.alloc(gen_place_from_identifier(source, b"_div"))),
        },
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Rem => gen_app(
        ha,
        source,
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Use(ha.alloc(gen_place_from_identifier(source, b"_rem"))),
        },
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::And => hir::ValExpr {
        // a && b
        // case a
        // of true  = > b
        // || false = > false
        source,
        kind: hir::ValExprKind::CaseOf {
          expr: ha.alloc(tr_place_expr(ha, dlogger, left_operand)),
          source: hir::CaseSource::And,
          case_options: {
            let mut v = Vec::new_in(ha);
            // true path
            v.push((
              hir::RefutablePatExpr {
                source,
                kind: hir::RefutablePatExprKind::ValPat(ha.alloc(hir::ValPatExpr {
                  source,
                  kind: hir::ValPatExprKind::Value(ha.alloc(gen_bool(source, true))),
                })),
              },
              tr_val_expr(ha, dlogger, right_operand),
            ));

            // false path
            v.push((
              hir::RefutablePatExpr {
                source,
                kind: hir::RefutablePatExprKind::ValPat(ha.alloc(hir::ValPatExpr {
                  source,
                  kind: hir::ValPatExprKind::Value(ha.alloc(gen_bool(source, false))),
                })),
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
          expr: ha.alloc(tr_place_expr(ha, dlogger, left_operand)),
          source: hir::CaseSource::Or,
          case_options: {
            let mut v = Vec::new_in(ha);
            // true path
            v.push((
              hir::RefutablePatExpr {
                source,
                kind: hir::RefutablePatExprKind::ValPat(ha.alloc(hir::ValPatExpr {
                  source,
                  kind: hir::ValPatExprKind::Value(ha.alloc(gen_bool(source, true))),
                })),
              },
              gen_bool(source, true),
            ));

            // false path
            v.push((
              hir::RefutablePatExpr {
                source,
                kind: hir::RefutablePatExprKind::ValPat(ha.alloc(hir::ValPatExpr {
                  source,
                  kind: hir::ValPatExprKind::Value(ha.alloc(gen_bool(source, false))),
                })),
              },
              tr_val_expr(ha, dlogger, right_operand),
            ));
            v
          },
        },
      },
      ast::BinaryOpKind::Equal => gen_app(
        ha,
        source,
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Use(ha.alloc(gen_place_from_identifier(source, b"_eq"))),
        },
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::NotEqual => gen_app(
        ha,
        source,
        hir::ValExpr {
          source,
          // TODO: apply logical law here
          kind: hir::ValExprKind::Use(ha.alloc(gen_place_from_identifier(source, b"_neq"))),
        },
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Less => gen_app(
        ha,
        source,
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Use(ha.alloc(gen_place_from_identifier(source, b"_l"))),
        },
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::LessEqual => gen_app(
        ha,
        source,
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Use(ha.alloc(gen_place_from_identifier(source, b"_le"))),
        },
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Greater => gen_app(
        ha,
        source,
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Use(ha.alloc(gen_place_from_identifier(source, b"_g"))),
        },
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::GreaterEqual => gen_app(
        ha,
        source,
        hir::ValExpr {
          source,
          kind: hir::ValExprKind::Use(ha.alloc(gen_place_from_identifier(source, b"_ge"))),
        },
        vec![
          ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Cons => hir::ValExpr {
        source,
        kind: hir::ValExprKind::Pair {
          fst: ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          snd: ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::Sequence => hir::ValExpr {
        source,
        kind: hir::ValExprKind::Sequence {
          fst: ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
          snd: ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::ModuleAccess => hir::ValExpr {
        source,
        kind: hir::ValExprKind::Use(ha.alloc(tr_place_expr(ha, dlogger, source))),
      },
      ast::BinaryOpKind::Range => todo!(),
      ast::BinaryOpKind::RangeInclusive => todo!(),
    },
    ast::ExprKind::Let {
      ref pattern,
      ref value,
      ref body,
      ..
    } => hir::ValExpr {
      source,
      kind: hir::ValExprKind::LetIn {
        pat: ha.alloc(tr_irrefutable_pat_expr(ha, dlogger, pattern)),
        val: ha.alloc(tr_val_expr(ha, dlogger, value)),
        body: ha.alloc(tr_val_expr(ha, dlogger, body)),
      },
    },
    ast::ExprKind::Mut {
      ref pattern,
      ref value,
    } => hir::ValExpr {
      source,
      kind: hir::ValExprKind::Assign {
        pat: ha.alloc(tr_irrefutable_pat_expr(ha, dlogger, pattern)),
        val: ha.alloc(tr_val_expr(ha, dlogger, value)),
      },
    },
  }
}

fn tr_place_expr<'hir, 'ast>(
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
) -> hir::PlaceExpr<'hir, 'ast> {
  match &source.kind {
    ast::ExprKind::Identifier(ref identifier) => gen_place_from_identifier(source, identifier),


    ast::ExprKind::Group(ref expr) => tr_place_expr(ha, dlogger, expr),
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::ModuleAccess => match right_operand.kind {
        ast::ExprKind::Identifier(ref field) => hir::PlaceExpr {
          source,
          kind: hir::PlaceExprKind::StructField {
            root: ha.alloc(tr_place_expr(ha, dlogger, left_operand)),
            field,
            field_source: right_operand,
          },
        },
        ast::ExprKind::Ref => hir::PlaceExpr {
          source,
          kind: hir::PlaceExprKind::Op(
            ha.alloc(tr_place_expr(ha, dlogger, left_operand)),
            hir::PlaceExprOpKind::Ref,
          ),
        },
        ast::ExprKind::UniqRef => hir::PlaceExpr {
          source,
          kind: hir::PlaceExprKind::Op(
            ha.alloc(tr_place_expr(ha, dlogger, left_operand)),
            hir::PlaceExprOpKind::UniqRef,
          ),
        },
        ast::ExprKind::Deref => hir::PlaceExpr {
          source,
          kind: hir::PlaceExprKind::Op(
            ha.alloc(tr_place_expr(ha, dlogger, left_operand)),
            hir::PlaceExprOpKind::Deref,
          ),
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
  tr_val_expr(ha, &mut dlogger, ast)
}
