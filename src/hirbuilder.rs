use super::ast;
use super::dlogger::DiagnosticLogger;
use super::hir;
use bumpalo::Bump;
use num_bigint::BigUint;
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
  dlogger: &mut DiagnosticLogger,
  identifier: &'ast [u8],
) -> hir::PlaceExpr<'hir, 'ast, &'hir Bump> {
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
        ast::BinaryOpKind::Assign => match left_operand.as_ref() {
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
        fields.push((id, (pat_src, field_pat)));
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
        kind: hir::ValPatExprKind::ActivePattern {
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
    }
    ref c @ (ast::ExprKind::Val(_) | ast::ExprKind::Identifier(_)) => {
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
    // this option will only ever be invoked if we have a
    // bare identifier without a reference
    // In this case we are using it
    ast::ExprKind::Identifier(_) => hir::ValExpr {
      source,
      kind: hir::ValExprKind::Use(
        // generate the place
        ha.alloc(tr_place_expr(ha, dlogger, source)),
        // we take ownership of the value at this place
        hir::UseKind::Take,
      ),
    },
    ast::ExprKind::Builtin(_) => hir::ValExpr {
      source,
      kind: hir::ValExprKind::Use(
        // generate the place
        ha.alloc(tr_place_expr(ha, dlogger, source)),
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
          arg: ha.alloc(tr_irrefutable_pat_expr(ha, dlogger, left_operand)),
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
            ha.alloc(gen_place_from_identifier(source, dlogger, b"_compose")),
            // we take ownership of the value at this place
            hir::UseKind::Take,
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
            ha.alloc(gen_place_from_identifier(source, dlogger, b"_add")),
            // we take ownership of the value at this place
            hir::UseKind::Take,
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
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(source, dlogger, b"_sub")),
            hir::UseKind::Take,
          ),
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
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(source, dlogger, b"_mul")),
            hir::UseKind::Take,
          ),
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
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(source, dlogger, b"_div")),
            hir::UseKind::Take,
          ),
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
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(source, dlogger, b"_rem")),
            hir::UseKind::Take,
          ),
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
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(source, dlogger, b"_eq")),
            hir::UseKind::Take,
          ),
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
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(
              source, dlogger, // TODO: apply logical law here
              b"_neq",
            )),
            hir::UseKind::Take,
          ),
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
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(source, dlogger, b"_l")),
            hir::UseKind::Take,
          ),
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
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(source, dlogger, b"_le")),
            hir::UseKind::Take,
          ),
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
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(source, dlogger, b"_g")),
            hir::UseKind::Take,
          ),
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
          kind: hir::ValExprKind::Use(
            ha.alloc(gen_place_from_identifier(source, dlogger, b"_ge")),
            hir::UseKind::Take,
          ),
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
      ast::BinaryOpKind::PlusAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let pat = tr_irrefutable_pat_expr(ha, dlogger, left_operand);

        // create val by adding together
        let val = gen_app(
          ha,
          source,
          hir::ValExpr {
            source,
            kind: hir::ValExprKind::Use(
              ha.alloc(gen_place_from_identifier(source, dlogger, b"_add")),
              hir::UseKind::Take,
            ),
          },
          vec![
            ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
            ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
          ],
        );

        hir::ValExpr {
          source,
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              kind: hir::ValExprKind::Unit,
            }),
          },
        }
      }
      ast::BinaryOpKind::MinusAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let pat = tr_irrefutable_pat_expr(ha, dlogger, left_operand);

        // create val by adding together
        let val = gen_app(
          ha,
          source,
          hir::ValExpr {
            source,
            kind: hir::ValExprKind::Use(
              ha.alloc(gen_place_from_identifier(source, dlogger, b"_sub")),
              hir::UseKind::Take,
            ),
          },
          vec![
            ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
            ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
          ],
        );

        hir::ValExpr {
          source,
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              kind: hir::ValExprKind::Unit,
            }),
          },
        }
      }
      ast::BinaryOpKind::MulAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let pat = tr_irrefutable_pat_expr(ha, dlogger, left_operand);

        // create val by adding together
        let val = gen_app(
          ha,
          source,
          hir::ValExpr {
            source,
            kind: hir::ValExprKind::Use(
              ha.alloc(gen_place_from_identifier(source, dlogger, b"_mul")),
              hir::UseKind::Take,
            ),
          },
          vec![
            ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
            ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
          ],
        );

        hir::ValExpr {
          source,
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              kind: hir::ValExprKind::Unit,
            }),
          },
        }
      }
      ast::BinaryOpKind::DivAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let pat = tr_irrefutable_pat_expr(ha, dlogger, left_operand);

        // create val by adding together
        let val = gen_app(
          ha,
          source,
          hir::ValExpr {
            source,
            kind: hir::ValExprKind::Use(
              ha.alloc(gen_place_from_identifier(source, dlogger, b"_div")),
              hir::UseKind::Take,
            ),
          },
          vec![
            ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
            ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
          ],
        );

        hir::ValExpr {
          source,
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              kind: hir::ValExprKind::Unit,
            }),
          },
        }
      }
      ast::BinaryOpKind::RemAssign => {
        // x += 1
        // x = x + 1
        // find place, ignore bindings, since they will be unused
        let pat = tr_irrefutable_pat_expr(ha, dlogger, left_operand);

        // create val by adding together
        let val = gen_app(
          ha,
          source,
          hir::ValExpr {
            source,
            kind: hir::ValExprKind::Use(
              ha.alloc(gen_place_from_identifier(source, dlogger, b"_rem")),
              hir::UseKind::Take,
            ),
          },
          vec![
            ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
            ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
          ],
        );

        hir::ValExpr {
          source,
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              kind: hir::ValExprKind::Unit,
            }),
          },
        }
      }
      ast::BinaryOpKind::Assign => {
        // this represents an assign at the tail end of an expression.
        // variables created in such assigns will not be available outside, and should warn about unused vars

        // translate the rhs
        let val = tr_val_expr(ha, dlogger, right_operand);

        // translate pattern (dropping vars, since they won't be used)
        // later, a warning will be sent in liveness checking
        let pat = tr_irrefutable_pat_expr(ha, dlogger, left_operand);

        hir::ValExpr {
          source,
          kind: hir::ValExprKind::LetIn {
            pat: ha.alloc(pat),
            val: ha.alloc(val),
            body: ha.alloc(hir::ValExpr {
              source,
              kind: hir::ValExprKind::Unit,
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
          let val = tr_val_expr(ha, dlogger, assign_value);

          // translate the lhs
          let pat = tr_irrefutable_pat_expr(ha, dlogger, assign_pat);

          // parse body in this context
          let body = tr_val_expr(ha, dlogger, right_operand);

          hir::ValExpr {
            source,
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
            kind: hir::ValExprKind::Sequence {
              fst: ha.alloc(tr_val_expr(ha, dlogger, left_operand)),
              snd: ha.alloc(tr_val_expr(ha, dlogger, right_operand)),
            },
          }
        }
      }
      ast::BinaryOpKind::ModuleAccess => hir::ValExpr {
        source,
        kind: hir::ValExprKind::Use(
          ha.alloc(tr_place_expr(ha, dlogger, source)),
          hir::UseKind::Take,
        ),
      },
      ast::BinaryOpKind::Range => todo!(),
      ast::BinaryOpKind::RangeInclusive => todo!(),
    },
  }
}

fn tr_place_expr<'hir, 'ast>(
  ha: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
) -> hir::PlaceExpr<'hir, 'ast, &'hir Bump> {
  match &source.kind {
    ast::ExprKind::Identifier(ref identifier) => {
      gen_place_from_identifier(source, dlogger, identifier)
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
        ast::ExprKind::Deref => hir::PlaceExpr {
          source,
          // A place can be the dereference of a pointer.
          // In that case we evaluate the struct_root and then dereference it
          kind: hir::PlaceExprKind::Deref(ha.alloc(tr_val_expr(ha, dlogger, left_operand))),
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
