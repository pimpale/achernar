use super::ast;
use super::dlogger::DiagnosticLogger;
use super::hir;
use bumpalo::Bump;
use hashbrown::hash_map::Entry;
use hashbrown::HashMap;
use num_bigint::BigInt;
use std::alloc::Allocator;

fn clone_in<A: Allocator, T: Clone>(allocator: A, slice: &[T]) -> Vec<T, A> {
  let mut v = Vec::new_in(allocator);
  v.extend_from_slice(slice);
  v
}

fn gen_apply_fn<'hir, 'ast>(
  allocator: &'hir Bump,
  source: &'ast ast::Expr,
  fun: hir::Expr<'hir, 'ast, &'hir Bump>,
  args: Vec<&'hir hir::Expr<'hir, 'ast, &'hir Bump>>,
) -> hir::Expr<'hir, 'ast, &'hir Bump> {
  args.iter().fold(fun, |acc, x| hir::Expr {
    source,
    kind: hir::ExprKind::Apply {
      fun: allocator.alloc(acc),
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

fn tr_pat<'hir, 'ast>(
  allocator: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
) -> hir::Pat<'hir, 'ast, &'hir Bump> {
  match source.kind {
    // propagate error
    ast::ExprKind::Error => hir::Pat {
      source,
      kind: hir::PatKind::Error,
    },
    ast::ExprKind::Error => hir::Pat {
      source,
      kind: hir::PatKind::Hole,
    },
    // transparent valueification
    ast::ExprKind::Error
    | ast::ExprKind::Nil
    | ast::ExprKind::Bool(_)
    | ast::ExprKind::Int(_)
    | ast::ExprKind::String { .. } => hir::Pat {
      source,
      kind: hir::PatKind::Value(allocator.alloc(tr_expr(allocator, dlogger, source))),
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
    ast::ExprKind::InferArg(_) => {
      dlogger.log_unexpected_infer_arg(source.range);
      hir::Pat {
        source,
        kind: hir::PatKind::Error,
      }
    }
    // elide groups
    ast::ExprKind::Group(ref body) => tr_pat(allocator, dlogger, body),
    ast::ExprKind::BindSplat => hir::Pat {
      source,
      kind: hir::PatKind::BindSplat,
    },
    ast::ExprKind::StructLiteral(ref body) => {
      // create a struct of literals
      let mut patterns = HashMap::new_in(allocator);

      // depth first search of binary tree
      let mut sequences = vec![body];

      let mut splat = None;

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
                } => match patterns.entry(clone_in(allocator, identifier)) {
                  Entry::Vacant(ve) => {
                    // identifier unique, translate rhs and insert into map
                    ve.insert(tr_pat(allocator, dlogger, right_operand));
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
              // explicit ignore
              ast::Expr {
                kind: ast::ExprKind::BindSplat,
                range,
                ..
              } => {
                // means a splat was the target of the assign
                // This is equivalent to a .. in rust
                if splat.is_none() {
                  // set the pattern to the rhs
                  splat = Some(tr_pat(allocator, dlogger, right_operand));
                } else {
                  dlogger.log_repeat_splat_in_pattern_struct(*range);
                }
              }
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
        kind: hir::PatKind::StructLiteral {
          splat: allocator.alloc(splat),
          patterns,
        },
      }
    }
    ast::ExprKind::UnaryOp {
      ref op,
      ref operand,
    } => match op {
      ast::UnaryOpKind::Posit => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(hir::Expr {
            source,
            kind: hir::ExprKind::Reference(b"_posit".to_vec_in(allocator)),
          }),
          param: allocator.alloc(tr_pat(allocator, dlogger, operand)),
        },
      },
      ast::UnaryOpKind::Negate => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(hir::Expr {
            source,
            kind: hir::ExprKind::Reference(b"_negate".to_vec_in(allocator)),
          }),
          param: allocator.alloc(tr_pat(allocator, dlogger, operand)),
        },
      },
      ast::UnaryOpKind::Ref => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(hir::Expr {
            source,
            kind: hir::ExprKind::Reference(b"_ref".to_vec_in(allocator)),
          }),
          param: allocator.alloc(tr_pat(allocator, dlogger, operand)),
        },
      },
      ast::UnaryOpKind::Deref => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(hir::Expr {
            source,
            kind: hir::ExprKind::Reference(b"_deref".to_vec_in(allocator)),
          }),
          param: allocator.alloc(tr_pat(allocator, dlogger, operand)),
        },
      },
      ast::UnaryOpKind::Not => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(hir::Expr {
            source,
            kind: hir::ExprKind::Reference(b"_not".to_vec_in(allocator)),
          }),
          param: allocator.alloc(tr_pat(allocator, dlogger, operand)),
        },
      },
      ast::UnaryOpKind::NoInfer => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(hir::Expr {
            source,
            kind: hir::ExprKind::Reference(b"_noinfer".to_vec_in(allocator)),
          }),
          param: allocator.alloc(tr_pat(allocator, dlogger, operand)),
        },
      },
      ast::UnaryOpKind::Complement => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(hir::Expr {
            source,
            kind: hir::ExprKind::Reference(b"_complement".to_vec_in(allocator)),
          }),
          param: allocator.alloc(tr_pat(allocator, dlogger, operand)),
        },
      },
      ast::UnaryOpKind::Val => hir::Pat {
        source,
        kind: hir::PatKind::Value(allocator.alloc(tr_expr(allocator, dlogger, operand))),
      },
      ast::UnaryOpKind::Bind => match **operand {
        // binds a variable to an identifier
        ast::Expr {
          kind: ast::ExprKind::Reference(ref identifier),
          ..
        } => hir::Pat {
          source,
          kind: hir::PatKind::BindIdentifier(clone_in(allocator, identifier)),
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
      | ast::UnaryOpKind::New
      | ast::UnaryOpKind::Loop
      | ast::UnaryOpKind::Pat) => {
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
          pattern: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
          ty: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        },
      },
      // TODO warn style
      ast::BinaryOpKind::RevConstrain => hir::Pat {
        source,
        kind: hir::PatKind::Annotate {
          // swap the expressions
          ty: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          pattern: allocator.alloc(tr_pat(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::Apply => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          param: allocator.alloc(tr_pat(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::RevApply => hir::Pat {
        source,
        kind: hir::PatKind::ActivePattern {
          // swap the expressions
          param: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
          function: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::And => hir::Pat {
        source,
        kind: hir::PatKind::And {
          left_operand: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
          right_operand: allocator.alloc(tr_pat(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::Or => hir::Pat {
        source,
        kind: hir::PatKind::Or {
          left_operand: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
          right_operand: allocator.alloc(tr_pat(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::Cons => hir::Pat {
        source,
        kind: hir::PatKind::Cons {
          left_operand: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
          right_operand: allocator.alloc(tr_pat(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::Range => hir::Pat {
        source,
        kind: hir::PatKind::Range {
          inclusive: false,
          left_operand: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          right_operand: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::RangeInclusive => hir::Pat {
        source,
        kind: hir::PatKind::Range {
          inclusive: true,
          left_operand: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          right_operand: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        },
      },
    },
  }
}

fn tr_expr<'hir, 'ast>(
  allocator: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
) -> hir::Expr<'hir, 'ast, &'hir Bump> {
  match source.kind {
    ast::ExprKind::Error => hir::Expr {
      source,
      kind: hir::ExprKind::Error,
    },
    ast::ExprKind::InferArg(_) => {
      dlogger.log_unexpected_infer_arg(source.range);
      hir::Expr {
        source,
        kind: hir::ExprKind::Error,
      }
    }
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
      // TODO allocate this via bumpalo
      kind: hir::ExprKind::Int(i.clone()),
    },
    ast::ExprKind::Float(ref i) => hir::Expr {
      source,
      // TODO allocate this via bumpalo
      kind: hir::ExprKind::Float(i.clone()),
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
            left_operand: allocator.alloc(hir::Expr {
              source,
              kind: hir::ExprKind::Int(BigInt::from(*x)),
            }),
            // second arg is the current tail of the list
            right_operand: allocator.alloc(acc),
          },
        }
      },
    ),
    ast::ExprKind::Label {
      ref label,
      ref body,
    } => hir::Expr {
      source,
      kind: hir::ExprKind::Label {
        label: label.to_vec_in(allocator),
        body: allocator.alloc(tr_expr(allocator, dlogger, body)),
      },
    },
    ast::ExprKind::Group(ref body) => tr_expr(allocator, dlogger, body),
    ast::ExprKind::Defer {
      label: ref maybe_label,
      ref body,
    } => {
      // fail if label wasn't specified none
      if let Some(label) = maybe_label {
        hir::Expr {
          source,
          kind: hir::ExprKind::Defer {
            label: label.to_vec_in(allocator),
            body: allocator.alloc(tr_expr(allocator, dlogger, body)),
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
    ast::ExprKind::Ret {
      label: ref maybe_label,
      ref body,
    } => {
      // fail if label wasn't specified none
      if let Some(label) = maybe_label {
        hir::Expr {
          source,
          kind: hir::ExprKind::Ret {
            label: label.to_vec_in(allocator),
            body: allocator.alloc(tr_expr(allocator, dlogger, body)),
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
    ast::ExprKind::StructLiteral(ref body) => hir::Expr {
      source,
      kind: hir::ExprKind::StructLiteral(allocator.alloc(tr_expr(allocator, dlogger, body))),
    },
    ast::ExprKind::Reference(ref identifier) => hir::Expr {
      source,
      kind: hir::ExprKind::Reference(clone_in(allocator, identifier)),
    },
    ref c @ ast::ExprKind::BindSplat => {
      dlogger.log_only_in_pattern(source.range, c);
      hir::Expr {
        source,
        kind: hir::ExprKind::Error,
      }
    }
    ast::ExprKind::CaseOf {
      ref expr,
      ref cases,
    } => {
      let mut case_options = Vec::new_in(allocator);

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
              tr_pat(allocator, dlogger, left_operand),
              tr_expr(allocator, dlogger, right_operand),
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
          expr: allocator.alloc(tr_expr(allocator, dlogger, expr)),
          case_options,
        },
      }
    }
    ast::ExprKind::UnaryOp {
      ref op,
      ref operand,
    } => match op {
      ast::UnaryOpKind::Posit => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_posit".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand))],
      ),
      ast::UnaryOpKind::Negate => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_negate".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand))],
      ),
      ast::UnaryOpKind::Ref => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_ref".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand))],
      ),
      ast::UnaryOpKind::Deref => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_deref".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand))],
      ),
      ast::UnaryOpKind::Not => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_not".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand))],
      ),
      ast::UnaryOpKind::Complement => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_complement".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand))],
      ),
      // TODO decompose
      ast::UnaryOpKind::ReturnOnError => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_return_on_error".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand))],
      ),
      ast::UnaryOpKind::Struct => hir::Expr {
        source,
        kind: hir::ExprKind::Struct(allocator.alloc(tr_expr(allocator, dlogger, operand))),
      },
      ast::UnaryOpKind::Enum => hir::Expr {
        source,
        kind: hir::ExprKind::Enum(allocator.alloc(tr_expr(allocator, dlogger, operand))),
      },
      ast::UnaryOpKind::New => hir::Expr {
        source,
        kind: hir::ExprKind::New(allocator.alloc(tr_expr(allocator, dlogger, operand))),
      },
      ast::UnaryOpKind::NoInfer => hir::Expr {
        source,
        kind: hir::ExprKind::NoInfer(allocator.alloc(tr_expr(allocator, dlogger, operand))),
      },
      ast::UnaryOpKind::Loop => hir::Expr {
        source,
        kind: hir::ExprKind::Loop(allocator.alloc(tr_expr(allocator, dlogger, operand))),
      },
      ast::UnaryOpKind::Pat => hir::Expr {
        source,
        kind: hir::ExprKind::Pat(allocator.alloc(tr_pat(allocator, dlogger, operand))),
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
          expr: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          ty: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::RevConstrain => hir::Expr {
        source,
        kind: hir::ExprKind::Annotate {
          ty: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          expr: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::Defun => hir::Expr {
        source,
        kind:
        // if argument is boxed with brackets, attempt to infer
        if let ast::Expr {
          kind: ast::ExprKind::InferArg(ref inferrable),
          ..
        } = **left_operand
        {
          // if we found an inferrable param
          hir::ExprKind::Defun {
            pattern: allocator.alloc(tr_pat(allocator, dlogger, inferrable)),
            result: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
            infer_pattern: true,
          }
        } else {
            // if concrete param
          hir::ExprKind::Defun {
            pattern: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
            result: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
            infer_pattern: false,
          }
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
        // whether to provide inference args or not
        kind: if let ast::Expr {
          kind: ast::ExprKind::InferArg(ref inferrable),
          ..
        } = **right_operand
        {
          // if argument is boxed with brackets, attempt to infer
          hir::ExprKind::Apply {
            fun: allocator.alloc(hir::Expr {
              source: left_operand,
              kind: hir::ExprKind::NoInfer(allocator.alloc(tr_expr(
                allocator,
                dlogger,
                left_operand,
              ))),
            }),
            arg: allocator.alloc(tr_expr(allocator, dlogger, inferrable)),
          }
        } else {
          // otherwise directly add
          hir::ExprKind::Apply {
            fun: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
            arg: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
          }
        },
      },
      ast::BinaryOpKind::RevApply => hir::Expr {
        source,
        kind: hir::ExprKind::Apply {
          // swap the expressions
          fun: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
          arg: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
        },
      },
      ast::BinaryOpKind::Compose => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_compose".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::PipeForward => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_pipe_forward".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::PipeBackward => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_pipe_backward".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Add => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_add".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Sub => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_sub".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Mul => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_mul".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Div => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_div".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Rem => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_rem".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Pow => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_pow".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::And => hir::Expr {
        // a && b
        // case a
        // of true  = > b
        // || false = > false
        source,
        kind: hir::ExprKind::CaseOf {
          expr: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          source: hir::CaseSource::And,
          case_options: {
            let mut v = Vec::new_in(allocator);
            // true path
            v.push((
              hir::Pat {
                source,
                kind: hir::PatKind::Value(allocator.alloc(gen_bool(source, true))),
              },
              tr_expr(allocator, dlogger, right_operand),
            ));

            // false path
            v.push((
              hir::Pat {
                source,
                kind: hir::PatKind::Value(allocator.alloc(gen_bool(source, false))),
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
          expr: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          source: hir::CaseSource::Or,
          case_options: {
            let mut v = Vec::new_in(allocator);
            // true path
            v.push((
              hir::Pat {
                source,
                kind: hir::PatKind::Value(allocator.alloc(gen_bool(source, true))),
              },
              gen_bool(source, true),
            ));

            // false path
            v.push((
              hir::Pat {
                source,
                kind: hir::PatKind::Value(allocator.alloc(gen_bool(source, false))),
              },
              tr_expr(allocator, dlogger, right_operand),
            ));
            v
          },
        },
      },
      ast::BinaryOpKind::Equal => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_eq".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::NotEqual => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_neq".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Less => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_l".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::LessEqual => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_le".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Greater => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_g".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::GreaterEqual => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_ge".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::RelativeComplement => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_relative_complement".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Union => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_union".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Intersection => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_intersection".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::SymmetricDifference => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_symmetric_difference".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::In => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_in".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Cons => hir::Expr {
        source,
        kind: hir::ExprKind::Cons {
          left_operand: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          right_operand: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::SuchThat => hir::Expr {
        source,
        kind: hir::ExprKind::Refinement {
          ty: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          refinement: allocator.alloc(tr_pat(allocator, dlogger, right_operand)),
        },
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
          pat: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
          val: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
          // this represents an assign at the tail end of an expression.
          // such assigns will not be available outside, and should warn about unused vars
          body: allocator.alloc(hir::Expr {
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
              pat: allocator.alloc(tr_pat(allocator, dlogger, assign_pat)),
              val: allocator.alloc(tr_expr(allocator, dlogger, assign_value)),
              // this represents an assign with a scope after it
              body: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
            },
          }
        } else {
          // continue
          hir::Expr {
            source,
            kind: hir::ExprKind::Sequence {
              fst: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
              snd: allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
            },
          }
        }
      }
      ast::BinaryOpKind::As => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_as".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
        ],
      ),
      ast::BinaryOpKind::Append => gen_apply_fn(
        allocator,
        source,
        hir::Expr {
          source,
          kind: hir::ExprKind::Reference(b"_append".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand)),
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
            kind: hir::ExprKind::StructAccess {
              root: allocator.alloc(tr_expr(allocator, dlogger, left_operand)),
              field: clone_in(allocator, field),
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
  allocator: &'hir Bump,
  mut dlogger: DiagnosticLogger,
) -> hir::Expr<'hir, 'ast, &'hir Bump> {
  tr_expr(allocator, &mut dlogger, ast)
}
