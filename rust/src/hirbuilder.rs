use super::ast;
use super::dlogger::DiagnosticLogger;
use super::hir;
use bumpalo::Bump;
use num_bigint::BigInt;
use std::alloc::Allocator;
use std::cell::RefCell;

struct LabelElement<'le, 'hir, 'ast, A: Allocator> {
  prior_le: Option<&'le LabelElement<'le, 'hir, 'ast, A>>,
  label: &'ast Vec<u8>,
  defers: RefCell<Vec<hir::Expr<'hir, 'ast, A>, A>>,
}

fn get_label<'le, 'hir, 'ast, A: Allocator>(
  mut le: Option<&'le LabelElement<'le, 'hir, 'ast, A>>,
  label: &[u8],
) -> Option<(&'le LabelElement<'le, 'hir, 'ast, A>, u64)> {
  let mut labels_up = 1;
  while le.is_some() {
    if le.map(|x| x.label == label) == Some(true) {
      return Some((&le.unwrap(), labels_up));
    } else {
      le = le.unwrap().prior_le;
      labels_up += 1;
    }
  }
  // if not found
  None
}

fn clone_in<A: Allocator, T: Clone>(allocator: A, slice: &[T]) -> Vec<T, A> {
  let mut v = Vec::new_in(allocator);
  v.extend_from_slice(slice);
  v
}

fn gen_apply_fn<'hir, 'ast>(
  allocator: &'hir Bump,
  source: Option<&'ast ast::Expr>,
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

fn tr_pat<'hir, 'ast>(
  allocator: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
) -> hir::Pat<'hir, 'ast, &'hir Bump> {
  match source.kind {
    // propagate error
    ast::ExprKind::None => hir::Pat {
      source: Some(source),
      kind: hir::PatKind::None,
    },
    ast::ExprKind::Hole => hir::Pat {
      source: Some(source),
      kind: hir::PatKind::Hole,
    },
    // transparent valueification
    ast::ExprKind::This
    | ast::ExprKind::NeverType
    | ast::ExprKind::Nil
    | ast::ExprKind::NilType
    | ast::ExprKind::Bool(_)
    | ast::ExprKind::BoolType
    | ast::ExprKind::Int(_)
    | ast::ExprKind::IntType
    | ast::ExprKind::Real(_)
    | ast::ExprKind::RealType
    | ast::ExprKind::String { .. } => hir::Pat {
      source: Some(source),
      kind: hir::PatKind::Value(allocator.alloc(tr_expr(allocator, dlogger, source, None))),
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
        source: Some(source),
        kind: hir::PatKind::None,
      }
    }
    // elide groups
    ast::ExprKind::Group(ref body) => tr_pat(allocator, dlogger, body),
    ast::ExprKind::BindSplat => hir::Pat {
      source: Some(source),
      kind: hir::PatKind::None,
    },
    ast::ExprKind::StructLiteral(ref body) => {
      // create a struct of literals
      let mut patterns = Vec::new_in(allocator);

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
                  ..
                } => patterns.push((
                  clone_in(allocator, identifier),
                  tr_pat(allocator, dlogger, right_operand), // make sure to parse rhs
                )),

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
        source: Some(source),
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
        source: Some(source),
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::Reference(b"_posit".to_vec_in(allocator)),
          }),
          param: allocator.alloc(tr_pat(allocator, dlogger, operand)),
        },
      },
      ast::UnaryOpKind::Negate => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::Reference(b"_negate".to_vec_in(allocator)),
          }),
          param: allocator.alloc(tr_pat(allocator, dlogger, operand)),
        },
      },
      ast::UnaryOpKind::Ref => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::Reference(b"_ref".to_vec_in(allocator)),
          }),
          param: allocator.alloc(tr_pat(allocator, dlogger, operand)),
        },
      },
      ast::UnaryOpKind::Deref => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::Reference(b"_deref".to_vec_in(allocator)),
          }),
          param: allocator.alloc(tr_pat(allocator, dlogger, operand)),
        },
      },
      ast::UnaryOpKind::Not => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::Reference(b"_not".to_vec_in(allocator)),
          }),
          param: allocator.alloc(tr_pat(allocator, dlogger, operand)),
        },
      },
      ast::UnaryOpKind::Complement => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::Reference(b"_complement".to_vec_in(allocator)),
          }),
          param: allocator.alloc(tr_pat(allocator, dlogger, operand)),
        },
      },
      ast::UnaryOpKind::Val => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::Value(allocator.alloc(tr_expr(allocator, dlogger, operand, None))),
      },
      ast::UnaryOpKind::Bind => match **operand {
        // binds a variable to an identifier
        ast::Expr {
          kind: ast::ExprKind::Reference(ref identifier),
          ..
        } => hir::Pat {
          source: Some(source),
          kind: hir::PatKind::BindIdentifier(clone_in(allocator, identifier)),
        },
        // ignores variable
        ast::Expr {
          kind: ast::ExprKind::Hole,
          ..
        } => hir::Pat {
          source: Some(source),
          kind: hir::PatKind::BindIgnore,
        },
        // handle error
        ast::Expr {
          range, ref kind, ..
        } => {
          dlogger.log_unexpected_bind_target(range, kind);
          hir::Pat {
            source: Some(source),
            kind: hir::PatKind::None,
          }
        }
      },
      // these operators must be valified
      ref
      c
      @
      (ast::UnaryOpKind::NilSafeAssert
      | ast::UnaryOpKind::Async
      | ast::UnaryOpKind::Await
      | ast::UnaryOpKind::Struct
      | ast::UnaryOpKind::Enum
      | ast::UnaryOpKind::New
      | ast::UnaryOpKind::Loop
      | ast::UnaryOpKind::Pat) => {
        dlogger.log_unexpected_unop_in_pattern(source.range, c);
        hir::Pat {
          source: Some(source),
          kind: hir::PatKind::None,
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
      | ast::BinaryOpKind::NilCoalesce
      | ast::BinaryOpKind::NilSafeRevApply
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
          source: Some(source),
          kind: hir::PatKind::None,
        }
      }
      ast::BinaryOpKind::Constrain => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::Ty {
          pattern: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
          ty: allocator.alloc(tr_expr(allocator, dlogger, right_operand, None)),
        },
      },
      ast::BinaryOpKind::Apply => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::ActivePattern {
          function: allocator.alloc(tr_expr(allocator, dlogger, left_operand, None)),
          param: allocator.alloc(tr_pat(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::RevApply => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::ActivePattern {
          // swap the expressions
          function: allocator.alloc(tr_expr(allocator, dlogger, right_operand, None)),
          param: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
        },
      },
      ast::BinaryOpKind::And => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::And {
          left_operand: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
          right_operand: allocator.alloc(tr_pat(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::Or => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::Or {
          left_operand: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
          right_operand: allocator.alloc(tr_pat(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::Cons => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::Cons {
          left_operand: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
          right_operand: allocator.alloc(tr_pat(allocator, dlogger, right_operand)),
        },
      },
      ast::BinaryOpKind::Range => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::Range {
          inclusive: false,
          left_operand: allocator.alloc(tr_expr(allocator, dlogger, left_operand, None)),
          right_operand: allocator.alloc(tr_expr(allocator, dlogger, right_operand, None)),
        },
      },
      ast::BinaryOpKind::RangeInclusive => hir::Pat {
        source: Some(source),
        kind: hir::PatKind::Range {
          inclusive: true,
          left_operand: allocator.alloc(tr_expr(allocator, dlogger, left_operand, None)),
          right_operand: allocator.alloc(tr_expr(allocator, dlogger, right_operand, None)),
        },
      },
    },
  }
}

fn tr_expr<'hir, 'ast>(
  allocator: &'hir Bump,
  dlogger: &mut DiagnosticLogger,
  source: &'ast ast::Expr,
  ls: Option<&LabelElement<'_, 'hir, 'ast, &'hir Bump>>,
) -> hir::Expr<'hir, 'ast, &'hir Bump> {
  match source.kind {
    ast::ExprKind::None => hir::Expr {
      source: Some(source),
      kind: hir::ExprKind::None,
    },
    ast::ExprKind::Hole => hir::Expr {
      source: Some(source),
      kind: hir::ExprKind::Hole,
    },
    ast::ExprKind::This => hir::Expr {
      source: Some(source),
      kind: hir::ExprKind::This,
    },
    ast::ExprKind::NeverType => hir::Expr {
      source: Some(source),
      kind: hir::ExprKind::NeverType,
    },
    ast::ExprKind::Nil => hir::Expr {
      source: Some(source),
      kind: hir::ExprKind::Nil,
    },
    ast::ExprKind::NilType => hir::Expr {
      source: Some(source),
      kind: hir::ExprKind::NilType,
    },
    ast::ExprKind::Bool(b) => hir::Expr {
      source: Some(source),
      kind: hir::ExprKind::Bool(b),
    },
    ast::ExprKind::BoolType => hir::Expr {
      source: Some(source),
      kind: hir::ExprKind::BoolType,
    },
    ast::ExprKind::Int(ref i) => hir::Expr {
      source: Some(source),
      // TODO allocate this via bumpalo
      kind: hir::ExprKind::Int(i.clone()),
    },
    ast::ExprKind::IntType => hir::Expr {
      source: Some(source),
      kind: hir::ExprKind::IntType,
    },
    ast::ExprKind::Real(ref i) => hir::Expr {
      source: Some(source),
      // TODO allocate this via bumpalo
      kind: hir::ExprKind::Real(i.clone()),
    },
    ast::ExprKind::RealType => hir::Expr {
      source: Some(source),
      kind: hir::ExprKind::RealType,
    },
    ast::ExprKind::String { ref value, .. } => value.iter().rev().fold(
      // start with a null at the end of the list
      hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::Nil,
      },
      // as we work our way backwards, we prepend the current char as an int to our list
      |acc, x| {
        gen_apply_fn(
          allocator,
          Some(source),
          hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::ConsFn,
          },
          vec![
            // first arg is the new expr for the int
            allocator.alloc(hir::Expr {
              source: Some(source),
              kind: hir::ExprKind::Int(BigInt::from(*x)),
            }),
            // second arg is the current tail of the list
            allocator.alloc(acc),
          ],
        )
      },
    ),
    ast::ExprKind::Label {
      ref label,
      ref body,
    } => {
      // Create boxed label
      let label_element = LabelElement {
        label,
        prior_le: ls,
        defers: RefCell::new(Vec::new_in(allocator)),
      };

      // parse body
      let scope = tr_expr(allocator, dlogger, body, Some(&label_element));

      // return label
      hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::Label {
          defers: label_element.defers.into_inner(),
          scope: allocator.alloc(scope),
        },
      }
    }
    ast::ExprKind::Group(ref body) => tr_expr(allocator, dlogger, body, ls),
    ast::ExprKind::Defer {
      label: ref maybe_label,
      ref body,
    } => {
      // fail if label wasn't specified none
      if let Some(label) = maybe_label {
        // clone last label element with matching name
        if let Some((dle, _)) = get_label(ls, label) {
          // we push the defer to the end
          dle
            .defers
            .borrow_mut()
            .push(tr_expr(allocator, dlogger, body, dle.prior_le));

          // return a nil element to replace the defer
          hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::Nil,
          }
        } else {
          // means that there are no matching labels
          // throw diagnostic
          dlogger.log_cannot_find_label_in_scope(source.range, label.clone());
          hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::None,
          }
        }
      } else {
        // a label was never properly provided
        // an error should already have been thrown, so don't double report
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::None,
        }
      }
    }
    ast::ExprKind::Ret {
      label: ref maybe_label,
      ref body,
    } => {
      // fail if label wasn't specified none
      if let Some(label) = maybe_label {
        // clone last label element with matching name
        if let Some((_, labels_up)) = get_label(ls, label) {
          // return a nil element to replace the defer
          hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::Ret {
              value: allocator.alloc(tr_expr(allocator, dlogger, body, ls)),
              labels_up,
            },
          }
        } else {
          // means that there are no matching labels
          // throw diagnostic
          dlogger.log_cannot_find_label_in_scope(source.range, label.clone());
          hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::None,
          }
        }
      } else {
        // a label was never properly provided
        // an error should already have been thrown, so don't double report
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::None,
        }
      }
    }
    ast::ExprKind::StructLiteral(ref body) => hir::Expr {
      source: Some(source),
      kind: hir::ExprKind::StructLiteral(allocator.alloc(tr_expr(allocator, dlogger, body, ls))),
    },
    ast::ExprKind::Reference(ref identifier) => hir::Expr {
      source: Some(source),
      kind: hir::ExprKind::Reference(clone_in(allocator, identifier)),
    },
    ref c @ ast::ExprKind::BindSplat => {
      dlogger.log_only_in_pattern(source.range, c);
      hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::None,
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
              tr_expr(allocator, dlogger, right_operand, ls),
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
        source: Some(source),
        kind: hir::ExprKind::CaseOf {
          expr: allocator.alloc(tr_expr(allocator, dlogger, expr, ls)),
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
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_posit".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::Negate => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_negate".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::Ref => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_ref".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::Deref => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_deref".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::Not => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_not".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::Complement => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_complement".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::NilSafeAssert => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_nil_safe_assert".to_vec_in(allocator)),
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::Async => hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::Async(allocator.alloc(tr_expr(allocator, dlogger, operand, ls))),
      },
      ast::UnaryOpKind::Await => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::AwaitFn,
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::Struct => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::StructFn,
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::Enum => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::EnumFn,
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::New => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::NewFn,
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::Loop => hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::Loop(allocator.alloc(tr_expr(allocator, dlogger, operand, ls))),
      },
      ast::UnaryOpKind::Pat => hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::Pat(allocator.alloc(tr_pat(allocator, dlogger, operand))),
      },
      ref c @ ast::UnaryOpKind::Val => {
        dlogger.log_unexpected_unop_in_expr(source.range, c);
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::None,
        }
      }
      ref c @ ast::UnaryOpKind::Bind => {
        dlogger.log_unexpected_unop_in_expr(source.range, c);
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::None,
        }
      }
    },
    ast::ExprKind::BinaryOp {
      ref op,
      ref left_operand,
      ref right_operand,
    } => match op {
      ast::BinaryOpKind::Constrain => hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::Ty {
          expr: allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          ty: allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        },
      },
      ast::BinaryOpKind::Defun => hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::Defun {
          pattern: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
          result: allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        },
      },
      ast::BinaryOpKind::CaseOption => {
        dlogger.log_only_in_case(source.range);
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::None,
        }
      }
      ast::BinaryOpKind::Apply => hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::Apply {
          fun: allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          arg: allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        },
      },
      ast::BinaryOpKind::RevApply => hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::Apply {
          // swap the expressions
          fun: allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
          arg: allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
        },
      },
      ast::BinaryOpKind::Compose => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_compose".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::PipeForward => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_pipe_forward".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::PipeBackward => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_pipe_backward".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::Add => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_add".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::Sub => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_sub".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::Mul => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_mul".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::Div => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_div".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::Rem => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_rem".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::Pow => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_pow".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::And => hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::And {
          left_operand: allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          right_operand: allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        },
      },
      ast::BinaryOpKind::Or => hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::Or {
          left_operand: allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          right_operand: allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        },
      },
      ast::BinaryOpKind::NilCoalesce => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_nil_coalesce".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::NilSafeRevApply => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_nil_safe_rev_apply".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::Equal => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_eq".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::NotEqual => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_neq".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::Less => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_l".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::LessEqual => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_le".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::Greater => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_g".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::GreaterEqual => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_ge".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::RelativeComplement => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_relative_complement".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::Union => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_union".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::Intersection => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_intersection".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::SymmetricDifference => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_symmetric_difference".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::In => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_in".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::Cons => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::ConsFn,
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::SuchThat => hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::Refinement {
          ty: allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          refinement: allocator.alloc(tr_pat(allocator, dlogger, right_operand)),
        },
      },
      ref c @ (ast::BinaryOpKind::RangeInclusive | ast::BinaryOpKind::Range) => {
        dlogger.log_unexpected_binop_in_expr(source.range, c);

        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::None,
        }
      }
      ast::BinaryOpKind::Assign => hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::LetIn {
          pat: allocator.alloc(tr_pat(allocator, dlogger, left_operand)),
          val: allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
          // this represents an assign at the tail end of an expression.
          // such assigns will not be available outside, and should warn about unused vars
          body: allocator.alloc(hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::Nil,
          }),
        },
      },
      ast::BinaryOpKind::Sequence => {
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
            source: Some(source),
            kind: hir::ExprKind::LetIn {
              pat: allocator.alloc(tr_pat(allocator, dlogger, assign_pat)),
              val: allocator.alloc(tr_expr(allocator, dlogger, assign_value, ls)),
              // this represents an assign with a scope after it
              body: allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
            },
          }
        } else {
          // continue
          gen_apply_fn(
            allocator,
            Some(source),
            hir::Expr {
              source: Some(source),
              kind: hir::ExprKind::SequenceFn,
            },
            vec![
              allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
              allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
            ],
          )
        }
      }
      ast::BinaryOpKind::As => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_as".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::Append => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Reference(b"_append".to_vec_in(allocator)),
        },
        vec![
          allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
          allocator.alloc(tr_expr(allocator, dlogger, right_operand, ls)),
        ],
      ),
      ast::BinaryOpKind::ModuleAccess => {
        if let ast::Expr {
          kind: ast::ExprKind::Reference(ref field),
          ..
        } = **right_operand
        {
          hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::StructAccess {
              root: allocator.alloc(tr_expr(allocator, dlogger, left_operand, ls)),
              field: clone_in(allocator, field),
            },
          }
        } else {
          dlogger.log_field_not_identifier(right_operand.range, &right_operand.kind);
          hir::Expr {
            source: Some(source),
            kind: hir::ExprKind::None,
          }
        }
      }
    },
  }
}

pub fn construct_hir<'hir, 'ast>(
  ast: &'ast ast::Expr,
  allocator: &'hir Bump,
  mut dlogger: DiagnosticLogger,
) -> hir::Expr<'hir, 'ast, &'hir Bump> {
  tr_expr(allocator, &mut dlogger, ast, None)
}
