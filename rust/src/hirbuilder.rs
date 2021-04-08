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
  label: &Vec<u8>,
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
  return None;
}

fn clone_in<A: Allocator, T: Clone>(allocator: A, vec: &Vec<T>) -> Vec<T, A> {
  let mut v = Vec::new_in(allocator);
  v.clone_from_slice(vec.as_slice());
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
  hir::Pat {
    source: Some(source),
    kind: hir::PatKind::None,
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
          defers: label_element.defers.into_inner().into(),
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
            ref bok => {
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
      ast::UnaryOpKind::Ref => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::RefFn,
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::Deref => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::DerefFn,
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::Not => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::NotFn,
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::Complement => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::ComplementFn,
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::NilSafeAssert => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::NilSafeAssertFn,
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
      ast::UnaryOpKind::Async => gen_apply_fn(
        allocator,
        Some(source),
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::AsyncFn,
        },
        vec![allocator.alloc(tr_expr(allocator, dlogger, operand, ls))],
      ),
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
      ast::UnaryOpKind::Val => {
        dlogger.log_unexpected_val(source.range);
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::None,
        }
      }
      ast::UnaryOpKind::Bind => {
        dlogger.log_unexpected_bind(source.range);
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

   // ast::BinaryOpKind::Constrain => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::ConstrainFn, }, ,
   // ast::BinaryOpKind::Defun => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::DefunFn, }, ,
   // ast::BinaryOpKind::CaseOption => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::CaseOptionFn, }, ,
   // ast::BinaryOpKind::Apply => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::ApplyFn, }, ,
   // ast::BinaryOpKind::RevApply => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::RevApplyFn, }, ,
   // ast::BinaryOpKind::Compose => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::ComposeFn, }, ,
   // ast::BinaryOpKind::PipeForward => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::PipeForwardFn, }, ,
   // ast::BinaryOpKind::PipeBackward => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::PipeBackwardFn, }, ,
   // ast::BinaryOpKind::Add => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::AddFn, }, ,
   // ast::BinaryOpKind::Sub => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::SubFn, }, ,
   // ast::BinaryOpKind::Mul => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::MulFn, }, ,
   // ast::BinaryOpKind::Div => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::DivFn, }, ,
   // ast::BinaryOpKind::Rem => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::RemFn, }, ,
   // ast::BinaryOpKind::Pow => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::PowFn, }, ,
   // ast::BinaryOpKind::And => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::AndFn, }, ,
   // ast::BinaryOpKind::Or => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::OrFn, }, ,
   // ast::BinaryOpKind::NilCoalesce => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::NilCoalesceFn, }, ,
   // ast::BinaryOpKind::NilSafeRevApply => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::NilSafeRevApplyFn, }, ,
   // ast::BinaryOpKind::Equal => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::EqualFn, }, ,
   // ast::BinaryOpKind::NotEqual => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::NotEqualFn, }, ,
   // ast::BinaryOpKind::Less => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::LessFn, }, ,
   // ast::BinaryOpKind::LessEqual => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::LessEqualFn, }, ,
   // ast::BinaryOpKind::Greater => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::GreaterFn, }, ,
   // ast::BinaryOpKind::GreaterEqual => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::GreaterEqualFn, }, ,
   // ast::BinaryOpKind::RelativeComplement => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::RelativeComplementFn, }, ,
   // ast::BinaryOpKind::Union => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::UnionFn, }, ,
   // ast::BinaryOpKind::Intersection => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::IntersectionFn, }, ,
   // ast::BinaryOpKind::SymmetricDifference => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::SymmetricDifferenceFn, }, ,
   // ast::BinaryOpKind::In => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::InFn, }, ,
   // ast::BinaryOpKind::Both => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::BothFn, }, ,
   // ast::BinaryOpKind::Either => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::EitherFn, }, ,
   // ast::BinaryOpKind::Range => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::RangeFn, }, ,
   // ast::BinaryOpKind::RangeInclusive => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::RangeInclusiveFn, }, ,
   // ast::BinaryOpKind::Assign => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::AssignFn, }, ,
   // ast::BinaryOpKind::Sequence => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::SequenceFn, }, ,
   // ast::BinaryOpKind::As => gen_apply_fn(allocator, Some(source, hir::Expr { source:Some(source), kind: hir::ExprKind::AsFn, }, ,



  ast::BinaryOpKind::ModuleAccess => ,


    }
  }
}

pub fn construct_hir<'hir, 'ast>(
  ast: &'ast ast::Expr,
  allocator: &'hir Bump,
  mut dlogger: DiagnosticLogger,
) -> hir::Expr<'hir, 'ast, &'hir Bump> {
  tr_expr(allocator, &mut dlogger, ast, None)
}
