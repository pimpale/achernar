use super::ast;
use super::dlogger::DiagnosticLogger;
use super::hir;
use bumpalo::Bump;
use num_bigint::BigInt;
use std::alloc::Allocator;
use std::cell::RefCell;

struct HirBuilder<'hir> {
  allocator: &'hir Bump,
  dlogger: DiagnosticLogger,
}

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

fn clone_in<'hir, A: Allocator, T: Clone>(allocator: A, vec: &Vec<T>) -> Vec<T, A> {
  let mut v = Vec::new_in(allocator);
  v.clone_from_slice(vec.as_slice());
  v
}

impl<'hir> HirBuilder<'hir> {
  fn gen_apply_fn<'ast>(
    &mut self,
    source: Option<&'ast ast::Expr>,
    fun: hir::Expr<'hir, 'ast, &'hir Bump>,
    args: Vec<&'hir hir::Expr<'hir, 'ast, &'hir Bump>>,
  ) -> hir::Expr<'hir, 'ast, &'hir Bump> {
    args.iter().fold(fun, |acc, x| hir::Expr {
      source,
      kind: hir::ExprKind::Apply {
        fun: self.allocator.alloc(acc),
        arg: x,
      },
    })
  }

  fn tr_pat<'ast>(&mut self, source: &'ast ast::Expr) -> hir::Pat<'hir, 'ast, &'hir Bump> {
      hir::Pat {
        source: Some(source),
        kind: hir::PatKind::None,
      }
  }

  fn tr_expr<'ast>(
    &mut self,
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
          self.gen_apply_fn(
            Some(source),
            hir::Expr {
              source: Some(source),
              kind: hir::ExprKind::ConsFn,
            },
            vec![
              // first arg is the new expr for the int
              self.allocator.alloc(hir::Expr {
                source: Some(source),
                kind: hir::ExprKind::Int(BigInt::from(*x)),
              }),
              // second arg is the current tail of the list
              self.allocator.alloc(acc),
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
          defers: RefCell::new(Vec::new_in(self.allocator)),
        };

        // parse body
        let scope = self.tr_expr(body, Some(&label_element));

        // return label
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::Label {
            defers: label_element.defers.into_inner().into(),
            scope: self.allocator.alloc(scope),
          },
        }
      }
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
              .push(self.tr_expr(body, dle.prior_le));

            // return a nil element to replace the defer
            hir::Expr {
              source: Some(source),
              kind: hir::ExprKind::Nil,
            }
          } else {
            // means that there are no matching labels
            // throw diagnostic
            self
              .dlogger
              .log_cannot_find_label_in_scope(source.range, label.clone());
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
                value: self.allocator.alloc(self.tr_expr(body, ls)),
                labels_up,
              },
            }
          } else {
            // means that there are no matching labels
            // throw diagnostic
            self
              .dlogger
              .log_cannot_find_label_in_scope(source.range, label.clone());
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
        kind: hir::ExprKind::StructLiteral(self.allocator.alloc(self.tr_expr(body, ls))),
      },
      ast::ExprKind::Reference(ref identifier) => hir::Expr {
        source: Some(source),
        kind: hir::ExprKind::Reference(clone_in(self.allocator, identifier)),
      },
      ast::ExprKind::CaseOf {
        ref expr,
        ref cases,
      } => {
        let mut case_options = Vec::new_in(self.allocator);

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
                case_options.push((self.tr_pat(left_operand), self.tr_expr(right_operand, ls)))
              }
              ref bok => {
                self
                  .dlogger
                  .log_expected_case_option_binop(current.range, bok);
              }
            },
            ref kind => {
              self
                .dlogger
                .log_expected_case_option_expr(current.range, kind);
            }
          }
        }

        // return case option
        hir::Expr {
          source: Some(source),
          kind: hir::ExprKind::CaseOf {
            expr: self.allocator.alloc(self.tr_expr(expr, ls)),
            case_options,
          },
        }
      }

      ast::ExprKind::UnaryOp {
          ref op,
          ref operand
      } => match op {
        ast::UnaryOpKind::Ref =>
        ast::UnaryOpKind::Deref =>
        ast::UnaryOpKind::Not=>
        ast::UnaryOpKind::Not=>
        ast::UnaryOpKind::Bind =>
        ast::UnaryOpKind::Complement =>
        ast::UnaryOpKind::Enum =>
        ast::UnaryOpKind::Struct =>
        ast::UnaryOpKind::Struct =>

        ast::UnaryOpKind::Async =>
        ast::UnaryOpKind::Await =>
      }
    }
  }
}

pub fn construct_hir<'hir, 'ast>(
  ast: &'ast ast::Expr,
  allocator: &'hir Bump,
  dlogger: DiagnosticLogger,
) -> hir::Expr<'hir, 'ast, &'hir Bump> {
  let mut hb = HirBuilder { allocator, dlogger };

  hb.tr_expr(ast, None)
}
