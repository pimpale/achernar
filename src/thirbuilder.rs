use super::ast;
use super::dlogger::DiagnosticLogger;
use super::hir;
use super::thir;
use bumpalo::Bump;
use num_bigint::BigInt;
use std::alloc::Allocator;

struct LabelScope<'thir, 'hir, 'ast, TA: Allocator, HA: Allocator> {
  declaration: Option<&'ast ast::Expr>,
  label: &'hir Vec<u8, HA>,
  defers: Vec<thir::Expr<'thir, 'ast, TA>>,
}

struct VarScope<'thir, 'hir, 'ast, TA: Allocator, HA: Allocator> {
  declaration: Option<&'ast ast::Expr>,
  var: &'hir Vec<u8, HA>,
  ty: &'thir thir::Ty<'thir, TA>,
}

struct VarEnvironment<'thir, 'hir, 'ast, TA: Allocator, HA: Allocator> {
  vars: Vec<VarScope<'thir, 'hir, 'ast, TA, HA>>,
}

struct LabelEnvironment<'thir, 'hir, 'ast, TA: Allocator, HA: Allocator> {
  labels: Vec<LabelScope<'thir, 'hir, 'ast, TA, HA>>,
}

fn Loookup_label<'env, 'thir, 'hir, 'ast, TA: Allocator, HA: Allocator>(
  env: &'env LabelEnvironment<'thir, 'hir, 'ast, TA, HA>,
  label: &[u8],
) -> Option<&'env LabelScope<'thir, 'hir, 'ast, TA, HA>> {
  for l in env.labels.iter().rev() {
    if label == l.label {
      return Some(l);
    }
  }
  // if not found
  None
}

fn lookup_var<'env, 'thir, 'hir, 'ast, TA: Allocator, HA: Allocator>(
  env: &'env VarEnvironment<'thir, 'hir, 'ast, TA, HA>,
  var: &[u8],
) -> Option<&'env VarScope<'thir, 'hir, 'ast, TA, HA>> {
  for v in env.vars.iter().rev() {
    if var == v.var {
      return Some(v);
    }
  }
  // if not found
  None
}

fn print_type<'thir, TA: Allocator>(
    ty: thir::Ty<'thir, TA>,
) -> String {
    format!("{:?}", ty)
}

// this function will attempt to bestow types on all of the components recursing from bottom up
fn tr_synth_expr<'thir, 'hir, 'ast, HA: Allocator>(
  allocator: &'thir Bump,
  mut dlogger: &mut DiagnosticLogger,
  source: &'hir hir::Expr<'hir, 'ast, HA>,
  label_env: &mut LabelEnvironment<'thir, 'hir, 'ast, &'thir Bump, HA>,
  var_env: &mut VarEnvironment<'thir, 'hir, 'ast, &'thir Bump, HA>,
) -> thir::Expr<'thir, 'ast, &'thir Bump> {
  match source.kind {
    hir::ExprKind::Error => thir::Expr {
      source: source.source,
      kind: thir::ExprKind::Error,
      ty: None,
    },
    hir::ExprKind::Loop(body) => {
      let ty = thir::Ty::Nil;

      let body = tr_check_expr(allocator, dlogger, body, label_env, var_env, &ty);

      thir::Expr {
        source: source.source,
        kind: thir::ExprKind::Loop(allocator.alloc(body)),
        ty: Some(&ty),
      }
    }
    // TODO how does noinfer work
    hir::ExprKind::NoInfer(_) => thir::Expr {
      source: source.source,
      kind: thir::ExprKind::Error,
      ty: None,
    },
    hir::ExprKind::Apply { fun, arg } => {
      // bottom up synthesize the function
      let fun_tr = tr_synth_expr(allocator, dlogger, fun, label_env, var_env);

      if let Some(thir::Ty::Fun { in_ty, out_ty}) = fun_tr.ty {
        // typecheck the argument
        let arg_tr = tr_check_expr(allocator, dlogger, arg, label_env, var_env, in_ty);

        // now return the applied function
        thir::Expr {
          source: source.source,
          kind: thir::ExprKind::Apply {
            fun: &fun_tr,
            arg: &arg_tr,
          },
          ty: Some(out_ty),
        }
      } else {
        // log an error that this value isn't callable
        dlogger.log_not_callable(
        // we will perform a basic synthesis typecheck here to maybe discover any errors
        // however, results won't be used, they're just to inform the user
        let _ = tr_synth_expr(allocator, dlogger, arg, label_env, var_env);

        thir::Expr {
          source: source.source,
          kind: thir::ExprKind::Error,
          ty: None,
        }
      }
    }
  }
}

// this function will attempt to check types on all the components recursing top down.
// the
fn tr_check_expr<'thir, 'hir, 'ast, HA: Allocator>(
  allocator: &'thir Bump,
  mut dlogger: &mut DiagnosticLogger,
  source: &'hir hir::Expr<'hir, 'ast, HA>,
  label_env: &mut LabelEnvironment<'thir, 'hir, 'ast, &'thir Bump, HA>,
  var_env: &mut VarEnvironment<'thir, 'hir, 'ast, &'thir Bump, HA>,
  ty: &thir::Ty<'thir, &'thir Bump>,
) -> thir::Expr<'thir, 'ast, &'thir Bump> {
  match source.kind {
    hir::ExprKind::Error => thir::Expr {
      source: source.source,
      kind: thir::ExprKind::Error,
      ty: None,
    },
    hir::ExprKind::Loop(body) => {
      // Loop has type nil.
      let body = tr_check_expr(allocator, dlogger, body, label_env, var_env);

      thir::Expr {
        source: source.source,
        kind: thir::ExprKind::Loop(allocator.allocate()),
        ty: None,
      }
    }
  }
}

pub fn construct_thir<'thir, 'hir, 'ast, HA: Allocator>(
  hir: hir::Expr<'hir, 'ast, HA>,
  allocator: &'thir Bump,
  mut dlogger: DiagnosticLogger,
) -> thir::Expr<'thir, 'ast, &'thir Bump> {
  let label_env = LabelEnvironment { labels: vec![] };
  let var_env = VarEnvironment { vars: vec![] };

  tr_expr(allocator, &mut dlogger, &hir, &mut label_env, &mut var_env)
}

// ls: Option<&LabelElement<'_, 'hir, 'ast, &'hir Bump>>,

//     ast::ExprKind::Label {
//       ref label,
//       ref body,
//     } => {
//       // Create boxed label
//       let label_element = LabelElement {
//         label,
//         prior_le: ls,
//         defers: RefCell::new(Vec::new_in(allocator)),
//       };
//
//       // parse body
//       let scope = tr_expr(allocator, dlogger, body, Some(&label_element));
//
//       // return label
//       hir::Expr {
//         source: Some(source),
//         kind: hir::ExprKind::Label {
//           defers: label_element.defers.into_inner(),
//           scope: allocator.alloc(scope),
//         },
//       }
//     }
//
//

//    ast::ExprKind::Defer {
//      label: ref maybe_label,
//      ref body,
//    } => {
//      // fail if label wasn't specified none
//      if let Some(label) = maybe_label {
//        // clone last label element with matching name
//        if let Some((dle, _)) = get_label(ls, label) {
//          // we push the defer to the end
//          dle
//            .defers
//            .borrow_mut()
//            .push(tr_expr(allocator, dlogger, body, dle.prior_le));
//
//          // return a nil element to replace the defer
//          hir::Expr {
//            source: Some(source),
//            kind: hir::ExprKind::Nil,
//          }
//        } else {
//          // means that there are no matching labels
//          // throw diagnostic
//          dlogger.log_cannot_find_label_in_scope(source.range, label.clone());
//          hir::Expr {
//            source: Some(source),
//            kind: hir::ExprKind::None,
//          }
//        }
//      } else {
//        // a label was never properly provided
//        // an error should already have been thrown, so don't double report
//        hir::Expr {
//          source: Some(source),
//          kind: hir::ExprKind::None,
//        }
//      }
//    }
//
//

//      // fail if label wasn't specified none
//      if let Some(label) = maybe_label {
//        // clone last label element with matching name
//        if let Some((_, labels_up)) = get_label(ls, label) {
//          // return a nil element to replace the defer
//          hir::Expr {
//            source: Some(source),
//            kind: hir::ExprKind::Ret {
//              value: allocator.alloc(tr_expr(allocator, dlogger, body)),
//              labels_up,
//            },
//          }
//        } else {
//          // means that there are no matching labels
//          // throw diagnostic
//          dlogger.log_cannot_find_label_in_scope(source.range, label.clone());
//          hir::Expr {
//            source: Some(source),
//            kind: hir::ExprKind::None,
//          }
//        }
//      } else {
//        // a label was never properly provided
//        // an error should already have been thrown, so don't double report
//        hir::Expr {
//          source: Some(source),
//          kind: hir::ExprKind::None,
//        }
//      }
//
//
//
