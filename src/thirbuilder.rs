use super::ast;
use super::dlogger::DiagnosticLogger;
use super::hir;
use super::thir;
use bumpalo::Bump;
use num_bigint::BigInt;
use std::alloc::Allocator;

struct LabelScope<'thir, 'ast, TA: Allocator> {
  declaration: Option<&'ast ast::Expr>,
  defers: Vec<thir::Expr<'thir, 'ast, TA>>,
  // used to typecheck any later returns
  return_ty: Option<&'thir thir::Ty<'thir, TA>>,
}

struct VarScope<'thir, 'ast, TA: Allocator> {
  declaration: Option<&'ast ast::Expr>,
  ty: &'thir thir::Ty<'thir, TA>,
}

fn lookup_maybe<'env, 'hir, HA: Allocator, Scope>(
  env: &'env Vec<(&'hir Vec<u8, HA>, Scope)>,
  label: &[u8],
) -> Option<(&'env Scope, u32)> {
  let mut count: u32 = 1;
  for (scopelabel, scope) in env.iter().rev() {
    if &label == scopelabel {
      return Some((scope, count));
    } else {
      count += 1;
    }
  }
  return None;
}

fn lookup<'env, 'hir, HA: Allocator, Scope>(
  env: &'env Vec<(&'hir Vec<u8, HA>, Scope)>,
  label: &[u8],
) -> &'env Scope {
  lookup_maybe(env, label).unwrap().0
}

fn lookup_exists<'env, 'hir, HA: Allocator, Scope>(
  env: &'env Vec<(&'hir Vec<u8, HA>, Scope)>,
  label: &[u8],
) -> bool {
  lookup_maybe(env, label).is_some()
}

fn lookup_count_up<'env, 'hir, HA: Allocator, Scope>(
  env: &'env Vec<(&'hir Vec<u8, HA>, Scope)>,
  label: &[u8],
) -> u32 {
  lookup_maybe(env, label).unwrap().1
}

fn update<'env, 'hir, HA: Allocator, Scope, F>(
  env: &'env mut Vec<(&'hir Vec<u8, HA>, Scope)>,
  label: &[u8],
  update: F,
) -> bool
where
  F: FnOnce(&'env mut Scope),
{
  for (scopelabel, scope) in env.iter_mut().rev() {
    if label == scopelabel.as_slice() {
      update(scope);
      return true;
    }
  }
  return false;
}

fn gen_sequence_fn<'thir, 'ast>(
  allocator: &'thir Bump,
  source: &'ast ast::Expr,
  stmnts: impl Iterator<Item = &'thir thir::Expr<'thir, 'ast, &'thir Bump>>,
  last: &'thir thir::Expr<'thir, 'ast, &'thir Bump>,
) -> &'thir thir::Expr<'thir, 'ast, &'thir Bump> {
  stmnts
    .chain(std::iter::once(last))
    .reduce(|acc, x| {
      allocator.alloc(thir::Expr {
        source,
        kind: thir::ExprKind::Sequence { fst: acc, snd: x },
        ty: x.ty,
      })
    })
    .unwrap()
}

fn print_ty<'thir, TA: Allocator>(ty: Option<&thir::Ty<'thir, TA>>) -> String {
  if let Some(ty) = ty {
    format!("{}", ty)
  } else {
    "UNKNOWN".to_owned()
  }
}

fn ty_equal<'thir, TA: Allocator>(a: &thir::Ty<'thir, TA>, b: &thir::Ty<'thir, TA>) -> bool {
  // TODO
  true
}

// this function will attempt to bestow types on all of the components recursing from bottom up
fn tr_synth_expr<'thir, 'hir, 'ast, HA: Allocator>(
  allocator: &'thir Bump,
  mut dlogger: &mut DiagnosticLogger,
  source: &'hir hir::Expr<'hir, 'ast, HA>,
  label_env: &mut Vec<(&'hir Vec<u8, HA>, LabelScope<'thir, 'ast, &'thir Bump>)>,
  var_env: &mut Vec<(&'hir Vec<u8, HA>, VarScope<'thir, 'ast, &'thir Bump>)>,
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
        ty: Some(allocator.alloc(ty)),
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

      if let Some(thir::Ty::Fun { in_ty, out_ty }) = fun_tr.ty {
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
        dlogger.log_not_callable(fun_tr.source.range, &print_ty(fun_tr.ty));

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
    hir::ExprKind::Label { label, body } => {
      // introduce label into the label environment
      label_env.push((
        &label,
        LabelScope {
          declaration: Some(source.source),
          defers: vec![],
          return_ty: None,
        },
      ));

      // now translate the body
      // the body must evaluate to nil
      let body_tr = tr_check_expr(allocator, dlogger, body, label_env, var_env, &thir::Ty::Nil);

      // pop label
      let (_, LabelScope { return_ty, .. }) = label_env.pop().unwrap();

      // ensure that there is at least one ret from label, otherwise throw error that label is unused.
      if let Some(return_ty) = return_ty {
        thir::Expr {
          source: source.source,
          kind: thir::ExprKind::Label(allocator.alloc(body_tr)),
          ty: Some(allocator.alloc(return_ty)),
        }
      } else {
        dlogger.log_unused_label(source.source.range);

        thir::Expr {
          source: source.source,
          kind: thir::ExprKind::Error,
          ty: None,
        }
      }
    }
    hir::ExprKind::Defer { label, body } => {
      // typecheck body
      let body_tr = tr_check_expr(allocator, dlogger, body, label_env, var_env, &thir::Ty::Nil);

      // attempt to attach the translated body to the defers list
      let updated = update(label_env, &label, |scope| scope.defers.push(body_tr));

      // if unable to update, then throw error that label is undefined
      if !updated {
        dlogger.log_cannot_find_label_in_scope(source.source.range, &label);
      }

      // all defers are replaced with a nil
      thir::Expr {
        source: source.source,
        kind: thir::ExprKind::Nil,
        ty: Some(allocator.alloc(thir::Ty::Nil)),
      }
    }
    hir::ExprKind::Ret { label, body } => {
      // check that the label exists
      if !lookup_exists(label_env, &label) {
        // return error that we can't find label
        dlogger.log_cannot_find_label_in_scope(source.source.range, &label);

        // for user convenience attempt typecheck and catch errors
        tr_synth_expr(allocator, dlogger, body, label_env, var_env);

        // end up returning error
        return thir::Expr {
          source: source.source,
          kind: thir::ExprKind::Error,
          ty: None,
        };
      }

      // if we have a concrete return type, then we can check type
      let body_tr = match lookup(label_env, &label) {
        LabelScope {
          return_ty: Some(ty),
          ..
        } => tr_check_expr(allocator, dlogger, body, label_env, var_env, ty),
        LabelScope {
          return_ty: None, ..
        } => {
          let tr = tr_synth_expr(allocator, dlogger, body, label_env, var_env);
          update(label_env, &label, |scope| scope.return_ty = tr.ty);
          tr
        }
      };

      // no name with a grave mark can appear
      let retvarid = b"`retval`".to_vec_in(allocator);

      // now construct the defer-ret construct
      thir::Expr {
        source: source.source,
        kind: thir::ExprKind::LetIn {
          // introduce variable with value of ret
          pat: allocator.alloc(thir::Pat {
            source: source.source,
            kind: thir::PatKind::BindIdentifier(retvarid),
            ty: body_tr.ty,
          }),
          val: allocator.alloc(body_tr),

          // write body
          body: gen_sequence_fn(
            allocator,
            source.source,
            // main statments
            lookup(label_env, &label).defers.iter().rev(),
            // terminator
            allocator.alloc(thir::Expr {
              source: source.source,
              kind: thir::ExprKind::Ret {
                labels_up: lookup_count_up(label_env, &label),

                // lookup variable introduced earlier
                value: allocator.alloc(thir::Expr {
                  source: source.source,
                  kind: thir::ExprKind::Reference(retvarid),
                  ty: body_tr.ty,
                }),
              },
              ty: Some(allocator.alloc(thir::Ty::Never)),
            }),
          ),
        },
        ty: Some(allocator.alloc(thir::Ty::Never)),
      }
    }
    thir::ExprKind::StructLiteral(body) => {}
  }
}

// this function will attempt to check types on all the components recursing top down.
// the
fn tr_check_expr<'thir, 'hir, 'ast, HA: Allocator>(
  allocator: &'thir Bump,
  mut dlogger: &mut DiagnosticLogger,
  source: &'hir hir::Expr<'hir, 'ast, HA>,
  label_env: &mut Vec<(&'hir Vec<u8, HA>, LabelScope<'thir, 'ast, &'thir Bump>)>,
  var_env: &mut Vec<(&'hir Vec<u8, HA>, VarScope<'thir, 'ast, &'thir Bump>)>,
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
      let body = tr_check_expr(allocator, dlogger, body, label_env, var_env, &thir::Ty::Nil);

      thir::Expr {
        source: source.source,
        kind: thir::ExprKind::Loop(allocator.allocate()),
        ty: None,
      }
    }
  }
}

pub fn construct_thir<'thir, 'hir, 'ast, HA: Allocator>(
  hir: &'hir hir::Expr<'hir, 'ast, HA>,
  allocator: &'thir Bump,
  mut dlogger: DiagnosticLogger,
) -> thir::Expr<'thir, 'ast, &'thir Bump> {
  tr_synth_expr(allocator, &mut dlogger, hir, &mut vec![], &mut vec![])
}