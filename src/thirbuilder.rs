use super::ast;
use super::dlogger::DiagnosticLogger;
use super::hir;
use super::thir;
use super::thirinterpret;
use super::utils::clone_in;
use bumpalo::Bump;
use hashbrown::HashMap;
use num_bigint::BigInt;
use std::alloc::Allocator;

struct LabelScope<'thir, 'hir, 'ast, TA: Allocator + Clone, HA: Allocator + Clone> {
  declaration: Option<&'ast ast::Expr>,
  defers: Vec<&'hir hir::Expr<'hir, 'ast, HA>>,
  // used to typecheck any later returns
  return_ty: Option<&'thir thir::Val<'thir, 'ast, TA>>,
}

struct VarTyScope<'thir, 'ast, TA: Allocator + Clone> {
  declaration: Option<&'ast ast::Expr>,
  ty: &'thir thir::Val<'thir, 'ast, TA>,
}

fn lookup_maybe<'env, Scope, A: Allocator + Clone, TA: Allocator + Clone>(
  env: &'env Vec<(&Vec<u8, A>, Scope), TA>,
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

fn lookup<'env, Scope, A: Allocator + Clone, TA: Allocator + Clone>(
  env: &'env Vec<(&Vec<u8, A>, Scope), TA>,
  label: &[u8],
) -> &'env Scope {
  lookup_maybe(env, label).unwrap().0
}

fn lookup_exists<'env, Scope, A: Allocator + Clone, TA: Allocator + Clone>(
  env: &'env Vec<(&Vec<u8, A>, Scope), TA>,
  label: &[u8],
) -> bool {
  lookup_maybe(env, label).is_some()
}

fn lookup_count_up<'env, Scope, A: Allocator + Clone, TA: Allocator + Clone>(
  env: &'env Vec<(&Vec<u8, A>, Scope), TA>,
  label: &[u8],
) -> usize {
  lookup_maybe(env, label).unwrap().1
}

fn update<'env, Scope, A: Allocator + Clone, TA: Allocator + Clone, F>(
  env: &'env mut Vec<(&Vec<u8, A>, Scope), TA>,
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

// this function will attempt to bestow types on all of the components recursing from bottom up
fn tr_synth_expr<'thir, 'hir, 'ast, HA: Allocator + Clone>(
  allocator: &'thir Bump,
  mut dlogger: &mut DiagnosticLogger,
  source: &'hir hir::Expr<'hir, 'ast, HA>,
  label_env: &mut Vec<(
    &'hir Vec<u8, HA>,
    LabelScope<'thir, 'hir, 'ast, &'thir Bump, HA>,
  )>,
  var_env: &mut Vec<(&'hir Vec<u8, HA>, VarTyScope<'thir, 'ast, &'thir Bump>)>,
) -> thir::Expr<'thir, 'ast, &'thir Bump> {
  match source.kind {
    hir::ExprKind::Error => thir::Expr {
      source: source.source,
      kind: thir::ExprKind::Error,
      ty: allocator.alloc(thir::Val::Error(thir::RuntimeError::InvalidSyntax)),
    },
    hir::ExprKind::Loop(body) => {
      let ty = thir::Val::Nil;

      let body = tr_check_expr(allocator, dlogger, body, label_env, var_env, &ty);

      thir::Expr {
        source: source.source,
        kind: thir::ExprKind::Loop(allocator.alloc(body)),
        ty: allocator.alloc(ty),
      }
    }
    // TODO how does noinfer work
    hir::ExprKind::NoInfer(_) => thir::Expr {
      source: source.source,
      kind: thir::ExprKind::Error,
      ty: allocator.alloc(thir::Val::Error),
    },
    hir::ExprKind::Apply { fun, arg } => {
      // bottom up synthesize the function
      let fun_tr = tr_synth_expr(allocator, dlogger, fun, label_env, var_env);

      if let thir::Val::FunTy { in_ty, out_ty } = fun_tr.ty {
        // typecheck the argument
        let arg_tr = tr_check_expr(allocator, dlogger, arg, label_env, var_env, in_ty);

        // now return the applied function
        thir::Expr {
          source: source.source,
          kind: thir::ExprKind::Apply {
            fun: &fun_tr,
            arg: &arg_tr,
          },
          ty: out_ty,
        }
      } else {
        // log an error that this value isn't callable
        dlogger.log_not_callable(fun_tr.source.range, fun_tr.ty);

        // we will perform a basic synthesis typecheck here to maybe discover any errors
        // however, results won't be used, they're just to inform the user
        let _ = tr_synth_expr(allocator, dlogger, arg, label_env, var_env);

        thir::Expr {
          source: source.source,
          kind: thir::ExprKind::Error,
          ty: allocator.alloc(thir::Val::Error),
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
      let body_tr = tr_check_expr(
        allocator,
        dlogger,
        body,
        label_env,
        var_env,
        &thir::Val::Nil,
      );

      // pop label
      let (_, LabelScope { return_ty, .. }) = label_env.pop().unwrap();

      // ensure that there is at least one ret from label, otherwise throw error that label is unused.
      if let Some(return_ty) = return_ty {
        thir::Expr {
          source: source.source,
          kind: thir::ExprKind::Label(allocator.alloc(body_tr)),
          ty: allocator.alloc(return_ty),
        }
      } else {
        dlogger.log_unused_label(source.source.range);

        thir::Expr {
          source: source.source,
          kind: thir::ExprKind::Error,
          ty: allocator.alloc(thir::Val::Error),
        }
      }
    }
    hir::ExprKind::Defer { label, body } => {
      // attempt to attach the translated body to the defers list
      let updated = update(label_env, &label, |scope| scope.defers.push(body));

      // if unable to update, then throw error that label is undefined
      if !updated {
        dlogger.log_cannot_find_label_in_scope(source.source.range, &label);
      }

      // all defers are replaced with a nil
      thir::Expr {
        source: source.source,
        kind: thir::ExprKind::Nil,
        ty: allocator.alloc(thir::Val::Nil),
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
          ty: allocator.alloc(thir::Val::Error),
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
          update(label_env, &label, |scope| scope.return_ty = Some(&tr.ty));
          tr
        }
      };

      // now make the defer-ret construct
      // ret 'x 0
      // to
      // let toret = 0;
      // (run defers);
      // ret 'x toret
      thir::Expr {
        source: source.source,
        kind: thir::ExprKind::LetIn {
          // introduce variable with value of ret
          pat: allocator.alloc(thir::Pat {
            source: source.source,
            kind: thir::PatKind::BindVariable,
            ty: body_tr.ty,
          }),
          val: allocator.alloc(body_tr),

          // write body
          body: lookup(label_env, &label).defers.iter().fold(
            // the last thing to execute is the actual ret
            allocator.alloc(thir::Expr {
              source: source.source,
              kind: thir::ExprKind::Ret {
                labels_up: lookup_count_up(label_env, &label),
                // lookup variable introduced earlier
                value: allocator.alloc(thir::Expr {
                  source: source.source,
                  kind: thir::ExprKind::Reference(0), // we will grab the most recent bound one
                  ty: body_tr.ty,
                }),
              },
              ty: allocator.alloc(thir::Val::NeverTy),
            }),
            // closure
            |acc, x| {
              // translate defer
              let tr_x =
                tr_check_expr(allocator, dlogger, x, label_env, var_env, &thir::Val::NilTy);
              // return sequenced defer
              allocator.alloc(thir::Expr {
                source: x.source,
                // we want to sequence it in the reverse order we saw them
                // so, later ones will be executed first
                kind: thir::ExprKind::Sequence {
                  fst: allocator.alloc(tr_x),
                  snd: acc,
                },
                ty: tr_x.ty,
              })
            },
          ),
        },
        ty: allocator.alloc(thir::Val::NeverTy),
      }
    }
    hir::ExprKind::StructLiteral(fields) => {
      let fields_tr = Vec::new_in(allocator);

      for (key, value) in fields.iter() {
        fields_tr.push((
          &clone_in(allocator, key),
          tr_synth_expr(allocator, dlogger, value, label_env, var_env),
        ));
      }

      let type_tr = Vec::new_in(allocator);

      for (&key, value) in fields_tr.iter() {
        type_tr.push((&key, value.ty));
      }

      thir::Expr {
        source: source.source,
        kind: thir::ExprKind::StructLiteral(fields_tr),
        ty: allocator.alloc(thir::Val::StructTy(type_tr)),
      }
    }
    hir::ExprKind::StructAccess { root, field } => {
      // translate root
      let root_tr = tr_synth_expr(allocator, dlogger, root, label_env, var_env);
      let field_tr = clone_in(allocator, &field);

      if let thir::Val::Struct(fields) = root_tr.ty {
        // if compatible, attempt to look up ty
        if let Some((field_ty, _)) = lookup_maybe(fields, &field_tr) {
          return thir::Expr {
            source: source.source,
            kind: thir::ExprKind::StructAccess {
              root: allocator.alloc(root_tr),
              field: field_tr,
            },
            ty: field_ty,
          };
        } else {
          // if field doesn't exist. return error
          dlogger.log_nonexistent_field(source.source.range, &field);
        }
      } else {
        // if ty is not a struct, return error
        dlogger.log_not_struct(source.source.range);
      }

      // if we had an error, return error
      thir::Expr {
        source: source.source,
        kind: thir::ExprKind::Error,
        ty: allocator.alloc(thir::Val::Error),
      }
    }
    hir::ExprKind::Reference(identifier) => {
      let lkup_val = lookup_maybe(var_env, &identifier);

      if let Some((varscope, debruijin_index)) = lkup_val {
        thir::Expr {
          source: source.source,
          kind: thir::ExprKind::Reference(debruijin_index),
          ty: varscope.ty,
        }
      } else {
        dlogger.log_variable_not_found(source.source.range, &identifier);
        // return error if not exist
        thir::Expr {
          source: source.source,
          kind: thir::ExprKind::Error,
          ty: allocator.alloc(thir::Val::Error),
        }
      }
    }
    hir::ExprKind::Annotate { expr, ty } => thir::Expr {
      source: source.source,
      kind: thir::ExprKind::Error,
      ty: allocator.alloc(thir::Val::Error),
    },
  }
}

// this function will attempt to check types on all the components recursing top down.
// the
fn tr_check_expr<'thir, 'hir, 'ast, HA: Allocator + Clone>(
  allocator: &'thir Bump,
  mut dlogger: &mut DiagnosticLogger,
  source: &'hir hir::Expr<'hir, 'ast, HA>,
  label_env: &mut Vec<(
    &'hir Vec<u8, HA>,
    LabelScope<'thir, 'hir, 'ast, &'thir Bump, HA>,
  )>,
  var_env: &mut Vec<(&'hir Vec<u8, HA>, VarTyScope<'thir, 'ast, &'thir Bump>)>,
  ty: &thir::Val<'thir, 'ast, &'thir Bump>,
) -> thir::Expr<'thir, 'ast, &'thir Bump> {
  match source.kind {
    hir::ExprKind::Error => thir::Expr {
      source: source.source,
      kind: thir::ExprKind::Error,
      ty: allocator.alloc(thir::Val::Error),
    },
    hir::ExprKind::Loop(body) => {
      // Loop has type nil.
      let body = tr_check_expr(
        allocator,
        dlogger,
        body,
        label_env,
        var_env,
        &thir::Val::Nil,
      );

      thir::Expr {
        source: source.source,
        kind: thir::ExprKind::Nil,
        ty: allocator.alloc(thir::Val::Error),
      }
    }
  }
}

pub fn construct_thir<'thir, 'hir, 'ast, HA: Allocator + Clone>(
  hir: &'hir hir::Expr<'hir, 'ast, HA>,
  allocator: &'thir Bump,
  mut dlogger: DiagnosticLogger,
) -> thir::Expr<'thir, 'ast, &'thir Bump> {
  tr_synth_expr(allocator, &mut dlogger, hir, &mut vec![], &mut vec![])
}
