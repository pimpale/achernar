use super::dlogger::DiagnosticLogger;
use super::hir;
use super::nbe;
use super::utils::clone_in;
use bumpalo::Bump;
use std::alloc::Allocator;
use std::collections::HashMap;

// this function will attempt to bestow types on all of the components recursing from bottom up
fn tr_synth_expr<'types, 'hir, 'ast, HA: Allocator + Clone>(
  allocator: &'types Bump,
  mut dlogger: &mut DiagnosticLogger,
  source: &'hir hir::ValExpr<'hir, 'ast, HA>,
  label_env: &mut Vec<Option<nbe::Val<'hir, 'ast, &'types Bump>>>,
  var_env: &mut Vec<nbe::Val<'hir, 'ast, &'types Bump>>,
) -> HashMap<u64, nbe::Val<'hir, 'ast, &'types Bump>> {
  match source.kind {
    hir::ValExprKind::Error => HashMap::new(),
    hir::ValExprKind::Loop(body) => {
      let nilty = nbe::Val::NilTy;

      // body should evaluate to nil
      let mut tytable = tr_check_expr(allocator, dlogger, body, label_env, var_env, &nilty);

      // add wrapping loop's type
      tytable.insert(source.id.unwrap(), nilty);

      tytable
    }
    hir::ValExprKind::App { fun, arg } => {
      // get the type of the lower function
      let fun_tytable = tr_synth_expr(allocator, dlogger, fun, label_env, var_env);

      if let Some(nbe::Val::PiTy {
          arg_ty,
          body_dep_ty,
      }) = fun_tytable.get(&fun.id.unwrap())
      {
        // typecheck the argument
        let arg_tytable = tr_check_expr(allocator, dlogger, arg, label_env, var_env, arg_ty);

        // find the output type by providing the value of arg to body_dep_ty

        // the big problem: We need to know the value of `arg` (not just the type!)

        // this would be straightforward if we disallowed mutation:
        // `arg` could be expressed as a simple function of the containing function's parameters
        // This would make things simple, as we could just use alpha equivalence to compare

        // however, since we're allowing mutation, i think we might have to deal with
        // symbolic execution of some kind

        let maybe_fun_val = nbe::eval(fun, &mut vec![], 0);
        let maybe_arg_val = nbe::eval(arg, &mut vec![], 0);

        let source_tytable = HashMap::new();

        if let (Ok(fun_val), Ok(arg_val)) = (maybe_fun_val, maybe_arg_val) {
          match nbe::apply(fun_val, arg_val) {
            Ok(result_ty) => {
              source_tytable.insert(source.id.unwrap(), nbe::read_back(result_ty));
            }
            Err(evalerr) => {
              // report error
              todo!();
            }
          }
        }

        todo!()
      } else {
        // log an error that this value isn't callable
        dlogger.log_not_callable(fun_tr.source.range, fun_tr.ty);

        // we will perform a basic synthesis typecheck here to maybe discover any errors
        // however, results won't be used, they're just to inform the user
        let _ = tr_synth_expr(allocator, dlogger, arg, label_env, var_env);

        thir::ValExpr {
          source: source.source,
          kind: hir::ValExprKind::Error,
          ty: allocator.alloc(Val::Error),
        }
      }
    }
    hir::ValExprKind::Label { label, body } => {
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
      let body_tr = tr_check_expr(allocator, dlogger, body, label_env, var_env, &Val::Nil);

      // pop label
      let (_, LabelScope { return_ty, .. }) = label_env.pop().unwrap();

      // ensure that there is at least one ret from label, otherwise throw error that label is unused.
      if let Some(return_ty) = return_ty {
        thir::ValExpr {
          source: source.source,
          kind: thir::ValExprKind::Label(allocator.alloc(body_tr)),
          ty: allocator.alloc(return_ty),
        }
      } else {
        dlogger.log_unused_label(source.source.range);

        thir::ValExpr {
          source: source.source,
          kind: thir::ValExprKind::Error,
          ty: allocator.alloc(Val::Error),
        }
      }
    }
    hir::ValExprKind::Ret { label, body } => {
      // check that the label exists
      if !lookup_exists(label_env, &label) {
        // return error that we can't find label
        dlogger.log_cannot_find_label_in_scope(source.source.range, &label);

        // for user convenience attempt typecheck and catch errors
        tr_synth_expr(allocator, dlogger, body, label_env, var_env);

        // end up returning error
        return thir::ValExpr {
          source: source.source,
          kind: thir::ValExprKind::Error,
          ty: allocator.alloc(Val::Error),
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

      thir::ValExpr {}
    }
    hir::ValExprKind::StructLiteral(fields) => {
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

      thir::ValExpr {
        source: source.source,
        kind: thir::ValExprKind::StructLiteral(fields_tr),
        ty: allocator.alloc(Val::StructTy(type_tr)),
      }
    }
    hir::ValExprKind::StructAccess { root, field } => {
      // translate root
      let root_tr = tr_synth_expr(allocator, dlogger, root, label_env, var_env);
      let field_tr = clone_in(allocator, &field);

      if let Val::Struct(fields) = root_tr.ty {
        // if compatible, attempt to look up ty
        if let Some((field_ty, _)) = lookup_maybe(fields, &field_tr) {
          return thir::ValExpr {
            source: source.source,
            kind: thir::ValExprKind::StructAccess {
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
      thir::ValExpr {
        source: source.source,
        kind: thir::ValExprKind::Error,
        ty: allocator.alloc(Val::Error),
      }
    }
    hir::ValExprKind::Reference(identifier) => {
      let lkup_val = lookup_maybe(var_env, &identifier);

      if let Some((varscope, debruijin_index)) = lkup_val {
        thir::ValExpr {
          source: source.source,
          kind: thir::ValExprKind::Reference(debruijin_index),
          ty: varscope.ty,
        }
      } else {
        dlogger.log_variable_not_found(source.source.range, &identifier);
        // return error if not exist
        thir::ValExpr {
          source: source.source,
          kind: thir::ValExprKind::Error,
          ty: allocator.alloc(Val::Error),
        }
      }
    }
    hir::ValExprKind::Annotate { expr, ty } => thir::ValExpr {
      source: source.source,
      kind: thir::ValExprKind::Error,
      ty: allocator.alloc(Val::Error),
    },
  }
}

// this function will attempt to check types on all the components recursing top down.
// the
fn tr_check_expr<'types, 'hir, 'ast, HA: Allocator + Clone>(
  allocator: &'types Bump,
  mut dlogger: &mut DiagnosticLogger,
  source: &'hir hir::ValExpr<'hir, 'ast, HA>,
  label_env: &mut Vec<Option<nbe::Val<'hir, 'ast, &'types Bump>>>,
  var_env: &mut Vec<nbe::Val<'hir, 'ast, &'types Bump>>,
  ty: &nbe::Val<'hir, 'ast, &'types Bump>,
) -> HashMap<u64, nbe::Val<'hir, 'ast, &'types Bump>> {
  match source.kind {
    hir::ValExprKind::Error => thir::ValExpr {
      source: source.source,
      kind: thir::ValExprKind::Error,
      ty: None,
    },
    hir::ValExprKind::Loop(body) => {
      // Loop has type nil.
      let body = tr_check_expr(allocator, dlogger, body, label_env, var_env, &Val::Nil);

      thir::ValExpr {
        source: source.source,
        kind: thir::ValExprKind::Nil,
        ty: allocator.alloc(Val::Error),
      }
    }
  }
}

pub fn construct_thir<'types, 'hir, 'ast, HA: Allocator + Clone>(
  hir: &'hir hir::ValExpr<'hir, 'ast, HA>,
  allocator: &'types Bump,
  mut dlogger: DiagnosticLogger,
) -> thir::ValExpr<'types, 'ast, &'types Bump> {
  tr_synth_expr(allocator, &mut dlogger, hir, &mut vec![], &mut vec![])
}
