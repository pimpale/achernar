use super::thir;
use std::alloc::Allocator;
use std::collections::HashMap;

enum TypeError {}

// computes the alpha equivalence of two normal form exprs
// since we use de brujin indexes, this is easy
pub fn alpha_equivalent<'thir, 'ast, TA: Allocator + Clone>(
  a: &thir::Expr<'thir, 'ast, TA>,
  b: &thir::Expr<'thir, 'ast, TA>,
) -> bool {
  // TODO validate recursively on
  true
}

// binds irrefutable pattern, assigning vars to var_env
// returns how many assignments were made
// INVARIANT: does not alter var_env at all
pub fn bind_irrefutable_pattern<'thir, 'ast, TA: Allocator + Clone>(
  p: &'thir thir::Pat<'thir, 'ast, TA>,
  val: thir::Val<'thir, 'ast, TA>,
  var_env: &mut Vec<thir::Val<'thir, 'ast, TA>>,
) -> Vec<thir::Val<'thir, 'ast, TA>> {
  match p.kind {
    thir::PatKind::Error => unimplemented!(),
    thir::PatKind::BindVariable => {
      vec![val]
    }
    thir::PatKind::BindIgnore => vec![],
    thir::PatKind::Cons {
      fst: fst_p,
      snd: snd_p,
    } => {
      let mut vars = vec![];
      if let thir::Val::Cons {
        fst: fst_val,
        snd: snd_val,
      } = val
      {
        // match first, then second side
        vars.extend(bind_irrefutable_pattern(fst_p, *fst_val, var_env));
        vars.extend(bind_irrefutable_pattern(fst_p, *snd_val, var_env));
      } else {
        unimplemented!();
      }
      vars
    }
    thir::PatKind::ActivePattern { fun: fun_expr, pat } => {
      // get function of active pattern
      let fun = eval(fun_expr, var_env, &mut vec![], false);

      // apply the transform to the provided val
      let tranformed_val = apply(fun, val);

      //now match on the pattern
      bind_irrefutable_pattern(pat, tranformed_val, var_env)
    }
    thir::PatKind::StructLiteral { splat, patterns } => {
      if let thir::Val::Struct(fields_vec) = val {
        let mut vars = vec![];

        // build field hashmap
        let mut fields: HashMap<&Vec<u8, TA>, thir::Val<'thir, 'ast, TA>> =
          fields_vec.into_iter().collect();

        for (field_name, pat) in &patterns {
          if let Some(val) = fields.remove(field_name) {
            vars.extend(bind_irrefutable_pattern(pat, val, var_env));
          } else {
            // log error that no such field exists on the given struct
            unimplemented!();
          }
        }

        // if hashmap isn't empty, then
        if let Some(splat_pat) = splat {
          // send the remaining values to the splat pattern
          vars.extend(bind_irrefutable_pattern(
            splat_pat,
            thir::Val::Struct(fields.into_iter().collect()),
            var_env,
          ));
        } else if !fields.is_empty() {
          // throw error about which fields are not being used
          unimplemented!();
        }

        vars
      } else {
        unimplemented!();
      }
    }
    // explain how these aren't able to be bound
    _ => unimplemented!(),
  }
}

pub fn apply<'thir, 'ast, TA: Allocator + Clone>(
  fun: thir::Val<'thir, 'ast, TA>,
  arg: thir::Val<'thir, 'ast, TA>,
) -> thir::Val<'thir, 'ast, TA> {
  match fun {
    thir::Val::Fun(closure) => apply_closure(closure, arg),
    // this is the case where the function is an unresolved
    thir::Val::Neutral {
      val: neutral_val,
      ty,
    } => {
      match *ty {
        thir::Val::FunTy { in_ty, out_ty } => thir::Val::Neutral {
          // Remember, FunTy is a Pi type
          // In Pi types, the second type depends on the value provided to the function
          ty: Box::new(apply_closure(*out_ty, arg)),
          //construct a new neutral app
          val: Box::new(thir::Neutral::App {
            fun: neutral_val,
            arg: thir::NormalForm {
              term: arg,
              ty: *in_ty,
            },
          }),
        },
        _ => thir::Val::Error(thir::RuntimeError::AppliedNonFunctionNeutral),
      }
    }
    _ => thir::Val::Error(thir::RuntimeError::AppliedNonFunction),
  }
}

pub fn apply_closure<'thir, 'ast, TA: Allocator + Clone>(
  clos: thir::Closure<'thir, 'ast, TA>,
  arg: thir::Val<'thir, 'ast, TA>,
) -> thir::Val<'thir, 'ast, TA> {
  // expand arg into the bound vars
  let bound_vars = bind_irrefutable_pattern(&clos.pat, arg, &mut clos.ext_env);

  // gather all variables
  clos.ext_env.extend(bound_vars);

  // finally, eval the closure
  eval(clos.expr, &mut clos.ext_env, &mut vec![], true)
}

// INVARIANT: the length of the vector will not change
// INVARIANT: if pure is enabled, then we won't alter any variables currently in var_env
pub fn eval<'thir, 'ast, TA: Allocator + Clone>(
  e: &'thir thir::Expr<'thir, 'ast, TA>,
  var_env: &mut Vec<thir::Val<'thir, 'ast, TA>>,
  label_env: &mut Vec<()>,
  pure: bool,
) -> thir::Val<'thir, 'ast, TA> {
  match e.kind {
    thir::ExprKind::Error => thir::Val::Error(thir::RuntimeError::InvalidSyntax),
    thir::ExprKind::Loop(body) => loop {
      if let thir::Val::Never { returned, levelsUp } = eval(body, var_env, label_env, pure) {
        break thir::Val::Never { returned, levelsUp };
      }
    },
    thir::ExprKind::Apply { fun, arg } => eval_apply(fun, arg),
    thir::ExprKind::Label(expr) => {
        label_env.push(());
        if let Never { eval(expr, var_env, label_env, pure);

    }
  }
}
