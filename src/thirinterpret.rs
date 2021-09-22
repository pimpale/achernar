use super::thir;
use std::alloc::Allocator;

enum TypeError{

}

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
pub fn bind_irrefutable_pattern<'thir, 'ast, TA: Allocator + Clone>(
  p: &'thir thir::Pat<'thir, 'ast, TA>,
  val: &'thir thir::Val<'thir, 'ast, TA>,
  allocator: TA,
  var_env: &mut Vec<thir::Val<'thir, 'ast, TA>>,
) -> Vec<thir::Val<'thir, 'ast, TA>> {
  let vars = vec![];
  match p.kind {
    thir::PatKind::Error => (),
    thir::PatKind::BindVariable => {
        vars.push(val);
    },
    thir::PatKind::BindIgnore => {
        // do nothing
        ()
    },
    thir::PatKind::Cons { fst: fst_p, snd: snd_p } => {
      if let thir::Val::Cons { fst: fst_val, snd: snd_val } = val {
          // match both sides
          vars.extend(&bind_irrefutable_pattern(fst_p, fst_val, allocator, var_env));
          vars.extend(&bind_irrefutable_pattern(fst_p, snd_val, allocator, var_env));
      } else {
          unimplemented!();
      }
    }
    thir::PatKind::ActivePattern { fun, arg } => {
        eval(fun)
    }
  }

  vars
}

pub fn apply_pure<'thir, 'ast, TA: Allocator + Clone>(
  e: &'thir thir::Expr<'thir, 'ast, TA>,
  allocator: TA,


pub fn eval<'thir, 'ast, TA: Allocator + Clone>(
  e: &'thir thir::Expr<'thir, 'ast, TA>,
  allocator: TA,
  var_env: &mut Vec<&'thir thir::Val<'thir, 'ast, TA>>,
  label_env: &mut Vec<&'thir thir::Val<'thir, 'ast, TA>>,
) -> thir::Val<'thir, 'ast, TA> {
  match e.kind {
    thir::ExprKind::Error => thir::Val::Error,
    thir::ExprKind::Loop(body) => loop {
      if let thir::Val::Never { returned, levelsUp } = eval(body, allocator, var_env, label_env) {
        break thir::Val::Never { returned, levelsUp };
      }
    },
    thir::ExprKind::Apply { fun, arg }  => {

    }

  }
}
