use super::hir;
use std::alloc::Allocator;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug)]
pub enum EvalError {
  InvalidSyntax,
  AppliedNonFunction,
  AppliedNonFunctionNeutral,
}

// represents the different states a variable can be in
#[derive(Debug)]
pub enum Var<'hir, 'ast, TA: Allocator + Clone> {
  MovedOut,
  MutablyBorrowed(Val<'hir, 'ast, TA>),
  ImmutablyBorrowed(Val<'hir, 'ast, TA>),
  Unborrowed(Val<'hir, 'ast, TA>),
}

#[derive(Debug)]
pub struct Closure<'hir, 'ast, TA: Allocator + Clone> {
  pub ext_env: Vec<Var<'hir, 'ast, TA>>, // external environment
  pub pat: hir::Pat<'hir, 'ast, TA>,
  pub expr: &'hir hir::Expr<'hir, 'ast, TA>,
}

// These are terms that have normalized completely, to the fullest extent possible
#[derive(Debug)]
pub enum Val<'hir, 'ast, TA: Allocator + Clone> {
  Error(EvalError), // signifies error in type resolution

  // Types
  Universe(usize), // type of a type is Universe(1)
  NilTy,
  NeverTy,
  BoolTy,
  U8Ty,
  U16Ty,
  U32Ty,
  U64Ty,
  I8Ty,
  I16Ty,
  I32Ty,
  I64Ty,
  F32Ty,
  F64Ty,

  // This is also known as a sigma type
  ConsTy {
    fst: Box<Val<'hir, 'ast, TA>>,
    snd: Box<Closure<'hir, 'ast, TA>>,
  },
  StructTy(Vec<(&'ast Vec<u8>, &'hir Val<'hir, 'ast, TA>), TA>),
  EnumTy(Vec<(&'ast Vec<u8>, &'hir Val<'hir, 'ast, TA>), TA>),

  // This is also known as a Pi Type
  FunTy {
    in_ty: Box<Val<'hir, 'ast, TA>>,
    out_ty: Box<Closure<'hir, 'ast, TA>>,
  },

  // Values
  Nil,
  Bool(bool),
  U8(u8),
  U16(u16),
  U32(u32),
  U64(u64),
  I8(i8),
  I16(i16),
  I32(i32),
  I64(i64),
  F32(f32),
  F64(f64),
  Cons {
    fst: Box<Val<'hir, 'ast, TA>>,
    snd: Box<Val<'hir, 'ast, TA>>,
  },
  Struct(HashMap<&'ast Vec<u8>, Var<'hir, 'ast, TA>>),
  Enum(Vec<(&'ast Vec<u8>, Val<'hir, 'ast, TA>)>),
  Fun(Closure<'hir, 'ast, TA>),
  Never {
    returned: Box<Val<'hir, 'ast, TA>>,
    levelsUp: usize,
  },
  // Neutral represents a value that we can't evaluate since we're missing information
  Neutral {
    val: Box<Neutral<'hir, 'ast, TA>>,
    ty: Box<Val<'hir, 'ast, TA>>,
  },
}

#[derive(Debug)]
pub enum Neutral<'hir, 'ast, TA: Allocator + Clone> {
  // De Brujin level (not index)
  // this counts from the top of the stack
  Var(u32),
  App {
    fun: Box<Neutral<'hir, 'ast, TA>>,
    arg: NormalForm<'hir, 'ast, TA>,
  },
  StructAccess {
    root: Box<Neutral<'hir, 'ast, TA>>,
    field: &'ast Vec<u8>,
  },
}

#[derive(Debug)]
pub struct NormalForm<'hir, 'ast, TA: Allocator + Clone> {
  term: Val<'hir, 'ast, TA>,
  ty: Val<'hir, 'ast, TA>,
}

impl<TA: Allocator + Clone> fmt::Display for Val<'_, '_, TA> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let val = match self {
      Val::Error(_) => "!!error!!".to_owned(),
      Val::Universe(level) => format!("(Universe {})", level),
      Val::NilTy => "Nil".to_owned(),
      Val::NeverTy => "Never".to_owned(),
      Val::BoolTy => "Bool".to_owned(),
      Val::U8Ty => "U8".to_owned(),
      Val::U16Ty => "U16".to_owned(),
      Val::U32Ty => "U32".to_owned(),
      Val::U64Ty => "U64".to_owned(),
      Val::I8Ty => "I8".to_owned(),
      Val::I16Ty => "I16".to_owned(),
      Val::I32Ty => "I32".to_owned(),
      Val::I64Ty => "I64".to_owned(),
      Val::F32Ty => "F32".to_owned(),
      Val::F64Ty => "F64".to_owned(),
      Val::ConsTy { fst, snd } => format!("(Cons {} {})", fst, snd),
      Val::StructTy(h) => format!(
        "struct {{ {} }}",
        h.iter().fold(String::new(), |a, (key, val)| {
          format!(
            "{}, {}: {}",
            a,
            std::str::from_utf8(key).unwrap(),
            val.to_string()
          )
        })
      ),
      Val::EnumTy(h) => format!(
        "enum {{ {} }}",
        h.iter().fold(String::new(), |a, (key, val)| {
          format!(
            "{}, {}: {}",
            a,
            std::str::from_utf8(key).unwrap(),
            val.to_string()
          )
        })
      ),
      Val::FunTy { in_ty, out_ty } => format!("{} -> {}", in_ty, out_ty.expr),
      Val::Nil => "()".to_owned(),
      Val::Bool(v) => format!("{}", v),
      Val::U8(v) => format!("{}u8", v),
      Val::U16(v) => format!("{}u16", v),
      Val::U32(v) => format!("{}u32", v),
      Val::U64(v) => format!("{}u64", v),
      Val::I8(v) => format!("{}i8", v),
      Val::I16(v) => format!("{}i16", v),
      Val::I32(v) => format!("{}i32", v),
      Val::I64(v) => format!("{}i64", v),
      Val::F32(v) => format!("{}f32", v),
      Val::F64(v) => format!("{}f64", v),
      Val::Cons { fst, snd } => format!("{}, {}", fst, snd),
      _ => "whoops, not implemented".to_owned(),
    };

    write!(f, "{}", val)
  }
}

// computes the alpha equivalence of two normal form exprs
// since we use de brujin indexes, this is easy
pub fn alpha_equivalent<'hir, 'ast, TA: Allocator + Clone>(
  a: &hir::Expr<'hir, 'ast, TA>,
  b: &hir::Expr<'hir, 'ast, TA>,
) -> bool {
  // TODO validate recursively on
  true
}

// binds irrefutable pattern, assigning vars to var_env
// returns how many assignments were made
// INVARIANT: does not alter var_env at all
pub fn bind_irrefutable_pattern<'hir, 'ast, TA: Allocator + Clone>(
  p: &'hir hir::Pat<'hir, 'ast, TA>,
  val: Val<'hir, 'ast, TA>,
  var_env: &mut Vec<Val<'hir, 'ast, TA>>,
) -> Vec<Val<'hir, 'ast, TA>> {
  match p.kind {
    hir::PatKind::Error => unimplemented!(),
    hir::PatKind::BindVariable => {
      vec![val]
    }
    hir::PatKind::BindIgnore => vec![],
    hir::PatKind::Cons {
      fst: fst_p,
      snd: snd_p,
    } => {
      let mut vars = vec![];
      if let Val::Cons {
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
    hir::PatKind::ActivePattern {
      fun: fun_expr,
      arg: pat,
    } => {
      // get function of active pattern
      let fun = eval(fun_expr, var_env, &mut vec![], false);

      // apply the transform to the provided val
      let tranformed_val = apply(fun, val);

      //now match on the pattern
      bind_irrefutable_pattern(pat, tranformed_val, var_env)
    }
    hir::PatKind::StructLiteral(patterns) => {
      if let Val::Struct(fields_vec) = val {
        let mut vars = vec![];

        // build field hashmap
        let mut fields: HashMap<&Vec<u8, TA>, Val<'hir, 'ast, TA>> =
          fields_vec.into_iter().collect();

        for (field_name, pat) in &patterns {
          if let Some(val) = fields.remove(field_name) {
            vars.extend(bind_irrefutable_pattern(pat, val, var_env));
          } else {
            // log error that no such field exists on the given struct
            unimplemented!();
          }
        }

        vars
      } else {
        // Log error that the the value isn't a struct
        unimplemented!();
      }
    }
    // explain how these aren't able to be bound
    _ => unimplemented!(),
  }
}

pub fn apply<'hir, 'ast, TA: Allocator + Clone>(
  fun: Val<'hir, 'ast, TA>,
  arg: Val<'hir, 'ast, TA>,
) -> Val<'hir, 'ast, TA> {
  match fun {
    Val::Fun(closure) => apply_closure(closure, arg),
    // this is the case where the function is an unresolved
    Val::Neutral {
      val: neutral_val,
      ty,
    } => {
      match *ty {
        Val::FunTy { in_ty, out_ty } => Val::Neutral {
          // Remember, FunTy is a Pi type
          // In Pi types, the second type depends on the value provided to the function
          ty: Box::new(apply_closure(*out_ty, arg)),
          //construct a new neutral app
          val: Box::new(Neutral::App {
            fun: neutral_val,
            arg: NormalForm {
              term: arg,
              ty: *in_ty,
            },
          }),
        },
        _ => Val::Error(EvalError::AppliedNonFunctionNeutral),
      }
    }
    _ => Val::Error(EvalError::AppliedNonFunction),
  }
}

pub fn apply_closure<'hir, 'ast, TA: Allocator + Clone>(
  clos: Closure<'hir, 'ast, TA>,
  arg: Val<'hir, 'ast, TA>,
) -> Val<'hir, 'ast, TA> {
  // expand arg into the bound vars
  let bound_vars = bind_irrefutable_pattern(&clos.pat, arg, &mut clos.ext_env);

  // gather all variables
  clos.ext_env.extend(bound_vars);

  // finally, eval the closure
  eval(clos.expr, &mut clos.ext_env, &mut vec![])
}

// INVARIANT: the length of the vector will not change
// INVARIANT: if pure is enabled, then we won't alter any variables currently in var_env
pub fn eval<'hir, 'ast, TA: Allocator + Clone>(
  e: &'hir hir::Expr<'hir, 'ast, TA>,
  var_env: &mut Vec<Var<'hir, 'ast, TA>>,
) -> Val<'hir, 'ast, TA> {
  match e.kind {
    hir::ExprKind::Error => Val::Error(EvalError::InvalidSyntax),
    hir::ExprKind::Loop(body) => loop {
      match eval(body, var_env) {
        v @ Val::Never { .. } => {
          break v;
        }
        Val::Nil => (),
        v => unimplemented!(),
      }
      Val::Nil
    },
    hir::ExprKind::Apply { fun, arg } => apply(fun, arg),
    hir::ExprKind::Label(expr) => match eval(expr, var_env) {
      Val::Never {
        levelsUp: 0,
        returned,
      } => returned,
      Val::Never { levelsUp, returned } => Val::Never { levelsUp, returned },
      v => unimplemented!(),
    },
    hir::ExprKind::Ret { labels_up, value } => Val::Never {
      returned: eval(value, var_env),
      levelsUp: labels_up,
    },
    hir::ExprKind::StructLiteral(fields) => Val::Struct(
      fields
        .into_iter()
        .map(|(f, v)| (f, Var::Unborrowed(v)))
        .collect(),
    ),
    hir::ExprKind::StructAccess { root, field } => match root {
      Val::Struct(fields) => {
        if let Some(var) = fields.get_mut(field) {
          match std::mem::replace(var, Var::MovedOut) {
            Var::Unborrowed(val) => val,
            Var::ImmutablyBorrowed(_) => unimplemented!(),
            Var::MutablyBorrowed(_) => unimplemented!(),
            Var::MovedOut => unimplemented!(),
          }
        } else {
          // throw error that no such field exists
          unimplemented!()
        }
      }
      // throw error that it's not a struct
      _ => unimplemented!(),
    },
    hir::ExprKind::Var(debruijin_index) => {
      // borrow value
      match std::mem::replace(&var_env[var_env.len() - debruijin_index], Var::MovedOut) {
        Var::Unborrowed(val) => val,
        Var::ImmutablyBorrowed(_) => unimplemented!(),
        Var::MutablyBorrowed(_) => unimplemented!(),
        Var::MovedOut => unimplemented!(),
      }
    },
    hir::ExprKind::Annotate {
    }
  }
}
