use super::ast;
use super::hir;
use super::utils::update;
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
// source is where the variable was first defined
#[derive(Debug)]
pub struct Var<'hir, 'ast, TA: Allocator + Clone> {
  kind: VarKind<'hir, 'ast, TA>,
  source: &'ast ast::Expr,
}

#[derive(Debug)]
pub enum VarKind<'hir, 'ast, TA: Allocator + Clone> {
  MovedOut {
    take: &'ast ast::Expr,
  },
  MutablyBorrowed {
    val: Val<'hir, 'ast, TA>,
    borrow: &'ast ast::Expr,
  },
  ImmutablyBorrowed {
    val: Val<'hir, 'ast, TA>,
    borrows: Vec<&'ast ast::Expr>,
  },
  Unborrowed {
    val: Val<'hir, 'ast, TA>,
  },
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
  Universe(usize), // type of a type is Universe(0)
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

  // Type of reference to a variable
  RefTy(Box<Val<'hir, 'ast, TA>>),
  MutRefTy(Box<Val<'hir, 'ast, TA>>),

  // This is also known as a sigma type
  ConsTy {
    fst: Box<Val<'hir, 'ast, TA>>,
    snd: Box<Closure<'hir, 'ast, TA>>,
  },
  StructTy(Vec<(&'ast Vec<u8>, Val<'hir, 'ast, TA>), TA>),
  EnumTy(Vec<(&'ast Vec<u8>, Val<'hir, 'ast, TA>), TA>),

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
  Enum {
    field: &'ast Vec<u8>,
    value: Box<Val<'hir, 'ast, TA>>,
  },
  Fun(Closure<'hir, 'ast, TA>),

  // References refer to a variable's debruijn level
  Ref(usize),
  MutRef(usize),

  Never {
    returned: Box<Val<'hir, 'ast, TA>>,
    levels_up: usize,
  },

  // Neutral represents a value that we can't evaluate since we're missing information
  Neutral {
    val: Box<Neutral<'hir, 'ast, TA>>,
    ty: Box<Val<'hir, 'ast, TA>>,
  },
}

#[derive(Debug)]
pub enum Neutral<'hir, 'ast, TA: Allocator + Clone> {
  // De Brujn level (not index)
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
        "struct({{ {} }})",
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
        "enum({{ {} }})",
        h.iter().fold(String::new(), |a, (key, val)| {
          format!(
            "{}, {}: {}",
            a,
            std::str::from_utf8(key).unwrap(),
            val.to_string()
          )
        })
      ),
      Val::FunTy { in_ty, out_ty } => format!("{} -> {}", in_ty, out_ty),
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
      Val::Struct(h) => format!(
        "{{ {} }}",
        h.iter().fold(String::new(), |a, (key, var)| {
          format!(
            "{}, {}: {}",
            a,
            std::str::from_utf8(key).unwrap(),
            match var {
              Var {
                kind: VarKind::ImmutablyBorrowed { val, borrows, .. },
                ..
              } => format!("{} <borrowed {}>", val, borrows.len()),
              Var {
                kind: VarKind::MutablyBorrowed { val, .. },
                ..
              } => format!("{} <borrowed mutably>", val),
              Var {
                kind: VarKind::Unborrowed { val, .. },
                ..
              } => format!("{}", val),
              Var {
                kind: VarKind::MovedOut { .. },
                ..
              } => format!("<moved out>"),
            }
          )
        })
      ),
      Val::Enum { field, value } => {
        format!("enum({}: {})", std::str::from_utf8(field).unwrap(), value)
      }
      Val::Fun(c) => c.to_string(),
      Val::Never {
        returned,
        levels_up,
      } => format!("<never ^{} ({})>", levels_up, returned),
      Val::Neutral { .. } => "<neutral>".to_owned(),
    };

    write!(f, "{}", val)
  }
}
impl<TA: Allocator + Clone> fmt::Display for Closure<'_, '_, TA> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<closure>")
  }
}

// computes the alpha equivalence of two normal form exprs
// since we use de bruijn indexes, this is easy
pub fn alpha_equivalent<'hir, 'ast, TA: Allocator + Clone>(
  a: &hir::Expr<'hir, 'ast, TA>,
  b: &hir::Expr<'hir, 'ast, TA>,
) -> bool {
  // TODO: validate recursively on
  true
}

// simply recursively perform the check
pub fn check_typed_by<'hir, 'ast, TA: Allocator + Clone>(
  val: &Val<'hir, 'ast, TA>,
  ty: Val<'hir, 'ast, TA>,
) -> bool {
  match (val, ty) {
    (Val::NilTy, Val::Universe(0)) => true,
    (Val::NeverTy, Val::Universe(0)) => true,
    (Val::BoolTy, Val::Universe(0)) => true,
    (Val::U8Ty, Val::Universe(0)) => true,
    (Val::U16Ty, Val::Universe(0)) => true,
    (Val::U32Ty, Val::Universe(0)) => true,
    (Val::U64Ty, Val::Universe(0)) => true,
    (Val::I8Ty, Val::Universe(0)) => true,
    (Val::I16Ty, Val::Universe(0)) => true,
    (Val::I32Ty, Val::Universe(0)) => true,
    (Val::I64Ty, Val::Universe(0)) => true,
    (Val::F32Ty, Val::Universe(0)) => true,
    (Val::F64Ty, Val::Universe(0)) => true,
    (Val::ConsTy { .. }, Val::Universe(0)) => true,
    (Val::StructTy(_), Val::Universe(0)) => true,
    (Val::EnumTy(_), Val::Universe(0)) => true,
    (Val::FunTy { .. }, Val::Universe(0)) => true,
    (Val::Nil, Val::NilTy) => true,
    (Val::Never { .. }, Val::NeverTy) => true,
    (Val::Bool(_), Val::BoolTy) => true,
    (Val::U8(_), Val::U8Ty) => true,
    (Val::U16(_), Val::U16Ty) => true,
    (Val::U32(_), Val::U32Ty) => true,
    (Val::U64(_), Val::U64Ty) => true,
    (Val::I8(_), Val::I8Ty) => true,
    (Val::I16(_), Val::I16Ty) => true,
    (Val::I32(_), Val::I32Ty) => true,
    (Val::I64(_), Val::I64Ty) => true,
    (Val::F32(_), Val::F32Ty) => true,
    (Val::F64(_), Val::F64Ty) => true,
    (
      Val::Cons {
        fst: fst_v,
        snd: snd_v,
      },
      Val::ConsTy {
        fst: fst_t,
        snd: snd_t,
      },
      // TODO: figure out how to properly calculate sigma types
    ) => {
      check_typed_by(fst_v, *fst_t) && check_typed_by(snd_v, apply_closure(&mut snd_t, Val::Nil))
    }
    (Val::F64(_), Val::F64Ty) => true,
  }
}

// binds irrefutable pattern, assigning vars to var_env
// returns how many assignments were made
// INVARIANT: does not alter var_env at all
pub fn bind_irrefutable_pattern<'hir, 'ast, 'env, TA: Allocator + Clone>(
  p: &'hir hir::Pat<'hir, 'ast, TA>,
  val: Val<'hir, 'ast, TA>,
  var_env: &'env mut Vec<Var<'hir, 'ast, TA>>,
) -> Vec<Var<'hir, 'ast, TA>> {
  match p.kind {
    hir::PatKind::Error => unimplemented!(),
    hir::PatKind::BindVariable => vec![Var {
      source: p.source,
      kind: VarKind::Unborrowed { val },
    }],
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
      let fun = eval(fun_expr, var_env, var_env.len());

      // apply the transform to the provided val
      let tranformed_val = apply(fun, val);

      //now match on the pattern
      bind_irrefutable_pattern(pat, tranformed_val, var_env)
    }
    hir::PatKind::StructLiteral(patterns) => {
      if let Val::Struct(fields_vec) = val {
        let mut vars = vec![];

        // build field hashmap
        let mut fields: HashMap<_, _> = fields_vec.into_iter().collect();

        for (field_name, pat) in &patterns {
          match fields.remove(field_name) {
            Some(Var {
              kind: VarKind::Unborrowed { val },
              ..
            }) => vars.extend(bind_irrefutable_pattern(pat, val, var_env)),
            // log that we can't unpack field when field is aready borrowed or moved out
            Some(Var {
              kind: VarKind::ImmutablyBorrowed { .. },
              ..
            }) => unimplemented!(),
            Some(Var {
              kind: VarKind::MutablyBorrowed { .. },
              ..
            }) => unimplemented!(),
            Some(Var {
              kind: VarKind::MovedOut { .. },
              ..
            }) => unimplemented!(),
            // log error that no such field exists on the given struct
            None => unimplemented!(),
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
    Val::Fun(closure) => apply_closure(&mut closure, arg),
    // this is the case where the function is an unresolved
    Val::Neutral {
      val: neutral_val,
      ty,
    } => {
      match *ty {
        Val::FunTy { in_ty, out_ty } => Val::Neutral {
          // Remember, FunTy is a Pi type
          // In Pi types, the second type depends on the value provided to the function
          // the function outputting the second type has (read) access to the variables bound from the first pattern
          // TODO: let the closure body make use of the first pattern
          // Ex:: Vec $n -> Vec $m -> Vec n + m
          ty: Box::new(apply_closure(&mut out_ty, arg)),
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

// INVARIANT: closure is not altered, mutability is required for optimization purposes
pub fn apply_closure<'hir, 'ast, TA: Allocator + Clone>(
  clos: &mut Closure<'hir, 'ast, TA>,
  arg: Val<'hir, 'ast, TA>,
) -> Val<'hir, 'ast, TA> {
  // expand arg into the bound vars
  let bound_vars = bind_irrefutable_pattern(&clos.pat, arg, &mut clos.ext_env);

  // get the min_mut_level (the level at which mutability is allowed)
  let min_mut_level = clos.ext_env.len();

  // append the bound vars
  clos.ext_env.extend(bound_vars);

  // eval the closure
  let result = eval(clos.expr, &mut clos.ext_env, min_mut_level);

  // unbind the bound_vars from the closure
  // since we have linear types, make sure that all types are consumed
  for Var { source, kind } in clos.ext_env.split_off(min_mut_level) {
    match kind {
      VarKind::MovedOut { .. } => (),
      // throw error here why linear types require to be consumed
      VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
      VarKind::MutablyBorrowed { .. } => unimplemented!(),
      VarKind::Unborrowed { .. } => unimplemented!(),
    }
  }

  result
}

// INVARIANT: the length of the vector will not change
// INVARIANT: won't alter any variables with in var_env where i > var_env
pub fn eval<'hir, 'ast, TA: Allocator + Clone>(
  e: &'hir hir::Expr<'hir, 'ast, TA>,
  var_env: &mut Vec<Var<'hir, 'ast, TA>>,
  min_mut_level: usize,
) -> Val<'hir, 'ast, TA> {
  match e.kind {
    hir::ExprKind::Error => Val::Error(EvalError::InvalidSyntax),
    hir::ExprKind::Loop(body) => loop {
      match eval(body, var_env, min_mut_level) {
        v @ Val::Never { .. } => {
          break v;
        }
        Val::Nil => (),
        v => unimplemented!(),
      }
      Val::Nil
    },
    hir::ExprKind::Apply { fun, arg } => apply(fun, arg),
    hir::ExprKind::Label(expr) => match eval(expr, var_env, min_mut_level) {
      Val::Never {
        levels_up: 0,
        returned,
      } => returned,
      Val::Never {
        levels_up,
        returned,
      } => Val::Never {
        levels_up,
        returned,
      },
      v => unimplemented!(),
    },
    hir::ExprKind::Ret { labels_up, value } => Val::Never {
      returned: Box::new(eval(value, var_env, min_mut_level)),
      levels_up: labels_up,
    },
    hir::ExprKind::StructLiteral(fields) => Val::Struct(
      fields
        .into_iter()
        .map(|((f, source), expr)| {
          (
            f,
            Var::Unborrowed {
              val: eval(&expr, var_env, min_mut_level),
              source,
            },
          )
        })
        .collect(),
    ),
    hir::ExprKind::StructFieldTake { root: hir::Expr { root_kind, .. }, field } => match root_kind {
      hir::ExprKind::Struct(fields) => {
        if let Some(Var { kind, source }) = fields.get_mut(field) {
          match std::mem::replace(var, Var::MovedOut { take: e.source }) {
            VarKind::Unborrowed { val, .. } => val,
            VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
            VarKind::MutablyBorrowed { .. } => unimplemented!(),
            VarKind::MovedOut { .. } => unimplemented!(),
          }
        } else {
          // throw error that no such field exists
          unimplemented!()
        }
      }
      // throw error that it's not a struct
      _ => unimplemented!(),
    },
    hir::ExprKind::TakeVar(debruijn_index) => {
      let debruijn_level = var_env.len() - debruijn_index;

      if debruijn_level < min_mut_level {
        // TODO: throw error that this variable may not be taken since it
        // is outside the mutable limit
        unimplemented!();
      }

      // now take var out
      match update(
        &mut var_env[debruijn_level],
        |Var { ref source, .. }| Var {
          source,
          kind: VarKind::MovedOut { take: e.source },
        },
      )
      .kind
      {
        VarKind::Unborrowed { val, .. } => val,
        VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
        VarKind::MutablyBorrowed { .. } => unimplemented!(),
        VarKind::MovedOut { .. } => unimplemented!(),
      }
    }
    // TODO: how to deal with the typing being different for synth and check
    hir::ExprKind::Annotate { expr, ty } => {
      // calculate type
      let ty = eval(ty, var_env);

      // calculate var
      let val = eval(expr, var_env);

      // verify that `val` has type `ty`
      if !check_typed_by(&val, ty) {
        // log error that val doesn't have type ty
        unimplemented!();
      }

      val
    }
    hir::ExprKind::CaseOf {
      expr,
      case_options,
      source,
    } => {}
  }
}
