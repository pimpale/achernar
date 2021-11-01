use super::ast;
use super::hir;
use std::alloc::Allocator;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
pub enum EvalError {
  Placeholder,
  InvalidSyntax,
  AppliedNonFunction,
  AppliedNonFunctionNeutral,
}

// represents the different states a variable can be in
// source is where the variable was first defined
#[derive(Debug, Clone)]
pub struct Var<'hir, 'ast, TA: Allocator + Clone> {
  kind: VarKind<'hir, 'ast, TA>,
  source: &'ast ast::Expr,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Closure<'hir, 'ast, TA: Allocator + Clone> {
  pub ext_env: Vec<Var<'hir, 'ast, TA>>, // external environment
  pub pat: &'hir hir::IrrefutablePatExpr<'hir, 'ast, TA>,
  pub expr: &'hir hir::ValExpr<'hir, 'ast, TA>,
}

// These are terms that have normalized completely, to the fullest extent possible
#[derive(Debug, Clone)]
pub enum Val<'hir, 'ast, TA: Allocator + Clone> {
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
  // Also known as a Sigma type
  // https://en.wikipedia.org/wiki/Dependent_type#%CE%A3_type
  SigmaTy {
    fst_ty: Box<Val<'hir, 'ast, TA>>,
    snd_dep_ty: Box<Closure<'hir, 'ast, TA>>,
  },
  StructTy(Vec<(&'ast Vec<u8>, Val<'hir, 'ast, TA>), TA>),
  EnumTy(Vec<(&'ast Vec<u8>, Val<'hir, 'ast, TA>), TA>),
  // Also known as a Pi type
  // https://en.wikipedia.org/wiki/Dependent_type#%CE%A0_type
  PiTy {
    arg_ty: Box<Val<'hir, 'ast, TA>>,
    body_dep_ty: Box<Closure<'hir, 'ast, TA>>,
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
  Pair {
    fst: Box<Val<'hir, 'ast, TA>>,
    snd: Box<Val<'hir, 'ast, TA>>,
  },
  Struct(HashMap<&'ast Vec<u8>, Var<'hir, 'ast, TA>>),
  Enum {
    field: &'ast Vec<u8>,
    value: Box<Val<'hir, 'ast, TA>>,
  },
  Fun(Closure<'hir, 'ast, TA>),

  // path of a level
  Ref(Place<'ast>),
  MutRef(Place<'ast>),

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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct NormalForm<'hir, 'ast, TA: Allocator + Clone> {
  term: Val<'hir, 'ast, TA>,
  ty: Val<'hir, 'ast, TA>,
}

#[derive(Debug, Clone)]
pub struct Place<'ast> {
  debruijn_level: usize,
  path_elems: Vec<&'ast Vec<u8>>,
}

impl<TA: Allocator + Clone> fmt::Display for Val<'_, '_, TA> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let val = match self {
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
      Val::SigmaTy { fst_ty, snd_dep_ty } => format!("(Sigma {}, {})", fst_ty, snd_dep_ty),
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
      Val::PiTy {
        arg_ty,
        body_dep_ty,
      } => format!("Pi {}, {}", arg_ty, body_dep_ty),
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
      Val::Pair { fst, snd } => format!("{}, {}", fst, snd),
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
      Val::Ref(place) => format!("&{}", place),
      Val::MutRef(place) => format!("&mut{}", place),
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

impl fmt::Display for Place<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut fieldstr = String::new();
    for x in self.path_elems.iter() {
      fieldstr.push(':');
      fieldstr.push(':');
      fieldstr += std::str::from_utf8(x).unwrap();
    }
    write!(f, "<place {}{}>", self.debruijn_level, fieldstr)
  }
}

pub fn is_copy<'hir, 'ast, 'env, TA: Allocator + Clone>(val: &Val<'hir, 'ast, TA>) -> bool {
  match val {
    Val::Universe(_) => true,
    Val::NilTy => true,
    Val::NeverTy => true,
    Val::BoolTy => true,
    Val::U8Ty => true,
    Val::U16Ty => true,
    Val::U32Ty => true,
    Val::U64Ty => true,
    Val::I8Ty => true,
    Val::I16Ty => true,
    Val::I32Ty => true,
    Val::I64Ty => true,
    Val::F32Ty => true,
    Val::F64Ty => true,
    Val::SigmaTy { .. } => true,
    Val::StructTy(_) => true,
    Val::EnumTy(_) => true,
    Val::PiTy { .. } => true,
    Val::Nil => true,
    Val::Bool(_) => true,
    Val::U8(_) => true,
    Val::U16(_) => true,
    Val::U32(_) => true,
    Val::U64(_) => true,
    Val::I8(_) => true,
    Val::I16(_) => true,
    Val::I32(_) => true,
    Val::I64(_) => true,
    Val::F32(_) => true,
    Val::F64(_) => true,
    Val::Pair { .. } => false,
    Val::Struct(_) => false,
    Val::Enum { .. } => false,
    Val::Fun(_) => false,
    Val::Ref(_) => false,
    Val::MutRef(_) => false,
    Val::Never { .. } => false,
    Val::Neutral { .. } => false,
  }
}

// binds irrefutable pattern, assigning vars to var_env
// returns how many assignments were made
// INVARIANT: does not alter var_env at all
pub fn bind_irrefutable_pat<'hir, 'ast, 'env, TA: Allocator + Clone>(
  p: &'hir hir::IrrefutablePatExpr<'hir, 'ast, TA>,
  val: Val<'hir, 'ast, TA>,
  var_env: &'env mut Vec<Var<'hir, 'ast, TA>>,
  mutation_allowed: bool,
) -> Result<Vec<Var<'hir, 'ast, TA>>, EvalError> {
  match &p.kind {
    hir::IrrefutablePatExprKind::Error => unimplemented!(),
    hir::IrrefutablePatExprKind::BindVariable => Ok(vec![Var {
      source: p.source,
      kind: VarKind::Unborrowed { val },
    }]),
    hir::IrrefutablePatExprKind::Nil => Ok(vec![]),
    hir::IrrefutablePatExprKind::BindPlace(ref place_expr) => {
      if !mutation_allowed {
        // return error how mutation isn't allowed
        unimplemented!();
      }
      let place = eval_place(place_expr, var_env, var_env.len())?;
      let var = get_var_if_overwritable(&place, var_env)?;

      // now write to var
      var.kind = VarKind::Unborrowed { val };

      Ok(vec![])
    }
    hir::IrrefutablePatExprKind::Pair {
      fst: fst_p,
      snd: snd_p,
    } => {
      let mut vars = vec![];
      if let Val::Pair {
        fst: fst_val,
        snd: snd_val,
      } = val
      {
        // match first, then second side
        vars.extend(bind_irrefutable_pat(
          fst_p,
          *fst_val,
          var_env,
          mutation_allowed,
        )?);
        vars.extend(bind_irrefutable_pat(
          snd_p,
          *snd_val,
          var_env,
          mutation_allowed,
        )?);
      } else {
        unimplemented!();
      }

      Ok(vars)
    }
    hir::IrrefutablePatExprKind::ActivePattern {
      fun: fun_expr,
      arg: pat,
    } => {
      // get function of active pattern
      let fun = eval(fun_expr, var_env, var_env.len())?;

      // apply the transform to the provided val
      let transformed_val = apply(fun, val)?;

      //now match on the pattern
      bind_irrefutable_pat(pat, transformed_val, var_env, mutation_allowed)
    }
    hir::IrrefutablePatExprKind::StructLiteral(patterns) => {
      if let Val::Struct(fields_vec) = val {
        let mut vars = vec![];

        // build field hashmap
        let mut fields: HashMap<_, _> = fields_vec.into_iter().collect();

        for (field_name, ref pat) in patterns {
          match fields.remove(field_name) {
            Some(Var {
              kind: VarKind::Unborrowed { val },
              ..
            }) => vars.extend(bind_irrefutable_pat(pat, val, var_env, mutation_allowed)?),
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

        Ok(vars)
      } else {
        // Log error that the the value isn't a struct
        unimplemented!();
      }
    }
  }
}

pub fn apply<'hir, 'ast, TA: Allocator + Clone>(
  fun: Val<'hir, 'ast, TA>,
  arg: Val<'hir, 'ast, TA>,
) -> Result<Val<'hir, 'ast, TA>, EvalError> {
  match fun {
    Val::Fun(closure) => apply_closure(closure, arg),
    // this is the case where the function is an unresolved
    Val::Neutral {
      val: neutral_val,
      ty,
    } => {
      match *ty {
        Val::PiTy {
          arg_ty,
          body_dep_ty,
        } => Ok(Val::Neutral {
          // In Pi types, the second type depends on the arg provided
          // Ex:: {$n:Nat} -> {$m:Nat} -> Vec n -> Vec m -> Vec n + m
          // So, our resultant type is
          ty: Box::new(apply_closure(*body_dep_ty, arg.clone())?),
          //construct a new neutral app
          val: Box::new(Neutral::App {
            fun: neutral_val,
            arg: NormalForm {
              term: arg,
              ty: *arg_ty,
            },
          }),
        }),
        _ => Err(EvalError::AppliedNonFunctionNeutral),
      }
    }
    _ => Err(EvalError::AppliedNonFunction),
  }
}

// INVARIANT: closure is not altered, mutability is required for optimization purposes
pub fn apply_closure<'hir, 'ast, 'clos, TA: Allocator + Clone>(
  mut clos: Closure<'hir, 'ast, TA>,
  arg: Val<'hir, 'ast, TA>,
) -> Result<Val<'hir, 'ast, TA>, EvalError> {
  // expand arg into the bound vars
  let bound_vars = bind_irrefutable_pat(clos.pat, arg, &mut clos.ext_env, false)?;

  // get the min_mut_level (the level at which mutability is allowed)
  let min_mut_level = clos.ext_env.len();

  // append the bound vars
  clos.ext_env.extend(bound_vars);

  // eval the closure
  let result = eval(clos.expr, &mut clos.ext_env, min_mut_level)?;

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

  Ok(result)
}

// returns true if the place represented here can be overwritten without affecting memory safety
// This means that the var must be writable and the target field must be moved out
// NOTE: this function doesn't verify that the path enforces the 1 mutable borrow in path rule
// TODO: incoprorate copy types
pub fn get_var_if_overwritable<'hir, 'ast, 'env, TA: Allocator + Clone>(
  place: &Place<'ast>,
  var_env: &'env mut Vec<Var<'hir, 'ast, TA>>,
) -> Result<&'env mut Var<'hir, 'ast, TA>, EvalError> {
  let mut cursor = &mut var_env[place.debruijn_level];
  for field in place.path_elems.iter() {
    let Var { kind, .. } = cursor;

    // get list of struct fields
    let fields = match kind {
      VarKind::MutablyBorrowed { val, .. } => match val {
        Val::Struct(fields) => fields,
        _ => {
          // this is an internal compiler error
          // object isn't struct
          unimplemented!()
        }
      },
      VarKind::Unborrowed { val } => match val {
        Val::Struct(fields) => fields,
        _ => {
          // this is an internal compiler error
          // object isn't struct
          unimplemented!()
        }
      },
      // can't assign to immutably borrowed
      VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
      VarKind::MovedOut { .. } => unimplemented!(),
    };

    // now get the value of field
    match fields.get_mut(field) {
      Some(var) => {
        // progress further
        cursor = var;
      }
      None => {
        // this is an internal compiler error
        // field doesn't exist
        unimplemented!()
      }
    }
  }

  // now confirm that the cursor has type movedOut
  match &cursor.kind {
    VarKind::MovedOut { .. } => Ok(cursor),
    // throw error about how we can't overwrite an existing value
    VarKind::Unborrowed { .. } => unimplemented!(),
    VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
    VarKind::MutablyBorrowed { .. } => unimplemented!(),
  }
}

// traverses the path, returning an error if the path is invalid
// It requires that all fields that it traverses must be unborrowed.
// This is because this function is primarily used either to move out of a place or to mutably borrow it
// In general, the rule that we are trying to enforce is that in any given place path, there can only be one mutable borrow along it.
pub fn get_var_if_mutably_borrowable<'hir, 'ast, 'env, TA: Allocator + Clone>(
  place: &Place<'ast>,
  var_env: &'env mut Vec<Var<'hir, 'ast, TA>>,
) -> Result<&'env mut Var<'hir, 'ast, TA>, EvalError> {
  // first we need to ensure that all parent structs are unborrowed

  let mut cursor = &mut var_env[place.debruijn_level];
  for field in place.path_elems.iter() {
    let Var { source, kind } = cursor;
    match kind {
      VarKind::Unborrowed { val } => match val {
        Val::Struct(fields) => {
          match fields.get_mut(field) {
            Some(var) => {
              // progress further
              cursor = var;
            }
            None => {
              // field doesn't exist
              unimplemented!()
            }
          }
        }
        _ => {
          // object isn't struct
          unimplemented!()
        }
      },
      // means that the struct is borrowed
      VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
      VarKind::MutablyBorrowed { .. } => unimplemented!(),
      VarKind::MovedOut { .. } => unimplemented!(),
    }
  }

  // now, we'll check that all child structs are unborrowed
  let mut child_fields = vec![&*cursor];
  while let Some(Var { kind, .. }) = child_fields.pop() {
    match kind {
      VarKind::Unborrowed { val } => {
        if let Val::Struct(fields) = val {
          child_fields.extend(fields.values());
        }
      }
      // means that the struct is borrowed
      VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
      VarKind::MutablyBorrowed { .. } => unimplemented!(),
      VarKind::MovedOut { .. } => unimplemented!(),
    }
  }

  Ok(cursor)
}

// traveres the path represented by the place, returning an error if the path is invalid
// It requires that all of the components along the path be either unborrowed or immutably borrowed.
// This function is typically used to create a immutable reference to the place that is specified
// It returns a result, with an erro if it ocln't resolve the speicifed path
pub fn get_var_if_immutably_borrowable<'hir, 'ast, 'env, TA: Allocator + Clone>(
  place: &Place<'ast>,
  var_env: &'env mut Vec<Var<'hir, 'ast, TA>>,
) -> Result<&'env mut Var<'hir, 'ast, TA>, EvalError> {
  let mut cursor = &mut var_env[place.debruijn_level];
  for field in place.path_elems.iter() {
    let Var { kind, .. } = cursor;

    // get list of struct fields
    let fields = match kind {
      VarKind::ImmutablyBorrowed { val, .. } => match val {
        Val::Struct(fields) => fields,
        _ => {
          // this is an internal compiler error
          // object isn't struct
          unimplemented!()
        }
      },
      VarKind::Unborrowed { val } => match val {
        Val::Struct(fields) => fields,
        _ => {
          // this is an internal compiler error
          // object isn't struct
          unimplemented!()
        }
      },
      VarKind::MutablyBorrowed { .. } => unimplemented!(),
      VarKind::MovedOut { .. } => unimplemented!(),
    };

    // now get the value of field
    match fields.get_mut(field) {
      Some(var) => {
        // progress further
        cursor = var;
      }
      None => {
        // this is an internal compiler error
        // field doesn't exist
        unimplemented!()
      }
    }
  }

  // now, we'll check that all child structs are unborrowed or immutably borrowed
  let mut child_fields = vec![&*cursor];
  while let Some(Var { kind, .. }) = child_fields.pop() {
    match kind {
      VarKind::Unborrowed { val } => {
        if let Val::Struct(fields) = val {
          child_fields.extend(fields.values());
        }
      }
      VarKind::ImmutablyBorrowed { val, .. } => {
        if let Val::Struct(fields) = val {
          child_fields.extend(fields.values());
        }
      }
      VarKind::MutablyBorrowed { .. } => unimplemented!(),
      VarKind::MovedOut { .. } => unimplemented!(),
    }
  }

  Ok(cursor)
}

pub fn eval_place<'hir, 'ast, TA: Allocator + Clone>(
  e: &'hir hir::PlaceExpr<'hir, 'ast, TA>,
  var_env: &mut Vec<Var<'hir, 'ast, TA>>,
  min_mut_level: usize,
) -> Result<Place<'ast>, EvalError> {
  match e.kind {
    hir::PlaceExprKind::Error => Err(EvalError::InvalidSyntax),
    hir::PlaceExprKind::Var(debruijn_index) => {
      let debruijn_level = var_env.len() - debruijn_index;

      let place = Place {
        debruijn_level,
        path_elems: vec![],
      };

      Ok(place)
    }
    hir::PlaceExprKind::StructField { root, field, .. } => {
      let mut root_place = eval_place(root, var_env, min_mut_level)?;
      root_place.path_elems.push(field);
      Ok(root_place)
    }
    hir::PlaceExprKind::Deref(val_expr) => {
      match eval(val_expr, var_env, min_mut_level)? {
        Val::Ref(place) => Ok(place),
        Val::MutRef(place) => Ok(place),
        // tried to dereference something that's not a reference
        _ => unimplemented!(),
      }
    }
  }
}

// INVARIANT: the length of the vector will not change
// INVARIANT: won't alter any variables with in var_env where i > var_env
pub fn eval<'hir, 'ast, TA: Allocator + Clone>(
  e: &'hir hir::ValExpr<'hir, 'ast, TA>,
  var_env: &mut Vec<Var<'hir, 'ast, TA>>,
  min_mut_level: usize,
) -> Result<Val<'hir, 'ast, TA>, EvalError> {
  match e.kind {
    hir::ValExprKind::Error => Err(EvalError::InvalidSyntax),
    hir::ValExprKind::Loop(body) => loop {
      match eval(body, var_env, min_mut_level)? {
        v @ Val::Never { .. } => {
          break Ok(v);
        }
        Val::Nil => (),
        // the body of a loop must evaluate to nil
        _ => unimplemented!(),
      }
    },
    hir::ValExprKind::App { fun, arg } => {
      let fun = eval(fun, var_env, min_mut_level)?;
      let arg = eval(arg, var_env, min_mut_level)?;

      apply(fun, arg)
    }
    hir::ValExprKind::Label(expr) => match eval(expr, var_env, min_mut_level)? {
      // if we've reached zero labels
      Val::Never {
        levels_up: 0,
        returned,
      } => Ok(*returned),
      Val::Never {
        levels_up,
        returned,
      } => Ok(Val::Never {
        levels_up: levels_up - 1,
        returned,
      }),
      // the body of a label must evaluate to never
      _ => unimplemented!(),
    },
    hir::ValExprKind::Ret { labels_up, value } => Ok(Val::Never {
      returned: Box::new(eval(value, var_env, min_mut_level)?),
      levels_up: labels_up,
    }),
    hir::ValExprKind::StructLiteral(ref fields) => {
      let mut field_map = HashMap::new();

      for (f, (f_source, ref expr)) in fields.iter() {
        let var = Var {
          source: f_source,
          kind: VarKind::Unborrowed {
            val: eval(&expr, var_env, min_mut_level)?,
          },
        };
        field_map.insert(*f, var);
      }

      Ok(Val::Struct(field_map))
    }
    hir::ValExprKind::Take(place_expr) => {
      let place = eval_place(place_expr, var_env, min_mut_level)?;

      if place.debruijn_level < min_mut_level {
        // TODO: throw error that this variable may not be taken since it
        // is outside the mutable limit
        unimplemented!();
      }

      let Var { kind, .. } = get_var_if_mutably_borrowable(&place, var_env)?;

      // now replace var
      match std::mem::replace(kind, VarKind::MovedOut { take: e.source }) {
        VarKind::Unborrowed { val, .. } => Ok(val),
        VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
        VarKind::MutablyBorrowed { .. } => unimplemented!(),
        VarKind::MovedOut { .. } => unimplemented!(),
      }
    }
    hir::ValExprKind::MutBorrow(place_expr) => {
      let place = eval_place(place_expr, var_env, min_mut_level)?;

      if place.debruijn_level < min_mut_level {
        // TODO: throw error that this variable may not be mutably since it
        // is outside the mutable limit
        unimplemented!();
      }

      let Var { kind, .. } = get_var_if_mutably_borrowable(&place, var_env)?;

      // create new kind, stealing val
      let newkind = match kind {
        VarKind::Unborrowed { val } => VarKind::MutablyBorrowed {
          val: std::mem::replace(val, Val::Nil),
          borrow: e.source,
        },
        VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
        VarKind::MutablyBorrowed { .. } => unimplemented!(),
        VarKind::MovedOut { .. } => unimplemented!(),
      };

      // overwrite with new kind
      *kind = newkind;

      // return the mutable ref
      Ok(Val::MutRef(place))
    }
    hir::ValExprKind::Borrow(place_expr) => {
      let place = eval_place(place_expr, var_env, min_mut_level)?;

      if place.debruijn_level < min_mut_level {
        // TODO: throw error that this variable may not be mutably since it
        // is outside the mutable limit
        unimplemented!();
      }

      let Var { kind, .. } = get_var_if_immutably_borrowable(&place, var_env)?;

      // now replace var
      let newkind = match kind {
        VarKind::Unborrowed { val } => VarKind::ImmutablyBorrowed {
          val: std::mem::replace(val, Val::Nil),
          borrows: vec![e.source],
        },
        VarKind::ImmutablyBorrowed { val, borrows } => {
          let mut newborrows = vec![e.source];
          newborrows.append(borrows);
          VarKind::ImmutablyBorrowed {
            val: std::mem::replace(val, Val::Nil),
            borrows: newborrows,
          }
        }
        //
        VarKind::MutablyBorrowed { .. } => unimplemented!(),
        VarKind::MovedOut { .. } => unimplemented!(),
      };

      // overwrite with new kind
      *kind = newkind;

      // return the immutable ref
      Ok(Val::Ref(place))
    }
    hir::ValExprKind::Annotate { val_expr, ty_expr } => {
      // calculate type
      let _ty = eval(ty_expr, var_env, min_mut_level)?;

      // calculate var
      let val = eval(val_expr, var_env, min_mut_level)?;

      // verify that `val` has type `ty`
      // TODO: need to read back

      // if !check_typed_by(&val, ty, var_env) {
      //   // log error that val doesn't have type ty
      //   unimplemented!();
      // }

      Ok(val)
    }
    hir::ValExprKind::CaseOf {
      ref expr,
      ref case_options,
      ref source,
    } => {
      // compare each function with

      todo!()
    }
    // TODO: don't crash when the user inputs a huge value
    hir::ValExprKind::Universe(n) => Ok(Val::Universe(n.try_into().unwrap())),
    hir::ValExprKind::NilTy => Ok(Val::NilTy),
    hir::ValExprKind::NeverTy => Ok(Val::NeverTy),
    hir::ValExprKind::BoolTy => Ok(Val::BoolTy),
    hir::ValExprKind::U8Ty => Ok(Val::U8Ty),
    hir::ValExprKind::U16Ty => Ok(Val::U16Ty),
    hir::ValExprKind::U32Ty => Ok(Val::U32Ty),
    hir::ValExprKind::U64Ty => Ok(Val::U64Ty),
    hir::ValExprKind::I8Ty => Ok(Val::I8Ty),
    hir::ValExprKind::I16Ty => Ok(Val::I16Ty),
    hir::ValExprKind::I32Ty => Ok(Val::I32Ty),
    hir::ValExprKind::I64Ty => Ok(Val::I64Ty),
    hir::ValExprKind::F32Ty => Ok(Val::F32Ty),
    hir::ValExprKind::F64Ty => Ok(Val::F64Ty),
    hir::ValExprKind::LamTy { arg_ty, body_dep_ty } => todo!(),
    hir::ValExprKind::Nil => Ok(Val::Nil),
    hir::ValExprKind::Bool(b) => Ok(Val::Bool(b)),
    hir::ValExprKind::Char(c) => Ok(Val::U32(c)),
    hir::ValExprKind::Int(i) => todo!(),
    hir::ValExprKind::Float(f) => todo!(),
    hir::ValExprKind::Struct(s) => todo!(),
    hir::ValExprKind::Enum(e) => todo!(),
    hir::ValExprKind::Pair { fst, snd } => todo!(),
    hir::ValExprKind::Lam { arg, body } => todo!(),
    hir::ValExprKind::Sequence { fst, snd } => todo!(),
    hir::ValExprKind::LetIn { pat, val, body } => todo!(),
  }
}

// turns a nbe value into canonical code that produces that value
pub fn read_back<'hir, 'ast, TA: Allocator + Clone>(
  val: Val<'hir, 'ast, TA>,
) -> hir::ValExpr<'hir, 'ast, TA> {
  todo!();
}

pub fn normalize<'hir, 'ast, TA: Allocator + Clone>(
  expr: &'hir hir::ValExpr<'hir, 'ast, TA>,
) -> Result<hir::ValExpr<'hir, 'ast, TA>, EvalError> {
  let val = eval(expr, &mut vec![], 0)?;

  Ok(read_back(val))
}

//
//     hir::ValExprKind::StructFieldTake {
//       root,
//       field: (field, field_source),
//     } => match &mut eval(root, var_env, min_mut_level) {
//       Val::Struct(fields) => {
//         if let Some(Var { kind, source }) = fields.get_mut(field) {
//           match std::mem::replace(kind, VarKind::MovedOut { take: e.source }) {
//             VarKind::Unborrowed { val, .. } => val,
//             VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
//             VarKind::MutablyBorrowed { .. } => unimplemented!(),
//             VarKind::MovedOut { .. } => unimplemented!(),
//           }
//         } else {
//           // throw error that no such field exists
//           unimplemented!()
//         }
//       }
//       _ => unimplemented!(),
//     },
//     hir::ValExprKind::StructFieldMutBorrow {
//       root,
//       field: (field, field_source),
//     } => {
//       let mut root_val = eval(root, var_env, min_mut_level);
//
//       // now we have to extract information that depends on the source
//       let (fields, debruijn_level, path_elems) = match root_val {
//         Val::TakeVar(debruijn_level) => (fields),
//         Val::MutRef {
//           debruijn_level,
//           path_elems,
//         } => {
//         }
//         // throw error that we can't mutably borrow from here
//         _ => unimplemented!(),
//       };
//
//
//      let Var { ref mut kind, .. } = get_mut_ref_var(debruijn_level, &path_elems, var_env);
//      let fields = match kind {
//        // here we deal with the potential status of the root struct
//        VarKind::Unborrowed {
//          val: Val::Struct(ref mut fields),
//          ..
//        } => fields,
//        VarKind::MutablyBorrowed {
//          val: Val::Struct(fields),
//          ..
//        } => fields,
//        // can't mutably borrow if the base is immutably borrowed already
//        // or if it has been moved out
//        VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
//        VarKind::MovedOut { .. } => unimplemented!(),
//        // means that the center wasn't a structo
//        _ => unimplemented!(),
//      }
//
//
//       if let Some(Var { kind, .. }) = fields.get_mut(field) {
//           // now replace var
//           match kind {
//             VarKind::Unborrowed { val } => {
//                 *kind = VarKind::MutablyBorrowed {val: *val, borrow: e.source};
//             },
//             VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
//             VarKind::MutablyBorrowed { .. } => unimplemented!(),
//             VarKind::MovedOut { .. } => unimplemented!(),
//           }
//
//           // return the mutable ref
//           Val::MutRef{ debruijn_level, path_elems: vec![] }
//       } else {
//         // throw error that no such field exists
//         unimplemented!()
//       }
//     }
//     hir::ValExprKind::StructFieldBorrow {
//       root,
//       field: (field, field_source),
//     } => {
//       let mut root_val = eval(root, var_env, min_mut_level);
//       let fields = match root_val {
//         Val::Struct(ref mut fields) => fields,
//         Val::Ref {
//           debruijn_level,
//           path_elems,
//         } => {
//           let Var { ref mut kind, .. } = get_ref_var(debruijn_level, &path_elems, var_env);
//           match kind {
//             VarKind::Unborrowed {
//               val: Val::Struct(ref mut fields),
//               ..
//             } => fields,
//             VarKind::ImmutablyBorrowed {
//               val: Val::Struct(fields),
//               ..
//             } => fields,
//             // can't mutably borrow if the base is immutably borrowed already
//             // or if it has been moved out
//             VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
//             VarKind::MovedOut { .. } => unimplemented!(),
//             // means that the center wasn't a structo
//             _ => unimplemented!(),
//           }
//         }
//         // throw error that we can't mutably borrow from here
//         _ => unimplemented!(),
//       };
//
//       if let Some(Var { kind, .. }) = fields.get_mut(field) {
//         match std::mem::replace(kind, VarKind::MovedOut { take: e.source }) {
//           VarKind::Unborrowed { val, .. } => val,
//           VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
//           VarKind::MutablyBorrowed { .. } => unimplemented!(),
//           VarKind::MovedOut { .. } => unimplemented!(),
//         }
//       } else {
//         // throw error that no such field exists
//         unimplemented!()
//       }
//     },
//

// // simply recursively perform the check
// // INVARIANT: doesn't modify var_env
// pub fn check_typed_by<'hir, 'ast, TA: Allocator + Clone>(
//   val: &Val<'hir, 'ast, TA>,
//   ty: Val<'hir, 'ast, TA>,
//   var_env: &Vec<Var<'hir, 'ast, TA>>,
// ) -> bool {
//   match (val, ty) {
//     (Val::Error(_), _) => false,
//     (Val::Universe(a), Val::Universe(b)) => a + 1 == b,
//     (Val::NilTy, Val::Universe(0)) => true,
//     (Val::NeverTy, Val::Universe(0)) => true,
//     (Val::BoolTy, Val::Universe(0)) => true,
//     (Val::U8Ty, Val::Universe(0)) => true,
//     (Val::U16Ty, Val::Universe(0)) => true,
//     (Val::U32Ty, Val::Universe(0)) => true,
//     (Val::U64Ty, Val::Universe(0)) => true,
//     (Val::I8Ty, Val::Universe(0)) => true,
//     (Val::I16Ty, Val::Universe(0)) => true,
//     (Val::I32Ty, Val::Universe(0)) => true,
//     (Val::I64Ty, Val::Universe(0)) => true,
//     (Val::F32Ty, Val::Universe(0)) => true,
//     (Val::F64Ty, Val::Universe(0)) => true,
//
//     // These two aren't strictly correct...
//     (Val::RefTy(_), Val::Universe(0)) => true,
//     (Val::MutRefTy(_), Val::Universe(0)) => true,
//
//     (Val::ConsTy { .. }, Val::Universe(0)) => true,
//     (Val::StructTy(_), Val::Universe(0)) => true,
//     (Val::EnumTy(_), Val::Universe(0)) => true,
//     (Val::FunTy { .. }, Val::Universe(0)) => true,
//     (Val::Nil, Val::NilTy) => true,
//     (Val::Never { .. }, Val::NeverTy) => true,
//     (Val::Bool(_), Val::BoolTy) => true,
//     (Val::U8(_), Val::U8Ty) => true,
//     (Val::U16(_), Val::U16Ty) => true,
//     (Val::U32(_), Val::U32Ty) => true,
//     (Val::U64(_), Val::U64Ty) => true,
//     (Val::I8(_), Val::I8Ty) => true,
//     (Val::I16(_), Val::I16Ty) => true,
//     (Val::I32(_), Val::I32Ty) => true,
//     (Val::I64(_), Val::I64Ty) => true,
//     (Val::F32(_), Val::F32Ty) => true,
//     (Val::F64(_), Val::F64Ty) => true,
//     (
//       Val::Ref {
//         debruijn_level,
//         fields,
//       },
//       Val::RefTy(ty),
//     ) => check_typed_by(
//       follow_ref_var(*debruijn_level, fields, var_env),
//       *ty,
//       var_env,
//     ),
//     (
//       Val::MutRef {
//         debruijn_level,
//         fields,
//       },
//       Val::MutRefTy(ty),
//     ) => check_typed_by(
//       follow_mut_ref_var(*debruijn_level, fields, var_env),
//       *ty,
//       var_env,
//     ),
//     (
//       Val::Cons {
//         fst: fst_v,
//         snd: snd_v,
//       },
//       Val::ConsTy {
//         fst: fst_t,
//         snd: snd_t,
//       },
//       // TODO: figure out how to properly calculate sigma types
//     ) => {
//       check_typed_by(fst_v, *fst_t, var_env)
//         && check_typed_by(snd_v, apply_closure(*snd_t, Val::Nil), var_env)
//     }
//   }
// }
