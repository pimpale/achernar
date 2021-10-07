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
  pub expr: &'hir hir::ValExpr<'hir, 'ast, TA>,
}

// These are terms that have normalized completely, to the fullest extent possible
#[derive(Debug)]
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

#[derive(Debug)]
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
    for x in self.path_elems {
      fieldstr.push(':');
      fieldstr.push(':');
      fieldstr += std::str::from_utf8(x).unwrap();
    }
    write!(f, "<place {}{}>", self.debruijn_level, fieldstr)
  }
}

// binds irrefutable pattern, assigning vars to var_env
// returns how many assignments were made
// INVARIANT: does not alter var_env at all
pub fn bind_irrefutable_pattern<'hir, 'ast, 'env, TA: Allocator + Clone>(
  p: &'hir hir::Pat<'hir, 'ast, TA>,
  val: Val<'hir, 'ast, TA>,
  var_env: &'env mut Vec<Var<'hir, 'ast, TA>>,
) -> Result<Vec<Var<'hir, 'ast, TA>>, EvalError> {
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
        vars.extend(bind_irrefutable_pattern(fst_p, *fst_val, var_env)?);
        vars.extend(bind_irrefutable_pattern(fst_p, *snd_val, var_env)?);
      } else {
        unimplemented!();
      }

      Ok(vars)
    }
    hir::PatKind::ActivePattern {
      fun: fun_expr,
      arg: pat,
    } => {
      // get function of active pattern
      let fun = eval(fun_expr, var_env, var_env.len())?;

      // apply the transform to the provided val
      let transformed_val = apply(fun, val)?;

      //now match on the pattern
      bind_irrefutable_pattern(pat, transformed_val, var_env)
    }
    hir::PatKind::StructLiteral(patterns) => {
      if let Val::Struct(fields_vec) = val {
        let mut vars = vec![];

        // build field hashmap
        let mut fields: HashMap<_, _> = fields_vec.into_iter().collect();

        for (field_name, ref pat) in patterns {
          match fields.remove(field_name) {
            Some(Var {
              kind: VarKind::Unborrowed { val },
              ..
            }) => vars.extend(bind_irrefutable_pattern(pat, val, var_env)?),
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
    // explain how these aren't able to be bound
    _ => unimplemented!(),
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
        Val::FunTy { in_ty, out_ty } => Ok(Val::Neutral {
          // Remember, FunTy is a Pi type
          // In Pi types, the second type depends on the value provided to the function
          // the function outputting the second type has (read) access to the variables bound from the first pattern
          // TODO: let the closure body make use of the first pattern
          // Ex:: Vec $n -> Vec $m -> Vec n + m
          ty: Box::new(apply_closure(*out_ty, arg)?),
          //construct a new neutral app
          val: Box::new(Neutral::App {
            fun: neutral_val,
            arg: NormalForm {
              term: arg,
              ty: *in_ty,
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
  clos: Closure<'hir, 'ast, TA>,
  arg: Val<'hir, 'ast, TA>,
) -> Result<Val<'hir, 'ast, TA>, EvalError> {
  // expand arg into the bound vars
  let bound_vars = bind_irrefutable_pattern(&clos.pat, arg, &mut clos.ext_env)?;

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

// traverses the path, returning an error if the path is invalid
// It requires that all fields that it traverses must be unborrowed.
// This is because this function is primarily used either to move out of a place or to mutably borrow it
pub fn get_var_if_mutably_borrowable<'hir, 'ast, 'env, TA: Allocator + Clone>(
  place: &Place<'ast>,
  var_env: &'env mut Vec<Var<'hir, 'ast, TA>>,
) -> Result<&'env mut Var<'hir, 'ast, TA>, EvalError> {

  // first we need to ensure that all parent structs are unborrowed

  let mut cursor = &mut var_env[place.debruijn_level];
  for field in place.path_elems {
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
  while let Some(Var {kind, ..}) = child_fields.pop() {
      match kind {
          VarKind::Unborrowed{ val} => {
           if let Val::Struct(fields) = val {
              child_fields.extend(fields.values());
           }
          },
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
  for field in place.path_elems {
    let Var { source, kind } = cursor;

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
      let mut place = eval_place(e, var_env, min_mut_level)?;
      place.path_elems.push(field);
      Ok(place)
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
        v => unimplemented!(),
      }
    },
    hir::ValExprKind::Apply { fun, arg } => {
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
      v => unimplemented!(),
    },
    hir::ValExprKind::Ret { labels_up, value } => Ok(Val::Never {
      returned: Box::new(eval(value, var_env, min_mut_level)?),
      levels_up: labels_up,
    }),
    hir::ValExprKind::StructLiteral(fields) => {
      let x = HashMap::new();

      for (f, (f_source, ref expr)) in fields.iter() {
        let var = Var {
          source: f_source,
          kind: VarKind::Unborrowed {
            val: eval(&expr, var_env, min_mut_level)?,
          },
        };
        x.insert(*f, var);
      }

      Ok(Val::Struct(x))
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

      // now replace var
      match kind {
        VarKind::Unborrowed { val } => {
          *kind = VarKind::MutablyBorrowed {
            val: *val,
            borrow: e.source,
          };
        }
        VarKind::ImmutablyBorrowed { .. } => unimplemented!(),
        VarKind::MutablyBorrowed { .. } => unimplemented!(),
        VarKind::MovedOut { .. } => unimplemented!(),
      }

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
      match kind {
        VarKind::Unborrowed { val } => {
          *kind = VarKind::ImmutablyBorrowed {
            val: *val,
            borrows: vec![e.source],
          };
        }
        VarKind::ImmutablyBorrowed { val, borrows } => {
            borrows.push(e.source);
            *kind = VarKind::ImmutablyBorrowed {
                val: *val,
                borrows: *borrows
            };
        },
        //
        VarKind::MutablyBorrowed { .. } => unimplemented!(),
        VarKind::MovedOut { .. } => unimplemented!(),
      }

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
    },
    hir::ValExprKind::CaseOf {
      expr,
      case_options,
      source,
    } => Ok(Val::Nil),
    hir::ValExprKind::Universe(n) => Ok(Val::Universe(n)),
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
    hir::ValExprKind::Nil => Ok(Val::Nil),
    hir::ValExprKind::Bool(b) => Ok(Val::Bool(b)),
    hir::ValExprKind::Char(c) => Ok(Val::U32(c)),
    hir::ValExprKind::Int(i) => todo!(),
    hir::ValExprKind::Float(f) => todo!(),
    hir::ValExprKind::Struct(s) => todo!(),
    hir::ValExprKind::Enum(e) => todo!(),
    hir::ValExprKind::Cons { fst, snd } => todo!(),
    hir::ValExprKind::Defun {
      pattern,
      result,
      infer_pattern,
    } => todo!(),
    hir::ValExprKind::Sequence { fst, snd } => todo!(),
    hir::ValExprKind::LetIn { pat, val, body } => todo!(),
  }
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
