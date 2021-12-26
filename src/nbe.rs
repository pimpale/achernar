use super::ast;
use super::hir;
use std::alloc::Allocator;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
pub enum EvalError {
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
  Present {
    val: Val<'hir, 'ast, TA>,
    borrows: HashMap<u64, &'ast ast::Expr>,
  },
}

#[derive(Debug, Clone)]
pub struct BorrowClosure<'hir, 'ast, TA: Allocator + Clone> {
  pub captured_env: Vec<Var<'hir, 'ast, TA>>,
  pub pat: &'hir hir::IrrefutablePatExpr<'hir, 'ast, TA>,
  pub expr: &'hir hir::ValExpr<'hir, 'ast, TA>,
}

#[derive(Debug, Clone)]
pub struct TakeClosure<'hir, 'ast, TA: Allocator + Clone> {
  pub captured_env: Vec<Var<'hir, 'ast, TA>>,
  pub pat: &'hir hir::IrrefutablePatExpr<'hir, 'ast, TA>,
  pub expr: &'hir hir::ValExpr<'hir, 'ast, TA>,
}

// These are terms that have normalized completely, to the fullest extent possible
#[derive(Debug, Clone)]
pub enum Ty<'hir, 'ast, TA: Allocator + Clone> {
  // pes
  Universe(usize), // type of a type is Universe(0)
  Nil,
  Never,
  Bool,
  U8,
  U16,
  U32,
  U64,
  I8,
  I16,
  I32,
  I64,
  F32,
  F64,
  Struct(Vec<(&'ast Vec<u8>, Ty<'hir, 'ast, TA>), TA>),
  Enum(Vec<(&'ast Vec<u8>, Ty<'hir, 'ast, TA>), TA>),
  // Also known as a Sigma type
  // https://en.wikipedia.org/wiki/Dependent_type#%CE%A3_type
  Sigma {
    // type of first
    fst_ty: Box<Ty<'hir, 'ast, TA>>,
    // function mapping value of first to the second type
    snd_dep_ty: BorrowClosure<'hir, 'ast, TA>,
  },
  // Also known as a Pi type
  // https://en.wikipedia.org/wiki/Dependent_type#%CE%A0_type
  Pi {
    // type of the agument
    arg_ty: Box<Ty<'hir, 'ast, TA>>,
    // function mapping argument's value to the result type
    body_dep_ty: BorrowClosure<'hir, 'ast, TA>,
  },
}

// These are terms that have normalized completely, to the fullest extent possible
#[derive(Debug, Clone)]
pub enum Val<'hir, 'ast, TA: Allocator + Clone> {
  // Types are also values
  Ty(Ty<'hir, 'ast, TA>),
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

  // closures
  BorrowFun(BorrowClosure<'hir, 'ast, TA>),
  TakeFun(TakeClosure<'hir, 'ast, TA>),

  // these are values with no definitions that take in a lifetime

  // path of a level
  Ref(Place<'ast>),
  UniqRef(Place<'ast>),

  Never {
    returned: Box<Val<'hir, 'ast, TA>>,
    levels_up: usize,
  },
  // Neutral represents a value that we can't evaluate since we're missing information
  Neutral {
    val: Box<Neutral<'hir, 'ast, TA>>,
    ty: Ty<'hir, 'ast, TA>,
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
  ty: Ty<'hir, 'ast, TA>,
}

#[derive(Debug, Clone)]
pub struct Place<'ast> {
  captured: bool,
  debruijn_level: usize,
  path_elems: Vec<&'ast Vec<u8>>,
}

impl<TA: Allocator + Clone> fmt::Display for Ty<'_, '_, TA> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let val = match self {
      Ty::Universe(level) => format!("(Universe {})", level),
      Ty::Nil => "Nil".to_owned(),
      Ty::Never => "Never".to_owned(),
      Ty::Bool => "Bool".to_owned(),
      Ty::U8 => "U8".to_owned(),
      Ty::U16 => "U16".to_owned(),
      Ty::U32 => "U32".to_owned(),
      Ty::U64 => "U64".to_owned(),
      Ty::I8 => "I8".to_owned(),
      Ty::I16 => "I16".to_owned(),
      Ty::I32 => "I32".to_owned(),
      Ty::I64 => "I64".to_owned(),
      Ty::F32 => "F32".to_owned(),
      Ty::F64 => "F64".to_owned(),
      Ty::Sigma { fst_ty, snd_dep_ty } => format!("(Sigma {}, {})", fst_ty, snd_dep_ty),
      Ty::Struct(h) => format!(
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
      Ty::Enum(h) => format!(
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
      Ty::Pi {
        arg_ty,
        body_dep_ty,
      } => format!("Pi {}, {}", arg_ty, body_dep_ty),
    };

    write!(f, "{}", val)
  }
}

impl<TA: Allocator + Clone> fmt::Display for Val<'_, '_, TA> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let val = match self {
      Val::Ty(ty) => format!("{}", ty),
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
                kind:
                  VarKind::Present {
                    val,
                    borrows,
                    ..
                  },
                ..
              } => format!(
                "{} <borrowed {}>",
                val,
                borrows.len(),
              ),
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
      Val::BorrowFun(c) => c.to_string(),
      Val::TakeFun(c) => c.to_string(),
      Val::Ref(place) => format!("&{}", place),
      Val::UniqRef(place) => format!("&!{}", place),
      Val::Never {
        returned,
        levels_up,
      } => format!("<never ^{} ({})>", levels_up, returned),
      Val::Neutral { .. } => "<neutral>".to_owned(),
    };

    write!(f, "{}", val)
  }
}

// TODO: be able to properly format code

impl<TA: Allocator + Clone> fmt::Display for BorrowClosure<'_, '_, TA> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<borrow closure>")
  }
}

impl<TA: Allocator + Clone> fmt::Display for TakeClosure<'_, '_, TA> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<take closure>")
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

pub fn can_copy<'hir, 'ast, 'env, TA: Allocator + Clone>(val: &Val<'hir, 'ast, TA>) -> bool {
  match val {
    Val::Ty(_) => true,
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
    Val::BorrowFun(_) => false,
    Val::TakeFun(_) => false,
    Val::Ref(_) => false,
    Val::UniqRef(_) => false,
    Val::Never { .. } => false,
    Val::Neutral { .. } => false,
  }
}

// binds irrefutable pattern, assigning vars to var_env
// returns how many assignments were made
// INVARIANT: does not alter var_env at all
pub fn bind_irrefutable_pat<'hir, 'ast, TA: Allocator + Clone>(
  p: &'hir hir::IrrefutablePatExpr<'hir, 'ast, TA>,
  val: Val<'hir, 'ast, TA>,
  var_env: &mut Vec<Var<'hir, 'ast, TA>>,
  captured_env: &mut Vec<Var<'hir, 'ast, TA>>,
) -> Result<Vec<Var<'hir, 'ast, TA>>, EvalError> {
  match &p.kind {
    hir::IrrefutablePatExprKind::Error => unimplemented!(),
    hir::IrrefutablePatExprKind::BindVariable => Ok(vec![Var {
      source: p.source,
      kind: VarKind::Present {
        val,
        borrows: HashMap::new(),
      },
    }]),
    hir::IrrefutablePatExprKind::Nil => Ok(vec![]),
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
          captured_env,
        )?);
        vars.extend(bind_irrefutable_pat(
          snd_p,
          *snd_val,
          var_env,
          captured_env,
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
      let fun = eval(fun_expr, var_env, captured_env)?;

      // apply the transform to the provided val
      let transformed_val = apply(fun, val)?;

      //now match on the pattern
      bind_irrefutable_pat(pat, transformed_val, var_env, captured_env)
    }
    hir::IrrefutablePatExprKind::Struct(patterns) => {
      if let Val::Struct(fields) = val {
        let mut vars = vec![];

        for (field_name, (_, pat)) in patterns {
          match fields.remove(*field_name) {
            // log that we can't unpack field when field is aready borrowed or moved out
            Some(Var {
              kind:
                VarKind::Present {
                  val,
                  borrows,
                  ..
                },
              ..
            }) => {
              if borrows.len() > 0 {
                // log error that there are still some active borrowso
                // this is a problem because the pointer will become dangling
                unimplemented!();
              }

              vars.extend(bind_irrefutable_pat(pat, val, var_env, captured_env)?)
            }
            // log error that the value has already been moved out
            Some(Var {
              kind: VarKind::MovedOut { .. },
              ..
            }) => unimplemented!(),
            // log error that no such field exists on the given struct
            None => unimplemented!(),
          }
        }

        // TODO: check that the rest of the fields are moved out or nonexistent

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
    Val::BorrowFun(closure) => apply_borrow_closure(&mut closure, arg),
    Val::TakeFun(closure) => apply_take_closure(closure, arg),
    // this is the case where the function is an unresolved
    Val::Neutral {
      val: neutral_val,
      ty,
    } => {
      match ty {
        Ty::Pi {
          arg_ty,
          body_dep_ty,
        } => Ok(Val::Neutral {
          // In Pi types, the second type depends on the arg provided
          // Ex:: {$n:Nat} -> {$m:Nat} -> Vec n -> Vec m -> Vec n + m
          // So, our resultant type is
          ty: match apply_borrow_closure(&mut body_dep_ty, arg.clone())? {
            Val::Ty(x) => x,
            // this should have been typechecked out!
            _ => unimplemented!(),
          },
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

// consumes the taking closure and the arg
pub fn apply_take_closure<'hir, 'ast, 'clos, TA: Allocator + Clone>(
  clos: TakeClosure<'hir, 'ast, TA>,
  arg: Val<'hir, 'ast, TA>,
) -> Result<Val<'hir, 'ast, TA>, EvalError> {
  let mut captured_vars = clos.captured_env;

  let initial_captured_len = captured_vars.len();

  // expand arg with the captured env
  let bound_vars = bind_irrefutable_pat(clos.pat, arg, &mut vec![], &mut captured_vars)?;

  let initial_bound_len = bound_vars.len();

  // eval the closure with our new vars
  let result = eval(clos.expr, &mut bound_vars, &mut captured_vars)?;

  // check for an internal compiler error
  assert!(initial_bound_len == bound_vars.len());
  assert!(initial_captured_len == captured_vars.len());

  // since we have linear types, make sure that all types are consumed
  for Var { source, kind } in bound_vars {
    match kind {
      VarKind::MovedOut { .. } => (),
      // throw error here why linear types require to be consumed
      VarKind::Present { .. } => unimplemented!(),
    }
  }

  for Var { source, kind } in captured_vars {
    match kind {
      VarKind::MovedOut { .. } => (),
      // throw error here why linear types require to be consumed
      VarKind::Present { .. } => unimplemented!(),
    }
  }

  Ok(result)
}

// doesn't consume the closure, only the arg
pub fn apply_borrow_closure<'hir, 'ast, 'clos, TA: Allocator + Clone>(
  clos: &mut BorrowClosure<'hir, 'ast, TA>,
  arg: Val<'hir, 'ast, TA>,
) -> Result<Val<'hir, 'ast, TA>, EvalError> {
  // this is low key a hack, we should just give the captured vars directly
  let mut captured_vars = &mut clos.captured_env;

  let initial_captured_len = captured_vars.len();

  // expand arg with the captured env
  let bound_vars = bind_irrefutable_pat(clos.pat, arg, &mut vec![], &mut *captured_vars)?;

  let initial_bound_len = bound_vars.len();

  // eval the closure with our new vars
  let result = eval(clos.expr, &mut bound_vars, &mut *captured_vars)?;

  // check for an internal compiler error
  assert!(initial_bound_len == bound_vars.len());
  assert!(initial_captured_len == captured_vars.len());

  // since we have linear types, make sure that all types are consumed
  for Var { source, kind } in bound_vars {
    match kind {
      VarKind::MovedOut { .. } => (),
      // throw error here why linear types require to be consumed
      VarKind::Present { .. } => unimplemented!(),
    }
  }

  // assert that we didn't consume any of the captured vars
  for Var { source, kind } in captured_vars {
    match kind {
      VarKind::MovedOut { .. } => unimplemented!(),
      // TODO: add lifetimes
      VarKind::Present { .. } => ()
    }
  }

  Ok(result)
}

pub fn eval_place<'hir, 'ast, TA: Allocator + Clone>(
  e: &'hir hir::PlaceExpr<'hir, 'ast, TA>,
  var_env: &mut Vec<Var<'hir, 'ast, TA>>,
  captured_env: &mut Vec<Var<'hir, 'ast, TA>>,
) -> Result<Place<'ast>, EvalError> {
  match e.kind {
    hir::PlaceExprKind::Error => Err(EvalError::InvalidSyntax),
    hir::PlaceExprKind::Var(debruijn_index) => Ok(Place {
      captured: false,
      debruijn_level: var_env.len() - debruijn_index,
      path_elems: vec![],
    }),
    hir::PlaceExprKind::CapturedVar(debruijn_index) => Ok(Place {
      captured: true,
      debruijn_level: captured_env.len() - debruijn_index,
      path_elems: vec![],
    }),
    hir::PlaceExprKind::StructField { root, field, .. } => {
      let mut root_place = eval_place(root, var_env, captured_env)?;
      root_place.path_elems.push(field);
      Ok(root_place)
    }
    hir::PlaceExprKind::Deref(val_expr) => {
      match eval(val_expr, var_env, captured_env)? {
        Val::Ref(place) => Ok(place),
        // tried to dereference something that's not a reference
        _ => unimplemented!(),
      }
    }
  }
}

// traverses the path represented by the place, returning an error if the path is invalid
// It requires that all of the components along the path be either unborrowed or borrowed.
// We then check that all of the children fields are unborrowed
pub fn borrow_var<'hir, 'ast, 'env, TA: Allocator + Clone>(
  e: &'hir hir::PlaceExpr<'hir, 'ast, TA>,
  var_env: &'env mut Vec<Var<'hir, 'ast, TA>>,
  captured_env: &'env mut Vec<Var<'hir, 'ast, TA>>,
) -> Result<Val<'hir, 'ast, TA>, EvalError> {
  let place = eval_place(e, var_env, captured_env)?;

  let mut cursor = if place.captured {
      &mut captured_env[place.debruijn_level]
  } else {
      &mut var_env[place.debruijn_level]
  };

  for field in place.path_elems.iter() {
    let Var { kind, .. } = cursor;
    // get list of struct fields
    let fields = match kind {
      VarKind::Present {
        val,
        borrows,
        ..
      } => {
        match val {
          Val::Struct(fields) => fields,
          _ => {
            // this is an internal compiler error
            // object isn't struct
            unimplemented!()
          }
        }
      }
      // means that we already moved out of here
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

  // now, we'll borrow this and all child fields
  let mut child_fields = vec![&mut *cursor];
  while let Some(Var { kind, .. }) = child_fields.pop() {
    match kind {
      VarKind::Present { val, borrows, } => {
        if let Val::Struct(fields) = val {
          child_fields.extend(fields.values_mut());
        }
        borrows.insert(*current_borrow_idx, source);
      }
      VarKind::MovedOut { .. } => unimplemented!(),
    }
  }

  // return ref
  Ok(Val::Ref(place.clone()))
}

pub fn unborrow_var<'hir, 'ast, 'env, TA: Allocator + Clone>(
  place: &Place<'ast>,
  var_env: &'env mut Vec<Var<'hir, 'ast, TA>>,
  captured_env: &'env mut Vec<Var<'hir, 'ast, TA>>,
) -> Result<(), EvalError> {
  // remove the borrow fields
}

// traverses the path represented by the place, returning an error if the path is invalid
// It requires that all of the components along the path be unborrowed.
// Takes the struct out
pub fn take_var<'hir, 'ast, 'env, TA: Allocator + Clone>(
  place: &Place<'ast>,
  var_env: &'env mut Vec<Var<'hir, 'ast, TA>>,
  captured_env: &'env mut Vec<Var<'hir, 'ast, TA>>,
) -> Result<Val<'hir, 'ast, TA>, EvalError> {
  let mut cursor = if place.captured {
      &mut captured_env[place.debruijn_level]
  } else {
      &mut var_env[place.debruijn_level]
  };

  for field in place.path_elems.iter() {
    let Var { kind, .. } = cursor;
    // get list of struct fields
    let fields = match kind {
      VarKind::Unborrowed { val, .. } => match val {
        Val::Struct(fields) => fields,
        _ => {
          // this is an internal compiler error
          // object isn't struct
          unimplemented!()
        }
      },
      VarKind::Borrowed { .. } => unimplemented!(),
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

  // move out all the child fields as well
  let mut child_fields = vec![&*cursor];
  while let Some(Var { kind, .. }) = child_fields.pop() {
    match kind {
      VarKind::Unborrowed { val } => {
        if let Val::Struct(fields) = val {
          child_fields.extend(fields.values());
        }
      }
      VarKind::UniqBorrowed { .. } => unimplemented!(),
      VarKind::Borrowed { .. } => unimplemented!(),
      VarKind::MovedOut { .. } => unimplemented!(),
    }
  }
  Ok(cursor)
}

// INVARIANT: the length of the vector will not change
pub fn eval<'hir, 'ast, TA: Allocator + Clone>(
  e: &'hir hir::ValExpr<'hir, 'ast, TA>,
  var_env: &mut Vec<Var<'hir, 'ast, TA>>,
  captured_env: &mut Vec<Var<'hir, 'ast, TA>>,
) -> Result<Val<'hir, 'ast, TA>, EvalError> {
  match e.kind {
    hir::ValExprKind::Error => Err(EvalError::InvalidSyntax),
    hir::ValExprKind::Loop(body) => loop {
      match eval(body, var_env, captured_env)? {
        v @ Val::Never { .. } => {
          break Ok(v);
        }
        Val::Nil => (),
        // the body of a loop must evaluate to nil
        _ => unimplemented!(),
      }
    },
    hir::ValExprKind::App { fun, arg } => {
      let fun = eval(fun, var_env, captured_env)?;
      let arg = eval(arg, var_env, captured_env)?;

      apply(fun, arg)
    }
    hir::ValExprKind::Struct(ref fields) => {
      let mut field_map = HashMap::new();

      for (f, (f_source, ref expr)) in fields.iter() {
        let var = Var {
          source: f_source,
          kind: VarKind::Unborrowed {
            val: eval(&expr, var_env, captured_env)?,
          },
        };
        field_map.insert(*f, var);
      }

      Ok(Val::Struct(field_map))
    }
    hir::ValExprKind::Use(place_expr, use_kind) => {
      let place = eval_place(place_expr, var_env, captured_env)?;

      let val = match use_kind {
        UseKind::Take => take_var(&place, var_env, captured_env)?,
        UseKind::UniqBorrow => uniqborrow_var(&place, var_env, captured_env)?,
        UseKind::Borrow => borrow_var(&place, var_env, captured_env)?,
      };

      Ok(val)
    }
    hir::ValExprKind::Annotate { val_expr, ty_expr } => {
      // calculate type
      let _ty = eval(ty_expr, var_env, captured_env)?;

      // calculate var
      let val = eval(val_expr, var_env, captured_env)?;

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
    hir::ValExprKind::Universe(n) => Ok(Val::Ty(Ty::Universe(n.try_into().unwrap()))),
    hir::ValExprKind::LamTy {
      arg_ty,
      body_dep_ty,
      use_kind,
    } => {
      let arg_ty = eval(arg_ty, var_env, captured_env)?;
      // we will have to construct an immutable
      let body_dep_ty = eval(body_dep_ty, var_env, captured_env)?;

      todo!();
    }
    hir::ValExprKind::Nil => Ok(Val::Nil),
    hir::ValExprKind::Bool(b) => Ok(Val::Bool(b)),
    hir::ValExprKind::Char(c) => Ok(Val::U32(c)),
    hir::ValExprKind::Int(i) => todo!(),
    hir::ValExprKind::Float(f) => todo!(),
    hir::ValExprKind::StructTy(s) => todo!(),
    hir::ValExprKind::EnumTy(e) => todo!(),
    hir::ValExprKind::Pair { fst, snd } => todo!(),
    hir::ValExprKind::Lam {
      arg,
      body,
      captured_vars,
      use_kind,
    } => todo!(),
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
  let val = eval(expr, &mut vec![], &mut vec![])?;

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
//             VarKind::Borrowed { .. } => unimplemented!(),
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
//        VarKind::Borrowed { .. } => unimplemented!(),
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
//             VarKind::Borrowed { .. } => unimplemented!(),
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
//             VarKind::Borrowed {
//               val: Val::Struct(fields),
//               ..
//             } => fields,
//             // can't mutably borrow if the base is immutably borrowed already
//             // or if it has been moved out
//             VarKind::Borrowed { .. } => unimplemented!(),
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
//           VarKind::Borrowed { .. } => unimplemented!(),
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
