use super::ast;
use hashbrown::hash_map::DefaultHashBuilder;
use hashbrown::HashMap;
use std::alloc::Allocator;
use std::fmt;

type DHB = DefaultHashBuilder;

// These are terms that have normalized completely, to the fullest extent possible
#[derive(Debug)]
pub enum Val<'thir, 'ast, TA: Allocator + Clone> {
  Error, // signifies error in type resolution

  // Types
  Universe(u32), // type of a type is Universe(0)
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
  ConsTy {
    fst: &'thir Val<'thir, 'ast, TA>,
    snd: &'thir Val<'thir, 'ast, TA>,
  },
  StructTy(HashMap<&'thir Vec<u8, TA>, &'thir Val<'thir, 'ast, TA>, DHB, TA>),
  EnumTy(HashMap<&'thir Vec<u8, TA>, &'thir Val<'thir, 'ast, TA>, DHB, TA>),
  FunTy {
    in_ty: &'thir Val<'thir, 'ast, TA>,
    out_ty:&'thir  Val<'thir, 'ast, TA>,
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
    fst: &'thir Val<'thir, 'ast, TA>,
    snd: &'thir Val<'thir, 'ast, TA>,
  },
  Struct(HashMap<&'thir Vec<u8, TA>, Val<'thir, 'ast, TA>, DHB, TA>),
  Enum(HashMap<&'thir Vec<u8, TA>, Val<'thir, 'ast, TA>, DHB, TA>),
  Fun(Expr<'thir, 'ast, TA>),
  Never {
    returned: &'thir Val<'thir, 'ast, TA>,
    levelsUp: u32,
  },
  // Neutral represents a value that we can't evaluate since we're missing information
  Neutral {
    term: Neutral<'thir, 'ast, TA>,
    ty: &'thir Val<'thir, 'ast, TA>,
  },
}

#[derive(Debug)]
pub enum Neutral<'thir, 'ast, TA: Allocator + Clone> {
  Var(u32), // De Brujin index
  App {
    fun: &'thir Neutral<'thir, 'ast, TA>,
    arg: NormalForm<'thir, 'ast, TA>,
  },
  StructAccess {
    root: &'thir Neutral<'thir, 'ast, TA>,
    field: &'thir Vec<u8, TA>,
  },
}

#[derive(Debug)]
pub struct NormalForm<'thir, 'ast, TA: Allocator + Clone> {
  term: &'thir Val<'thir, 'ast, TA>,
  ty: &'thir Val<'thir, 'ast, TA>,
}

impl<TA: Allocator + Clone> fmt::Display for Val<'_, '_, TA> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let val = match self {
      Val::Error => "!!error!!".to_owned(),
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
            format!("{}", val)
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
            format!("{}", val)
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
      _ => "whoops, not implemented".to_owned()
    };

    write!(f, "{}", val)
  }
}

// TA-> ThirAllocator

#[derive(Debug)]
pub enum ExprKind<'thir, 'ast, TA: Allocator + Clone> {
  // An error when parsing
  Error,
  // Loops until a scope is returned
  Loop(&'thir Expr<'thir, 'ast, TA>),
  // applies a function
  Apply {
    fun: &'thir Expr<'thir, 'ast, TA>,
    arg: &'thir Expr<'thir, 'ast, TA>,
  },
  // Wraps a term in a label that can be deferred or returned from
  Label(&'thir Expr<'thir, 'ast, TA>),
  // Returns from a scope with a value
  Ret {
    // the number of labels up to find the correct one (de brujin index)
    labels_up: u32,
    value: &'thir Expr<'thir, 'ast, TA>,
  },
  // constructs a new compound ty
  StructLiteral(HashMap<&'thir Vec<u8, TA>, Expr<'thir, 'ast, TA>, DHB, TA>),
  // Accessing the module of a module object
  StructAccess {
    root: &'thir Expr<'thir, 'ast, TA>,
    field: Vec<u8, TA>,
  },
  // A reference to a previously defined variable
  Reference(&'thir Vec<u8, TA>),
  // Switches on a pattern
  CaseOf {
    expr: &'thir Expr<'thir, 'ast, TA>,
    case_options: Vec<(Pat<'thir, 'ast, TA>, Expr<'thir, 'ast, TA>), TA>,
  },
  // Quotes pattern
  Pat(&'thir Pat<'thir, 'ast, TA>),

  // Literals for values
  Nil,
  NilType,
  NeverType,
  Bool(bool),
  BoolType,

  // Type stuff
  // creates a pub struct from an ad hoc compound object
  Struct(&'thir Expr<'thir, 'ast, TA>),
  // creates a disjoint union from an ad hoc compound object
  Enum(&'thir Expr<'thir, 'ast, TA>),
  // Creates a function constructing the compound type provided
  New(&'thir Expr<'thir, 'ast, TA>),
  // creates a tuple
  Cons {
    fst: &'thir Expr<'thir, 'ast, TA>,
    snd: &'thir Expr<'thir, 'ast, TA>,
  },
  // Create Function
  Defun {
    pattern: &'thir Pat<'thir, 'ast, TA>,
    result: &'thir Expr<'thir, 'ast, TA>,
    infer_pattern: bool,
  },
  // Sequence
  Sequence {
    fst: &'thir Expr<'thir, 'ast, TA>,
    snd: &'thir Expr<'thir, 'ast, TA>,
  },
  // Assign value to place
  LetIn {
    pat: &'thir Pat<'thir, 'ast, TA>,
    val: &'thir Expr<'thir, 'ast, TA>,
    body: &'thir Expr<'thir, 'ast, TA>,
  },
}

#[derive(Debug)]
pub struct Expr<'thir, 'ast, TA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: ExprKind<'thir, 'ast, TA>,
  pub ty: &'thir Val<'thir, 'ast, TA>,
}

#[derive(Debug)]
pub enum PatKind<'thir, 'ast, TA: Allocator + Clone> {
  // An error when parsing
  Error,
  // Irrefutably matches a single element to new variable
  BindIdentifier(&'thir Vec<u8, TA>),
  // Ignore
  BindIgnore,
  // match with a variety of types
  Range {
    inclusive: bool,
    left_operand: &'thir Expr<'thir, 'ast, TA>,
    right_operand: &'thir Expr<'thir, 'ast, TA>,
  },
  // constrains the type of a value
  Annotate {
    pattern: &'thir Pat<'thir, 'ast, TA>,
    ty: &'thir Expr<'thir, 'ast, TA>,
  },
  // destructure a tuple
  Cons {
    left_operand: &'thir Pat<'thir, 'ast, TA>,
    right_operand: &'thir Pat<'thir, 'ast, TA>,
  },
  // Selects a function and calls it with the scrutinee.
  // The result is then refutably matched with the argument provided
  // Example: Array($a, $b, $c) = someFunc();
  ActivePattern {
    function: &'thir Expr<'thir, 'ast, TA>,
    param: &'thir Pat<'thir, 'ast, TA>,
  },
  // Refutable pattern of a value
  Value(&'thir Expr<'thir, 'ast, TA>),
  // Evaluates the second pattern iff the first pattern matches, matches if both are true
  And {
    left_operand: &'thir Pat<'thir, 'ast, TA>,
    right_operand: &'thir Pat<'thir, 'ast, TA>,
  },
  // Evaluates the second pattern iff the first pattern doesn't match, matches if at least one is true
  Or {
    left_operand: &'thir Pat<'thir, 'ast, TA>,
    right_operand: &'thir Pat<'thir, 'ast, TA>,
  },
  // Depub structures a field of a pub struct object
  StructLiteral {
    // whether or not the struct TAs an other matcher
    // $* = _
    splat: Option<&'thir Pat<'thir, 'ast, TA>>,
    patterns: Vec<(&'thir Vec<u8, TA>, Pat<'thir, 'ast, TA>), TA>,
  },
}

#[derive(Debug)]
pub struct Pat<'thir, 'ast, TA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: PatKind<'thir, 'ast, TA>,
  pub ty: &'thir Val<'thir, 'ast, TA>,
}
