use super::ast;
use hashbrown::HashMap;
use std::alloc::Allocator;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Ty<'thir, TA: Allocator> {
  Ty,
  Nil,
  Bool,
  UInt8,
  UInt16,
  UInt32,
  UInt64,
  Int8,
  Int16,
  Int32,
  Int64,
  Float32,
  Float64,
  Cons {
    left: &'thir Ty<'thir, TA>,
    right: &'thir Ty<'thir, TA>,
  },
  Struct(HashMap<Vec<u8, TA>, Ty<'thir, TA>, TA>),
  Enum(HashMap<Vec<u8, TA>, Ty<'thir, TA>, TA>),
  Fun {
    in_ty: &'thir Ty<'thir, TA>,
    out_ty: &'thir Ty<'thir, TA>,
  },
}

impl<TA: Allocator> fmt::Display for Ty<'_, TA> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let val = match self {
      Ty::Ty => "type".to_owned(),
      Ty::Nil => "nil".to_owned(),
      Ty::Bool => "bool".to_owned(),
      Ty::UInt8 => "uint8".to_owned(),
      Ty::UInt16 => "uint16".to_owned(),
      Ty::UInt32 => "uint32".to_owned(),
      Ty::UInt64 => "uint64".to_owned(),
      Ty::Int8 => "int8".to_owned(),
      Ty::Int16 => "int16".to_owned(),
      Ty::Int32 => "int32".to_owned(),
      Ty::Int64 => "int64".to_owned(),
      Ty::Float32 => "float32".to_owned(),
      Ty::Float64 => "float64".to_owned(),
      Ty::Cons { left, right } => format!("{}, {}", left, right),
      Ty::Struct(h) => format!(
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
      Ty::Enum(h) => format!(
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
      Ty::Fun { in_ty, out_ty } => format!("{} -> {}", in_ty, out_ty),
    };

    write!(f, "{}", val)
  }
}

pub enum Val<'thir, 'ast, TA: Allocator> {
  Nil,
  Bool(bool),
  UInt8(u8),
  UInt16(u16),
  UInt32(u32),
  UInt64(u64),
  Int8(i8),
  Int16(i16),
  Int32(i32),
  Int64(i64),
  Float32(f32),
  Float64(f64),
  Cons {
    left: &'thir Val<'thir, 'ast, TA>,
    right: &'thir Val<'thir, 'ast, TA>,
  },
  Struct {
    fields: HashMap<Vec<u8, TA>, Val<'thir, 'ast, TA>, TA>,
  },
  Enum {
    fields: HashMap<Vec<u8, TA>, Val<'thir, 'ast, TA>, TA>,
  },
  Fun(Expr<'thir, 'ast, TA>),
  Ty(Ty<'thir, TA>),
  Never {
    returned: &'thir Val<'thir, 'ast, TA>,
    levelsUp: u32,
  },
}

// TA-> ThirAllocator

#[derive(Debug)]
pub enum ExprKind<'thir, 'ast, TA: Allocator> {
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
    // the number of labels up to find the correct one
    labels_up: u64,
    value: &'thir Expr<'thir, 'ast, TA>,
  },
  // constructs a new compound ty
  StructLiteral(&'thir Expr<'thir, 'ast, TA>),
  // Accessing the module of a module object
  StructAccess {
    root: &'thir Expr<'thir, 'ast, TA>,
    field: Vec<u8, TA>,
  },
  // A reference to a previously defined variable
  Reference(Vec<u8, TA>),
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
    left_operand: &'thir Expr<'thir, 'ast, TA>,
    right_operand: &'thir Expr<'thir, 'ast, TA>,
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
pub struct Expr<'thir, 'ast, TA: Allocator> {
  pub source: &'ast ast::Expr,
  pub kind: ExprKind<'thir, 'ast, TA>,
  pub ty: Option<&'thir Ty<'thir, TA>>,
}

#[derive(Debug)]
pub enum PatKind<'thir, 'ast, TA: Allocator> {
  // An error when parsing
  Error,
  // Irrefutably matches a single element to new variable
  BindIdentifier(Vec<u8, TA>),
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
    splat: &'thir Option<Pat<'thir, 'ast, TA>>,
    patterns: Vec<(Vec<u8, TA>, Pat<'thir, 'ast, TA>), TA>,
  },
}

#[derive(Debug)]
pub struct Pat<'thir, 'ast, TA: Allocator> {
  pub source: &'ast ast::Expr,
  pub kind: PatKind<'thir, 'ast, TA>,
  pub ty: Option<Ty<'thir, TA>>,
}
