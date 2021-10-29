use super::ast;
use super::hir;
use num_bigint::BigInt;
use num_rational::BigRational;
use std::alloc::Allocator;
use super::hir::CaseSource;

// TA -> HirAllocator

#[derive(Debug)]
pub enum ValExprKind<'thir, 'ast, TA: Allocator + Clone> {
  // An error when parsing
  Error,
  // Loops until a scope is returned
  Loop(&'thir ValExpr<'thir, 'ast, TA>),
  // applies a function
  Apply {
    fun: &'thir ValExpr<'thir, 'ast, TA>,
    arg: &'thir ValExpr<'thir, 'ast, TA>,
  },
  // Wraps a term in a label that can be deferred or returned from
  Label(&'thir ValExpr<'thir, 'ast, TA>),
  // Returns from a scope with a value
  Ret {
    labels_up: usize,
    value: &'thir ValExpr<'thir, 'ast, TA>,
  },
  // constructs a new compound ty
  StructLiteral(Vec<(&'ast Vec<u8>, (&'ast ast::Expr, ValExpr<'thir, 'ast, TA>)), TA>),

  // discards the rest of the fields
  Take(&'thir PlaceExpr<'thir, 'ast, TA>),
  Borrow(&'thir PlaceExpr<'thir, 'ast, TA>),
  MutBorrow(&'thir PlaceExpr<'thir, 'ast, TA>),

  // Annotate the value with the type
  Annotate {
    val_expr: &'thir ValExpr<'thir, 'ast, TA>,
    ty_expr: &'thir ValExpr<'thir, 'ast, TA>,
  },
  // Switches on a pattern
  CaseOf {
    expr: &'thir PlaceExpr<'thir, 'ast, TA>,
    case_options: Vec<(Pat<'thir, 'ast, TA>, ValExpr<'thir, 'ast, TA>), TA>,
    source: CaseSource,
  },

  // Literals
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

  Nil,
  Bool(bool),
  Char(u32),
  Int(&'ast BigInt),
  Float(&'ast BigRational),

  // Type stuff
  // creates a pub struct from an ad hoc compound object
  Struct(&'thir ValExpr<'thir, 'ast, TA>),
  // creates a disjoint union from an ad hoc compound object
  Enum(&'thir ValExpr<'thir, 'ast, TA>),
  // creates a tuple
  Cons {
    fst: &'thir ValExpr<'thir, 'ast, TA>,
    snd: &'thir ValExpr<'thir, 'ast, TA>,
  },
  // Create Function
  Defun {
    pattern: &'thir Pat<'thir, 'ast, TA>,
    result: &'thir ValExpr<'thir, 'ast, TA>,
  },
  // Sequence
  Sequence {
    fst: &'thir ValExpr<'thir, 'ast, TA>,
    snd: &'thir ValExpr<'thir, 'ast, TA>,
  },
  // Assign value to place
  LetIn {
    pat: &'thir Pat<'thir, 'ast, TA>,
    val: &'thir ValExpr<'thir, 'ast, TA>,
    body: &'thir ValExpr<'thir, 'ast, TA>,
  },
}

#[derive(Debug)]
pub struct ValExpr<'thir, 'ast, TA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: ValExprKind<'thir, 'ast, TA>,
  pub ty: &'thir hir::ValExpr<'thir, 'ast, TA>
}

#[derive(Debug)]
pub enum PlaceExprKind<'thir, 'ast, TA: Allocator + Clone> {
  Error,
  // creates
  StructField {
    root: &'thir PlaceExpr<'thir, 'ast, TA>,
    field_source: &'ast ast::Expr,
    field: &'ast Vec<u8>,
  },

  // dereferncing a pointer gives a place
  Deref(&'thir ValExpr<'thir, 'ast, TA>),

  // A reference to a previously defined variable
  // debruijin index
  Var(usize),
}

#[derive(Debug)]
pub struct PlaceExpr<'thir, 'ast, TA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: PlaceExprKind<'thir, 'ast, TA>,
}

#[derive(Debug)]
pub enum PatKind<'thir, 'ast, TA: Allocator + Clone> {
  // An error when parsing
  Error,
  // Irrefutably matches a single element to new variable
  BindVariable,
  // Irrefutably discards a variable
  BindIgnore,
  // write the variable to a location
  BindPlace(PlaceExpr<'thir, 'ast, TA>),
  // match with a variety of types
  Range {
    inclusive: bool,
    left_operand: &'thir ValExpr<'thir, 'ast, TA>,
    right_operand: &'thir ValExpr<'thir, 'ast, TA>,
  },
  // destructure a tuple
  Cons {
    fst: &'thir Pat<'thir, 'ast, TA>,
    snd: &'thir Pat<'thir, 'ast, TA>,
  },
  // Selects a function and calls it with the scrutinee.
  // The result is then refutably matched with the argument provided
  // Example: Array($a, $b, $c) = someFunc();
  ActivePattern {
    fun: &'thir ValExpr<'thir, 'ast, TA>,
    arg: &'thir Pat<'thir, 'ast, TA>,
  },
  // Refutable pattern of a value
  Value(&'thir ValExpr<'thir, 'ast, TA>),
  // Evaluates the second pattern iff the first pattern matches, matches if both are true
  // none of these may bind any variables
  And {
    left_operand: &'thir Pat<'thir, 'ast, TA>,
    right_operand: &'thir Pat<'thir, 'ast, TA>,
  },
  // Evaluates the second pattern iff the first pattern doesn't match, matches if at least one is true
  // none of these may bind any variables
  Or {
    left_operand: &'thir Pat<'thir, 'ast, TA>,
    right_operand: &'thir Pat<'thir, 'ast, TA>,
  },
  // Depub structures a field of a pub struct object
  StructLiteral(Vec<(&'ast Vec<u8>, Pat<'thir, 'ast, TA>), TA>),
}

#[derive(Debug)]
pub struct Pat<'thir, 'ast, TA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: PatKind<'thir, 'ast, TA>,
  pub ty: &'thir hir::ValExpr<'thir, 'ast, TA>
}


