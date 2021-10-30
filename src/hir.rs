use super::ast;
use num_bigint::BigInt;
use num_rational::BigRational;
use std::alloc::Allocator;

#[derive(Debug)]
pub enum CaseSource {
  Case,
  IfElse,
  And,
  Or,
}

// HA -> HirAllocator

#[derive(Debug)]
pub enum ValExprKind<'hir, 'ast, HA: Allocator + Clone> {
  // Error in parsing
  Error,
  // Loops until a scope is returned
  Loop(&'hir ValExpr<'hir, 'ast, HA>),
  // applies a function
  App {
    fun: &'hir ValExpr<'hir, 'ast, HA>,
    arg: &'hir ValExpr<'hir, 'ast, HA>,
  },
  // Wraps a term in a label that can be deferred or returned from
  Label(&'hir ValExpr<'hir, 'ast, HA>),
  // Returns from a scope with a value
  Ret {
    labels_up: usize,
    value: &'hir ValExpr<'hir, 'ast, HA>,
  },
  // constructs a new compound ty
  StructLiteral(Vec<(&'ast Vec<u8>, (&'ast ast::Expr, ValExpr<'hir, 'ast, HA>)), HA>),

  // discards the rest of the fields
  Take(&'hir PlaceExpr<'hir, 'ast, HA>),
  Borrow(&'hir PlaceExpr<'hir, 'ast, HA>),
  MutBorrow(&'hir PlaceExpr<'hir, 'ast, HA>),

  // Annotate the value with the type
  Annotate {
    val_expr: &'hir ValExpr<'hir, 'ast, HA>,
    ty_expr: &'hir ValExpr<'hir, 'ast, HA>,
  },
  // Switches on a pattern
  CaseOf {
    expr: &'hir PlaceExpr<'hir, 'ast, HA>,
    case_options: Vec<(RefutablePatExpr<'hir, 'ast, HA>, ValExpr<'hir, 'ast, HA>), HA>,
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
  Struct(&'hir ValExpr<'hir, 'ast, HA>),
  // creates a disjoint union from an ad hoc compound object
  Enum(&'hir ValExpr<'hir, 'ast, HA>),
  // creates a tuple
  Pair {
    fst: &'hir ValExpr<'hir, 'ast, HA>,
    snd: &'hir ValExpr<'hir, 'ast, HA>,
  },
  // Create Function
  Lam {
    pattern: &'hir IrrefutablePatExpr<'hir, 'ast, HA>,
    result: &'hir ValExpr<'hir, 'ast, HA>,
  },
  LamTy {
    pattern: &'hir IrrefutablePatExpr<'hir, 'ast, HA>,
    result: &'hir ValExpr<'hir, 'ast, HA>,
  },
  // Sequence
  Sequence {
    fst: &'hir ValExpr<'hir, 'ast, HA>,
    snd: &'hir ValExpr<'hir, 'ast, HA>,
  },
  // Assign value to place
  LetIn {
    pat: &'hir IrrefutablePatExpr<'hir, 'ast, HA>,
    val: &'hir ValExpr<'hir, 'ast, HA>,
    body: &'hir ValExpr<'hir, 'ast, HA>,
  },
}

#[derive(Debug)]
pub struct ValExpr<'hir, 'ast, HA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: ValExprKind<'hir, 'ast, HA>,
}

#[derive(Debug)]
pub enum PlaceExprKind<'hir, 'ast, HA: Allocator + Clone> {
  Error,
  // creates
  StructField {
    root: &'hir PlaceExpr<'hir, 'ast, HA>,
    field_source: &'ast ast::Expr,
    field: &'ast Vec<u8>,
  },

  // dereferncing a pointer gives a place
  Deref(&'hir ValExpr<'hir, 'ast, HA>),

  // A reference to a previously defined variable
  // debruijin index
  Var(usize),
}

#[derive(Debug)]
pub struct PlaceExpr<'hir, 'ast, HA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: PlaceExprKind<'hir, 'ast, HA>,
}

// A pattern that may not be rejected, and can bind variables
#[derive(Debug)]
pub enum IrrefutablePatExprKind<'hir, 'ast, HA: Allocator + Clone> {
  // An error when parsing
  Error,
  // Always matches with Nil, but doesn't bind anything
  Nil,
  // Irrefutably matches a single element to new variable
  BindVariable,
  // write the variable to a location
  BindPlace(PlaceExpr<'hir, 'ast, HA>),
  // destructure a tuple
  Pair {
    fst: &'hir IrrefutablePatExpr<'hir, 'ast, HA>,
    snd: &'hir IrrefutablePatExpr<'hir, 'ast, HA>,
  },
  // Selects a function and calls it with the scrutinee.
  // The result is then refutably matched with the argument provided
  // Also can be used to drop values during matching
  // Example: Array($a, $b, $c) = someFunc();
  // Example: _ () = someFunc();
  ActivePattern {
    fun: &'hir ValExpr<'hir, 'ast, HA>,
    arg: &'hir IrrefutablePatExpr<'hir, 'ast, HA>,
  },
  // Depub structures a field of a pub struct object
  StructLiteral(Vec<(&'ast Vec<u8>, IrrefutablePatExpr<'hir, 'ast, HA>), HA>),
}

#[derive(Debug)]
pub struct IrrefutablePatExpr<'hir, 'ast, HA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: IrrefutablePatExprKind<'hir, 'ast, HA>,
}

// A pattern that may reject, and can bind variables
#[derive(Debug)]
pub enum RefutablePatExprKind<'hir, 'ast, HA: Allocator + Clone> {
  // An error when parsing
  Error,
  // Irrefutably match this expr
  IrrefutablePat(&'hir IrrefutablePatExpr<'hir, 'ast, HA>),
  // Match against a value or fail
  ValPat(&'hir ValPatExpr<'hir, 'ast, HA>),
  // destructure a tuple
  Pair {
    fst: &'hir RefutablePatExpr<'hir, 'ast, HA>,
    snd: &'hir RefutablePatExpr<'hir, 'ast, HA>,
  },
  // Selects a function and calls it with the scrutinee.
  // The result is then refutably matched with the argument provided
  // Also can be used to drop values during matching
  // Example: Array($a, $b, $c) = someFunc();
  // Example: _ () = someFunc();
  ActivePattern {
    fun: &'hir ValExpr<'hir, 'ast, HA>,
    arg: &'hir IrrefutablePatExpr<'hir, 'ast, HA>,
  },
  // Depub structures a field of a pub struct object
  StructLiteral(Vec<(&'ast Vec<u8>, RefutablePatExpr<'hir, 'ast, HA>), HA>),
  // Evaluates the second pattern iff the first pattern matches, matches if both are true
  // none of these may bind any variables
  And {
    fst: &'hir RefutablePatExpr<'hir, 'ast, HA>,
    snd: &'hir RefutablePatExpr<'hir, 'ast, HA>,
  },
}

#[derive(Debug)]
pub struct RefutablePatExpr<'hir, 'ast, HA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: RefutablePatExprKind<'hir, 'ast, HA>,
}

// a pattern that can reject, and can't bind any variables
#[derive(Debug)]
pub enum ValPatExprKind<'hir, 'ast, HA: Allocator + Clone> {
  // An error when parsing
  Error,
  // Irrefutably discards a variable
  Ignore,
  // match with a variety of types
  Range {
    inclusive: bool,
    left_operand: &'hir ValExpr<'hir, 'ast, HA>,
    right_operand: &'hir ValExpr<'hir, 'ast, HA>,
  },
  // Selects a function and calls it with the scrutinee.
  // The result is then refutably matched with the argument provided
  // Also can be used to drop values during matching
  // Example: Array($a, $b, $c) = someFunc();
  // Example: _ () = someFunc();
  ActivePattern {
    fun: &'hir ValExpr<'hir, 'ast, HA>,
    arg: &'hir ValPatExpr<'hir, 'ast, HA>,
  },
  // Destructures a field of a struct object
  StructLiteral(Vec<(&'ast Vec<u8>, ValPatExpr<'hir, 'ast, HA>), HA>),
  // destructure a tuple
  Pair {
    fst: &'hir ValPatExpr<'hir, 'ast, HA>,
    snd: &'hir ValPatExpr<'hir, 'ast, HA>,
  },
  // Evaluates the second pattern iff the first pattern matches, matches if both are true
  // none of these may bind any variables
  And {
    fst: &'hir ValPatExpr<'hir, 'ast, HA>,
    snd: &'hir ValPatExpr<'hir, 'ast, HA>,
  },
  // Evaluates the second pattern iff the first pattern doesn't match, matches if at least one is true
  // none of these may bind any variables
  Or {
    fst: &'hir ValPatExpr<'hir, 'ast, HA>,
    snd: &'hir ValPatExpr<'hir, 'ast, HA>,
  },
  // Refutable pattern of a value
  Value(&'hir ValExpr<'hir, 'ast, HA>),
}

#[derive(Debug)]
pub struct ValPatExpr<'hir, 'ast, HA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: ValPatExprKind<'hir, 'ast, HA>,
}

//  // functions
//  // Math with bools
//  BoolNotFn,
//  // Math with integers
//  IntAddFn,
//  IntSubFn,
//  IntMulFn,
//  IntDivFn,
//  IntRemFn,
//  // Math with rationals
//  RationalAddFn,
//  RationalSubFn,
//  RationalMulFn,
//  RationalDivFn,
//  RationalRemFn,
//  // Conversion between integers and rationals
//  IntToRationalFn,    // promote int to rational
//  RationalToIntRNEFn, // round to nearest even
//  RationalToIntRTZFn, // round to zero
//  RationalToIntRDNFn, // round down
//  RationalToIntRUPFn, // round up
//  // Bit Vectors
//  // Unsigned Operations
//  UnsignedBitVecFn, // creates a bitvector from an integer
//  UnsignedBitVecAddFn,
//  UnsignedBitVecAddOverflowFn,
//  UnsignedBitVecSubFn,
//  UnsignedBitVecSubOverflowFn,
//  UnsignedBitVecMulFn,
//  UnsignedBitVecMulOverflowFn,
//  UnsignedBitVecDivFn,
//  UnsignedBitVecRemFn,
//  UnsignedBitVecDivRemFn,
//  UnsignedBitVecShrFn, // logical shift right
//  UnsignedBitVecShlFn, // shift left
//  UnsignedBitVecRolFn, // rotate left
//  UnsignedBitVecRorFn, // rotate right
//  UnsignedBitVecAndFn,
//  UnsignedBitVecOrFn,
//  UnsignedBitVecXorFn,
//  UnsignedBitVecNotFn,
//  // Signed Operations
//  SignedBitVecFn, // creates a bitvector from an integer
//  SignedBitVecAddFn,
//  SignedBitVecAddOverflowFn,
//  SignedBitVecSubFn,
//  SignedBitVecSubOverflowFn,
//  SignedBitVecMulFn,
//  SignedBitVecMulOverflowFn,
//  SignedBitVecDivFn,
//  SignedBitVecRemFn,
//  SignedBitVecDivRemFn,
//  SignedBitVecShrFn, // arithmetic shift right
//  SignedBitVecShlFn, // shift left
//  SignedBitVecAndFn,
//  SignedBitVecOrFn,
//  SignedBitVecXorFn,
//  SignedBitVecNotFn,
//  SignedBitVecNegateFn,
//
//  // Math with floats
//  FloatFn,
//  FloatAddFn,
//  FloatSubFn,
//  FloatMulFn,
//  FloatDivFn,
//  FloatRemFn,
//  FloatDivRemFn,
//  // Conversion between bitvecs and floats
//  BitVecToFloatFn,    // promote bitVec to float
//  FloatToBitVecRNEFn, // round to nearest even
//  FloatToBitVecRTZFn, // round to zero
//  FloatToBitVecRDNFn, // round down
//  FloatToBitVecRUPFn, // round up
//
//  // Handle Memory addresses
//  RefFn,
//  DerefFn,
