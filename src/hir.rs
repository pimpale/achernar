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
pub enum ExprKind<'hir, 'ast, HA: Allocator + Clone> {
  // Error in parsing
  Error,
  // Loops until a scope is returned
  Loop(&'hir Expr<'hir, 'ast, HA>),
  // Does not infer types
  NoInfer(&'hir Expr<'hir, 'ast, HA>),
  // applies a function
  Apply {
    fun: &'hir Expr<'hir, 'ast, HA>,
    arg: &'hir Expr<'hir, 'ast, HA>,
  },
  // Wraps a term in a label that can be deferred or returned from
  Label (&'hir Expr<'hir, 'ast, HA>),
  // Returns from a scope with a value
  Ret {
    labels_up: usize,
    value: &'hir Expr<'hir, 'ast, HA>,
  },
  // constructs a new compound ty
  StructLiteral(Vec<(&'ast Vec<u8>, Expr<'hir, 'ast, HA>), HA>),
  // Accessing the module of a module object
  StructAccess {
    root: &'hir Expr<'hir, 'ast, HA>,
    field: &'ast Vec<u8>,
  },
  // A reference to a previously defined variable
  // debruijin index
  Var(usize),
  // Constrain the value
  Annotate {
    expr: &'hir Expr<'hir, 'ast, HA>,
    ty: &'hir Expr<'hir, 'ast, HA>,
  },
  // Switches on a pattern
  CaseOf {
    expr: &'hir Expr<'hir, 'ast, HA>,
    case_options: Vec<(Pat<'hir, 'ast, HA>, Expr<'hir, 'ast, HA>), HA>,
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
  Int(BigInt),
  Float(BigRational),

  // Type stuff
  // creates a pub struct from an ad hoc compound object
  Struct(&'hir Expr<'hir, 'ast, HA>),
  // creates a disjoint union from an ad hoc compound object
  Enum(&'hir Expr<'hir, 'ast, HA>),
  // creates a tuple
  Cons {
    fst: &'hir Expr<'hir, 'ast, HA>,
    snd: &'hir Expr<'hir, 'ast, HA>,
  },
  // Create Function
  Defun {
    pattern: &'hir Pat<'hir, 'ast, HA>,
    result: &'hir Expr<'hir, 'ast, HA>,
    infer_pattern: bool,
  },
  // Sequence
  Sequence {
    fst: &'hir Expr<'hir, 'ast, HA>,
    snd: &'hir Expr<'hir, 'ast, HA>,
  },
  // Assign value to place
  LetIn {
    pat: &'hir Pat<'hir, 'ast, HA>,
    val: &'hir Expr<'hir, 'ast, HA>,
    body: &'hir Expr<'hir, 'ast, HA>,
  },
}

#[derive(Debug)]
pub struct Expr<'hir, 'ast, HA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: ExprKind<'hir, 'ast, HA>,
}

#[derive(Debug)]
pub enum PatKind<'hir, 'ast, HA: Allocator + Clone> {
  // An error when parsing
  Error,
  // Irrefutably matches a single element to new variable
  BindVariable,
  // Irrefutably discards a variable
  BindIgnore,
  // Hole
  Hole,
  // match with a variety of types
  Range {
    inclusive: bool,
    left_operand: &'hir Expr<'hir, 'ast, HA>,
    right_operand: &'hir Expr<'hir, 'ast, HA>,
  },
  // constrains the type of a value
  Annotate {
    pattern: &'hir Pat<'hir, 'ast, HA>,
    ty: &'hir Expr<'hir, 'ast, HA>,
  },
  // destructure a tuple
  Cons {
    fst: &'hir Pat<'hir, 'ast, HA>,
    snd: &'hir Pat<'hir, 'ast, HA>,
  },
  // Selects a function and calls it with the scrutinee.
  // The result is then refutably matched with the argument provided
  // Example: Array($a, $b, $c) = someFunc();
  ActivePattern {
    fun: &'hir Expr<'hir, 'ast, HA>,
    arg: &'hir Pat<'hir, 'ast, HA>,
  },
  // Refutable pattern of a value
  Value(&'hir Expr<'hir, 'ast, HA>),
  // Evaluates the second pattern iff the first pattern matches, matches if both are true
  // none of these may bind any variables
  And {
    left_operand: &'hir Pat<'hir, 'ast, HA>,
    right_operand: &'hir Pat<'hir, 'ast, HA>,
  },
  // Evaluates the second pattern iff the first pattern doesn't match, matches if at least one is true
  // none of these may bind any variables
  Or {
    left_operand: &'hir Pat<'hir, 'ast, HA>,
    right_operand: &'hir Pat<'hir, 'ast, HA>,
  },
  // Depub structures a field of a pub struct object
  StructLiteral(Vec<(&'ast Vec<u8>, Pat<'hir, 'ast, HA>), HA>),
}

#[derive(Debug)]
pub struct Pat<'hir, 'ast, HA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: PatKind<'hir, 'ast, HA>,
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
