use super::ast;
use num_bigint::BigInt;
use num_rational::BigRational;
use std::alloc::Allocator;

// HA -> HirAllocator

#[derive(Debug)]
pub enum ExprKind<'hir, 'ast, HA: Allocator> {
  // An error when parsing
  None,
  This,
  // Hole
  Hole,
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
  Label {
    defers: Vec<Expr<'hir, 'ast, HA>, HA>,
    scope: &'hir Expr<'hir, 'ast, HA>,
  },
  // Returns from a scope with a value
  Ret {
    // the number of labels up to find the correct one
    labels_up: u64,
    value: &'hir Expr<'hir, 'ast, HA>,
  },
  // constructs a new compound ty
  StructLiteral(&'hir Expr<'hir, 'ast, HA>),
  // Accessing the module of a module object
  StructAccess {
    root: &'hir Expr<'hir, 'ast, HA>,
    field: Vec<u8, HA>,
  },
  // A reference to a previously defined variable
  Reference(Vec<u8, HA>),
  // Switches on a pattern
  CaseOf {
    expr: &'hir Expr<'hir, 'ast, HA>,
    case_options: Vec<(Pat<'hir, 'ast, HA>, Expr<'hir, 'ast, HA>), HA>,
  },
  // Quotes pattern
  Pat(&'hir Pat<'hir, 'ast, HA>),
  // Constrain the value
  Annotate {
    expr: &'hir Expr<'hir, 'ast, HA>,
    ty: &'hir Expr<'hir, 'ast, HA>,
  },
  // short circuiting and
  And {
    left_operand: &'hir Expr<'hir, 'ast, HA>,
    right_operand: &'hir Expr<'hir, 'ast, HA>,
  },
  // short circuiting or
  Or {
    left_operand: &'hir Expr<'hir, 'ast, HA>,
    right_operand: &'hir Expr<'hir, 'ast, HA>,
  },
  // Creates a new type that always matches the pattern provided
  Refinement {
    ty: &'hir Expr<'hir, 'ast, HA>,
    refinement: &'hir Pat<'hir, 'ast, HA>,
  },
  // Literals for values
  Nil,
  NilType,
  NeverType,
  Bool(bool),
  BoolType,
  // actually bigint
  IntType,
  Int(BigInt),
  RationalType,
  Rational(BigRational),

  // Type stuff
  // creates a pub struct from an ad hoc compound object
  Struct(&'hir Expr<'hir, 'ast, HA>),
  // creates a disjoint union from an ad hoc compound object
  Enum(&'hir Expr<'hir, 'ast, HA>),
  // Creates a function constructing the compound type provided
  New(&'hir Expr<'hir, 'ast, HA>),
  // creates a tuple
  Cons {
    left_operand: &'hir Expr<'hir, 'ast, HA>,
    right_operand: &'hir Expr<'hir, 'ast, HA>,
  },
  // Create Function
  Defun {
    pattern: &'hir Pat<'hir, 'ast, HA>,
    result: &'hir Expr<'hir, 'ast, HA>,
    infer_pattern: bool,
  },
  // Sequence
  Sequence {
    left_operand: &'hir Expr<'hir, 'ast, HA>,
    right_operand: &'hir Expr<'hir, 'ast, HA>,
  },
  // Assign value to place
  LetIn {
    pat: &'hir Pat<'hir, 'ast, HA>,
    val: &'hir Expr<'hir, 'ast, HA>,
    body: &'hir Expr<'hir, 'ast, HA>,
  },

  // functions
  // Math with bools
  BoolNotFn,
  // Math with integers
  IntAddFn,
  IntSubFn,
  IntMulFn,
  IntDivFn,
  IntRemFn,
  // Math with rationals
  RationalAddFn,
  RationalSubFn,
  RationalMulFn,
  RationalDivFn,
  RationalRemFn,
  // Conversion between integers and rationals
  IntToRationalFn,    // promote int to rational
  RationalToIntRNEFn, // round to nearest even
  RationalToIntRTZFn, // round to zero
  RationalToIntRDNFn, // round down
  RationalToIntRUPFn, // round up
  // Bit Vectors
  // Unsigned Operations
  UnsignedBitVecFn, // creates a bitvector from an integer
  UnsignedBitVecAddFn,
  UnsignedBitVecAddOverflowFn,
  UnsignedBitVecSubFn,
  UnsignedBitVecSubOverflowFn,
  UnsignedBitVecMulFn,
  UnsignedBitVecMulOverflowFn,
  UnsignedBitVecDivFn,
  UnsignedBitVecRemFn,
  UnsignedBitVecDivRemFn,
  UnsignedBitVecShrFn, // logical shift right
  UnsignedBitVecShlFn, // shift left
  UnsignedBitVecRolFn, // rotate left
  UnsignedBitVecRorFn, // rotate right
  UnsignedBitVecAndFn,
  UnsignedBitVecOrFn,
  UnsignedBitVecXorFn,
  UnsignedBitVecNotFn,
  // Signed Operations
  SignedBitVecFn, // creates a bitvector from an integer
  SignedBitVecAddFn,
  SignedBitVecAddOverflowFn,
  SignedBitVecSubFn,
  SignedBitVecSubOverflowFn,
  SignedBitVecMulFn,
  SignedBitVecMulOverflowFn,
  SignedBitVecDivFn,
  SignedBitVecRemFn,
  SignedBitVecDivRemFn,
  SignedBitVecShrFn, // arithmetic shift right
  SignedBitVecShlFn, // shift left
  SignedBitVecAndFn,
  SignedBitVecOrFn,
  SignedBitVecXorFn,
  SignedBitVecNotFn,
  SignedBitVecNegateFn,

  // Math with floats
  FloatFn,
  FloatAddFn,
  FloatSubFn,
  FloatMulFn,
  FloatDivFn,
  FloatRemFn,
  FloatDivRemFn,
  // Conversion between bitvecs and floats
  BitVecToFloatFn,    // promote bitVec to float
  FloatToBitVecRNEFn, // round to nearest even
  FloatToBitVecRTZFn, // round to zero
  FloatToBitVecRDNFn, // round down
  FloatToBitVecRUPFn, // round up

  // Handle Memory addresses
  RefFn,
  DerefFn,
}

#[derive(Debug)]
pub struct Expr<'hir, 'ast, HA: Allocator> {
  pub source: Option<&'ast ast::Expr>,
  pub kind: ExprKind<'hir, 'ast, HA>,
}

#[derive(Debug)]
pub enum PatKind<'hir, 'ast, HA: Allocator> {
  // An error when parsing
  None,
  // Irrefutably matches a single element to new variable
  BindIdentifier(Vec<u8, HA>),
  // Ignore
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
    left_operand: &'hir Pat<'hir, 'ast, HA>,
    right_operand: &'hir Pat<'hir, 'ast, HA>,
  },
  // Selects a function and calls it with the scrutinee.
  // The result is then refutably matched with the argument provided
  // Example: Array($a, $b, $c) = someFunc();
  ActivePattern {
    function: &'hir Expr<'hir, 'ast, HA>,
    param: &'hir Pat<'hir, 'ast, HA>,
  },
  // Refutable pattern of a value
  Value(&'hir Expr<'hir, 'ast, HA>),
  // Evaluates the second pattern iff the first pattern matches, matches if both are true
  And {
    left_operand: &'hir Pat<'hir, 'ast, HA>,
    right_operand: &'hir Pat<'hir, 'ast, HA>,
  },
  // Evaluates the second pattern iff the first pattern doesn't match, matches if at least one is true
  Or {
    left_operand: &'hir Pat<'hir, 'ast, HA>,
    right_operand: &'hir Pat<'hir, 'ast, HA>,
  },
  // Depub structures a field of a pub struct object
  StructLiteral {
    // whether or not the struct has an other matcher
    // $* = _
    splat: &'hir Option<Pat<'hir, 'ast, HA>>,
    patterns: Vec<(Vec<u8, HA>, Pat<'hir, 'ast, HA>), HA>,
  },
}

#[derive(Debug)]
pub struct Pat<'hir, 'ast, HA: Allocator> {
  pub source: Option<&'ast ast::Expr>,
  pub kind: PatKind<'hir, 'ast, HA>,
}

