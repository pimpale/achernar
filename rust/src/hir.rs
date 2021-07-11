use super::ast;
use num_bigint::BigInt;
use num_rational::BigRational;
use std::alloc::Allocator;

#[derive(Debug)]
pub enum ExprKind<'hir, 'ast, A: Allocator> {
  // An error when parsing
  None,
  This,
  // Hole
  Hole,
  // Loops until a scope is returned
  Loop(&'hir Expr<'hir, 'ast, A>),
  // Does not infer types
  NoInfer(&'hir Expr<'hir, 'ast, A>),
  // applies a function
  Apply {
    fun: &'hir Expr<'hir, 'ast, A>,
    arg: &'hir Expr<'hir, 'ast, A>,
  },
  // Wraps a term in a label that can be deferred or returned from
  Label {
    defers: Vec<Expr<'hir, 'ast, A>, A>,
    scope: &'hir Expr<'hir, 'ast, A>,
  },
  // Returns from a scope with a value
  Ret {
    // the number of labels up to find the correct one
    labels_up: u64,
    value: &'hir Expr<'hir, 'ast, A>,
  },
  // constructs a new compound ty
  StructLiteral(&'hir Expr<'hir, 'ast, A>),
  // Accessing the module of a module object
  StructAccess {
    root: &'hir Expr<'hir, 'ast, A>,
    field: Vec<u8, A>,
  },
  // A reference to a previously defined variable
  Reference(Vec<u8, A>),
  // Switches on a pattern
  CaseOf {
    expr: &'hir Expr<'hir, 'ast, A>,
    case_options: Vec<(Pat<'hir, 'ast, A>, Expr<'hir, 'ast, A>), A>,
  },
  // Quotes pattern
  Pat(&'hir Pat<'hir, 'ast, A>),
  // Constrain the value
  Ty {
    expr: &'hir Expr<'hir, 'ast, A>,
    ty: &'hir Expr<'hir, 'ast, A>,
  },
  // short circuiting and
  And {
    left_operand: &'hir Expr<'hir, 'ast, A>,
    right_operand: &'hir Expr<'hir, 'ast, A>,
  },
  // short circuiting or
  Or {
    left_operand: &'hir Expr<'hir, 'ast, A>,
    right_operand: &'hir Expr<'hir, 'ast, A>,
  },
  // Creates a new type that always matches the pattern provided
  Refinement {
    ty: &'hir Expr<'hir, 'ast, A>,
    refinement: &'hir Pat<'hir, 'ast, A>,
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
  Struct(&'hir Expr<'hir, 'ast, A>),
  // creates a disjoint union from an ad hoc compound object
  Enum(&'hir Expr<'hir, 'ast, A>),
  // Creates a function constructing the compound type provided
  New(&'hir Expr<'hir, 'ast, A>),
  // creates a tuple
  Cons {
    left_operand: &'hir Expr<'hir, 'ast, A>,
    right_operand: &'hir Expr<'hir, 'ast, A>,
  },
  // Create Function
  Defun {
    pattern: &'hir Pat<'hir, 'ast, A>,
    result: &'hir Expr<'hir, 'ast, A>,
    infer_pattern: bool,
  },
  // Sequence
  Sequence {
    left_operand: &'hir Expr<'hir, 'ast, A>,
    right_operand: &'hir Expr<'hir, 'ast, A>,
  },
  // Assign value to place
  LetIn {
    pat: &'hir Pat<'hir, 'ast, A>,
    val: &'hir Expr<'hir, 'ast, A>,
    body: &'hir Expr<'hir, 'ast, A>,
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

//  // Handle memory address + ownership
//  hir_EK_PlaceType, // this is the ty of a valid place that may be assigned to
//                    // or take reference of
//  hir_EK_PatternType, // PlaceType | StructPattern | IntPatternType |
//                      // RationalPatternType | Splat | TODO
//
//  // Handle memory addresses
//  hir_EK_GetMemAddrFn,   // PlaceType($x) -> Ref(x)
//  hir_EK_DerefMemAddrFn, // Ref($x) -> PlaceType(x)
//
//  // Returns a place from a memory address
//  hir_EK_MutateMemAddrFn,


}

#[derive(Debug)]
pub struct Expr<'hir, 'ast, A: Allocator> {
  pub source: Option<&'ast ast::Expr>,
  pub kind: ExprKind<'hir, 'ast, A>,
}

#[derive(Debug)]
pub enum PatKind<'hir, 'ast, A: Allocator> {
  // An error when parsing
  None,
  // Irrefutably matches a single element to new variable
  BindIdentifier(Vec<u8, A>),
  // Ignore
  BindIgnore,
  // Hole
  Hole,
  // match with a variety of types
  Range {
    inclusive: bool,
    left_operand: &'hir Expr<'hir, 'ast, A>,
    right_operand: &'hir Expr<'hir, 'ast, A>,
  },
  // constrains the type of a value
  Ty {
    pattern: &'hir Pat<'hir, 'ast, A>,
    ty: &'hir Expr<'hir, 'ast, A>,
  },
  // destructure a tuple
  Cons {
    left_operand: &'hir Pat<'hir, 'ast, A>,
    right_operand: &'hir Pat<'hir, 'ast, A>,
  },
  // Selects a function and calls it with the scrutinee.
  // The result is then refutably matched with the argument provided
  // Example: Array($a, $b, $c) = someFunc();
  ActivePattern {
    function: &'hir Expr<'hir, 'ast, A>,
    param: &'hir Pat<'hir, 'ast, A>,
  },
  // Refutable pattern of a value
  Value(&'hir Expr<'hir, 'ast, A>),
  // Evaluates the second pattern iff the first pattern matches, matches if both are true
  And {
    left_operand: &'hir Pat<'hir, 'ast, A>,
    right_operand: &'hir Pat<'hir, 'ast, A>,
  },
  // Evaluates the second pattern iff the first pattern doesn't match, matches if at least one is true
  Or {
    left_operand: &'hir Pat<'hir, 'ast, A>,
    right_operand: &'hir Pat<'hir, 'ast, A>,
  },
  // Depub structures a field of a pub struct object
  StructLiteral {
    // whether or not the struct has an other matcher
    // $* = _
    splat: &'hir Option<Pat<'hir, 'ast, A>>,
    patterns: Vec<(Vec<u8, A>, Pat<'hir, 'ast, A>), A>,
  },
}

#[derive(Debug)]
pub struct Pat<'hir, 'ast, A: Allocator> {
  pub source: Option<&'ast ast::Expr>,
  pub kind: PatKind<'hir, 'ast, A>,
}

