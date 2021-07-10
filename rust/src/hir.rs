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
    //
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
  Never,
  NeverType,
  Bool(bool),
  BoolType,
  // actually bigint
  IntType,
  Int(BigInt),
  RealType,
  Real(BigRational),

  // Type stuff
  // creates a pub struct from an ad hoc compound object
  StructFn,
  // creates a disjoint union from an ad hoc compound object
  EnumFn,
  // Creates a function constructing the compound type provided
  NewFn,
  // creates a tuple
  ConsFn,
  // Create Function
  Defun {
    pattern: &'hir Pat<'hir, 'ast, A>,
    result: &'hir Expr<'hir, 'ast, A>,
    infer_pattern: bool,
  },
  // Sequence
  SequenceFn,
  // Assign value to place
  LetIn {
    pat: &'hir Pat<'hir, 'ast, A>,
    val: &'hir Expr<'hir, 'ast, A>,
    body: &'hir Expr<'hir, 'ast, A>,
  },
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

// // Math with integers
// IntAddFn,
// IntSubFn,
// IntMulFn,
// IntDivFn,
// IntRemFn,
// // Math with reals
// RealAddFn,
// RealSubFn,
// RealMulFn,
// RealDivFn,
// RealRemFn,
// // Conversion between integers and reals
// RealToIntRNE, // round to nearest even
// RealToIntRTZ, // round to zero
// RealToIntRDN, // round down
// RealToIntRUP, // round up
// IntToReal,    // promote int to real
// // Bit Vectors
// // Unsigned Operations
// UnsignedBitVecFn, // creates a bitvector from an integer
// UnsignedBitVecAddFn,
// UnsignedBitVecAddOverflowFn,
// UnsignedBitVecSubFn,
// UnsignedBitVecSubOverflowFn,
// UnsignedBitVecMulFn,
// UnsignedBitVecMulOverflowFn,
// UnsignedBitVecDivFn,
// UnsignedBitVecRemFn,
// UnsignedBitVecDivRemFn,
// UnsignedBitVecShrFn, // logical shift right
// UnsignedBitVecShlFn, // shift left
// UnsignedBitVecRolFn, // rotate left
// UnsignedBitVecRorFn, // rotate right
// UnsignedBitVecAndFn,
// UnsignedBitVecOrFn,
// UnsignedBitVecXorFn,
// UnsignedBitVecNotFn,
// // Signed Operations
// SignedBitVecFn, // creates a bitvector from an integer
// SignedBitVecAddFn,
// SignedBitVecAddOverflowFn,
// SignedBitVecSubFn,
// SignedBitVecSubOverflowFn,
// SignedBitVecMulFn,
// SignedBitVecMulOverflowFn,
// SignedBitVecDivFn,
// SignedBitVecRemFn,
// SignedBitVecDivRemFn,
// SignedBitVecShrFn, // arithmetic shift right
// SignedBitVecShlFn, // shift left
// SignedBitVecAndFn,
// SignedBitVecOrFn,
// SignedBitVecXorFn,
// SignedBitVecNotFn,
// SignedBitVecNegate,

//  // Math with floats
//
//  // Handle memory address + ownership
//  hir_EK_PlaceType, // this is the ty of a valid place that may be assigned to
//                    // or take reference of
//  hir_EK_PatternType, // PlaceType | StructPattern | IntPatternType |
//                      // RealPatternType | Splat | TODO
//
//  // Handle memory addresses
//  hir_EK_GetMemAddrFn,   // PlaceType($x) -> Ref(x)
//  hir_EK_DerefMemAddrFn, // Ref($x) -> PlaceType(x)
//
//  // Returns a place from a memory address
//  hir_EK_MutateMemAddrFn,
