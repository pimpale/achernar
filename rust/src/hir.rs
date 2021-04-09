use super::ast;
use num_bigint::BigInt;
use num_rational::BigRational;
use std::alloc::Allocator;

pub enum ExprKind<'hir, 'ast, A:Allocator> {
  // An error when parsing
  None,
  This,
  // Loops until a scope is returned
  Loop(&'hir Expr<'hir, 'ast, A>),

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
  // Creates a Reference T -> &T
  RefFn,
  // Dereferences &T -> T
  DerefFn,
  // not
  NotFn,
  ComplementFn,
  AsyncFn,
  AwaitFn,
  NilSafeAssertFn,
  // Create Function
  Defun {
    pattern: &'hir Pat<'hir, 'ast, A>,
    result: &'hir Expr<'hir, 'ast, A>,
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

pub struct Expr<'hir, 'ast, A:Allocator> {
  pub source: Option<&'ast ast::Expr>,
  pub kind: ExprKind<'hir, 'ast, A>,
}

pub enum BindTargetKind<A:Allocator>{
  Identifier(Vec<u8, A>),
}

pub enum PatKind<'hir, 'ast, A:Allocator> {
  // An error when parsing
  None,
  // Bind all fields of the struct to their respective names
  Splat,
  // Match a single element of the
  Ignore,
  None,
  // Irrefutably matches a single element to new variable
  Bind {
    pattern: &'hir Pat<'hir, 'ast, A>,
    target: BindTargetKind<A>,
    ty: &'hir Expr<'hir, 'ast, A>
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
  // Apply like a pattern (means that any of the arguments can use pattern syntax)
  ActivePattern {
    function: &'hir Expr<'hir, 'ast, A>,
    param: &'hir Pat<'hir, 'ast, A>,
  },
  // Refutable pattern of a value
  Value(&'hir Pat<'hir, 'ast, A>),
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
  StructEntry {
    field: String,
    pattern: &'hir Pat<'hir, 'ast, A>,
  },
}

pub struct Pat<'hir, 'ast, A:Allocator> {
  pub source: Option<&'ast ast::Expr>,
  pub kind: PatKind<'hir, 'ast, A>,
}

//  // Math with integers
//  hir_EK_IntAddFn,
//  hir_EK_IntSubFn,
//  hir_EK_IntMulFn,
//  hir_EK_IntDivFn,
//  hir_EK_IntRemFn,
//  hir_EK_IntDivRemFn,
//  // Math with reals
//  hir_EK_RealAddFn,
//  hir_EK_RealSubFn,
//  hir_EK_RealMulFn,
//  hir_EK_RealDivFn,
//  hir_EK_RealRemFn,
//  // Conversion between integers and reals
//  hir_EK_RealRoundFn,
//  hir_EK_RNE, // round to nearest
//  hir_EK_RTZ, // round to zero
//  hir_EK_RDN, // round down
//  hir_EK_RUP, // round up
//  hir_EK_IntPromoteFn,
//  // Bit Vectors
//  hir_EK_SignedBitVecFn, // creates a bitvector from an integer
//  // Unsigned Operations
//  hir_EK_UnsignedBitVecAddFn,
//  hir_EK_UnsignedBitVecAddOverflowFn,
//  hir_EK_UnsignedBitVecSubFn,
//  hir_EK_UnsignedBitVecSubOverflowFn,
//  hir_EK_UnsignedBitVecMulFn,
//  hir_EK_UnsignedBitVecMulOverflowFn,
//  hir_EK_UnsignedBitVecDivFn,
//  hir_EK_UnsignedBitVecRemFn,
//  hir_EK_UnsignedBitVecDivRemFn,
//  hir_EK_UnsignedBitVecShrFn,
//  hir_EK_UnsignedBitVecShrOverflowFn,
//  hir_EK_UnsignedBitVecShlFn,
//  hir_EK_UnsignedBitVecShlOverflowFn,
//  hir_EK_UnsignedBitVecRolFn,
//  hir_EK_UnsignedBitVecRorFn,
//  hir_EK_UnsignedBitVecFn, // creates a bitvector from an integer
//  // Signed Operations
//  hir_EK_SignedBitVecAddFn,
//  hir_EK_SignedBitVecAddOverflowFn,
//  hir_EK_SignedBitVecSubFn,
//  hir_EK_SignedBitVecSubOverflowFn,
//  hir_EK_SignedBitVecMulFn,
//  hir_EK_SignedBitVecMulOverflowFn,
//  hir_EK_SignedBitVecDivFn,
//  hir_EK_SignedBitVecRemFn,
//  hir_EK_SignedBitVecDivRemFn,
//  hir_EK_SignedBitVecShrFn,
//  hir_EK_SignedBitVecShrOverflowFn,
//  hir_EK_SignedBitVecShlFn,
//  hir_EK_SignedBitVecShlOverflowFn,
//  hir_EK_SignedBitVecRolFn,
//  hir_EK_SignedBitVecRorFn,
//
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
