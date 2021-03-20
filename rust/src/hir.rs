use super::ast;
use num_bigint::BigInt;

enum ExprData<'ast> {
  // An error when parsing
  None,
  // Loops until a scope is returned
  Loop {
    body: Box<Expr<'ast>>,
  },

  // applies a function
  Apply {
    function: Box<Expr<'ast>>,
    param: Box<Expr<'ast>>,
  },
  // Wraps a term in a label that can be deferred or returned from
  Label {
    label: String,
    scope: Box<Expr<'ast>>,
  },
  // Wraps a term in a label that can be deferred or returned from
  Defer {
    label: String,
    deferred: Box<Expr<'ast>>,
  },
  // Returns from a scope with a value
  Ret {
    label: String,
    value: Box<Expr<'ast>>,
  },
  // Constructs a new compound type
  StructLiteral {
    body: Box<Expr<'ast>>,
  },
  // Accessing the module of a module object
  StructAccess {
    root: Box<Expr<'ast>>,
    field: String,
  },
  // A reference to a previously defined variable
  Reference {
    identifier: String,
  },
  // Switches on a pattern
  CaseOf {
    root: Box<Expr<'ast>>,
    cases: Vec<Expr<'ast>>,
  },
  // Switches on a pattern
  CaseOption {
    pattern: Box<Pat<'ast>>,
    result: Box<Expr<'ast>>,
  },
  // Quotes pattern
  Pat {
    pat: Box<Pat<'ast>>,
  },

  // Literals for values
  Nil,
  NilType,
  Never,
  NeverType,
  Bool {
    value: bool,
  },
  BoolType,
  // actually bigint
  Int {
    value: BigInt,
  },
  IntType,

  // Type stuff
  // creates a struct from an ad hoc compound object
  StructFn,
  // creates a disjoint union from an ad hoc compound object
  EnumFn,
  // Creates a function constructing the compound type provided
  NewFn,
  // creates a tuple
  ConsFn,

  // Create Function
  Defun {
    pattern: Box<Pat<'ast>>,
    result: Box<Expr<'ast>>,
  },

  // Sequence
  // () -> () -> ()
  Sequence,

  // Assign value to place
  // Pattern($x) -> x -> void
  Assign {
    target: Box<Pat<'ast>>,
    value: Box<Expr<'ast>>,
  },
}

struct Expr<'ast> {
  source: &'ast ast::Expr,
  data: ExprData<'ast>,
}

enum PatData<'ast> {
  // An error when parsing
  None,
  // Irrefutably matches a single element to new variable
  Bind {
    pattern: Box<Pat<'ast>>,
    identifier: String,
  },
  // Irrefutably matches, and ignores result
  BindIgnore,
  // automagically deconstructs a struct
  BindSplat,
  // constrains the type of a pattern expression by a type value
  Constrain {
    pattern: Box<Pat<'ast>>,
    r#type: Box<Expr<'ast>>,
  },
  // Apply like a pattern (means that any of the arguments can use pattern syntax)
  Apply {
    function: Box<Expr<'ast>>,
    param: Box<Pat<'ast>>,
  },
  // Refutable pattern of a value
  Expr,
  // Evaluates the second pattern iff the first pattern matches, matches if both are true
  And,
  // Evaluates the second pattern iff the first pattern doesn't match, matches if at least one is true
  Or,
  // Destructures a field of a struct object
  StructEntry {
    field: String,
    pattern: Box<Pat<'ast>>,
  },
}

struct Pat<'ast> {
  source: &'ast ast::Expr,
  data: PatData<'ast>,
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
//  hir_EK_PlaceType, // this is the type of a valid place that may be assigned to
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



