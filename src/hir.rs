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
  // constructs a new compound ty
  Struct(Vec<(&'ast Vec<u8>, (&'ast ast::Expr, ValExpr<'hir, 'ast, HA>)), HA>),

  // use place
  Use(&'hir PlaceExpr<'hir, 'ast>),

  // Annotate the value with the type
  Annotate {
    val_expr: &'hir ValExpr<'hir, 'ast, HA>,
    ty_expr: &'hir ValExpr<'hir, 'ast, HA>,
  },
  // Switches on a pattern
  CaseOf {
    expr: &'hir PlaceExpr<'hir, 'ast>,
    case_options: Vec<(RefutablePatExpr<'hir, 'ast, HA>, ValExpr<'hir, 'ast, HA>), HA>,
    source: CaseSource,
  },

  // Literals
  Unit,
  Bool(bool),
  Char(u32),
  Int(&'ast BigInt),
  Float(&'ast BigRational),

  // Type stuff
  // creates a pub struct from an ad hoc compound object
  StructTy(Vec<(&'ast Vec<u8>, (&'ast ast::Expr, ValExpr<'hir, 'ast, HA>)), HA>),
  // creates a disjoint union from an ad hoc compound object
  EnumTy(Vec<(&'ast Vec<u8>, (&'ast ast::Expr, ValExpr<'hir, 'ast, HA>)), HA>),
  // creates a tuple
  Pair {
    fst: &'hir ValExpr<'hir, 'ast, HA>,
    snd: &'hir ValExpr<'hir, 'ast, HA>,
  },
  // Create Function
  Lam {
    arg: &'hir IrrefutablePatExpr<'hir, 'ast, HA>,
    body: &'hir ValExpr<'hir, 'ast, HA>,
  },
  LamTy {
    // a pattern the type of the argument
    arg_ty: &'hir RefutablePatExpr<'hir, 'ast, HA>,
    // a function from arg_ty -> Type yielding the output type
    body_dep_ty: &'hir ValExpr<'hir, 'ast, HA>,
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
  // Assign value to place
  Assign {
    pat: &'hir IrrefutablePatExpr<'hir, 'ast, HA>,
    val: &'hir ValExpr<'hir, 'ast, HA>,
  },
}

#[derive(Debug)]
pub struct ValExpr<'hir, 'ast, HA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: ValExprKind<'hir, 'ast, HA>,
}

#[derive(Debug, Clone, Copy)]
pub enum PlaceExprOpKind {
  Ref,
  UniqRef,
  Deref,
}

#[derive(Debug)]
pub enum PlaceExprKind<'hir, 'ast> {
  Error,
  // creates
  StructField {
    root: &'hir PlaceExpr<'hir, 'ast>,
    field_source: &'ast ast::Expr,
    field: &'ast Vec<u8>,
  },
  Op(&'hir PlaceExpr<'hir, 'ast>, PlaceExprOpKind),
  // A reference to a previously defined variable
  Var(&'ast [u8]),
  Builtin(Builtin),
}

#[derive(Debug)]
pub struct PlaceExpr<'hir, 'ast> {
  pub source: &'ast ast::Expr,
  pub kind: PlaceExprKind<'hir, 'ast>,
}

// A pattern that may not be rejected, and can bind variables
#[derive(Debug)]
pub enum IrrefutablePatExprKind<'hir, 'ast, HA: Allocator + Clone> {
  // An error when parsing
  Error,
  // Always matches with Unit, but doesn't bind anything
  Unit,
  // Irrefutably matches a single element to new variable
  BindVariable(&'ast Vec<u8>),
  // destructure a tuple
  Pair {
    fst: &'hir IrrefutablePatExpr<'hir, 'ast, HA>,
    snd: &'hir IrrefutablePatExpr<'hir, 'ast, HA>,
  },
  // Depub structures a field of a pub struct object
  Struct(
    Vec<
      (
        &'ast Vec<u8>,
        (&'ast ast::Expr, IrrefutablePatExpr<'hir, 'ast, HA>),
      ),
      HA,
    >,
  ),
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
  // Depub structures a field of a pub struct object
  Struct(
    Vec<
      (
        &'ast ast::Expr,
        (&'ast Vec<u8>, RefutablePatExpr<'hir, 'ast, HA>),
      ),
      HA,
    >,
  ),
  // Evaluates the second pattern iff the first pattern matches, matches if both are true
  // none of these may bind any variables
  And {
    fst: &'hir IrrefutablePatExpr<'hir, 'ast, HA>,
    snd: &'hir ValPatExpr<'hir, 'ast, HA>,
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
    fst: &'hir ValExpr<'hir, 'ast, HA>,
    snd: &'hir ValExpr<'hir, 'ast, HA>,
  },
  // Selects a function and calls it with the scrutinee.
  // The result is then refutably matched with the argument provided
  // Also can be used to drop values during matching
  // Example: Array($a, $b, $c) = someFunc();
  // Example: _ () = someFunc();
  Constructor {
    fun: &'hir ValExpr<'hir, 'ast, HA>,
    arg: &'hir ValPatExpr<'hir, 'ast, HA>,
  },
  // destructure a tuple
  Pair {
    fst: &'hir ValPatExpr<'hir, 'ast, HA>,
    snd: &'hir ValPatExpr<'hir, 'ast, HA>,
  },
  // Destructures a field of a struct object
  Struct(Vec<(&'ast Vec<u8>, (&'ast ast::Expr, ValPatExpr<'hir, 'ast, HA>)), HA>),
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
  // TODO: this is not really correct, we want to have a pattern for int, bool, string, and tuple only
  // everything else has to be done via constructor functions
  Value(&'hir ValExpr<'hir, 'ast, HA>),
}

#[derive(Debug)]
pub struct ValPatExpr<'hir, 'ast, HA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: ValPatExprKind<'hir, 'ast, HA>,
}

#[derive(Debug)]
pub enum IntOp {
  Add,         // u -> u -> u
  AddOverflow, // u -> u -> (u, u)
  Sub,         // u -> u -> u
  SubOverflow, // u -> u -> (u, u)
  Mul,         // u -> u -> u
  MulOverflow, // u -> u -> (u, u)
  Div,         // u -> u -> u
  Rem,         // u -> u -> u
  DivRem,      // u -> u -> (u, u)
  ShlL,        // u -> u -> u
  ShrL,        // u -> u -> u
  ShrA,        // u -> u -> u
  Rol,         // u -> u -> u
  Ror,         // u -> u -> u
  And,         // u -> u -> u
  Or,          // u -> u -> u
  Xor,         // u -> u -> u
  Inv,         // u -> u
  Neg,         // u -> u
}

#[derive(Debug)]
pub enum FloatOp {
  Add,    // f -> f -> f
  Sub,    // f -> f -> f
  Mul,    // f -> f -> f
  Div,    // f -> f -> f
  Rem,    // f -> f -> f
  DivRem, // f -> f -> (f, f)
  Neg,    // f -> f
}

#[derive(Debug)]
pub enum RoundingMode {
  RNE, // round to nearest even
  RTZ, // round to zero
  RDN, // round down
  RUP, // round up
}

#[derive(Debug)]
pub struct IntTy {
  pub signed: bool,
  pub size: u8,
}

#[derive(Debug)]
pub struct FloatTy {
  pub size: u8,
}

#[derive(Debug)]
pub enum Builtin {
  UnitTy,
  NeverTy,
  BoolTy,
  // Math with bools
  BoolNot,
  // Ints
  IntTy(IntTy),
  // Floats
  FloatTy(FloatTy),
  // Math with ints
  IntOp { ty: IntTy, op: IntOp },
  // Math with floats
  FloatOp { ty: FloatTy, op: FloatOp },
  // Convert one kind of type to another
  ConvIntIntOp { src: IntTy, dest: IntTy },
  ConvIntFloatOp { src: IntTy, dest: FloatTy },
  ConvFloatIntOp { src: FloatTy, dest: IntTy },
  ConvFloatFloatOp { src: FloatTy, dest: FloatTy },
  // Handle Memory addresses
  Ref,
  Deref,
  // Forget memory
  Forget,
}
