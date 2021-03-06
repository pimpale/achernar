use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;
use serde::{Deserialize, Serialize};
use strum::AsRefStr;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Metadata {
  pub range: Range,
  pub significant: bool,
  pub value: Vec<u8>,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum BinaryOpKind {
  // Type coercion
  Constrain,
  RevConstrain,
  // Function definition
  Defun,
  // CaseOption
  CaseOption,
  // Function call
  Apply,
  RevApply,
  // Function composition
  Compose,
  // Function Piping
  PipeForward,
  PipeBackward,
  // Math
  Add,
  Sub,
  Mul,
  Div,
  Rem,
  Pow,
  // Booleans
  And,
  Or,
  // Comparison
  Equal,
  NotEqual,
  Less,
  LessEqual,
  Greater,
  GreaterEqual,
  // Set Operations
  RelativeComplement,
  Union,
  Intersection,
  SymmetricDifference,
  In,
  // List Operations
  Append,
  // Type Manipulation
  Cons,
  SuchThat,
  // Range
  Range,
  RangeInclusive,
  // Assign
  Assign,
  // Sequence
  Sequence,
  // Pattern rename
  As,
  // Module Access
  ModuleAccess,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum UnaryOpKind {
  // Function noninference
  NoInfer,
  // Math
  Negate,
  Posit,
  // Memory Referencing
  Ref,
  Deref,
  // Boolean
  Not,
  // Set Operations
  Complement,
  // Optional Manipulation
  ReturnOnError,
  // Compound type manipulation
  Struct,
  Enum,
  New,
  // syntactic constructs
  Loop,
  Val,
  Pat,
  // (PATTERN ONLY) matches a single element to new variable
  Bind,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum ExprKind {
  // An error when parsing
  None,
  // Representing the current
  This,
  // Literal for void
  Nil,
  // Literal for void type
  NilType,
  // Literal for never type
  NeverType,
  // Literal for bool type
  BoolType,
  // Literal for int type
  IntType,
  // Literal for rational type
  RationalType,
  // Literal for an integer number
  Int(BigInt),
  // Literal for a boolean
  Bool(bool),
  // Literal for a rational number
  Rational(BigRational),
  // A string literal
  String {
    value: Vec<u8>,
    block: bool,
  },
  // Wraps a term in a label that can be deferred or returned from
  Label {
    label: Vec<u8>,
    body: Box<Expr>,
  },
  // Defer until label
  Defer {
    label: Option<Vec<u8>>,
    body: Box<Expr>,
  },
  // Returns from a scope with a value
  Ret {
    label: Option<Vec<u8>>,
    body: Box<Expr>,
  },
  // Constructs a new compound type
  StructLiteral(Box<Expr>),
  // Binary operation
  BinaryOp {
    op: BinaryOpKind,
    left_operand: Box<Expr>,
    right_operand: Box<Expr>,
  },
  // Unary operation
  UnaryOp {
    op: UnaryOpKind,
    operand: Box<Expr>,
  },
  // Matches an expression to the first matching pattern and destructures it
  CaseOf {
    expr: Box<Expr>,
    cases: Box<Expr>,
  },
  // Introduces new scope and label
  Group(Box<Expr>),
  // Provides an argument to infer
  InferArg(Box<Expr>),
  // A reference to a previously defined variable
  Reference(Vec<u8>),
  // Hole, helps with deducing valid types
  Hole,
  // (PATTERN ONLY) Automagically deconstructs and binds a struct
  BindSplat,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Expr {
  pub range: Range,
  pub metadata: Vec<Metadata>,
  pub kind: ExprKind,
}
