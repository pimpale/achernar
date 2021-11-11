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
  Pipe,
  // Function composition
  Compose,
  // Math
  Add,
  Sub,
  Mul,
  Div,
  Rem,
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
  // Type Manipulation
  Cons,
  // Range
  Range,
  RangeInclusive,
  // Assign
  Assign,
  PlusAssign,
  MinusAssign,
  MulAssign,
  DivAssign,
  RemAssign,
  // Sequence
  Sequence,
  // Module Access
  ModuleAccess,
}

// none of these are overloadable "unary operators" in the traditional sense,
// all of them are syntax
#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum UnaryOpKind {
  // Memory Referencing
  Ref,
  UniqRef,
  Deref,
  // Optional Manipulation
  ReturnOnError,
  // Compound type manipulation
  Struct,
  Enum,
  // (PATTERN ONLY) computes a value in a pattern
  Val,
  // (PATTERN ONLY) matches a single element to new variable
  Bind,
}

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum ExprKind {
  // An error when parsing
  Error,
  // Literals
  Nil,
  Int(BigInt),
  Bool(bool),
  Float(BigRational),
  Type(Option<BigInt>),
  String {
    value: Vec<u8>,
    block: bool,
  },
  // a lifetime literal
  Lifetime(Vec<u8>),
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
  // A reference to a previously defined variable
  Identifier(Vec<u8>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Expr {
  pub range: Range,
  pub metadata: Vec<Metadata>,
  pub kind: ExprKind,
}
