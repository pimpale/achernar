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

#[derive(Serialize, Deserialize, Clone, Debug, AsRefStr)]
pub enum ExprKind {
  // An error when parsing
  Error,
  Nil,
  Int(BigInt),
  Bool(bool),
  Float(BigRational),
  Type(Option<BigInt>),
  // a lifetime literal
  Lifetime(Vec<u8>),
  String {
    value: Vec<u8>,
    block: bool,
  },
  // Constructs a new compound type
  StructLiteral(Box<Expr>),
  // Binary operation
  BinaryOp {
    op: BinaryOpKind,
    left_operand: Box<Expr>,
    right_operand: Box<Expr>,
  },
  // these fields can only be used with module access
  // they signify a memory ref, uniqref, or deref
  Ref,
  UniqRef,
  Deref,
  // Matches an expression to the first matching pattern and destructures it
  CaseOf {
    expr: Box<Expr>,
    cases: Box<Expr>,
  },
  // Introduces new scope and label
  Group(Box<Expr>),
  // A reference to a previously defined variable
  Identifier(Vec<u8>),
  // Builtin thing
  Builtin(Vec<u8>),
  // struct  and enumify
  Struct(Box<Expr>),
  Enum(Box<Expr>),

  // (PATTERN ONLY) computes a value in a pattern
  Val(Box<Expr>),

  // (PATTERN ONLY) matches a single element to new variable
  Bind(Vec<u8>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Expr {
  pub range: Range,
  pub metadata: Vec<Metadata>,
  pub kind: ExprKind,
}
