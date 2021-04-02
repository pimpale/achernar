use lsp_types::Range;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Identifier {
  pub range: Range,
  pub value: Vec<u8>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Metadata {
  pub range: Range,
  pub significant: bool,
  pub value: Vec<u8>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Label {
  pub range: Range,
  pub value: Vec<u8>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum BinaryOpKind {
  // Type coercion
  Constrain,
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
  Negate,
  Posit,
  // Booleans
  And,
  Or,
  // Nil Manipulation
  NilCoalesce,
  NilSafeRevApply,
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
  Append, // ++ append
  // Type Manipulation
  Both,
  Either,
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

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum UnaryOpKind {
  // Memory Referencing
  Ref,
  Deref,
  // Boolean
  Not,
  // Set Operations
  Complement,
  // Nil Manipulation
  NilSafeAssert,
  // Async operators
  Async,
  Await,
  // syntactic constructs
  Loop,
  Val,
  Pat,
  Bind,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum ExprData {
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
  // Literal for real type
  RealType,
  // Literal for an integer number
  Int {
    positive: bool,
    values: Vec<u32>,
  },
  // Literal for a boolean
  Bool(bool),
  // Literal for a real (floating point) number
  Real {
    positive: bool,
    numerator: Vec<u32>,
    denominator: Vec<u32>,
  },
  // A string literal
  String {
    value: Vec<u8>,
    block: bool,
  },
  // Wraps a term in a label that can be deferred or returned from
  Label {
    label: Label,
    body: Box<Expr>,
  },
  // Defer until label
  Defer {
    label: Option<Label>,
    body: Box<Expr>,
  },
  // Returns from a scope with a value
  Ret {
    label: Option<Label>,
    body: Box<Expr>,
  },
  // Constructs a new compound type
  Struct(Box<Expr>),
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
  // if then else expression
  IfThen {
    cond_expr: Box<Expr>,
    then_expr: Box<Expr>,
    else_expr: Box<Expr>,
  },
  // Matches an expression to the first matching pattern and destructures it
  CaseOf {
    expr: Box<Expr>,
    cases: Box<Expr>,
  },
  // Introduces new scope and label
  Group(Box<Expr>),
  // A reference to a previously defined variable
  Reference(Option<Identifier>),
  // (PATTERN ONLY) ignores a single element
  BindIgnore,
  // (PATTERN ONLY) Automagically deconstructs and binds a struct
  BindSplat,
  // (PATTERN ONLY) matches a single element to new variable
  Bind(Option<Identifier>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Expr {
  pub range: Range,
  pub metadata: Vec<Metadata>,
  pub kind: ExprData,
}
