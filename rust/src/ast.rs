use lsp_types::Range;
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub enum IdentifierData {
  None,
  Identifier(String),
}

#[derive(Serialize, Deserialize)]
pub struct Identifier {
  range: Range,
  data: IdentifierData,
}

#[derive(Serialize, Deserialize)]
pub struct Metadata {
  range: Range,
  significant: bool,
  data: String,
}

#[derive(Serialize, Deserialize)]
pub enum LabelData {
  None,
  Label(String),
}

#[derive(Serialize, Deserialize)]
pub struct Label {
  range: Range,
  data: LabelData,
}

#[derive(Serialize, Deserialize)]
pub enum BinaryOpKind {
  None,
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
  // Memory Referencing
  Ref,
  Deref,
  // Booleans
  Not,
  And,
  Or,
  // Comparison
  CompEqual,
  CompNotEqual,
  CompLess,
  CompLessEqual,
  CompGreater,
  CompGreaterEqual,
  // Set Operations
  Union,
  Intersection,
  Difference,
  In,
  // Type Manipulation
  Cons,
  Sum,
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

#[derive(Serialize, Deserialize)]
pub enum ExprData {
  // An error when parsing
  None,
  // Literal for void
  Nil,
  // Literal for void type
  NilType,
  // Literal for never type
  NeverType,
  // Literal for an integer number
  Int {
    positive: bool,
    values: Vec<u32>,
  },
  // Literal for a boolean
  Bool {
    value: bool,
  },
  // Literal for a real (floating point) number
  Real {
    positive: bool,
    numerator: Vec<u32>,
    denominator: Vec<u32>,
  },
  // A string literal
  String {
    value: String,
  },
  // Loops until a scope is returned
  Loop {
    body: Box<Expr>,
  },
  // embed a computed value in a pattern
  Val {
    value: Box<Expr>,
  },
  // quote a pattern
  Pat {
    value: Box<Expr>,
  },
  // Wraps a term in a label that can be deferred or returned from
  Label {
    label: Label,
    body: Box<Expr>,
  },
  // Defer until label
  Defer {
    label: Label,
    body: Box<Expr>,
  },
  // Returns from a scope with a value
  Ret {
    label: Label,
    body: Box<Expr>,
  },
  // Constructs a new compound type
  Struct {
    value: Box<Expr>,
  },
  // Binary operation
  BinaryOp {
    op: BinaryOpKind,
    left_operand: Box<Expr>,
    right_operand: Box<Expr>,
  },
  // if then else expression
  IfThen {
    expr: Box<Expr>,
    then_expr: Box<Expr>,
    else_expr: Box<Expr>,
  },
  // Matches an expression to the first matching pattern and destructures it
  CaseOf {
    expr: Box<Expr>,
    cases: Box<Expr>,
  },
  // Introduces new scope and label
  Group {
    value: Box<Expr>,
  },
  // A reference to a previously defined variable
  Reference {
    identifier: Identifier,
  },
  // (PATTERN ONLY) ignores a single element
  BindIgnore,
  // (PATTERN ONLY) Automagically deconstructs and binds a struct
  BindSplat,
  // (PATTERN ONLY) matches a single element to new variable
  Bind {
    identifier: Identifier,
  },
}

#[derive(Serialize, Deserialize)]
pub struct Expr {
  range: Range,
  metadata: Vec<Metadata>,
  data: ExprData,
}
