use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;
use strum::AsRefStr;

#[derive(Debug, Clone, AsRefStr, PartialEq)]
pub enum TokenKind {
  // function, type, or variable
  Identifier(Vec<u8>),
  // Keywords
  Loop,   // loop
  Ret,    // ret
  Defer,  // defer
  Case,   // case
  Of,     // of
  As,     // as
  Val,    // val
  Struct, // struct
  Enum,   // enum
  New,    // new
  // Literals and constants
  Inf,                                    // inf
  Nan,                                    // nan
  Nil,                                    // nil
  Bool(bool),                             // true, false
  String { value: Vec<u8>, block: bool }, // "string"
  Int(BigInt),                            // 7
  Float(BigRational),                     // 0.7
  // Math Operators
  Plus,  // +
  Minus, // -
  Mul,   // *
  Div,   // /
  Rem,   // %
  // Boolean Operators
  And, // and
  Or,  // or
  // Error manipulation
  ReturnOnError, // ?
  // Type operators
  Cons,     // ,
  SuchThat, // |
  // Range Operators
  Range,          // ..
  RangeInclusive, // ..=
  // Comparison and Equality
  Equal,        // ==
  NotEqual,     // !=
  Less,         // <
  LessEqual,    // <=
  Greater,      // >
  GreaterEqual, // >=
  // Assignment
  Assign, // =
  // Reference
  Ref,    // &
  MutRef, // &*
  Deref,  // @
  // labels
  Label(Vec<u8>), // 'x
  // Arrows
  PipeForward,  // |>
  PipeBackward, // <|
  Compose,      // >>
  Defun,        // ->
  // CaseOptions
  CaseOption, // ||
  // Other Miscellaneous Operator Things
  NoInfer,      // !
  Bind,         // $
  ParenLeft,    // (
  ParenRight,   // )
  BracketLeft,  // [
  BracketRight, // ]
  BraceLeft,    // {
  BraceRight,   // }
  Constrain,    // :
  RevConstrain, // ::
  ModuleAccess, // .
  Sequence,     // ;
  // Comments, and Attributes
  Metadata { value: Vec<u8>, significant: bool }, // #!attribute and # comment
}

#[derive(Debug, Clone)]
pub struct Token {
  pub kind: Option<TokenKind>,
  pub range: Range,
}

impl Token {
  pub fn new(kind: TokenKind, range: Range) -> Self {
    Token {
      kind: Some(kind),
      range,
    }
  }
}
