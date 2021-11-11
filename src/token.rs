use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;
use strum::AsRefStr;

#[derive(Debug, Clone, AsRefStr, PartialEq)]
pub enum TokenKind {
  // function, type, or variable
  Identifier(Vec<u8>),
  // Keywords
  Case,     // case
  Of,       // of
  Val,      // val
  StructOp, // struct
  EnumOp,   // enum
  Let,      // let
  In,       // in
  // Literals and constants
  Type,                                   // type
  NilTy,                                  // nil
  NeverTy,                                // never
  BoolTy,                                 // bool
  U8Ty,                                   // u8
  U16Ty,                                  // u16
  U32Ty,                                  // u32
  U64Ty,                                  // u64
  I8Ty,                                   // u8
  I16Ty,                                  // u16
  I32Ty,                                  // u32
  I64Ty,                                  // u64
  F32Ty,                                  // f32
  F64Ty,                                  // f64
  Lifetime(Vec<u8>),                      // 'lifetime
  Inf,                                    // inf
  Nan,                                    // nan
  Nil,                                    // ()
  Bool(bool),                             // true, false
  String { value: Vec<u8>, block: bool }, // "string"
  Int(BigInt),                            // 7
  Float(BigRational),                     // 0.7
  LifetimeTy,                             // lifetime
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
  Cons, // ,
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
  Assign,      // =
  PlusAssign,  // +=
  MinusAssign, // -=
  MulAssign,   // *=
  DivAssign,   // /=
  RemAssign,   // %=
  // Reference
  Ref,     // &
  UniqRef, // &!
  Deref,   // @
  // Arrows
  Pipe,    // |
  Compose, // >>
  Defun,   // ->
  // CaseOptions
  CaseOption, // ||
  // Other Miscellaneous Operator Things
  BindVar,      // $
  MutateVar,    // !
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
