use lsp_types::Range;
use num_bigint::BigInt;

pub enum TokenKind {
  // These are not tokens, and do not contain token data
  Eof,
  // if it's tk none you can't assume anything
  None,
  // function, type, or variable
  Identifier(Vec<u8>),
  // Keywords
  If,     // if
  Then,   // then
  Else,   // else
  Loop,   // loop
  Of,     // of
  Ret,    // ret
  Defer,  // defer
  Case,   // case
  As,     // as
  Where,  // where
  Dyn,    // dyn
  Impl,   // impl
  This,   // this
  Val,    // val
  Pat,    // pat
  Async,  // async
  Await,  // await
  Import, // import
  // Literals and constants
  Inf,                                   // inf
  Nan,                                   // nan
  Bool(bool),                            // true, false
  BoolType,                              // bool
  RealType,                              // 0.7
  String { value: Vec<u8>, block: bool }, // "string"
  Int(BigInt),                           // 7
  NilType,                               // nil
  NeverType,                             // never
  // Math Operators
  Add, // +
  Sub, // -
  Mul, // *
  Div, // /
  Rem, // %
  Pow, // ^
  // Boolean Operators
  And, // and
  Or,  // or
  // Set Operators
  Union,        // \/ union
  Intersection, // /\ intersection
  Append,       // ++ append
  Difference,   // -- difference
  In,           // in membership
  // Type operators
  Cons, // ,
  Sum,  // |
  // Range Operators
  Range,          // ..
  RangeInclusive, // ..=
  // Comparison and Equality
  CompEqual,        // ==
  CompNotEqual,     // /=
  CompLess,         // <
  CompLessEqual,    // <=
  CompGreater,      // >
  CompGreaterEqual, // >=
  // Assignment
  Assign, // =
  // Reference
  Ref,
  Deref,
  // labels
  Label(Vec<u8>), // 'x
  // Arrows
  PipeForward,  // |>
  PipeBackward, // <|
  Compose,      // >>
  Arrow,        // ->
  // CaseOptions
  CaseOption, // ||
  // Other Miscellaneous Operator Things
  Bind,         // $
  Ignore,       // $_
  Splat,        // $*
  ParenLeft,    // (
  ParenRight,   // )
  BracketLeft,  // [
  BracketRight, // ]
  BraceLeft,    // {
  BraceRight,   // }
  Constrain,    // :
  FieldAccess,  // ::
  RevApply,     // .
  Sequence,     // ;
  // Comments, and Attributes
  Metadata, // #!attribute and # comment
}

pub struct Token {
  kind: TokenKind,
  range: Range,
}

impl Token {
  pub fn new(kind: TokenKind, range: Range) -> Self {
    Token { kind, range }
  }
}
