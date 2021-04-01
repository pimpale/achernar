use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;

#[derive(Debug, Clone)]
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
  Inf,                                    // inf
  Nan,                                    // nan
  Bool(bool),                             // true, false
  String { value: Vec<u8>, block: bool }, // "string"
  Int(BigInt),                            // 7
  Real(BigRational),                      // 0.7
  NilType,                                // nil
  NeverType,                              // never
  BoolType,                               // bool
  IntType,                                // int
  RealType,                               // real
  // Math Operators
  Plus, // +
  Minus, // -
  Mul, // *
  Div, // /
  Rem, // %
  Pow, // **
  Negate, // -- difference
  // Boolean Operators
  Not, // not
  And, // and
  Or,  // or
  // Set Operators (also work on bitvectors)
  Complement,          // ~
  RelativeComplement,  // \
  Union,               // \/
  Intersection,        // /\
  SymmetricDifference, // ^
  In,                  // in
  // List operators
  Append,     // ++ append
  // Type operators
  Both,   // ,
  Either, // |
  // Range Operators
  Range,          // ..
  RangeInclusive, // ..=
  // Comparison and Equality
  Equal,        // ==
  NotEqual,     // /=
  Less,         // <
  LessEqual,    // <=
  Greater,      // >
  GreaterEqual, // >=
  // Assignment
  Assign, // =
  // Reference
  Ref,   // &
  Deref, // @
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
  Metadata { value: Vec<u8>, significant: bool }, // #!attribute and # comment
}

#[derive(Debug, Clone)]
pub struct Token {
  pub kind: TokenKind,
  pub range: Range,
}

impl Token {
  pub fn new(kind: TokenKind, range: Range) -> Self {
    Token { kind, range }
  }
}
