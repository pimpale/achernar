use super::span::Span;
use num_bigint::BigInt;

pub enum TokenData {
  // These are not tokens, and do not contain token data
  Eof,
  // if it's tk none you can't assume anything
  None,
  // function, type, or variable
  Identifier(String),
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
  True,                                  // true
  False,                                 // false
  Bool(bool),                                  // bool
  Real,                                  // 0.7
  String { value: String, block: bool }, // "string"
  Int(BigInt),                           // 7
  Nil,                                   // ()
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
  // labels
  Label(String), // 'x
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
  ModuleAccess, // ::
  RevApply,     // .
  Sequence,     // ;
  // Comments, and Attributes
  Metadata, // #!attribute and # comment
}

pub struct Token {
  span: Span,
  data: TokenData,
}
