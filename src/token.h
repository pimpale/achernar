#ifndef TOKEN_H
#define TOKEN_H

#include "com_bigdecimal.h"
#include "com_bigint.h"
#include "com_define.h"
#include "com_loc.h"
#include "com_str.h"

typedef enum {
  // These are not tokens, and do not contain token data
  tk_Eof,
  // if it's tk none you can't assume anything
  tk_None,
  // function, type, or variable
  tk_Identifier,
  // Keywords
  tk_Loop,   // loop
  tk_Of,     // of
  tk_Pat,    // pat
  tk_Ret,    // ret
  tk_Defer,  // defer
  tk_Case,   // case
  tk_At,     // at
  tk_Where,  // where
  tk_Dyn,    // dyn
  tk_Impl,   // impl
  tk_Self,   // this
  // Literals and constants
  tk_Inf,       // inf
  tk_Nan,       // nan
  tk_Real,      // 0.7
  tk_String,    // "string"
  tk_Int,       // 7
  tk_Void,      // ()
  tk_VoidType,  // void
  tk_NeverType, // never
  // Math Operators
  tk_Add, // +
  tk_Sub, // -
  tk_Mul, // *
  tk_Div, // /
  tk_Rem, // %
  tk_Pow, // ^
  // Boolean Operators
  tk_And, // and
  tk_Or,  // or
  tk_Xor, // xor
  // Set Operators
  tk_Union,        // \/ union
  tk_Intersection, // /\ intersection
  tk_Append,       // ++ append
  tk_Difference,   // -- difference
  tk_In,           // in membership
  // Type operators
  tk_Cons, // ,
  tk_Sum,  // |
  // Range Operators
  tk_Range,          // ..
  tk_RangeInclusive, // ..=
  // Comparison and Equality
  tk_CompEqual,        // ==
  tk_CompNotEqual,     // /=
  tk_CompLess,         // <
  tk_CompLessEqual,    // <=
  tk_CompGreater,      // >
  tk_CompGreaterEqual, // >=
  // Assignment
  tk_Assign, // =
  // labels
  tk_Label, // 'x
  // Arrows
  tk_PipeForward,  // |>
  tk_PipeBackward, // <|
  tk_Compose,      // >>
  tk_Arrow,        // ->
  // CaseOptions
  tk_CaseOption, // ||
  // Other Miscellaneous Operator Things
  tk_Bind,         // $
  tk_Ignore,       // $_
  tk_Splat,        // $*
  tk_ParenLeft,    // (
  tk_ParenRight,   // )
  tk_BracketLeft,  // [
  tk_BracketRight, // ]
  tk_BraceLeft,    // {
  tk_BraceRight,   // }
  tk_Constrain,    // :
  tk_ModuleAccess, // ::
  tk_RevApply,     // .
  tk_Sequence,     // ;
  // Comments, and Attributes
  tk_Metadata, // #!attribute and # comment
} tk_Kind;

typedef enum { tk_SLK_DoubleQuote, tk_SLK_Block } tk_StringLiteralKind;
typedef enum { tk_IK_Literal, tk_IK_Strop } tk_IdentifierKind;

typedef struct Token_s {
  // The type of this token
  tk_Kind kind;
  // position in the file
  com_loc_Span span;
  // This points to
  // null terminated string in case of identifier, tk_StringLiteral,
  // tk_Comment, tk_Documentation, or tk_Annotation u64 in case of
  // tk_IntLiteral double double in case of tk_FloatLiteral Otherwise must
  // be null
  union {
    struct {
      com_str data;
      tk_IdentifierKind kind;
    } identifierToken;
    struct {
      com_str data;
    } labelToken;
    struct {
      bool significant;
      com_str content;
    } metadataToken;
    struct {
      bool data;
    } boolToken;
    struct {
      com_str data;
      tk_StringLiteralKind kind;
    } stringToken;
    struct {
      com_bigint data;
    } intToken;
    struct {
      com_bigdecimal data;
    } realToken;
  };
} Token;

com_str tk_strKind(tk_Kind val);

#endif
