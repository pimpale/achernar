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
  tk_Match,  // match
  tk_Ret,    // ret
  tk_Defer,  // defer
  tk_Let,    // let
  tk_FnType, // Fn
  tk_At,     // at
  tk_New,    // new
  // Literals and constants
  tk_Inf,       // inf
  tk_Nan,       // nan
  tk_Real,      // 0.7
  tk_NilType,   // Nil
  tk_NeverType, // Never
  tk_String,    // "string"
  tk_Int,       // 7
  // Math Operators
  tk_Plus,  // +
  tk_Minus, // -
  tk_Mul,   // *
  tk_Div,   // /
  tk_Rem,   // %
  // Set Operators
  tk_Union,         // ++
  tk_Difference,    // --
  tk_Intersection,  // ^
  tk_SymDifference, // !^
  // Type operators
  tk_Product, // ,
  tk_Sum,     // |
  // Memory Operators
  tk_Ref,   // &
  tk_Deref, // @
  // Logical Operators
  tk_And, // and
  tk_Or,  // or
  tk_Not, // not
  tk_Xor, // xor
  // Range Operators
  tk_Range,          // ..
  tk_RangeInclusive, // ..=
  // Comparison and Equality
  tk_CompEqual,        // ==
  tk_CompNotEqual,     // !=
  tk_CompLess,         // <
  tk_CompLessEqual,    // <=
  tk_CompGreater,      // >
  tk_CompGreaterEqual, // >=
  // Assignment
  tk_Assign, // =
  // Arrows
  tk_Pipe,  // ->
  tk_Arrow, // =>
  // Other Miscellaneous Operator Things
  tk_Bind,         // $
  tk_BindIgnore,   // $_
  tk_ParenLeft,    // (
  tk_ParenRight,   // )
  tk_BracketLeft,  // [
  tk_BracketRight, // ]
  tk_BraceLeft,    // {
  tk_BraceRight,   // }
  tk_FieldAccess,  // .
  tk_Constrain,    // :
  // Label
  tk_Label, // 'label
  // Comments, and Attributes
  tk_Metadata, // #attribute and #! comment
} tk_Kind;

typedef enum { tk_SLK_DoubleQuote, tk_SLK_Block } tk_StringLiteralKind;

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
    } identifierToken;
    struct {
      com_str data;
    } macroToken;
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
