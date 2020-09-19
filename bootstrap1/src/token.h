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
  tk_Loop,      // loop
  tk_Match,     // match
  tk_Ret,       // ret
  tk_Defer,     // defer
  tk_Def,       // def
  tk_Fn,        // fn
  tk_FnType,    // Fn
  tk_New,       // new
  tk_Lhs,       // lhs
  tk_Type,      // type
  tk_Mod,       // mod
  tk_Use,       // use
  tk_Has,       // has
  tk_Let,       // let
  tk_Self,      // self
  tk_SelfType,  // Self
  tk_NeverType, // Never
  // Literals and constants
  tk_String, // "string"
  tk_Real,   // 0.7
  tk_Int,    // 7
  // Math Operators
  tk_Add,  // +
  tk_Sub,  // -
  tk_Mul,  // *
  tk_Div,  // /
  // Set Operators
  tk_Union,         // ++
  tk_Difference,    // --
  tk_Intersection,  // &
  tk_SymDifference, // ^
  // Type operators
  tk_Product, // ,
  tk_Sum,     // |
  // Memory Operators
  tk_Ref,     // $
  tk_Deref,   // @
  // Logical Operators
  tk_And, // and
  tk_Or,  // or
  tk_Not, // not
  tk_Xor, // xor
  // Range Operators
  tk_Range,          // ..
  tk_RangeInclusive, // ..=
  tk_Ineq,           // ...
  tk_IneqInclusive,  // ...=
  // Comparison and Equality
  tk_CompEqual,        // ==
  tk_CompNotEqual,     // !=
  tk_CompLess,         // <
  tk_CompLessEqual,    // <=
  tk_CompGreater,      // >
  tk_CompGreaterEqual, // >=
  // Assignment
  tk_Record,     // .=
  tk_Define,     // :=
  tk_Assign,     // =
  // Arrows
  tk_Pipe,  // ->
  tk_Arrow, // =>
  // Other Miscellaneous Operator Things
  tk_ParenLeft,    // (
  tk_ParenRight,   // )
  tk_BracketLeft,  // [
  tk_BracketRight, // ]
  tk_BraceLeft,    // {
  tk_BraceRight,   // }
  tk_FieldAccess,  // .
  tk_Constrain,    // :
  tk_Underscore,   // _
  // Label
  tk_Label,        // 'label
  // Comments, and Attributes
  tk_Hashbang, // #! interpreter
  tk_Metadata, // #attribute() and ##comment
} tk_Kind;

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
      com_str content;
    } hashbangToken;
    struct {
      bool data;
    } boolToken;
    struct {
      com_str data;
      bool block;
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
