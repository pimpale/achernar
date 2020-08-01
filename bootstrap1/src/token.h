#ifndef TOKEN_H
#define TOKEN_H

#include "com_define.h"
#include "com_streamposition.h"
#include "com_str.h"

typedef enum {
  // These are not tokens, and do not contain token data
  tk_Eof,
  tk_None,
  // function, type, or variable
  tk_Identifier,
  // Keywords
  tk_Never,     // unreachable type for when a function does not return
  tk_Loop,      // loop
  tk_Match,     // match
  tk_Val,       // val
  tk_Return,    // return
  tk_Defer,     // defer
  tk_Fn,        // fn
  tk_Pat,       // pat
  tk_As,        // as
  tk_Struct,    // struct
  tk_Enum,      // enum
  tk_Type,      // type
  tk_Namespace, // namespace
  tk_Use,       // use
  // Literals and constants
  tk_Nil,    // nil
  tk_Bool,   // true
  tk_String, // "string"
  tk_Char,   // 'a'
  tk_Float,  // 0.7
  tk_Int,    // 7
  // Math Operators
  // Unary
  tk_Add,    // +
  tk_Sub,    // -
  tk_Mul,    // *
  tk_Div,    // /
  tk_Mod,    // %
  // Logical Operators
  tk_And, // &&
  tk_Or,  // ||
  tk_Not, // !
  // Comparison and Equality
  tk_CompEqual,        // ==
  tk_CompNotEqual,     // !=
  tk_CompLess,         // <
  tk_CompLessEqual,    // <=
  tk_CompGreater,      // >
  tk_CompGreaterEqual, // >=
  // Type Modifiers
  tk_Ref,   // &
  tk_Deref, // @
  // Assignment
  tk_Define,    // :=
  tk_Assign,    // =
  tk_AssignAdd, // +=
  tk_AssignSub, // -=
  tk_AssignMul, // *=
  tk_AssignDiv, // /=
  tk_AssignMod, // %=
  // Arrows
  tk_Pipe,  // ->
  tk_Arrow, // =>
  // Scope resolution
  tk_ScopeResolution, // ::
  // Types
  tk_Tuple, // ,
  tk_Union, // |
  // Other Miscellaneous Operator Things
  tk_ParenLeft,    // (
  tk_ParenRight,   // )
  tk_BracketLeft,  // [
  tk_BracketRight, // ]
  tk_BraceLeft,    // {
  tk_BraceRight,   // }
  tk_FieldAccess,  // .
  tk_Colon,        // :
  tk_Semicolon,    // ;
  tk_Backtick,     // `
  tk_Underscore,   // _
  tk_Rest,         // ..
  tk_Dollar,       // $
  // Macros
  tk_Label, // 'label
  tk_Macro, // macroidentifier!
  // Comments, and Attributes
  tk_Comment, // #{ comment }# and # comment
} tk_Kind;

const u8 *tk_strKind(tk_Kind val);

typedef struct Token_s {
  // The type of this token
  tk_Kind kind;
  // position in the file
  com_streamposition_Span span;
  // This points to
  // null terminated string in case of identifier, tk_StringLiteral,
  // tk_Comment, tk_Documentation, or tk_Annotation u64 in case of
  // tk_IntLiteral double double in case of tk_FloatLiteral Otherwise must
  // be null
  union {
    struct {
      com_str_mut data;
    } identifierToken;
    struct {
      com_str_mut data;
    } macroToken;
    struct {
      com_str_mut data;
    } labelToken;
    struct {
      com_str_mut comment;
      com_str_mut scope;
    } commentToken;
    struct {
      bool data;
    } boolToken;
    struct {
      u8 *data;
      usize data_len;
    } stringToken;
    struct {
      u64 data;
    } intToken;
    struct {
      f64 data;
    } floatToken;
    struct {
      u8 data;
    } charToken;
  };
} Token;

#endif
