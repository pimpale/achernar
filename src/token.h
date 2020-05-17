#ifndef TOKEN_H
#define TOKEN_H

#include <stddef.h>
#include <stdint.h>

#include "error.h"
#include "lncol.h"

typedef enum {
  // This is not a token, and does not contain token data
  TK_None,
  // function, type, or variable
  TK_Identifier,
  // Keywords
  TK_Void,      // is both type and value
  TK_If,        // if
  TK_Else,      // else
  TK_While,     // while
  TK_Match,     // match
  TK_Break,     // break
  TK_Let,       // let
  TK_Continue,  // continue
  TK_Return,    // return
  TK_Defer,     // defer
  TK_Stmnt,     // stmnt
  TK_Fn,        // fn
  TK_Pat,       // pat
  TK_As,        // as
  TK_Struct,    // struct
  TK_Enum,      // enum
  TK_Type,      // type
  TK_Macro,     // macro
  TK_Namespace, // namespace
  TK_Use,       // use
  // Literals and constants
  TK_BoolLiteral,   // true | false
  TK_StringLiteral, // "string"
  TK_CharLiteral,   // 'a'
  TK_FloatLiteral,  // 0.7
  TK_IntLiteral,    // 7
  // Math Operators
  TK_Add, // +
  TK_Sub, // -
  TK_Mul, // *
  TK_Div, // /
  TK_Mod, // %
  // Logical Operators
  TK_And, // &&
  TK_Or,  // ||
  TK_Not, // !
  // Comparison and Equality
  TK_CompEqual,        // ==
  TK_CompNotEqual,     // !=
  TK_CompLess,         // <
  TK_CompLessEqual,    // <=
  TK_CompGreater,      // >
  TK_CompGreaterEqual, // >=
  // Type Modifiers
  TK_Ref,   // &
  TK_Deref, // @
  // Assignment
  TK_Assign,    // =
  TK_AssignAdd, // +=
  TK_AssignSub, // -=
  TK_AssignMul, // *=
  TK_AssignDiv, // /=
  TK_AssignMod, // %=
  // Arrows
  TK_Pipe,  // ->
  TK_Arrow, // =>
  // Scope resolution
  TK_ScopeResolution, // ::
  // Types
  TK_Tuple, // ,
  TK_Union, // |
  // Other Miscellaneous Operator Things
  TK_ParenLeft,    // (
  TK_ParenRight,   // )
  TK_BracketLeft,  // [
  TK_BracketRight, // ]
  TK_BraceLeft,    // {
  TK_BraceRight,   // }
  TK_FieldAccess,  // .
  TK_Colon,        // :
  TK_Semicolon,    // ;
  TK_Underscore,   // _
  TK_Backtick,     // `
  TK_Rest,         // ..
  TK_Dollar,       // $
  // Macros
  TK_Builtin,
  TK_MacroCall, // macrocall!
  // Comments, and Attributes
  TK_Comment, // #{ comment }# and # comment
} TokenKind;

typedef struct Token_s {
  TokenKind kind; // The type of this token
  // This points to
  // null terminated string in case of identifier, TK_StringLiteral,
  // TK_Comment, TK_Documentation, or TK_Annotation uint64_t in case of
  // TK_IntLiteral double double in case of TK_FloatLiteral Otherwise must
  // be null
  union {
    char *identifier;
    char *macro_call;
    char *builtin;
    struct {
      char *comment;
      char *scope;
    } comment;
    char *string_literal;
    uint64_t int_literal;
    double float_literal;
    char char_literal;
    bool bool_literal;
  };
  Span span; // position in the file
  DiagnosticKind error;
} Token;

#endif
