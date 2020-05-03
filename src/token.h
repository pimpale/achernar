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
  TK_If,        // if
  TK_Else,      // else
  TK_When,      // when
  TK_While,     // while
  TK_With,      // with
  TK_Match,     // match
  TK_Break,     // break
  TK_Continue,  // continue
  TK_Return,    // return
  TK_Function,  // fn
  TK_Let,       // let
  TK_Struct,    // struct
  TK_Pack,      // pack
  TK_Union,     // union
  TK_Enum,      // enum
  TK_TypeAlias, // type
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
  // Bitwise Operators
  TK_BitAnd,     // &
  TK_BitOr,      // |
  TK_BitXor,     // ^
  TK_BitNot,     // ~
  TK_ShiftLeft,  // <<
  TK_ShiftRight, // >>
  // Comparison and Equality
  TK_Equal,            // ==
  TK_NotEqual,         // !=
  TK_CompLess,         // <
  TK_CompLessEqual,    // <=
  TK_CompGreater,      // >
  TK_CompGreaterEqual, // >=
  // Type Modifiers
  TK_Ref,     // $
  TK_Deref,   // @
  TK_Stream,  // ..
  // Assignment
  TK_Assign,       // =
  TK_AssignAdd,    // +=
  TK_AssignSub,    // -=
  TK_AssignMul,    // *=
  TK_AssignDiv,    // /=
  TK_AssignMod,    // %=
  TK_AssignBitAnd, // &=
  TK_AssignBitOr,  // |=
  // Pipelines
  TK_Pipe, // ->
  // Scope resolution
  TK_ScopeResolution, // ::
  // Other Miscellaneous Operator Things
  TK_Void,         // is both type and value
  TK_ParenLeft,    // (
  TK_ParenRight,   // )
  TK_BracketLeft,  // [
  TK_BracketRight, // ]
  TK_BraceLeft,    // {
  TK_BraceRight,   // }
  TK_FieldAccess,  // .
  TK_Comma,        // ,
  TK_Colon,        // :
  TK_Semicolon,    // ;
  TK_Underscore,   // _
  // Macros and Builtins
  TK_Macro, // macro!
  TK_Builtin, // _builtin
  // Comments, and Attributes
  TK_Comment,    // #{ comment }# and # comment
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
    char *macro;
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
