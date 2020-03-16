#ifndef TOKEN_H
#define TOKEN_H

#include <stddef.h>
#include <stdint.h>

#include "lncol.h"
#include "error.h"

typedef enum {
  // This is not a token, and does not contain token data
  T_None,
  // function, type, or variable
  T_Identifier,
  // Keywords
  T_If,       // if
  T_Else,     // else
  T_While,    // while
  T_For,      // for
  T_With,     // with
  T_Match,    // match
  T_Break,    // break
  T_Continue, // continue
  T_Return,   // return
  T_Pass,     // pass
  T_Function, // fn
  T_Let,      // let
  T_Struct,   // struct
  T_Alias,    // alias
  T_Typeof,   // typeof
  T_Sizeof,   // sizeof
  T_Alignof,  // sizeof
  // Literals and constants
  T_StringLiteral, // "string"
  T_CharLiteral,   // 'a'
  T_FloatLiteral,  // 0.7
  T_IntLiteral,    // 7
  // Math Operators
  T_Add, // +
  T_Sub, // -
  T_Mul, // *
  T_Div, // /
  T_Mod, // %
  // Logical Operators
  T_And, // &&
  T_Or,  // ||
  T_Not, // !
  // Bitwise Operators
  T_BitAnd,     // &
  T_BitOr,      // |
  T_BitXor,     // ^
  T_BitNot,     // ~
  T_ShiftLeft,  // <<
  T_ShiftRight, // >>
  // Comparison and Equality
  T_Equal,            // ==
  T_NotEqual,         // !=
  T_CompLess,         // <
  T_CompLessEqual,    // <=
  T_CompGreater,      // >
  T_CompGreaterEqual, // >=
  // Memory Manipulation Operators
  T_Ref,   // $
  T_Deref, // @
  // Assignment
  T_Assign, // =
  // Pipelines
  T_Pipe, // ->
  // Other Miscellaneous Operator Things
  T_ParenLeft,    // (
  T_ParenRight,   // )
  T_BracketLeft,  // [
  T_BracketRight, // ]
  T_BraceLeft,    // {
  T_BraceRight,   // }
  T_Dot,          // .
  T_Comma,        // ,
  T_Colon,        // :
  T_Semicolon,    // ;
  // Macros
  T_Macro,        // macro!
  // Comments, and Attributes
  T_Comment,   // #[ comment ]# and # comment
  T_AttrLeft,  // [[
  T_AttrRight, // ]]
} TokenType;


typedef struct Token_s {
  TokenType type; // The type of this token
  Span span; // position in the file
  // This points to
  // null terminated string in case of identifier, T_StringLiteral,
  // T_Comment, T_Documentation, or T_Annotation uint64_t in case of
  // T_IntLiteral double double in case of T_FloatLiteral Otherwise must
  // be null
  union {
    char *identifier;
    char *macro;
    char *comment;
    char *stringLiteral;
    char *annotationLiteral;
    uint64_t intLiteral;
    double floatLiteral;
    char charLiteral;
  };

  DiagnosticType error;
} Token;

void destroyToken(Token *token);
void printToken(Token *token);

#endif
