#ifndef TOKEN_H
#define TOKEN_H

#include <stddef.h>
#include <stdint.h>

#include "error.h"

typedef enum {
  // function, type, or variable
  SymIdentifier,
  // Control flow constructs
  SymIf,       // if
  SymElse,     // else
  SymDoWhile,  // do
  SymWhile,    // while
  SymWith,     // with
  SymFor,      // for
  // Literals and constants
  SymStringLiteral,  // "string"
  SymFloatLiteral,   // 0.7
  SymIntLiteral,     // 7
  // Math Operators
  SymAdd,  // +
  SymSub,  // -
  SymMul,  // *
  SymDiv,  // /
  SymMod,  // %
  // Logical Operators
  SymAnd,  // &&
  SymOr,   // ||
  SymNot,  // !
  // Bitwise Operators
  SymBitAnd,      // &
  SymBitOr,       // |
  SymBitXor,      // ^
  SymBitNot,      // ~
  SymShiftLeft,   // <<
  SymShiftRight,  // >>
  // Comparison and Equality
  SymEqual,             // ==
  SymNotEqual,          // !=
  SymCompLess,          // <
  SymCompLessEqual,     // <=
  SymCompGreater,       // >
  SymCompGreaterEqual,  // >=
  // Memory Manipulation Operators
  SymRef,     // $
  SymDeref,   // @
  // Assignment
  SymAssign,  // =
  // Other Miscellaneous Operator Things
  SymParenLeft,     // (
  SymParenRight,    // )
  SymBracketLeft,   // [
  SymBracketRight,  // ]
  SymBraceLeft,     // {
  SymBraceRight,    // }
  SymDot,           // .
  SymComma,         // ,
  SymColon,         // :
  SymSemicolon,     // ;
  // Comments, Documentation, and Annotations
  SymComment,        // #* comment *# and # comment
  SymDocumentation,  // #"Docstring"
  SymAnnotation,     // #@Annotation
} SymType;

typedef struct {
  SymType type;
  // This points to
  // null terminated string in case of SymStringLiteral, SymComment,
  // SymDocumentation, or SymAnnotation uint64_t in case of SymIntLiteral double
  // in case of SymFloatLiteral Otherwise must be null
  void* payload;
} Token;

typedef struct {
  Token* val;
  ErrVal err;
} ResultTokenPtr;

Token* newToken(SymType type, void* payload);
void printToken(Token* ptr);
void deleteToken(Token* ptr);

#endif
