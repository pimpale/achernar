#ifndef TOKEN_H
#define TOKEN_H

#include <stddef.h>
#include <stdint.h>

#include "parseable.h"
#include "vector.h"
#include "error.h"

typedef enum {
  // function, type, or variable
  TokenIdentifier,
  // Keywords
  TokenIf,       // if
  TokenElse,     // else
  TokenWhile,    // while
  TokenFor,      // for
  TokenWith,     // with
  TokenMatch,     // match
  TokenBreak,    // break
  TokenContinue, // continue
  TokenReturn,   // return
  // Literals and constants
  TokenStringLiteral,    // "string"
  TokenCharLiteral, // 'a'
  TokenFloatLiteral,     // 0.7
  TokenIntLiteral,       // 7
  // Math Operators
  TokenAdd, // +
  TokenSub, // -
  TokenMul, // *
  TokenDiv, // /
  TokenMod, // %
  // Logical Operators
  TokenAnd, // &&
  TokenOr,  // ||
  TokenNot, // !
  // Bitwise Operators
  TokenBitAnd,     // &
  TokenBitOr,      // |
  TokenBitXor,     // ^
  TokenBitNot,     // ~
  TokenShiftLeft,  // <<
  TokenShiftRight, // >>
  // Comparison and Equality
  TokenEqual,            // ==
  TokenNotEqual,         // !=
  TokenCompLess,         // <
  TokenCompLessEqual,    // <=
  TokenCompGreater,      // >
  TokenCompGreaterEqual, // >=
  // Memory Manipulation Operators
  TokenRef,   // $
  TokenDeref, // @
  // Assignment
  TokenAssign, // =
  // Pipelines
  TokenPipe, // |>
  // Other Miscellaneous Operator Things
  TokenParenLeft,    // (
  TokenParenRight,   // )
  TokenBracketLeft,  // [
  TokenBracketRight, // ]
  TokenBraceLeft,    // {
  TokenBraceRight,   // }
  TokenDot,          // .
  TokenComma,        // ,
  TokenColon,        // :
  TokenSemicolon,    // ;
  // Comments, and Annotations
  TokenComment,       // #* comment *# and # comment
  TokenAnnotation     // #@Annotation
} TokenType;

typedef struct Token_s {
  TokenType type;
  // This points to
  // null terminated string in case of identifier, TokenStringLiteral, TokenComment,
  // TokenDocumentation, or TokenAnnotation uint64_t in case of TokenIntLiteral double
  // double in case of TokenFloatLiteral
  // Otherwise must be null
  union {
    char* identifier;
    char* comment;
    char* stringLiteral;
    char* annotationLiteral;
    uint64_t intLiteral;
    double floatLiteral;
    char charLiteral;
  };
} Token;

typedef struct ResultToken_s {
  Token val;
  ErrVal err;
} ResultToken;


// Returns the next available token, or a EofError
ResultToken nextToken(Parseable* stream);
void destroyToken(Token *token);
void printToken(Token *token);

#endif
