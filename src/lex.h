#ifndef LEX_H
#define LEX_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "parseable.h"
#include "vector.h"

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
  // Operators
  SymAdd,              // +
  SymSub,              // -
  SymMul,              // *
  SymDiv,              // /
  SymAssign,           // =
  SymEqual,            // ==
  SymNotEqual,         // !=
  SymShiftLeft,        // >>
  SySymShiftRight,     // <<
  SymCompLess,         // <
  SymCompLessEqual,    // <=
  SymCompGreater,      // >
  SymCompGreaterEqual  // >=
} SymType;

typedef struct {
  SymType type;
  void* payload;
} Token;

Token* newToken(SymType type, void* payload);
void freeToken(Token* ptr);

// Appends all tokens found in this file to the tokens vector
void lexFile(Parseable* stream, Vector* tokens);

#endif
