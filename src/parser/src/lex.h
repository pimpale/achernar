#ifndef LEX_H
#define LEX_H

#include "token.h"
#include "lexer.h"
#include "vector.h"

// Initializes Token to the value of the next token if there has not been an
// error. Otherwise the token will have the type TokenNone and will contain a
// diagnostic. Tokens will be deallocated when the lexer is deallocated.
Token tk_next(Lexer *lexer, Vector* diagnostics, Allocator* a);

#endif