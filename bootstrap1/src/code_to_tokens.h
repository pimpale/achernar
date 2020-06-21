#ifndef CODE_TO_TOKENS_H
#define CODE_TO_TOKENS_H

#include "token.h"
#include "lexer.h"
#include "vector.h"

// Initializes Token to the value of the next token if there has not been an
// error. Otherwise the token will have the type TokenNone and will contain a
// diagnostic. Tokens will be deallocated when the lexer is deallocated.
// 
// Any diagnostics will be allocated from `a`
Token tk_next(Lexer *lexer, Vector* diagnostics, Allocator* a);

#endif
