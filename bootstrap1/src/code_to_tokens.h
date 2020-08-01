#ifndef CODE_TO_TOKENS_H
#define CODE_TO_TOKENS_H

#include "com_define.h"
#include "com_reader.h"
#include "com_allocator.h"
#include "diagnostic.h"
#include "token.h"

// Initializes Token to the value of the next token if there has not been an
// error. Otherwise the token will have the type TokenNone and will contain a
// diagnostic. Tokens will be deallocated when the lexer is deallocated.
// 
// Any diagnostics will be allocated from `diagnostics`
Token tk_next(com_reader *reader, DiagnosticLogger* diagnostics, com_allocator* a);

#endif
