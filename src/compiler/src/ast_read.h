#ifndef AST_READ
#define AST_READ

#include "ast.h"
#include "lexer.h"
#include "allocator.h"
#include "vector.h"

typedef struct {
  Allocator *allocator;
  Lexer *lexer;

  Token next;
} Reader;

// returns true if the next statement exists 
bool read_nextStmntAndCheck(Reader* reader, Vector* diagnostics);

#endif
