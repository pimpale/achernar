#ifndef AST_PARSE_H
#define AST_PARSE_H

#include "allocator.h"
#include "ast.h"
#include "vector.h"
#include "queue.h"
#include "lexer.h"

typedef struct {
  Allocator *a;
  Lexer *lexer;

  // Queue[Token]
  Queue next_tokens_queue;
  // Queue[Vector[Diagnostic]] queue of vectors of peeked diagnostics for each peeked token
  Queue next_diagnostics_queue;
} Parser;

// Uses memory allocated from a to build a parser with the source as lp
Parser parse_create(Lexer *lp, Allocator* a);

// returns true if there is a next stmnt
bool parse_nextStmntAndCheckNext(Stmnt *s, Vector* diagnostics, Parser *parser);

// Frees memory associated with the parser and cleans up
void parse_destroy(Parser *pp);

#endif
