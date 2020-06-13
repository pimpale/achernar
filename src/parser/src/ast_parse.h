#ifndef PARSE_H_
#define PARSE_H_

#include "allocator.h"
#include "ast.h"
#include "vector.h"
#include "queue.h"
#include "lexer.h"

typedef struct Parser_s {
  Allocator *a;
  Lexer *lexer;

  // Queue<Token>
  Queue next_tokens_queue;
  // Queue<Vector<Diagnostic>> queue of vectors of peeked diagnostics for each peeked token
  Queue next_diagnostics_queue;

  int64_t paren_depth;
  int64_t brace_depth;
  int64_t bracket_depth;
} Parser;

// Uses memory allocated from a to build a parser with the source as lp
Parser parse_create(Lexer *lp, Allocator* a);

// returns true if eof hit
bool parse_nextStmntIfExists(Stmnt *s, Vector* diagnostics, Parser *parser);

// Frees memory associated with the parser and cleans up
Allocator* parse_release(Parser *pp);

#endif
