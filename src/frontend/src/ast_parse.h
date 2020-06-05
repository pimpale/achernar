#ifndef PARSE_H_
#define PARSE_H_

#include "allocator.h"
#include "ast.h"
#include "vector.h"
#include "lexer.h"

typedef struct Parser_s {
  Allocator *a;
  Lexer *lexer;
  // Vector<Token>
  Vector next_tokens_stack;
  // Vector<Vector<Diagnostic>> vector of peeked diagnostics for each peeked token
  Vector next_diagnostics_stack;
  // Vector<Vector<Comment>> vector of peeked comments for each peeked token
  Vector next_comments_stack;
  // Vector<Vector<Comment>> (stack of vectors of comments)
  Vector comments;
  // list of nested scopes for breaking out of stuff
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
