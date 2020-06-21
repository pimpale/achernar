#ifndef CODE_TO_AST_H
#define CODE_TO_AST_H

#include "ast.h"

#include "allocator.h"
#include "vector.h"
#include "queue.h"
#include "lexer.h"

typedef struct {
  Allocator *a;
  Lexer *lexer;

  // Queue[Token]
  Queue next_tokens_queue;
} AstFromCodeConstructor;

// Uses memory allocated from a to build a parser with the source as lp
AstFromCodeConstructor ast_create(Lexer *lp, Allocator* a);

// returns true if there is a next stmnt
bool ast_nextStmntAndCheckNext(ast_Stmnt *s, DiagnosticLogger* diagnostics, AstFromCodeConstructor *parser);

// Frees memory associated with the parser and cleans up
void ast_destroy(AstFromCodeConstructor *pp);

#endif
