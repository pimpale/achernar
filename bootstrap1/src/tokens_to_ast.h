#ifndef CODE_TO_AST_H
#define CODE_TO_AST_H

#include "ast.h"

#include "com_allocator.h"
#include "com_vec.h"
#include "com_queue.h"
#include "com_reader.h"
#include "diagnostic.h"

typedef struct {
  com_allocator *_a;
  com_reader* _reader;
  com_queue _next_tokens_queue;
} AstConstructor;

// Uses memory allocated from a to build a parser with the source as r
AstConstructor ast_create(com_reader *r, com_allocator* a);

// returns true if there is a next stmnt
bool ast_nextStmntAndCheckNext(ast_Stmnt *s, DiagnosticLogger* diagnostics, AstConstructor *parser);

// Frees memory associated with the parser and cleans up
void ast_destroy(AstConstructor *pp);

#endif
