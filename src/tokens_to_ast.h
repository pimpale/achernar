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
} ast_Constructor;

// Uses memory allocated from a to build a parser with the source as r
ast_Constructor ast_create(com_reader *r, com_allocator* a);

// parse statement with errors
ast_Expr* ast_parseExpr(DiagnosticLogger* diagnostics, ast_Constructor *parser);

// test eof 
bool ast_eof(ast_Constructor *parser, DiagnosticLogger*d);

// Frees memory associated with the parser and cleans up
void ast_destroy(ast_Constructor *pp);

#endif
