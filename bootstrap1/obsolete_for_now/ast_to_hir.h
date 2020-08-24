#ifndef HIR_CONSTRUCT_H
#define HIR_CONSTRUCT_H

#include "com_allocator.h"
#include "hir.h"
#include "tokens_to_ast.h"

// HirConstructor should:
// Walk AST Tree, Clone
// Convert into tabular Format
// Resolve Variable Names

typedef struct {
  com_allocator *_a;
  AstConstructor *_parser;

  // com_vec[Identifier]
  // Each index points to an identifier located in identifier_table
  com_vec _scope;
  // the actual vector holding everything
  com_vec _identifier_table;

} HirConstructor;

HirConstructor hir_create(AstConstructor *parser, com_allocator *a);

bool hir_nextStmntAndCheckNext(hir_Stmnt *stmnt, DiagnosticLogger *diagnostics,
                               HirConstructor *constructor);

void hir_destroy(HirConstructor *constructor);

#endif
