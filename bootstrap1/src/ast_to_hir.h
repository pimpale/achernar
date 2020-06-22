#ifndef Hir_CONSTRUCT_H
#define Hir_CONSTRUCT_H

#include "allocator.h"
#include "hir.h"
#include "tokens_to_ast.h"

// HirConstructor should:
// Walk AST Tree, Clone
// Convert into tabular Format
// Resolve Variable Names

typedef struct {
  Allocator *a;
  AstConstructor *parser;

  // Vector[Identifier]
  // Each index points to an identifier located in identifier_table
  Vector scope;
  // the actual vector holding everything
  Vector identifier_table;

} HirConstructor;

HirConstructor hir_create(AstConstructor *parser, Allocator *a);

bool hir_nextStmntAndCheckNext(hir_Stmnt *stmnt, DiagnosticLogger *diagnostics,
                               HirConstructor *constructor);

void hir_destroy(HirConstructor *constructor);

#endif
