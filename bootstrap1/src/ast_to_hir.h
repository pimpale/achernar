#ifndef HIR_CONSTRUCT_H
#define HIR_CONSTRUCT_H

#include "allocator.h"
#include "tokens_to_ast.h"

// HirConstructor should:
// Walk AST Tree, Clone 
// Convert into tabular Format
// Resolve Variable Names

typedef struct {
  Allocator *a;
  AstFromCodeConstructor*parser;

  // Vector[Identifier]
  // Each index points to a char*
  Vector* id_label_table;
  Vector* id_type_table;
  Vector* id_val_table;
  Vector* id_namespace_table;

} HIRConstructor;

HIRConstructor hir_create(AstFromCodeConstructor* parser, Allocator*a);

bool hir_nextStmntAndCheckNext(Vector* diagnostics, HIRConstructor* constructor);

void hir_destroy(HIRConstructor *constructor);

#endif

