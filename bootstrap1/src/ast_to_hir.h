#ifndef HIR_CONSTRUCT_H
#define HIR_CONSTRUCT_H

#include "allocator.h"
#include "code_to_ast.h"

// HirConstructor should:
// Walk AST Tree, Clone 
// Convert into tabular Format
// Resolve Variable Names

typedef struct {
  Allocator *a;
  Parser *parser;

  // Vector[Identifier]
  // Each index points to a HIRcomment
  Vector* comment_table;
  Vector* label_table;
  Vector* identifier_table;

} HIRConstructor;

HIRConstructor hirconstructor_create(Parser* parser, Allocator*a);

bool hirconstructor_nextStmntAndCheckNext(Vector* diagnostics, HIRConstructor* constructor);

void hirconstuctor_destroy(HIRConstructor *constructor);

#endif

