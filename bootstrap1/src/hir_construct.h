#ifndef HIR_CONSTRUCT_H
#define HIR_CONSTRUCT_H

#include "allocator.h"
#include "ast_parse.h"

// HirConstructor should:
// Walk AST Tree, Clone 
// Convert into tabular Format

typedef struct {
  Allocator *a;
  Parser *p;


} HIRConstructor;

HIRConstructor hirconstructor_create(Parser* parser, Allocator*a);

bool hirconstructor_nextStmntAndCheckNext(Vector* diagnostics, HIRConstructor* constructor);

#endif

