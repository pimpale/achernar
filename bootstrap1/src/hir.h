#ifndef HIR_H
#define HIR_H

#include <stdint.h>

#include "ast.h"

typedef struct {
  AstNode* first_decl;
  char** segments;
  size_t segments_len;
} HirIdentifier; 

typedef struct {
  uint64_t val;
} IdentifierNodeId;


#endif // src/hir_h_INCLUDED

