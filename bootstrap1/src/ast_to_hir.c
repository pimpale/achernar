#include "ast_to_hir.h"

typedef struct {
  // vector of identifier ids
  // Vector[IdentifierId]
  Vector *identifiers;
} Scope;

typedef enum {
  IK_Namespace,
  IK_Identifier,
} IdentifierKind;

typedef struct {
  IdentifierKind kind;
  union {
    struct {
      Scope* value;
    } namespaceIdentifier;
    struct {
      char* value;
    } identifier;
  };
} Identifier;


HIRConstructor hirconstructor_create(AstFromCodeConstructor* parser, Allocator*a) {
  HIRConstructor hc; 
  hc.a = a;
  hc.parser = parser;
  return hc;
}




bool hirconstructor_nextStmntAndCheckNext(Vector* diagnostics, HIRConstructor* constructor) {

  return false;
}
