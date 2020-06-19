#include "ast_to_hir.h"


typedef enum {
    IK_Namespace,
    IK_Identifier,
} IdentifierKind;

typedef struct {
    // Vector[Identifier]
    Vector *identifiers;
} Scope;

typedef struct {
  IdentifierKind kind;
  union {
    struct {
      Scope* namespace;
    } namespace;
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
