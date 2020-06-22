#include "ast_to_hir.h"
#include <string.h>

// the stuff that actually goes inside the identifier table
typedef enum { IK_Namespace, IK_Type, IK_Variable, IK_Label } IdentifierKind;

typedef struct {
  IdentifierKind kind;
  char *value;
  union {
    struct {
      Vector scope;
    } namespaceIdentifier;
  };
} Identifier;

// the stuff that goes inside the scope table
// 

// creates a HirConstructor using a
HirConstructor hirconstructor_create(AstConstructor *parser, Allocator *a) {
  HirConstructor hc;
  hc.a = a;
  hc.parser = parser;
  hc.scope = vec_create(a);
  hc.identifier_table = vec_create(a);
  return hc;
}

static size_t hirconstructor_insert( 

// returns index
static size_t hirconstructor_lookup(HirConstructor *constructor,
                                    char *identifier, IdentifierKind kind) {
  for (uint64_t i =
           (uint64_t)VEC_LEN(&constructor->scope, Identifier) - 1;
       i >= 0; i--) {
    Identifier *candidate =
        VEC_GET(&constructor->scope, i, Identifier);

    if(candidate->kind == IK_Namespace) {

    }
    if (candidate->kind == kind && !strcmp(candidate->value, identifier)) {
      return (size_t)i;
    }
  }
}

static void hirconstructor

void hir_construct_Stmnt(hir_Stmnt *stmnt, ast_Stmnt *source,
                         DiagnosticLogger *diagnostics,
                         HirConstructor *constructor) {
  stmnt->source = ptr;

  switch (ptr->kind) {
  case ast_SK_None: {
    stmnt->kind = hir_SK_None;
    break;
  }
  case ast_SK_Macro: {
    *dlogger_append(diagnostics) = diagnostic_standalone(
        ptr->common.span, DSK_Error,
        "macros are not permitted at this compilation stage");
    stmnt->kind = hir_SK_None;
    break;
  }
  case ast_SK_Use: {
    identifier
  }
  }

  bool hirconstructor_nextStmntAndCheckNext(hir_Stmnt * stmnt,
                                            DiagnosticLogger * diagnostics,
                                            HirConstructor * constructor) {
    return false;
  }
