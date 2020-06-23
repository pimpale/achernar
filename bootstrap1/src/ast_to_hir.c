#include "ast_to_hir.h"
#include <string.h>

// the stuff that actually goes inside the identifier table
typedef enum { IK_Namespace, IK_Type, IK_Variable, IK_Label } IdentifierKind;

typedef struct {
  IdentifierKind kind;
  char *value;
  union {
    struct {
      // made of the same stuff as a scope
      Vector scope;
    } namespaceIdentifier;
  };
} Identifier;

// the stuff that goes inside the scope table
typedef enum {
  SEK_Boundary,
  SEK_Identifier,
} ScopeEntryKind;

typedef struct {
  ScopeEntryKind kind;
  union {
    size_t index;
  };
} ScopeEntry;

// creates a HirConstructor using a
HirConstructor hirconstructor_create(AstConstructor *parser, Allocator *a) {
  HirConstructor hc;
  hc.a = a;
  hc.parser = parser;
  hc.scope = vec_create(a);
  hc.identifier_table = vec_create(a);
  return hc;
}

// inserts the specified identifier into the identifier table and onto the
// current scope
static size_t hirconstructor_insert(HirConstructor *constructor,
                                    char *identifier, IdentifierKind kind) {
  // first append to identifier table
  *VEC_PUSH(&constructor->identifier_table, Identifier) =
      (Identifier){.kind = kind, .value = identifier};
  // it is known that vec len must be at least one, so no overflow is possible
  // here
  size_t index = VEC_LEN(&constructor->identifier_table, Identifier) - 1;
  // add index to the scope
  *VEC_PUSH(&constructor->scope, ScopeEntry) =
      (ScopeEntry){.kind = SEK_Identifier, .index = index};
  return index;
}

// pushes a boundary
static void hirconstructor_push_boundary(HirConstructor *constructor) {
  *VEC_PUSH(&constructor->scope, ScopeEntry) =
      (ScopeEntry){.kind = SEK_Boundary};
}

// returns true if the value was found in the scope, false if not
// sets `result` to the value if true
// `result` is undefined otherwise
static bool scope_lookup(Vector *scope, HirConstructor *constructor,
                         const char *name, IdentifierKind kind,
                         size_t *result) {
  for (uint64_t i = (uint64_t)VEC_LEN(scope, ScopeEntry) - 1; i >= 0; i--) {
    // get entry
    ScopeEntry entry = *VEC_GET(scope, i, ScopeEntry);
    if (entry.kind != SEK_Identifier) {
      continue;
    }

    // get identifier from entry
    Identifier *candidate =
        VEC_GET(&constructor->identifier_table, entry.index, Identifier);

    if (candidate->kind == kind && !strcmp(candidate->value, name)) {
      // if it matched our requirements, then set `result` and return true
      *result = entry.index;
      return true;
    } else if (candidate->kind == IK_Namespace) {
      // if a namespace then we have to look through it
      if (scope_lookup(&candidate->namespaceIdentifier.scope, constructor, name,
                       kind, result)) {
        // if the lookup succeeded then we can return true
        return true;
      }
    }
  }
  // if we went through everthing and found nothing
  return false;
}



// returns true if value was found in the current scope, false if not.
// If returned true, `result` would be set to its index
static bool hirconstructor_lookup(HirConstructor *constructor, const char *name,
                                  IdentifierKind kind, size_t *result) {
  return scope_lookup(&constructor->scope, constructor, name, kind, result);
}

static void hir_construct_Reference(Vector *result, ast_Stmnt *source, DiagnosticLogger *diagnostics, HirConstructor *constructor) {
  

}


static void hir_construct_Stmnt(Vector *result, ast_Stmnt *source,
                         DiagnosticLogger *diagnostics,
                         HirConstructor *constructor) {
  switch (source->kind) {
  case ast_SK_None: {
    // noop
    break;
  }
  case ast_SK_Macro: {
    // just log the error, don't insert anything
    *dlogger_append(diagnostics) = diagnostic_standalone(
        source->common.span, DSK_Error,
        "macros are not permitted at this compilation stage");
    break;
  }
  case ast_SK_Use: {
      size_t ret;
      bool succ = hirconstructor_lookup(constructor, source->useStmnt.path.
  }
  }

  bool hirconstructor_nextStmntAndCheckNext(hir_Stmnt * stmnt,
                                            DiagnosticLogger * diagnostics,
                                            HirConstructor * constructor) {
    return false;
  }
