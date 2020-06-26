#include "ast_to_hir.h"
#include <string.h>

// the stuff that actually goes inside the identifier table
typedef enum {
  hir_IK_Namespace,
  hir_IK_Identifier,
  hir_IK_Label
} IdentifierKind;

typedef struct {
  IdentifierKind kind;

  // simply a duplicate for convenience
  char *value;

  union {
    struct {
      // namespace source
      ast_Stmnt *source;
      // made of the same stuff as a scope
      Vector scope;
      // the namespace scope does not include its parent scopes, but does
      // include uses
    } namespaceIdentifier;
    struct {
      // label source
      ast_LabelBinding *source;
    } labelIdentifier;
    struct {
      // source for a type or val reference
      ast_Binding *source;
    } identifierIdentifier;
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
HirConstructor hir_create(AstConstructor *parser, Allocator *a) {
  HirConstructor hc;
  hc.a = a;
  hc.parser = parser;
  hc.scope = vec_create(a);
  hc.identifier_table = vec_create(a);
  return hc;
}

// inserts the specified identifier into the identifier table and onto the
// current scope
static IdentifierId  scope_add(Vector *scope, HirConstructor *constructor,
                         Identifier identifier) {
  // first append to identifier table
  *VEC_PUSH(&constructor->identifier_table, Identifier) = identifier;
  // it is known that vec len must be at least one, so no overflow is possible
  // here
  size_t index = VEC_LEN(&constructor->identifier_table, Identifier) - 1;
  // add index to the scope
  *VEC_PUSH(&constructor->scope, ScopeEntry) =
      (ScopeEntry){.kind = SEK_Identifier, .index = index};

  // return the identifier id
  return (IdentifierId) {
      .valid=true,
      .id =index
  };
}

// pushes a boundary
static void hir_push_boundary(HirConstructor *constructor) {
  *VEC_PUSH(&constructor->scope, ScopeEntry) =
      (ScopeEntry){.kind = SEK_Boundary};
}

static Identifier *hir_getIdentifier(HirConstructor *constructor,
                                     IdentifierId id) {
  assert(id.valid);
  return VEC_GET(&constructor->identifier_table, id.id, Identifier);
}

// Looks through a scope and its parents
static IdentifierId scope_lookup(Vector *scope, HirConstructor *constructor,
                                 const char *name, IdentifierKind kind) {
  assert(name != NULL);
  // start from the last valid index, and iterate towards 0
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
      return (IdentifierId){.id = entry.index, .valid = true};
    }
  }

  // if we went through all entries in this scope and found nothing
  return (IdentifierId){.valid = false};
}

static hir_Reference hir_construct_Reference(ast_Reference *in,
                                             DiagnosticLogger *diagnostics,
                                             HirConstructor *constructor) {
  switch (in->kind) {
  case ast_RK_None: {
    // do nothing, (error should've already been noted by the  AST)
    goto NO_HIR;
    break;
  }
  case ast_RK_Path: {
    // ensure that it's a valid path
    assert(in->path.segments_len > 0);

    // first iterate through all namespace parts of the reference (it's possible
    // that there isn't one) These define the namespace part of the identifier
    char **ns_segments = in->path.segments;
    size_t ns_segments_len = in->path.segments_len - 1;
    // the current scope we're looking at
    // we start off looking at our present environment
    Vector *scope = &constructor->scope;
    for (size_t i = 0; i < ns_segments_len; i++) {
      const char *ns = ns_segments[i];
      IdentifierId result =
          scope_lookup(scope, constructor, ns, hir_IK_Namespace);
      if (result.valid) {
        Identifier *idet = hir_getIdentifier(constructor, result);
        assert(idet->kind == hir_IK_Namespace);
        // set the scope to this namespace
        scope = &idet->namespaceIdentifier.scope;
      } else {
        *dlogger_append(diagnostics) =
            diagnostic_standalone(in->span, DSK_Error, "namespace not found");
        goto NO_HIR;
      }
    }

    char *ident_name = in->path.segments[in->path.segments_len - 1];
    IdentifierId id_result =
        scope_lookup(scope, constructor, ident_name, hir_IK_Identifier);
    if (!id_result.valid) {
      *dlogger_append(diagnostics) = diagnostic_standalone(
          in->span, DSK_Error, "type or variable not found");
      goto NO_HIR;
    }
    Identifier *ident = hir_getIdentifier(constructor, id_result);
    return (hir_Reference){.kind = hir_RK_Path,
                           .source = in,
                           .path.first_decl =
                               ident->identifierIdentifier.source,
                           .path.id = id_result};
  }
  }
NO_HIR:
  return (hir_Reference){.kind = hir_RK_None, .source = in};
}

static hir_Binding hir_construct_Binding(ast_Binding *in, DiagnosticLogger *diagnostics, HirConstructor *constructor) {
  switch(in->kind) {
    case ast_BK_None: {
      // just return error
      return (hir_Binding) {
          .kind = hir_BK_None,
          .source = in
      };
    }
    case ast_BK_Ignore: {
      return (hir_Binding) {
          .kind = hir_BK_Ignore,
          .source = in
      };
    }
    case ast_BK_Bind: {
      Identifier identifier = {
          .kind = hir_IK_Identifier,
          .value = in->bind.val,
          .identifierIdentifier = {
              .source = in
          }
      };
      IdentifierId id = scope_add(&constructor->scope, constructor, identifier);
      assert(id.valid);
      return (hir_Binding) {
          .kind = hir_BK_Bind,
          .binding = {
              .id = id
          }
      };
    }
  }
  // abort if fallthrough somehow
  abort();
}

static void hir_certain_construct_Namespace(ast_Stmnt *in, DiagnosticLogger *diagnostics, HirConstructor *constructor) {
  assert(in->kind == ast_SK_Namespace);
  
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
      bool succ = hir_lookup(constructor, source->useStmnt.path.
  }
  }

  bool hir_nextStmntAndCheckNext(hir_Stmnt * stmnt,
                                 DiagnosticLogger * diagnostics,
                                 HirConstructor * constructor) {
    return false;
  }
