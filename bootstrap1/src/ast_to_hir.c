#include "ast_to_hir.h"

// the stuff that actually goes inside the identifier table
typedef enum {
  SEK_Namespace,
  SEK_Identifier,
  SEK_Label,
  SEK_ScopeBoundary,
} ScopeElemKind;

typedef struct {
  ScopeElemKind kind;
  union {
    struct {
      // made of the same stuff as a scope
      com_vec scope;
      // name of namespace
      com_str name;
    } namespace;
    hir_Label *label;
    hir_Identifier *identifier;
  };
} ScopeElem;

static com_vec hir_alloc_vec(com_allocator *a) {
  return com_vec_create(com_allocator_alloc(
      a, (com_allocator_HandleData){.len = 20,
                                    .flags = com_allocator_defaults(a) |
                                             com_allocator_NOLEAK |
                                             com_allocator_REALLOCABLE}));
}

// creates a hir_Constructor using a
hir_Constructor hir_create(ast_Constructor *parser, com_allocator *a) {
  hir_Constructor hc;
  hc._a = a;
  hc._parser = parser;
  hc._scope = hir_alloc_vec(a);
  hc._identifier_table = hir_alloc_vec(a);
  return hc;
}

// inserts the specified identifier into the identifier table and onto the
// current scope
static IdentifierId scope_add(com_vec *scope, hir_Constructor *constructor,
                              Identifier identifier) {
  // first append to identifier table
  *VEC_PUSH(&constructor->identifier_table, Identifier) = identifier;
  // it is known that vec len must be at least one, so no overflow is possible
  // here
  size_t index = VEC_LEN(&constructor->identifier_table, Identifier) - 1;
  // add index to the scope
  *VEC_PUSH(&constructor->scope, ScopeElem) =
      (ScopeElem){.kind = SEK_Identifier, .index = index};

  // return the identifier id
  return (IdentifierId){.valid = true, .id = index};
}

// pushes a boundary
static void hir_push_boundary(hir_Constructor *constructor) {
  *VEC_PUSH(&constructor->scope, ScopeElem) = (ScopeElem){.kind = SEK_Boundary};
}

static Identifier *hir_getIdentifier(hir_Constructor *constructor,
                                     IdentifierId id) {
  assert(id.valid);
  return VEC_GET(&constructor->identifier_table, id.id, Identifier);
}

// Looks through a scope and its parents
static IdentifierId scope_lookup(com_vec *scope, hir_Constructor *constructor,
                                 const char *name, IdentifierKind kind) {
  assert(name != NULL);
  // start from the last valid index, and iterate towards 0
  for (uint64_t i = (uint64_t)VEC_LEN(scope, ScopeElem) - 1; i >= 0; i--) {
    // get entry
    ScopeElem entry = *VEC_GET(scope, i, ScopeElem);
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
                                             hir_Constructor *constructor) {
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
    com_vec *scope = &constructor->scope;
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

static hir_Binding hir_construct_Binding(ast_Binding *in,
                                         DiagnosticLogger *diagnostics,
                                         hir_Constructor *constructor) {
  switch (in->kind) {
  case ast_BK_None: {
    // just return error
    return (hir_Binding){.kind = hir_BK_None, .source = in};
  }
  case ast_BK_Ignore: {
    return (hir_Binding){.kind = hir_BK_Ignore, .source = in};
  }
  case ast_BK_Bind: {
    Identifier identifier = {.kind = hir_IK_Identifier,
                             .value = in->bind.val,
                             .identifierIdentifier = {.source = in}};
    IdentifierId id = scope_add(&constructor->scope, constructor, identifier);
    assert(id.valid);
    return (hir_Binding){.kind = hir_BK_Bind, .binding = {.id = id}};
  }
  }
  // abort if fallthrough somehow
  abort();
}

static void hir_certain_construct_Namespace(ast_Stmnt *in,
                                            DiagnosticLogger *diagnostics,
                                            hir_Constructor *constructor) {
  assert(in->kind == ast_SK_Namespace);
}

static void hir_construct_Stmnt(com_vec *result, ast_Stmnt *source,
                                DiagnosticLogger *diagnostics,
                                hir_Constructor *constructor) {
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
                                 hir_Constructor * constructor) {
    return false;
  }
