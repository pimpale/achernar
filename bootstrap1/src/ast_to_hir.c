#include "ast_to_hir.h"
#include "com_assert.h"
#include "com_str.h"

// the stuff that actually goes inside the identifier table
typedef enum {
  SEK_Namespace,
  SEK_Identifier,
  SEK_Label,
  SEK_Boundary,
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
    usize label;
    usize identifier;
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
  hir_Constructor hc = (hir_Constructor){._a = a,
                                         ._parser = parser,
                                         ._scope = hir_alloc_vec(a),
                                         ._identifier_table = hir_alloc_vec(a),
                                         ._label_table = hir_alloc_vec(a)};
  return hc;
}

// pushes a boundary
static void hir_push_boundary(hir_Constructor *constructor) {
  *com_vec_push_m(&constructor->_scope, ScopeElem) =
      (ScopeElem){.kind = SEK_Boundary};
}

// Utility structs to prevent accidentallyp plugging in the the wrong index
typedef struct {
  const usize index;
  const bool valid;
} LabelId;

typedef struct {
  const usize index;
  const bool valid;
} IdentifierId;

static hir_Label *hir_getLabel(const hir_Constructor *constructor, LabelId id) {
  com_assert_m(id.valid, "id is invalid");
  return com_vec_get_m(&constructor->_label_table, id.index, hir_Label);
}

static hir_Identifier *hir_getIdentifier(const hir_Constructor *constructor,
                                         IdentifierId id) {

  com_assert_m(id.valid, "id is invalid");
  return com_vec_get_m(&constructor->_identifier_table, id.index,
                       hir_Identifier);
}

// inserts the specified identifier into the identifier table and onto the
// current scope
static IdentifierId hir_addIdentifier(hir_Constructor *constructor,
                                      hir_Identifier identifier) {
  // first append to identifier table
  *com_vec_push_m(&constructor->_identifier_table, hir_Identifier) = identifier;
  // it is known that vec len must be at least one, so no overflow is possible
  // here
  usize index =
      com_vec_len_m(&constructor->_identifier_table, hir_Identifier) - 1;
  // add index to the scope
  *com_vec_push_m(&constructor->_identifier_table, ScopeElem) =
      (ScopeElem){.kind = SEK_Identifier, .identifier = index};

  // return the identifier id
  return (IdentifierId){.index = index, .valid = true};
}

// Looks through a scope
static LabelId hir_lookupLabel(com_vec *scope, hir_Constructor *constructor,
                               com_str name) {
  // start from the last valid index, and iterate towards 0
  for (usize i_plus_one = com_vec_len_m(scope, ScopeElem); i_plus_one > 0;
       i_plus_one--) {
    const usize i = i_plus_one - 1;

    // get entry
    ScopeElem entry = *com_vec_get_m(scope, i, ScopeElem);
    if (entry.kind == SEK_Label) {
      continue;
    }

    LabelId id = (LabelId){.index = entry.label, .valid = true};
    // get identifier from entry
    hir_Label *candidate = hir_getLabel(constructor, id);

    if (com_str_equal(candidate->name, name)) {
      // if it matched our requirements, then set `result` and return true
      return id;
    }
  }

  // if we went through all entries in this scope and found nothing
  return (LabelId){.index = 0, .valid = false};
}

static IdentifierId hir_construct_ReferenceHelper(
    com_str name, const com_vec *scope, ast_ModReference *mod,
    DiagnosticLogger *diagnostics, const hir_Constructor *constructor) {
  switch (mod->kind) {
  case ast_MRK_None: {
    return (IdentifierId){.index = 0, .valid = false};
  }
  case ast_MRK_Reference: {
    for (usize i_plus_one = com_vec_len_m(scope, ScopeElem); i_plus_one > 0;
         i_plus_one--) {
      const usize i = i_plus_one - 1;

      // get entry
      ScopeElem entry = *com_vec_get_m(scope, i, ScopeElem);
      if (entry.kind == SEK_Namespace &&
          com_str_equal(name, entry.namespace.name)) {
        return hir_construct_ReferenceHelper(name, &entry.namespace.scope,
                                             mod->reference.mod, diagnostics,
                                             constructor);
      }
    }
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = mod->span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("could not resolve mod name"),
                     .children_len = 0};
    return (IdentifierId){.index = 0, .valid = false};
  }
  case ast_MRK_Omitted: {
    // start from the last valid index, and iterate towards 0
    for (usize i_plus_one = com_vec_len_m(scope, ScopeElem); i_plus_one > 0;
         i_plus_one--) {
      const usize i = i_plus_one - 1;

      // get entry
      ScopeElem entry = *com_vec_get_m(scope, i, ScopeElem);
      if (entry.kind == SEK_Identifier) {
        continue;
      }

      IdentifierId id = (IdentifierId){.index = entry.label, .valid = true};
      // get identifier from entry
      hir_Identifier *candidate = hir_getIdentifier(constructor, id);

      if (com_str_equal(candidate->name, name)) {
        // if it matched our requirements, then set `result` and return true
        return id;
      }
    }

    // if we went through all entries in this scope and found nothing
    // Throw error
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = mod->span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("could not resolve name"),
                     .children_len = 0};
    return (IdentifierId){.index = 0, .valid = false};
  }
  }
}

static hir_Reference hir_construct_Reference(ast_Reference *in,
                                             DiagnosticLogger *diagnostics,
                                             hir_Constructor *constructor) {
  switch (in->kind) {
  case ast_RK_None: {
    return (hir_Reference){.kind = hir_RK_None, .source = in};
  }
  case hir_RK_Reference: {
    IdentifierId id = hir_construct_ReferenceHelper(
        in->reference.name, &constructor->_scope, in->reference.mod,
        diagnostics, constructor);
    if (id.valid) {
      return (hir_Reference){
          .kind = hir_RK_Reference,
          .source = in,
          .reference = {.val = hir_getIdentifier(constructor, id)}};
    } else {
      return (hir_Reference){.kind = hir_RK_None, .source = in};
    }
  }
  }
}

static hir_Binding hir_construct_Binding(ast_Binding *in,
                                         attr_UNUSED DiagnosticLogger *diagnostics,
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
    hir_Identifier identifier = {.declaration = in, .name = in->bind.val};
    IdentifierId id = hir_addIdentifier(constructor, identifier);
    return (hir_Binding){.kind = hir_BK_Bind,
                         .bind = {.val = hir_getIdentifier(constructor, id)}};
  }
  }
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
    usize ret;
      bool succ = hir_lookup(constructor, source->useStmnt.path.
  }
  }

  bool hir_nextStmntAndCheckNext(hir_Stmnt * stmnt,
                                 DiagnosticLogger * diagnostics,
                                 hir_Constructor * constructor) {
    return false;
  }
