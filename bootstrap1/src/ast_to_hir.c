#include "ast_to_hir.h"
#include "com_assert.h"
#include "com_str.h"
#include "com_mem.h"

// the stuff that actually goes inside the identifier table
typedef enum {
  SEK_Mod,
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
      // name of mod
      com_str name;
    } mod;
    usize label;
    usize identifier;
  };
} ScopeElem;

// utility method to allocate some noleak memory from the constructor
static void *hir_alloc(ast_Constructor *constructor, usize len) {
  return com_allocator_handle_get((com_allocator_alloc(
      constructor->_a,
      (com_allocator_HandleData){.len = len,
                                 .flags = com_allocator_defaults(constructor->_a) |
                                          com_allocator_NOLEAK})));
}

#define hir_alloc_obj_m(constructor, type)                                        \
  (type *)hir_alloc((constructor), sizeof(type))

static com_vec hir_alloc_vec(hir_Constructor *constructor) {
  return com_vec_create(com_allocator_alloc(
      constructor->_a,
      (com_allocator_HandleData){
          .len = 20, .flags = com_allocator_defaults(constructor->_a)}));
}


// creates a hir_Constructor using a
hir_Constructor hir_create(ast_Constructor *parser, com_allocator *a) {
  hir_Constructor hc;
  hc._a = a;
  hc._parser = parser;
  hc._scope = hir_alloc_vec(&hc);
  hc._identifier_table = hir_alloc_vec(&hc);
  hc._label_table = hir_alloc_vec(&hc);
  return hc;
}

// pushes a boundary
static void hir_push_boundary(hir_Constructor *constructor) {
  *com_vec_push_m(&constructor->_scope, ScopeElem) =
      (ScopeElem){.kind = SEK_Boundary};
}

// Utility structs to prevent accidentallyp plugging in the the wrong index

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

typedef struct {
  com_vec *scope;
  bool valid;
} MaybeScopeMod;

// returns null if couldn't find anything
static MaybeScopeMod
hir_constructor_ModReferenceHelper(com_vec *scope, ast_ModReference *mod,
                                   DiagnosticLogger *diagnostics,
                                   hir_Constructor *constructor) {
  switch (mod->kind) {
  case ast_MRK_None: {
    return (MaybeScopeMod){.valid = false};
  }
  case ast_MRK_Reference: {
    for (usize i_plus_one = com_vec_len_m(scope, ScopeElem); i_plus_one > 0;
         i_plus_one--) {
      const usize i = i_plus_one - 1;

      // get entry
      ScopeElem entry = *com_vec_get_m(scope, i, ScopeElem);
      if (entry.kind == SEK_Mod &&
          com_str_equal(mod->reference.name, entry.mod.name)) {
        return hir_constructor_ModReferenceHelper(
            &entry.mod.scope, mod->reference.mod, diagnostics, constructor);
      }
    }
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = mod->span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("could not resolve mod name"),
                     .children_len = 0};
    return (MaybeScopeMod){.valid = false};
  }
  case ast_MRK_Omitted: {
    return (MaybeScopeMod){.scope = &constructor->_scope, .valid = true};
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

    MaybeScopeMod ret = hir_constructor_ModReferenceHelper(
        &constructor->_scope, in->reference.mod,
        diagnostics, constructor);

    if(ret.valid) {
      // the scope is the space specified by the modreference
      com_vec* scope = ret.scope;

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

        if (com_str_equal(candidate->name, in->reference.name)) {
          // we've found our variable
          return (hir_Reference){
            .kind = hir_RK_Reference, .source = in, .reference = {.val = id}};
          }
      }

      // if we went through all entries in this scope and found nothing
      // Throw error
      *dlogger_append(diagnostics) =
          (Diagnostic){.span = in->span,
                       .severity = DSK_Error,
                       .message = com_str_lit_m("could not resolve name"),
                       .children_len = 0};
    }
    return (hir_Reference){.kind = hir_RK_None, .source = in};
  }
  }
}

static hir_Binding
hir_construct_Binding(ast_Binding *in,
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
    return (hir_Binding){.kind = hir_BK_Bind, .bind = {.val = id}};
  }
  }
}

static com_vec hir_construct_Stmnt(ast_Stmnt *in, DiagnosticLogger *diagnostics,
                                   hir_Constructor *constructor);

// returns vector of statements
static com_vec hir_certain_construct_Mod(ast_Stmnt *in,
                                         DiagnosticLogger *diagnostics,
                                         hir_Constructor *constructor) {
  com_assert_m(in->kind == ast_SK_Mod, "mod node required");

  // bail if the binding mod is broken
  if (in->modStmnt.name->kind == ast_MBK_None) {
    return hir_alloc_vec(constructor);
  }

  com_vec stmnts = hir_alloc_vec(constructor->_a);
  hir_push_boundary(constructor);
  for (usize i = 0; i < in->modStmnt.stmnts_len; i++) {
    com_vec child_stmnts =
        hir_construct_Stmnt(&in->modStmnt.stmnts[i], diagnostics, constructor);
    usize len = com_vec_length(&child_stmnts);
    com_vec_pop(&child_stmnts, com_vec_push(&stmnts, len), len);
    com_vec_destroy(&child_stmnts);
  }

  com_vec mod_scope = hir_alloc_vec(constructor->_a);

  // now we pop off the stack till we encounter a boundary
  // push it into the mod scope
  while (true) {
    ScopeElem se;
    com_vec_pop_m(&constructor->_scope, &se, ScopeElem);
    if (se.kind == SEK_Boundary) {
      break;
    } else {
      // push to the front of the scope
      *com_vec_push_m(&mod_scope, ScopeElem) = se;
    }
  }

  // push mod into the current scope
  *com_vec_push_m(&constructor->_scope, ScopeElem) = (ScopeElem){
      .kind = SEK_Mod,
      .mod = {.scope = mod_scope, .name = in->modStmnt.name->binding.value}};

  return stmnts;
}
static com_vec
hir_certain_construct_Use(ast_Stmnt *in,
                          DiagnosticLogger *diagnostics,
                          hir_Constructor *constructor) {
  com_assert_m(in->kind == ast_SK_Use, "use node required");

  // copy everything in the scope ot the current scope
  MaybeScopeMod ret = hir_constructor_ModReferenceHelper(&constructor->_scope,
                                                         in->useStmnt.path, diagnostics, constructor) ;
  if(ret.valid) {
     // copy everything from mod into the current scope
     usize len = com_vec_length(ret.scope) ;
     com_mem_move(com_vec_push(&constructor->_scope, len), com_vec_get(ret.scope, 0), len);
  }

  return hir_alloc_vec(constructor);
}

static com_vec hir_certain_construct_ValDeclDefine(ast_Stmnt *in,
                          DiagnosticLogger *diagnostics,
                          hir_Constructor *constructor) {
  com_assert_m(in->kind == ast_SK_ValDeclDefine, "val decl define node required");
  // push a var decl then push an assignment to it 
  com_vec stmnts= hir_alloc_vec(constructor);

  *com_vec_push_m(&stmnts, hir_Stmnt) = (hir_Stmnt) {
      .kind= hir_SK_ValDecl,
      .source=in,
      .valDecl={hir_construct_Pat(in->valDeclDefine.pat, diagnostics, constructor)},
  };

  hir_Val* assign = hir_alloc_obj_m(constructor, hir_Val);
  *assign = (hir_Val) {
            .source = NULL,
            .kind=hir_VK_BinaryOp,
            .binaryOp = {
                .op = hir_VEBOK_Assign,
                .left_operand = 

      }  }

  *com_vec_push_m(&stmnts, hir_Stmnt) = (hir_Stmnt) {
      .kind= hir_SK_Val,
      .source=in,
      .val={},
  };


}

static com_vec hir_construct_Stmnt(ast_Stmnt *in, DiagnosticLogger *diagnostics,
                                   hir_Constructor *constructor) {
  switch (in->kind) {
  case ast_SK_None: {
    // empty vector
    return hir_alloc_vec(constructor);
  }
  case ast_SK_Macro: {
    // just log the error, don't insert anything
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = in->common.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("macros are not permitted"),
                     .children_len = 0};
    return hir_alloc_vec(constructor);
  }
  case ast_SK_Mod: {
    return hir_certain_construct_Mod(in, diagnostics, constructor);
  }
  case ast_SK_Use: {
    return hir_certain_construct_Use(in, diagnostics, constructor);
  }
  case ast_SK_Val: {
    return hir_certain_construct_Val(in, diagnostics, constructor);
  }
  case ast_SK_ValDecl: {
    return hir_certain_construct_ValDecl(in, diagnostics, constructor);
  }
  case ast_SK_ValDeclDefine: {
    return hir_certain_construct_ValDeclDefine(in, diagnostics, constructor);
  }
  }

    return false;
  }

