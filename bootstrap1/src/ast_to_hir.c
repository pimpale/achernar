#include "ast_to_hir.h"
#include "com_assert.h"
#include "com_mem.h"
#include "com_str.h"


// the stuff that actually goes inside the identifier table
typedef struct {
    hir_Expr* expr;
    com_str identifier;
} LabelTableElem;

// utility method to allocate some noleak memory from the constructor
static void *hir_alloc(hir_Constructor *constructor, usize len) {
  return com_allocator_handle_get((com_allocator_alloc(
      constructor->_a, (com_allocator_HandleData){
                           .len = len,
                           .flags = com_allocator_defaults(constructor->_a) |
                                    com_allocator_NOLEAK})));
}

#define hir_alloc_obj_m(constructor, type)                                     \
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


// Looks through a scope
static LabelId hir_lookupLabel(com_vec *scope, hir_Constructor *constructor,
                               com_str name) {
  // start from the last valid index, and iterate towards 0
  for (usize i_plus_one = com_vec_len_m(scope, LabelTableElem); i_plus_one > 0;
       i_plus_one--) {
    const usize i = i_plus_one - 1;

    // get entry
    LabelTableElem entry = *com_vec_get_m(scope, i, LabelTableElem);
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



static hir_Reference* hir_construct_Reference(ast_Reference *in,
                                             DiagnosticLogger *diagnostics,
                                             hir_Constructor *constructor) {
  hir_Reference* reference = hir_alloc_obj_m(constructor, hir_Reference);
  switch (in->kind) {
  case ast_RK_None: {
      *reference=  (hir_Reference){.kind = hir_RK_None, .source = in};
      return reference;
  }
  case hir_RK_Reference: {
    MaybeScopeMod ret = hir_constructor_ModReferenceHelper(
        &constructor->_scope, in->reference.mod, diagnostics, constructor);

    if (ret.valid) {
      // the scope is the space specified by the modreference
      com_vec *scope = ret.scope;

      // start from the last valid index, and iterate towards 0
      for (usize i_plus_one = com_vec_len_m(scope, LabelTableElem); i_plus_one > 0;
           i_plus_one--) {
        const usize i = i_plus_one - 1;

        // get entry
        LabelTableElem entry = *com_vec_get_m(scope, i, LabelTableElem);
        if (entry.kind == SEK_Identifier) {
          continue;
        }

        IdentifierId id = (IdentifierId){.index = entry.label, .valid = true};
        // get identifier from entry
        hir_Identifier *candidate = hir_getIdentifier(constructor, id);

        if (com_str_equal(candidate->name, in->reference.name)) {
          // we've found our variable
          *reference = (hir_Reference){
              .kind = hir_RK_Reference, .source = in, .reference = {.val = id}};
          return reference;
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
    *reference =  (hir_Reference){.kind = hir_RK_None, .source = in};
    return reference;
  }
  }
}

static hir_Binding*
hir_construct_Binding(ast_Binding *in,
                      attr_UNUSED DiagnosticLogger *diagnostics,
                      hir_Constructor *constructor) {
  hir_Binding* binding = hir_alloc_obj_m(constructor, hir_Binding);
  switch (in->kind) {
  case ast_BK_None: {
    // just return error
    *binding = (hir_Binding){.kind = hir_BK_None, .source = in};
    break;
  }
  case ast_BK_Ignore: {
    *binding = (hir_Binding){.kind = hir_BK_Ignore, .source = in};
    break;
  }
  case ast_BK_Bind: {
    hir_Identifier identifier = {.declaration = in, .name = in->bind.val};
    IdentifierId id = hir_addIdentifier(constructor, identifier);
    *binding = (hir_Binding){.kind = hir_BK_Bind, .bind = {.val = id}};
  }
  }
  return binding;
}

static com_vec hir_construct_Stmnt(ast_Stmnt *in, DiagnosticLogger *diagnostics,
                                   hir_Constructor *constructor) ;

static hir_Lval* hir_construct_Lval(ast_Val *in, DiagnosticLogger *diagnostics,
                                   hir_Constructor *constructor) ;

static hir_Val* hir_construct_Val(ast_Val *in, DiagnosticLogger *diagnostics,
                                   hir_Constructor *constructor) ;

static hir_Pat* hir_construct_Pat(ast_Pat *in, DiagnosticLogger *diagnostics,
                                   hir_Constructor *constructor) ;


static hir_Lval* hir_construct_Lval(ast_Val *in, DiagnosticLogger *diagnostics,
                                   hir_Constructor *constructor) {
  hir_Lval *lval = hir_alloc_obj_m(constructor, hir_Lval);
  lval->source = in;
  switch(in->kind) {
  case ast_VK_FieldAccess: {
    lval->fieldAccess.root = hir_construct_Val(in->fieldAccess.root, diagnostics, constructor);
    lval->fieldAccess.field = hir_construct_Val
  }
  case ast_VK_BinaryOp:

  }

}



static com_vec hir_construct_Stmnt(ast_Stmnt *in, DiagnosticLogger *diagnostics,
                                   hir_Constructor *constructor) {

  com_vec stmnts = hir_alloc_vec(constructor);
  switch (in->kind) {
  case ast_SK_None: {
    // empty vector
    break;
  }
  case ast_SK_Macro: {
    // just log the error, don't insert anything
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = in->common.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("macros are not permitted"),
                     .children_len = 0};
    break;
  }
  case ast_SK_Mod: {
    // bail if the binding mod is broken
    if (in->modStmnt.name->kind == ast_MBK_None) {
      return hir_alloc_vec(constructor);
    }

    hir_push_boundary(constructor);
    for (usize i = 0; i < in->modStmnt.stmnts_len; i++) {
      com_vec child_stmnts = hir_construct_Stmnt(&in->modStmnt.stmnts[i],
                                                 diagnostics, constructor);
      usize len = com_vec_length(&child_stmnts);
      com_vec_pop(&child_stmnts, com_vec_push(&stmnts, len), len);
      com_vec_destroy(&child_stmnts);
    }

    com_vec mod_scope = hir_alloc_vec(constructor);

    // now we pop off the stack till we encounter a boundary
    // push it into the mod scope
    while (true) {
      LabelTableElem se;
      com_vec_pop_m(&constructor->_scope, &se, LabelTableElem);
      if (se.kind == SEK_Boundary) {
        break;
      } else {
        // push to the front of the scope
        *com_vec_push_m(&mod_scope, LabelTableElem) = se;
      }
    }

    // push mod into the current scope
    *com_vec_push_m(&constructor->_scope, LabelTableElem) = (LabelTableElem){
        .kind = SEK_Mod,
        .mod = {.scope = mod_scope, .name = in->modStmnt.name->binding.value}};
    break;
  }
  case ast_SK_Use: {
    // copy everything in the scope ot the current scope
    MaybeScopeMod ret = hir_constructor_ModReferenceHelper(
        &constructor->_scope, in->useStmnt.path, diagnostics, constructor);
    if (ret.valid) {
      // copy everything from mod into the current scope
      usize len = com_vec_length(ret.scope);
      com_mem_move(com_vec_push(&constructor->_scope, len),
                   com_vec_get(ret.scope, 0), len);
    }
    break;
  }
  case ast_SK_Val: {
    *com_vec_push_m(&stmnts, hir_Stmnt) =
        (hir_Stmnt){.kind = hir_SK_Val,
                    .source = in,
                    .valDecl = {.val = hir_construct_Val(in->val.val, diagnostics,
                                                         constructor)}};
    break;
  }
  case ast_SK_ValDecl: {
    // push a var decl then push an assignment to it
    hir_Val *rhs = hir_alloc_obj_m(constructor, hir_Val);
    *rhs = (hir_Val){.source = NULL, .kind = hir_VK_UndefinedLiteral};

    *com_vec_push_m(&stmnts, hir_Stmnt) = (hir_Stmnt){
        .kind = hir_SK_ValDecl,
        .source = in,
        .valDecl = {.pat = hir_construct_Pat(in->valDecl.pat, diagnostics,
                                             constructor),
                    .val = rhs},
    };
    break;
  }
  case ast_SK_ValDeclDefine: {
    *com_vec_push_m(&stmnts, hir_Stmnt) = (hir_Stmnt){
        .kind = hir_SK_ValDecl,
        .source = in,
        .valDecl = {.pat = hir_construct_Pat(in->valDeclDefine.pat, diagnostics,
                                             constructor),
                    .val = hir_construct_Val(in->valDeclDefine.val, diagnostics,
                                             constructor)},
    };
    break;
  }
  case ast_SK_DeferStmnt: {
    *com_vec_push_m(&stmnts, hir_Stmnt) = (hir_Stmnt){
        .kind = hir_SK_Defer,
        .source = in,
        .deferStmnt = {.val = hir_construct_Val(in->deferStmnt.val, diagnostics,
                                                constructor)},
    };
  }
  }

  return stmnts;
}
