#include "ast_to_hir.h"

#include "com_allocator.h"
#include "com_assert.h"
#include "com_imath.h"
#include "com_mem.h"
#include "com_strcopy.h"
#include "com_vec.h"
#include "com_writer.h"

#include "hir.h"

// utility method to allocate some noleak memory from the parser
static void *hir_alloc(com_allocator *a, usize len) {
  return com_allocator_handle_get((com_allocator_alloc(
      a, (com_allocator_HandleData){.len = len,
                                    .flags = com_allocator_defaults(a) |
                                             com_allocator_NOLEAK})));
}

#define hir_alloc_obj_m(a, type) (type *)hir_alloc((a), sizeof(type))

// utility macro  to create a vector
#define hir_alloc_vec_m(a)                                                     \
  com_vec_create(com_allocator_alloc(                                          \
      a, (com_allocator_HandleData){.len = 10,                                 \
                                    .flags = com_allocator_defaults(a) |       \
                                             com_allocator_NOLEAK |            \
                                             com_allocator_REALLOCABLE}))

typedef struct {
  com_str label;
  // Queue<hir_Expr>
  com_queue defers;
  hir_Expr *scope;
} LabelStackElement;

typedef struct {
  // Vector<LabelStackElement>
  com_vec _elements;
} LabelStack;

static LabelStack LabelStack_create(com_allocator *a) {
  return (LabelStack){._elements = hir_alloc_vec_m(a)};
}

static void LabelStack_destroy(LabelStack *ls) {
  com_vec_destroy(&ls->_elements);
}

// returns true if success, false if fail
static bool LabelStack_pushLabel(LabelStack *ls, hir_Expr *scope,
                                 const ast_Label *label, com_allocator *a) {

  switch (label->kind) {
  case ast_LK_None: {
    // we don't give an error because one should already have been given
    return false;
  }
  case ast_LK_Label:

    *com_vec_push_m(&ls->_elements, LabelStackElement) =
        (LabelStackElement){.scope = scope,
                            .label = label->label.label,
                            .defers = com_queue_create(hir_alloc_vec_m(a))};
    return true;
  }
}

// returns NULL for not found
// will give errors for the label
static LabelStackElement *LabelStack_getLabel(LabelStack *ls, ast_Label *label,

                                              DiagnosticLogger *dl) {
  switch (label->kind) {
  case ast_LK_None: {
    return NULL;
  }
  case ast_LK_Label: {

    for (usize i_plus_one = com_vec_len_m(&ls->_elements, LabelStackElement);
         i_plus_one >= 1; i_plus_one--) {
      usize i = i_plus_one - 1;
      // get label at index
      LabelStackElement *lse =
          com_vec_get_m(&ls->_elements, i, LabelStackElement);
      if (com_str_equal(lse->label, label->label.label)) {
        return lse;
      }
    }

    // if we get over here it means that the provided label didn't have a match
    Diagnostic *hint = dlogger_append(dl, false);
    *hint = (Diagnostic){.span = label->span,
                         .severity = DSK_Hint,
                         .message = com_str_demut(com_strcopy_noleak(
                             label->label.label, dlogger_alloc(dl))),
                         .children_len = 0};

    *dlogger_append(dl, true) = (Diagnostic){
        .span = label->span,
        .severity = DSK_Error,
        .message = com_str_lit_m("could not find label name in scope"),
        .children = hint,
        .children_len = 1};

    return NULL;
  }
  }
}

// returns a com_vec of hir_Exprs
static com_vec LabelStack_popLabel(LabelStack *ls) {
  LabelStackElement top;
  com_vec_pop_m(&ls->_elements, &top, LabelStackElement);
  return com_queue_release(&top.defers);
}

static hir_Expr *hir_referenceExpr(const ast_Expr *from,
                                   attr_UNUSED LabelStack *ls, com_allocator *a,
                                   com_str ref) {
  hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
  obj->from = from;
  obj->kind = hir_EK_Reference;
  obj->reference.reference = ref;
  return obj;
}

static hir_Expr *hir_intLiteralExpr(const ast_Expr *from,
                                    attr_UNUSED LabelStack *ls,
                                    com_allocator *a, i64 lit) {
  hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
  obj->from = from;
  obj->kind = hir_EK_Int;
  obj->intLiteral.value = com_bigint_create(com_allocator_alloc(
      a, (com_allocator_HandleData){.len = 8,
                                    .flags = com_allocator_defaults(a) |
                                             com_allocator_NOLEAK |
                                             com_allocator_REALLOCABLE}));
  com_bigint_set_i64(&obj->intLiteral.value, lit);
  return obj;
}

static hir_Expr *hir_applyExpr(const ast_Expr *from, attr_UNUSED LabelStack *ls,
                               com_allocator *a, hir_Expr *fn,
                               hir_Expr *param) {
  hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
  obj->from = from;
  obj->kind = hir_EK_Apply;
  obj->apply.fn = fn;
  obj->apply.param = param;
  return obj;
}

// returns an instantiated
static hir_Expr *hir_simpleExpr(const ast_Expr *from,
                                attr_UNUSED LabelStack *ls, com_allocator *a,
                                hir_ExprKind ek) {
  hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
  obj->from = from;
  obj->kind = ek;
  return obj;
}

static hir_Expr *hir_noneExpr(const ast_Expr *from, LabelStack *ls,
                              com_allocator *a) {
  return hir_simpleExpr(from, ls, a, hir_EK_None);
}

static hir_Expr *hir_translateExpr(const ast_Expr *vep, LabelStack *ls,
                                   DiagnosticLogger *diagnostics,
                                   com_allocator *a) {
  com_assert_m(vep != NULL, "expr is null");

  switch (vep->kind) {
  case ast_EK_None: {
    return hir_noneExpr(vep, ls, a);
  }
  case ast_EK_Nil: {
    return hir_simpleExpr(vep, ls, a, hir_EK_Nil);
  }
  case ast_EK_BindIgnore: {
    return hir_simpleExpr(vep, ls, a, hir_EK_BindIgnore);
  }
  case ast_EK_Int: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    obj->kind = hir_EK_Int;
    obj->intLiteral.value = vep->intLiteral.value;
    return obj;
  }
  case ast_EK_Real: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    obj->kind = hir_EK_Real;
    obj->realLiteral.value = vep->realLiteral.value;
    return obj;
  }
  case ast_EK_Group: {
    return hir_translateExpr(vep, ls, diagnostics, a);
  }
  case ast_EK_String: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;

    // construct recursive data structure containing all functions
    // Apply "," with each character
    // while loop tho

    hir_Expr *active = obj;

    for (usize i = 0; i < vep->stringLiteral.value.len - 1; i++) {
      u8 c = vep->stringLiteral.value.data[i];
      hir_Expr *next = hir_alloc_obj_m(a, hir_Expr);
      // clang-format off
      active = hir_applyExpr(vep, ls, a,
          hir_applyExpr(vep, ls,  a,
              hir_referenceExpr(vep, ls, a, com_str_lit_m(",")),
              hir_intLiteralExpr(vep, ls, a, c)
          ),
          next
      );
      // clang-format on
      active = next;
    }
    // the last element in the call chain is just the string value
    active = hir_intLiteralExpr(
        vep, ls, a,
        vep->stringLiteral.value.data[vep->stringLiteral.value.len - 1]);
    return obj;
  }
  case ast_EK_Bind: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    switch (vep->bind.bind->kind) {
    case ast_IK_Identifier: {
      obj->kind = hir_EK_Bind;
      obj->bind.bind = vep->bind.bind->id.name;
      break;
    }
    case ast_IK_None: {
      obj->kind = hir_EK_None;
      break;
    }
    }
    return obj;
  }
  case ast_EK_Loop: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    obj->kind = hir_EK_Loop;
    obj->loop.body = hir_translateExpr(vep->loop.body, ls, diagnostics, a);
    return obj;
  }
  case ast_EK_Label: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    obj->kind = hir_EK_Label;
    // push new label element
    bool didPushLabel = LabelStack_pushLabel(ls, obj, vep->label.label, a);

    // if expr is group, then we evaluate the group's body
    // TODO is this the best way to handle this?
    if (vep->label.val->kind == ast_EK_Group) {
      obj->label.expr =
          hir_translateExpr(vep->label.val->group.expr, ls, diagnostics, a);
    } else {
      obj->label.expr = hir_translateExpr(vep->label.val, ls, diagnostics, a);
    }

    // only pop off label if we managed to push one
    if (didPushLabel) {
      // pop label element off
      // this gives us the defers in the correct order
      com_vec defers = LabelStack_popLabel(ls);
      // note record these
      obj->label.defer_len = com_vec_len_m(&defers, hir_Expr);
      obj->label.defer = com_vec_release(&defers);
    } else {
      obj->label.defer_len = 0;
    }
    return obj;
  }
  case ast_EK_Ret: {
    LabelStackElement *lse =
        LabelStack_getLabel(ls, vep->ret.label, diagnostics);
    if (lse != NULL) {
      hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
      obj->from = vep;
      obj->kind = hir_EK_Ret;
      obj->ret.scope = lse->scope;
      obj->ret.expr = hir_translateExpr(vep, ls, diagnostics, a);
      return obj;
    } else {
      // means that we didn't manage to find the label
      return hir_noneExpr(vep, ls, a);
    }
  }
  case ast_EK_Defer: {

    LabelStackElement *lse =
        LabelStack_getLabel(ls, vep->ret.label, diagnostics);
    *com_queue_push_m(&lse->defers, hir_Expr) = todefer;

    *push_prop_m(&obj) =
        mkprop_m("defer_label", hir_translateLabel(vep->defer.label, a));
    *push_prop_m(&obj) =
        mkprop_m("defer_val", hir_translateExpr(vep->defer.val, a));
    break;
  }
  case ast_EK_Struct: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    obj->kind = hir_EK_Struct;
    obj->structLiteral.expr =
        hir_translateExpr(vep->structLiteral.expr, ls, diagnostics, a);
    break;
  }
  case ast_EK_Reference: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    switch (vep->reference.reference->kind) {
    case ast_IK_None: {
      obj->kind = hir_EK_None;
      break;
    }
    case ast_IK_Identifier: {
      obj->kind = hir_EK_Reference;
      obj->reference.reference = vep->reference.reference->id.name;
    }
    }
    break;
  }
  case ast_EK_BinaryOp: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    *push_prop_m(&obj) =
        mkprop_m("binary_operation",
                 com_json_str_m(ast_strExprBinaryOpKind(vep->binaryOp.op)));
    *push_prop_m(&obj) =
        mkprop_m("binary_left_operand",
                 hir_translateExpr(vep->binaryOp.left_operand, a));
    *push_prop_m(&obj) =
        mkprop_m("binary_right_operand",
                 hir_translateExpr(vep->binaryOp.right_operand, a));
    break;
  }
  case ast_EK_CaseOf: {
    *push_prop_m(&obj) =
        mkprop_m("caseof_expr", hir_translateExpr(vep->caseof.expr, a));
    *push_prop_m(&obj) =
        mkprop_m("caseof_cases", hir_translateExpr(vep->caseof.cases, a));
    break;
  }
  }
  return hir_translateobjectify(&obj);
}

hir_Expr *hir_constructExpr(const ast_Expr *vep, DiagnosticLogger *diagnostics,
                            com_allocator *a) {
  LabelStack ls = LabelStack_create(a);
  hir_Expr *val = hir_translateExpr(vep->label.val, &ls, diagnostics, a);
  LabelStack_destroy(&ls);
  return val;
}
