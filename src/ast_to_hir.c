#include "ast_to_hir.h"

#include "com_allocator.h"
#include "com_assert.h"
#include "com_imath.h"
#include "com_mem.h"
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

hir_Expr *hir_referenceExpr(ast_Expr *from, com_allocator *a, com_str ref) {
  hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
  obj->from = from;
  obj->kind = hir_EK_Reference;
  obj->reference.reference = ref;
  return obj;
}

hir_Expr *hir_intLiteralExpr(ast_Expr *from, com_allocator *a, i64 lit) {
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

hir_Expr *hir_applyExpr(ast_Expr *from, com_allocator *a, hir_Expr *fn,
                        hir_Expr *param) {
  hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
  obj->from = from;
  obj->kind = hir_EK_Apply;
  obj->apply.fn = fn;
  obj->apply.param = param;
  return obj;
}

hir_Expr *hir_translateExpr(ast_Expr *vep, DiagnosticLogger *diagnostics,
                            com_allocator *a) {
  com_assert_m(vep != NULL, "expr is null");

  hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
  obj->from = vep;

  switch (vep->kind) {
  case ast_EK_None: {
    obj->kind = hir_EK_None;
    break;
  }
  case ast_EK_Nil: {
    obj->kind = hir_EK_Nil;
    break;
  }
  case ast_EK_BindIgnore: {
    obj->kind = hir_EK_BindIgnore;
    break;
  }
  case ast_EK_Int: {
    obj->kind = hir_EK_Int;
    obj->intLiteral.value = vep->intLiteral.value;
    break;
  }
  case ast_EK_Real: {
    obj->kind = hir_EK_Real;
    obj->realLiteral.value = vep->realLiteral.value;
    break;
  }
  case ast_EK_String: {

    // construct recursive data structure containing all functions
    // Apply "," with each character
    // while loop tho

    hir_Expr *active = obj;

    for (usize i = 0; i < vep->stringLiteral.value.len - 1; i++) {
      u8 c = vep->stringLiteral.value.data[i];
      hir_Expr *next = hir_alloc_obj_m(a, hir_Expr);
      // clang-format off
      active = hir_applyExpr(vep, a,
          hir_applyExpr(vep, a,
              hir_referenceExpr(vep, a, com_str_lit_m(",")),
              hir_intLiteralExpr(vep, a, c)
          ),
          next
      );
      // clang-format on
      active = next;
    }
    // the last element in the call chain is just the string value
    active = hir_intLiteralExpr(
        vep, a,
        vep->stringLiteral.value.data[vep->stringLiteral.value.len - 1]);
    break;
  }
  case ast_EK_Bind: {
    switch (vep->bind.bind->kind) {
    case ast_IK_Identifier: {
      obj->kind = hir_EK_BindIgnore;
      obj->bind.bind = vep->bind.bind->id.name;
      break;
    }
    case ast_IK_None: {
      obj->kind = hir_EK_None;
      break;
    }
    }
    break;
  }
  case ast_EK_: {
    *push_prop_m(&obj) = mkprop_m("at_pat", hir_translateExpr(vep->at.pat, a));
    *push_prop_m(&obj) =
        mkprop_m("at_assignable", hir_translateExpr(vep->at.assignable, a));
    break;
  }
  case ast_EK_Int: {
    *push_prop_m(&obj) =
        mkprop_m("int", hir_translatebigint(vep->intLiteral.value, a));
    break;
  }
  case ast_EK_Real: {
    *push_prop_m(&obj) =
        mkprop_m("real", hir_translatebigdecimal(vep->realLiteral.value, a));
    break;
  }
  case ast_EK_String: {
    *push_prop_m(&obj) =
        mkprop_m("string", com_json_str_m(vep->stringLiteral.value));
    break;
  }
  case ast_EK_Struct: {
    *push_prop_m(&obj) =
        mkprop_m("struct_expr", hir_translateExpr(vep->structLiteral.expr, a));
    break;
  }
  case ast_EK_Loop: {
    *push_prop_m(&obj) =
        mkprop_m("loop_body", hir_translateExpr(vep->loop.body, a));
    break;
  }
  case ast_EK_ModuleAccess: {
    *push_prop_m(&obj) =
        mkprop_m("module_root", hir_translateExpr(vep->moduleAccess.module, a));
    *push_prop_m(&obj) = mkprop_m(
        "module_name", hir_translateIdentifier(vep->moduleAccess.field, a));
    break;
  }
  case ast_EK_Reference: {
    *push_prop_m(&obj) = mkprop_m(
        "reference", hir_translateIdentifier(vep->reference.reference, a));
    break;
  }
  case ast_EK_BinaryOp: {
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
  case ast_EK_Ret: {
    *push_prop_m(&obj) =
        mkprop_m("ret_label", hir_translateLabel(vep->ret.label, a));
    *push_prop_m(&obj) =
        mkprop_m("ret_value", hir_translateExpr(vep->ret.expr, a));
    break;
  }
  case ast_EK_Defer: {
    *push_prop_m(&obj) =
        mkprop_m("defer_label", hir_translateLabel(vep->defer.label, a));
    *push_prop_m(&obj) =
        mkprop_m("defer_val", hir_translateExpr(vep->defer.val, a));
    break;
  }
  case ast_EK_CaseOf: {
    *push_prop_m(&obj) =
        mkprop_m("caseof_expr", hir_translateExpr(vep->caseof.expr, a));
    *push_prop_m(&obj) =
        mkprop_m("caseof_cases", hir_translateExpr(vep->caseof.cases, a));
    break;
  }
  case ast_EK_Group: {
    *push_prop_m(&obj) =
        mkprop_m("group_expr", hir_translateExpr(vep->group.expr, a));
    break;
  }
  case ast_EK_Label: {
    *push_prop_m(&obj) =
        mkprop_m("label_val", hir_translateExpr(vep->label.val, a));
    *push_prop_m(&obj) =
        mkprop_m("label_label", hir_translateLabel(vep->label.label, a));
    break;
  }
  }
  return hir_translateobjectify(&obj);
}
