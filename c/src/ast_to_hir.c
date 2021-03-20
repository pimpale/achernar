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
  // Queue<*hir_Expr>
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
  case ast_LK_Label: {

    *com_vec_push_m(&ls->_elements, LabelStackElement) =
        (LabelStackElement){.scope = scope,
                            .label = label->label.label,
                            .defers = com_queue_create(hir_alloc_vec_m(a))};
    return true;
  }
  }
  com_assert_unreachable_m("label kind is invalid") ;
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
  com_assert_unreachable_m("label kind is invalid") ;
}

// returns a com_vec of hir_Exprs
static com_vec LabelStack_popLabel(LabelStack *ls) {
  LabelStackElement top;
  com_vec_pop_m(&ls->_elements, &top, LabelStackElement);
  return com_queue_release(&top.defers);
}

// Forward Declare
static hir_Expr *hir_translateExpr(const ast_Expr *vep, LabelStack *ls,
                                   DiagnosticLogger *diagnostics,
                                   com_allocator *a);

// Forward Declare
static hir_Pat *hir_translatePat(const ast_Expr *vep, LabelStack *ls,
                                 DiagnosticLogger *diagnostics,
                                 com_allocator *a);

static hir_Expr *hir_referenceExpr(const ast_Expr *from, com_allocator *a,
                                   com_str ref) {
  hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
  obj->from = from;
  obj->kind = hir_EK_Reference;
  obj->reference.reference = ref;
  return obj;
}

static hir_Expr *hir_createIntLiteralExpr(const ast_Expr *from,
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

static hir_Expr *hir_createBoolLiteralExpr(const ast_Expr *from,
                                           com_allocator *a, bool lit) {
  hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
  obj->from = from;
  obj->kind = hir_EK_Bool;
  obj->boolLiteral.value = lit;
  return obj;
}

static hir_Expr *hir_applyExpr(const ast_Expr *from, com_allocator *a,
                               hir_Expr *fn, hir_Expr *param) {
  hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
  obj->from = from;
  obj->kind = hir_EK_Apply;
  obj->apply.fn = fn;
  obj->apply.param = param;
  return obj;
}

// apply a function twice
static hir_Expr *hir_applyTwoExpr(const ast_Expr *from, com_allocator *a,
                                  hir_Expr *fn, hir_Expr *param1,
                                  hir_Expr *param2) {
  return hir_applyExpr(from, a, hir_applyExpr(from, a, fn, param1), param2);
}

// Translates a binary operation into a function application, left to right
static hir_Expr *hir_translateBinOpExpr(const ast_Expr *from, LabelStack *ls,
                                        DiagnosticLogger *diagnostics,
                                        com_allocator *a, hir_Expr *fn) {
  com_assert_m(from->kind == ast_EK_BinaryOp,
               "provided ast_expr is not a bin op");
  return hir_applyTwoExpr(
      from, a, fn,
      hir_translateExpr(from->binaryOp.left_operand, ls, diagnostics, a),
      hir_translateExpr(from->binaryOp.right_operand, ls, diagnostics, a));
}

// Translates a binary operation into a function application
static hir_Expr *hir_translateReferenceBinOpExpr(const ast_Expr *from,
                                                 LabelStack *ls,
                                                 DiagnosticLogger *diagnostics,
                                                 com_allocator *a,
                                                 com_str fname) {
  return hir_translateBinOpExpr(from, ls, diagnostics, a,
                                hir_referenceExpr(from, a, fname));
}

static hir_Expr *hir_caseOptionExpr(const ast_Expr *from, com_allocator *a,
                                    hir_Pat *pattern, hir_Expr *result) {

  // create a case option
  hir_Expr *co = hir_alloc_obj_m(a, hir_Expr);
  co->kind = hir_EK_CaseOption;
  co->from = from;
  co->caseoption.pattern = pattern;
  co->caseoption.result = result;
  return co;
}

// Forward declaration
static hir_Pat *hir_exprPat(const ast_Expr *from, com_allocator *a,
                            hir_Expr *expr);

// returns an instantiated
static hir_Expr *hir_simpleExpr(const ast_Expr *from, com_allocator *a,
                                hir_ExprKind ek) {
  hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
  obj->from = from;
  obj->kind = ek;
  return obj;
}

static hir_Expr *hir_noneExpr(const ast_Expr *from, com_allocator *a) {
  return hir_simpleExpr(from, a, hir_EK_None);
}

static hir_Expr *hir_translateExpr(const ast_Expr *vep, LabelStack *ls,
                                   DiagnosticLogger *diagnostics,
                                   com_allocator *a) {
  com_assert_m(vep != NULL, "source is null");

  switch (vep->kind) {
  case ast_EK_None: {
    return hir_noneExpr(vep, a);
  }
  case ast_EK_Bind: {
    *dlogger_append(diagnostics, true) = (Diagnostic){
        .span = vep->common.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("bind is only valid in a pattern"),
        .children_len = 0};
    return hir_noneExpr(vep, a);
  }
  case ast_EK_BindIgnore: {
    *dlogger_append(diagnostics, true) = (Diagnostic){
        .span = vep->common.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("bind ignore is only valid in a pattern"),
        .children_len = 0};
    return hir_noneExpr(vep, a);
  }
  case ast_EK_BindSplat: {
    *dlogger_append(diagnostics, true) = (Diagnostic){
        .span = vep->common.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("bind splat is only valid in a pattern"),
        .children_len = 0};
    return hir_noneExpr(vep, a);
  }
  case ast_EK_Void: {
    return hir_simpleExpr(vep, a, hir_EK_Void);
  }
  case ast_EK_VoidType: {
    return hir_simpleExpr(vep, a, hir_EK_VoidType);
  }
  case ast_EK_NeverType: {
    return hir_simpleExpr(vep, a, hir_EK_NeverType);
  }
  case ast_EK_Bool: {
    return hir_createBoolLiteralExpr(vep, a, vep->boolLiteral.value);
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
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    obj->kind = hir_EK_Group;
    obj->group.expr = hir_translateExpr(vep->group.expr, ls, diagnostics, a);
    return obj;
  }
  case ast_EK_String: {
    // construct recursive data structure containing all functions
    // Apply "," with each character

    // the final element of the list is void
    hir_Expr *tail = hir_simpleExpr(vep, a, hir_EK_Void);
    for (usize i_plus_one = vep->stringLiteral.value.len; i_plus_one > 0;
         i_plus_one--) {
      usize i = i_plus_one - 1;
      // start from end of string

      // tail = str[i] : tail
      // clang-format off
      tail = hir_applyTwoExpr(vep, a, 
          hir_referenceExpr(vep, a, com_str_lit_m(",")),
          hir_createIntLiteralExpr(vep, a, vep->stringLiteral.value.data[i]),
          tail);
      // clang-format on
    }
    return tail;
  }
  case ast_EK_Loop: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    obj->kind = hir_EK_Loop;
    obj->loop.expr = hir_translateExpr(vep->loop.body, ls, diagnostics, a);
    return obj;
  }
  case ast_EK_Pat: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    obj->kind = hir_EK_Pat;
    obj->pat.pattern = hir_translatePat(vep->pat.pat, ls, diagnostics, a);
    return obj;
  }
  case ast_EK_Val: {
    *dlogger_append(diagnostics, true) = (Diagnostic){
        .span = vep->common.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("val expr is only valid in a pattern"),
        .children_len = 0};
    return hir_noneExpr(vep, a);
  }
  case ast_EK_Label: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    obj->kind = hir_EK_Label;
    // push new label element
    bool didPushLabel = LabelStack_pushLabel(ls, obj, vep->label.label, a);

    // IMPORTANT!
    // if expr is group, then we evaluate the group's body
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
      obj->label.defer_len = com_vec_len_m(&defers, hir_Expr *);
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
      return hir_noneExpr(vep, a);
    }
  }
  case ast_EK_Defer: {
    LabelStackElement *lse =
        LabelStack_getLabel(ls, vep->ret.label, diagnostics);
    if (lse != NULL) {
      *com_queue_push_m(&lse->defers, hir_Expr *) =
          hir_translateExpr(vep->defer.val, ls, diagnostics, a);
      // now return void
      return hir_simpleExpr(vep, a, hir_EK_Void);
    } else {
      return hir_noneExpr(vep, a);
    }
  }
  case ast_EK_Struct: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    obj->kind = hir_EK_StructLiteral;
    obj->structLiteral.expr =
        hir_translateExpr(vep->structLiteral.expr, ls, diagnostics, a);
    return obj;
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
    return obj;
  }
  case ast_EK_IfThen: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    obj->kind = hir_EK_CaseOf;
    obj->caseof.expr = hir_translateExpr(vep->ifthen.expr, ls, diagnostics, a);
    obj->caseof.cases_len = 2;
    obj->caseof.cases = hir_alloc(a, sizeof(hir_Expr) * 2);
    // first case is if the condition is true
    obj->caseof.cases[0] = hir_caseOptionExpr(
        vep->ifthen.then_expr, a,
        hir_exprPat(vep->ifthen.then_expr, a,
                    hir_createBoolLiteralExpr(vep->ifthen.then_expr, a, true)),
        hir_translateExpr(vep->ifthen.then_expr, ls, diagnostics, a));

    // second case is if it is false
    obj->caseof.cases[1] = hir_caseOptionExpr(
        vep->ifthen.else_expr, a,
        hir_exprPat(vep->ifthen.else_expr, a,
                    hir_createBoolLiteralExpr(vep->ifthen.else_expr, a, false)),
        hir_translateExpr(vep->ifthen.else_expr, ls, diagnostics, a));

    return obj;
  }
  case ast_EK_CaseOf: {
    hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
    obj->from = vep;
    obj->kind = hir_EK_CaseOf;
    obj->caseof.expr = hir_translateExpr(vep->caseof.expr, ls, diagnostics, a);

    // we will store all cases in here
    // cases :: Vector(hir_Expr.&)
    com_vec cases = hir_alloc_vec_m(a);

    // do depth first on this binary op tree
    // optstack  :: Stack(ast_Expr.&)
    com_vec optstack = hir_alloc_vec_m(a);
    *com_vec_push_m(&optstack, const ast_Expr *) = vep;
    // while the stack isn't empty
    while (com_vec_len_m(&optstack, const ast_Expr *) != 0) {
      // pop the first one off the stack
      const ast_Expr *current;
      com_vec_pop_m(&optstack, &current, const ast_Expr *);

      if (current->kind == ast_EK_BinaryOp &&
          current->binaryOp.op == ast_EBOK_Defun) {

        // push the translated option to the vec
        *com_vec_push_m(&cases, hir_Expr *) = hir_caseOptionExpr(
            current, a,
            hir_translatePat(vep->binaryOp.left_operand, ls, diagnostics, a),
            hir_translateExpr(vep->binaryOp.right_operand, ls, diagnostics, a));

      } else if (current->kind == ast_EK_BinaryOp &&
                 current->binaryOp.op == ast_EBOK_CaseOption) {
        // push both the left and right operands to the stack
        *com_vec_push_m(&optstack, const ast_Expr *) =
            current->binaryOp.left_operand;
        *com_vec_push_m(&optstack, const ast_Expr *) =
            current->binaryOp.right_operand;
      } else {
        // is neither
        *dlogger_append(diagnostics, true) =
            (Diagnostic){.span = current->common.span,
                         .severity = DSK_Error,
                         .message = com_str_lit_m("expected a case option"),
                         .children_len = 0};
      }
    }
    // now destroy the optstack
    com_vec_destroy(&optstack);

    // release cases
    obj->caseof.cases_len = com_vec_len_m(&cases, hir_Expr *);
    obj->caseof.cases = com_vec_release(&cases);

    return obj;
  }
  case ast_EK_BinaryOp: {
    // we branch here on the differnet operators
    switch (vep->binaryOp.op) {
    // none
    case ast_EBOK_None: {
      return hir_noneExpr(vep, a);
    }
    case ast_EBOK_As: {
      *dlogger_append(diagnostics, true) = (Diagnostic){
          .span = vep->common.span,
          .severity = DSK_Error,
          .message = com_str_lit_m("as operator is only valid in a pattern"),
          .children_len = 0};
      return hir_noneExpr(vep, a);
    }
    // Type coercion
    case ast_EBOK_Constrain: {
      *dlogger_append(diagnostics, true) =
          (Diagnostic){.span = vep->common.span,
                       .severity = DSK_Error,
                       .message = com_str_lit_m(
                           "constrain operator is only valid in a pattern"),
                       .children_len = 0};
      return hir_noneExpr(vep, a);
    }
    // Function definition
    case ast_EBOK_Defun: {
      hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
      obj->from = vep;
      obj->kind = hir_EK_Defun;
      obj->defun.pattern =
          hir_translatePat(vep->binaryOp.left_operand, ls, diagnostics, a);
      obj->defun.value =
          hir_translateExpr(vep->binaryOp.right_operand, ls, diagnostics, a);
      return obj;
    }
    // CaseOption
    case ast_EBOK_CaseOption: {
      *dlogger_append(diagnostics, true) = (Diagnostic){
          .span = vep->common.span,
          .severity = DSK_Error,
          .message = com_str_lit_m(
              "case option operator is only valid in a case context"),
          .children_len = 0};
      return hir_noneExpr(vep, a);
    }
    // Function call
    case ast_EBOK_Apply: {
      return hir_applyExpr(
          vep, a,
          hir_translateExpr(vep->binaryOp.left_operand, ls, diagnostics, a),
          hir_translateExpr(vep->binaryOp.right_operand, ls, diagnostics, a));
    }
    // Reverse application (Userspace)
    case ast_EBOK_RevApply: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("."));
    }
    // Booleans
    // TODO type constrain right side to boolean
    case ast_EBOK_And: {
      hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
      obj->from = vep;
      obj->kind = hir_EK_CaseOf;
      // the expr being considered is the left operand
      obj->caseof.expr =
          hir_translateExpr(vep->binaryOp.left_operand, ls, diagnostics, a);
      obj->caseof.cases_len = 2;
      obj->caseof.cases = hir_alloc(a, sizeof(hir_Expr) * 2);
      // first case is if the condition is true
      obj->caseof.cases[0] = hir_caseOptionExpr(
          vep, a, hir_exprPat(vep, a, hir_createBoolLiteralExpr(vep, a, true)),
          hir_translateExpr(vep->binaryOp.right_operand, ls, diagnostics, a));

      // second case is if it is false
      obj->caseof.cases[1] = hir_caseOptionExpr(
          vep, a, hir_exprPat(vep, a, hir_createBoolLiteralExpr(vep, a, false)),
          hir_createBoolLiteralExpr(vep, a, false));
      return obj;
    }
    case ast_EBOK_Or: {
      hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
      obj->from = vep;
      obj->kind = hir_EK_CaseOf;
      // the expr being considered is the left operand
      obj->caseof.expr =
          hir_translateExpr(vep->binaryOp.left_operand, ls, diagnostics, a);
      obj->caseof.cases_len = 2;
      obj->caseof.cases = hir_alloc(a, sizeof(hir_Expr) * 2);
      // first case is if the condition is true
      obj->caseof.cases[0] = hir_caseOptionExpr(
          vep, a, hir_exprPat(vep, a, hir_createBoolLiteralExpr(vep, a, true)),
          hir_createBoolLiteralExpr(vep, a, true));

      // second case is if it is false
      obj->caseof.cases[1] = hir_caseOptionExpr(
          vep, a, hir_exprPat(vep, a, hir_createBoolLiteralExpr(vep, a, false)),
          hir_translateExpr(vep->binaryOp.right_operand, ls, diagnostics, a));
      return obj;
    }
    // Function composition (Userspace)
    case ast_EBOK_Compose: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m(">>"));
    }
    // Function Piping
    case ast_EBOK_PipeForward: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("|>"));
    }
    case ast_EBOK_PipeBackward: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("<|"));
    }
    // Math
    case ast_EBOK_Add: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("+"));
    }
    case ast_EBOK_Sub: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("-"));
    }
    case ast_EBOK_Mul: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("*"));
    }
    case ast_EBOK_Div: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("/"));
    }
    case ast_EBOK_Rem: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("%"));
    }
    case ast_EBOK_Pow: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("^"));
    }
    // Comparison
    case ast_EBOK_CompEqual: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("=="));
    }
    case ast_EBOK_CompNotEqual: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("/="));
    }
    case ast_EBOK_CompLess: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("<"));
    }
    case ast_EBOK_CompLessEqual: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("<="));
    }
    case ast_EBOK_CompGreater: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m(">"));
    }
    case ast_EBOK_CompGreaterEqual: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m(">="));
    }
    // Set Operations
    case ast_EBOK_Union: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("/\\"));
    }
    case ast_EBOK_Intersection: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("\\/"));
    }
    case ast_EBOK_Difference: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("--"));
    }
    case ast_EBOK_In: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("in"));
    }
    // Type Manipulation
    case ast_EBOK_Cons: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m(","));
    }
    case ast_EBOK_Sum: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("|"));
    }
      // Range
    case ast_EBOK_Range: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m(".."));
    }
    case ast_EBOK_RangeInclusive: {
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("..="));
    }
    // Assign
    case ast_EBOK_Assign: {
      hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
      obj->from = vep;
      obj->kind = hir_EK_Assign;
      obj->assign.pattern =
          hir_translateExpr(vep->binaryOp.left_operand, ls, diagnostics, a);
      obj->assign.value =
          hir_translateExpr(vep->binaryOp.right_operand, ls, diagnostics, a);
      return obj;
    }
    // Sequence
    case ast_EBOK_Sequence: {
      // TODO
      return hir_translateReferenceBinOpExpr(vep, ls, diagnostics, a,
                                             com_str_lit_m("in"));
    }
    // Module Access
    case ast_EBOK_ModuleAccess: {
      // ensure that the right operand is an identifier
      if (vep->binaryOp.right_operand->kind != ast_EK_Reference) {
        *dlogger_append(diagnostics, true) =
            (Diagnostic){.span = vep->binaryOp.right_operand->common.span,
                         .severity = DSK_Error,
                         .message = com_str_lit_m("expected an identifier"),
                         .children_len = 0};
        return hir_noneExpr(vep, a);
      }

      switch (vep->binaryOp.right_operand->reference.reference->kind) {
      case ast_IK_None: {
        // ensure that identifier is valid
        *dlogger_append(diagnostics, true) = (Diagnostic){
            .span = vep->binaryOp.right_operand->reference.reference->span,
            .severity = DSK_Error,
            .message = com_str_lit_m("identifier must be valid"),
            .children_len = 0};
        return hir_noneExpr(vep, a);
      }
      case ast_IK_Identifier: {
        hir_Expr *obj = hir_alloc_obj_m(a, hir_Expr);
        obj->from = vep;
        obj->kind = hir_EK_ModuleAccess;
        obj->moduleAccess.module = hir_translateExpr(vep, ls, diagnostics, a);
        obj->moduleAccess.field =
            vep->binaryOp.right_operand->reference.reference->id.name;
        return obj;
      }
      }
    }
    }
  }
  }
}

static hir_Pat *hir_applyPat(const ast_Expr *from, com_allocator *a,
                             hir_Pat *fn, hir_Pat *param) {
  hir_Pat *obj = hir_alloc_obj_m(a, hir_Pat);
  obj->from = from;
  obj->kind = hir_PK_Apply;
  obj->apply.fn = fn;
  obj->apply.param = param;
  return obj;
}

// Translates a binary operation into a function application, left to right
static hir_Pat *hir_translateApplyBinOpPat(const ast_Expr *from, LabelStack *ls,
                                           DiagnosticLogger *diagnostics,
                                           com_allocator *a, hir_Pat *fn) {
  com_assert_m(from->kind == ast_EK_BinaryOp,
               "provided ast_expr is not a bin op");

  return hir_applyPat(
      from, a,
      hir_applyPat(
          from, a, fn,
          hir_translatePat(from->binaryOp.left_operand, ls, diagnostics, a)),
      hir_translatePat(from->binaryOp.right_operand, ls, diagnostics, a));
}

// creates a pat out of a expr
static hir_Pat *hir_exprPat(const ast_Expr *from, com_allocator *a,
                            hir_Expr *expr) {

  hir_Pat *obj = hir_alloc_obj_m(a, hir_Pat);
  obj->from = from;
  obj->kind = hir_PK_Expr;
  obj->expr.expr = expr;
  return obj;
}

// Translates a binary operation into a function application
static hir_Pat *hir_translateReferenceBinOpPat(const ast_Expr *from,
                                               LabelStack *ls,
                                               DiagnosticLogger *diagnostics,
                                               com_allocator *a,
                                               com_str fname) {
  return hir_translateApplyBinOpPat(
      from, ls, diagnostics, a,
      hir_exprPat(from, a, hir_referenceExpr(from, a, fname)));
}

// returns an instantiated
static hir_Pat *hir_simplePat(const ast_Expr *from, com_allocator *a,
                              hir_PatKind ek) {
  hir_Pat *obj = hir_alloc_obj_m(a, hir_Pat);
  obj->from = from;
  obj->kind = ek;
  return obj;
}

static hir_Pat *hir_nonePat(const ast_Expr *from, com_allocator *a) {
  return hir_simplePat(from, a, hir_PK_None);
}

static hir_Pat *hir_translatePat(const ast_Expr *vep, LabelStack *ls,
                                 DiagnosticLogger *diagnostics,
                                 com_allocator *a) {

  com_assert_m(vep != NULL, "source is null");
  switch (vep->kind) {
  case ast_EK_BinaryOp: {
    // we branch here on the differnet operators
    switch (vep->binaryOp.op) {
    // none
    case ast_EBOK_None: {
      return hir_nonePat(vep, a);
    }
    // Function call
    case ast_EBOK_Apply: {
      return hir_applyPat(
          vep, a,
          hir_translatePat(vep->binaryOp.left_operand, ls, diagnostics, a),
          hir_translatePat(vep->binaryOp.right_operand, ls, diagnostics, a));
    }
    // At expr
    case ast_EBOK_As: {
      // ensure rhs is a valid binding
      // ensure the binding is valid
      if (vep->binaryOp.right_operand->kind != ast_EK_Bind ||
          vep->binaryOp.right_operand->bind.bind->kind != ast_IK_Identifier) {
        *dlogger_append(diagnostics, true) =
            (Diagnostic){.span = vep->common.span,
                         .severity = DSK_Error,
                         .message = com_str_lit_m(
                             "Right hand side of as must be a valid binding"),
                         .children_len = 0};
        return hir_nonePat(vep, a);
      }
      hir_Pat *obj = hir_alloc_obj_m(a, hir_Pat);
      obj->from = vep;
      obj->kind = hir_PK_Bind;
      obj->bind.pattern =
          hir_translatePat(vep->binaryOp.left_operand, ls, diagnostics, a);
      obj->bind.name = vep->binaryOp.right_operand->bind.bind->id.name;
      return obj;
    }
    // Type coercion
    case ast_EBOK_Constrain: {
      hir_Pat *obj = hir_alloc_obj_m(a, hir_Pat);
      obj->from = vep;
      obj->kind = hir_PK_Constrain;
      obj->constrain.value =
          hir_translatePat(vep->binaryOp.left_operand, ls, diagnostics, a);
      obj->constrain.type =
          hir_translateExpr(vep->binaryOp.right_operand, ls, diagnostics, a);
      return obj;
    }
    case ast_EBOK_ModuleAccess: {
      return hir_exprPat(vep, a, hir_translateExpr(vep, ls, diagnostics, a));
    }
    // Booleans
    case ast_EBOK_And: {
      hir_Pat *obj = hir_alloc_obj_m(a, hir_Pat);
      obj->from = vep;
      obj->kind = hir_PK_And;
      obj->andPat.fst =
          hir_translatePat(vep->binaryOp.left_operand, ls, diagnostics, a);
      obj->andPat.snd =
          hir_translateExpr(vep->binaryOp.right_operand, ls, diagnostics, a);
      return obj;
    }
    case ast_EBOK_Or: {
      hir_Pat *obj = hir_alloc_obj_m(a, hir_Pat);
      obj->from = vep;
      obj->kind = hir_PK_Or;
      obj->orPat.fst =
          hir_translatePat(vep->binaryOp.left_operand, ls, diagnostics, a);
      obj->orPat.snd =
          hir_translateExpr(vep->binaryOp.right_operand, ls, diagnostics, a);
      return obj;
    }
    // Reverse application (Userspace)
    case ast_EBOK_RevApply: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("."));
    }
      // Function composition (Userspace)
    case ast_EBOK_Compose: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m(">>"));
    }
    // Function Piping
    case ast_EBOK_PipeForward: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("|>"));
    }
    case ast_EBOK_PipeBackward: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("<|"));
    }
    // Math
    case ast_EBOK_Add: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("+"));
    }
    case ast_EBOK_Sub: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("-"));
    }
    case ast_EBOK_Mul: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("*"));
    }
    case ast_EBOK_Div: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("/"));
    }
    case ast_EBOK_Rem: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("%"));
    }
    case ast_EBOK_Pow: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("^"));
    }
    // Comparison
    case ast_EBOK_CompEqual: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("=="));
    }
    case ast_EBOK_CompNotEqual: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("/="));
    }
    case ast_EBOK_CompLess: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("<"));
    }
    case ast_EBOK_CompLessEqual: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("<="));
    }
    case ast_EBOK_CompGreater: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m(">"));
    }
    case ast_EBOK_CompGreaterEqual: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m(">="));
    }
    // Set Operations
    case ast_EBOK_Union: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("/\\"));
    }
    case ast_EBOK_Intersection: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("\\/"));
    }
    case ast_EBOK_Difference: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("--"));
    }
    case ast_EBOK_In: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("in"));
    }
    // Type Manipulation
    case ast_EBOK_Cons: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m(","));
    }
    case ast_EBOK_Sum: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("|"));
    }
      // Range
    case ast_EBOK_Range: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m(".."));
    }
    case ast_EBOK_RangeInclusive: {
      return hir_translateReferenceBinOpPat(vep, ls, diagnostics, a,
                                            com_str_lit_m("..="));
    }
    // Function definition
    case ast_EBOK_Defun:
    // CaseOption
    case ast_EBOK_CaseOption:
    // Assign
    case ast_EBOK_Assign:
    // Sequence
    case ast_EBOK_Sequence: {
      *dlogger_append(diagnostics, true) = (Diagnostic){
          .span = vep->common.span,
          .severity = DSK_Error,
          .message = com_str_lit_m("operator not permitted in pattern"),
          .children_len = 0};
      return hir_nonePat(vep, a);
    }
    }
  }
  case ast_EK_None: {
    return hir_nonePat(vep, a);
  }
  case ast_EK_Bind: {
    switch (vep->bind.bind->kind) {
    case ast_IK_Identifier: {
      hir_Pat *obj = hir_alloc_obj_m(a, hir_Pat);
      obj->from = vep;
      obj->kind = hir_PK_Bind;
      obj->bind.pattern = hir_simplePat(vep, a, hir_PK_BindIgnore);
      obj->bind.name = vep->bind.bind->id.name;
      return obj;
    }
    case ast_IK_None: {
      return hir_nonePat(vep, a);
    }
    }
  }
  case ast_EK_BindSplat: {
    return hir_simplePat(vep, a, hir_PK_BindSplat);
  }
  case ast_EK_BindIgnore: {
    return hir_simplePat(vep, a, hir_PK_BindIgnore);
  }
  case ast_EK_Group: {
    return hir_translatePat(vep->group.expr, ls, diagnostics, a);
  }
  case ast_EK_Val: {
    return hir_exprPat(vep, a,
                       hir_translateExpr(vep->val.val, ls, diagnostics, a));
  }
  // struct
  case ast_EK_Struct: {
    // construct recursive data structure where each field in the struct is a
    // condition Apply `and` with each field mentioned

    // this operation loses knowledge about the grouping of the sequences
    // this whole operation takes advantage of the fact that the grouping doesn't matter in this case
    // since sequences are always evaluated from left to right
    // (or does it matter?) TODO figure this out

    // the final element of the `and` expression is always true
    hir_Pat *tail =
        hir_exprPat(vep, a, hir_createBoolLiteralExpr(vep, a, true));

    // do depth first on the sequenceable tree
    // sequences :: Stack(ast_Expr.&)
    com_vec sequences = hir_alloc_vec_m(a);
    *com_vec_push_m(&sequences, const ast_Expr *) = vep;

    while (com_vec_len_m(&sequences, const ast_Expr *) != 0) {
      const ast_Expr *current;
      com_vec_pop_m(&sequences, &current, const ast_Expr *);

      // if it is a sequence, then we push both children on the stack
      if (current->kind == ast_EK_BinaryOp &&
          current->binaryOp.op == ast_EBOK_Sequence) {
        // push both the left and right operands to the stack
        *com_vec_push_m(&sequences, const ast_Expr *) =
            current->binaryOp.left_operand;
        *com_vec_push_m(&sequences, const ast_Expr *) =
            current->binaryOp.right_operand;
      } else if (current->kind == ast_EK_BinaryOp &&
                 current->binaryOp.op == ast_EBOK_Assign) {
        // if it is an assign, we need push it to structEntries
        hir_Pat *obj = hir_alloc_obj_m(a, hir_Pat);
        obj->from = current;
        obj->kind = hir_PK_StructEntry;
        obj->structEntry.field = hir_translatePat(
            current->binaryOp.left_operand, ls, diagnostics, a);
        obj->structEntry.pattern = hir_translatePat(
            current->binaryOp.right_operand, ls, diagnostics, a);

        // tail = tail and obj

        hir_Pat* oldtail = tail;
        tail = hir_alloc_obj_m(a, hir_Pat);
        tail->from = current;
        tail->kind = hir_PK_And;
        tail->andPat.fst = oldtail;
        tail->structEntry.pattern = obj;
      } else {
        *dlogger_append(diagnostics, true) =
            (Diagnostic){.span = current->common.span,
                         .severity = DSK_Error,
                         .message = com_str_lit_m("only assigns are permitted in a struct pattern"),
                         .children_len = 0};
      }
    }

    com_vec_destroy(&sequences);
    return tail;
  }
  // these are automatically promoted to an expr
  case ast_EK_Int:
  case ast_EK_Real:
  case ast_EK_Bool:
  case ast_EK_String:
  case ast_EK_Reference:
  case ast_EK_Nil:
  case ast_EK_NilType:
  case ast_EK_NeverType: {
    return hir_exprPat(vep, a, hir_translateExpr(vep, ls, diagnostics, a));
  }
  // any other expression is illegal
  case ast_EK_Label:
  case ast_EK_Loop:
  case ast_EK_Defer:
  case ast_EK_Ret:
  case ast_EK_IfThen:
  case ast_EK_CaseOf:
  case ast_EK_Pat: {
    *dlogger_append(diagnostics, true) = (Diagnostic){
        .span = vep->common.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("expression not permitted in pattern"),
        .children_len = 0};
    return hir_nonePat(vep, a);
  }
  }
}

hir_Expr *hir_constructExpr(const ast_Expr *vep, DiagnosticLogger *diagnostics,
                            com_allocator *a) {
  LabelStack ls = LabelStack_create(a);
  hir_Expr *val = hir_translateExpr(vep->label.val, &ls, diagnostics, a);
  LabelStack_destroy(&ls);
  return val;
}
