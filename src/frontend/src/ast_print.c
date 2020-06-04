#include "ast_print.h"

#include "allocator.h"
#include "arena_allocator.h"
#include "ast.h"
#include "json.h"
#include "token.h"

static j_Elem lnColJson(LnCol lncol, Allocator *a) {
  j_Prop *ptrs = ALLOC_ARR(a, 2, j_Prop);
  ptrs[0] = J_PROP(J_ASCIZ("ln"), J_INT_ELEM(J_UINT(lncol.ln.val)));
  ptrs[1] = J_PROP(J_ASCIZ("col"), J_INT_ELEM(J_UINT(lncol.col.val)));
  return J_OBJECT_ELEM(ptrs, 2);
}

static j_Elem spanJson(Span span, Allocator *a) {
  return J_NULL_ELEM;
  j_Prop *ptrs = ALLOC_ARR(a, 2, j_Prop);

  ptrs[0] = J_PROP(J_ASCIZ("start"), lnColJson(span.start, a));
  ptrs[1] = J_PROP(J_ASCIZ("end"), lnColJson(span.end, a));
  return J_OBJECT_ELEM(ptrs, 2);
}

static j_Elem diagnosticJson(Diagnostic diagnostic, Allocator *a) {
  j_Prop *ptrs = ALLOC_ARR(a, 2, j_Prop);

  ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ(strDiagnosticKind(diagnostic.kind))));
  ptrs[1] = J_PROP(J_ASCIZ("span"), spanJson(diagnostic.span, a));
  return J_OBJECT_ELEM(ptrs, 2);
}

static j_Elem commentJson(Comment comment, Allocator *a) {
  j_Prop *ptrs = ALLOC_ARR(a, 3, j_Prop);
  ptrs[0] = J_PROP(J_ASCIZ("scope"), J_STR_ELEM(J_ASCIZ(comment.scope)));
  ptrs[1] = J_PROP(J_ASCIZ("comment"), J_STR_ELEM(J_ASCIZ(comment.data)));
  ptrs[2] = J_PROP(J_ASCIZ("span"), spanJson(comment.span, a));
  return J_OBJECT_ELEM(ptrs, 3);
}

static j_Elem commentsJson(Comment *comments, size_t comments_len,
                             Allocator *a) {
  j_Elem *ptrs = ALLOC_ARR(a, comments_len, j_Elem);
  for (size_t i = 0; i < comments_len; i++) {
    ptrs[i] = commentJson(comments[i], a);
  }
  return J_ARRAY_ELEM(ptrs, comments_len);
}

static j_Elem stmntJson(Stmnt *s, Allocator *a);
static j_Elem typeExprJson(TypeExpr *tep, Allocator *a);
static j_Elem valueExprJson(ValueExpr *vep, Allocator *a);
static j_Elem patternExprJson(PatternExpr *pep, Allocator *a);
static j_Elem builtinJson(Builtin *bp, Allocator *a);
static j_Elem
patternStructMemberExprJson(struct PatternStructMemberExpr_s *psmep, Allocator *a);
static j_Elem typeStructMemberExprJson(struct TypeStructMemberExpr_s *tsmep,
                                         Allocator *a);
static j_Elem valueStructMemberExprJson(struct ValueStructMemberExpr_s *vsmep,
                                          Allocator *a);
static j_Elem matchCaseExprJson(struct MatchCaseExpr_s *mcep, Allocator *a);

static j_Elem builtinJson(Builtin *bp, Allocator *a) {
  if (bp == NULL) {
    return J_NULL_ELEM;
  }
  size_t ptrs_len = 4;
  j_Prop *ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
  ptrs[0] = J_PROP(J_ASCIZ("span"), spanJson(bp->span, a));
  ptrs[1] =
      J_PROP(J_ASCIZ("comments"), commentsJson(bp->comments, bp->comments_len, a));
  ptrs[2] = J_PROP(J_ASCIZ("name"), J_STR_ELEM(J_ASCIZ(bp->name)));

  j_Elem *parameter_ptrs = ALLOC_ARR(a, bp->parameters_len, j_Elem);
  for (size_t i = 0; i < bp->parameters_len; i++) {
    parameter_ptrs[i] = stmntJson(&bp->parameters[i], a);
  }
  ptrs[3] =
      J_PROP(J_ASCIZ("parameters"), J_ARRAY_ELEM(parameter_ptrs, bp->parameters_len));
  return J_OBJECT_ELEM(ptrs, ptrs_len);
}

static j_Elem patternExprJson(PatternExpr *pp, Allocator *a) {
  if (pp == NULL) {
    return J_NULL_ELEM;
  }

  size_t ptrs_len = 0;
  j_Prop *ptrs = NULL;
  // 1 reserved for span
  // 2 reserved for comments
  switch (pp->kind) {
  case PEK_None: {
    ptrs_len = 3;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("PEK_None")));
    break;
  }
  case PEK_Group: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("PEK_Group")));
    ptrs[3] = J_PROP(J_ASCIZ("value"), patternExprJson(pp->groupExpr.value, a));
    break;
  }
  case PEK_ValueRestriction: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("PEK_ValueRestriction")));
    char *pevrk;
    switch (pp->valueRestriction.restriction) {
    case PEVRK_CompEqual: {
      pevrk = "PEVRK_CompEqual";
      break;
    }
    case PEVRK_CompNotEqual: {
      pevrk = "PEVRK_CompNotEqual";
      break;
    }
    case PEVRK_CompLess: {
      pevrk = "PEVRK_CompLess";
      break;
    }
    case PEVRK_CompLessEqual: {
      pevrk = "PEVRK_CompLessEqual";
      break;
    }
    case PEVRK_CompGreater: {
      pevrk = "PEVRK_CompGreater";
      break;
    }
    case PEVRK_CompGreaterEqual: {
      pevrk = "PEVRK_CompGreaterEqual";
      break;
    }
    }
    ptrs[3] = J_PROP(J_ASCIZ("restriction"), J_STR_ELEM(J_ASCIZ(pevrk)));
    ptrs[4] =
        J_PROP(J_ASCIZ("value"), valueExprJson(pp->valueRestriction.valueExpr, a));
    break;
  }
  case PEK_TypeRestriction: {
    if (pp->typeRestriction.has_binding) {
      ptrs_len = 6;
      ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
      ptrs[5] = J_PROP(J_ASCIZ("binding"),
                       J_STR_ELEM(J_ASCIZ(pp->typeRestriction.binding)));
    } else {
      ptrs_len = 5;
      ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    }

    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("PEK_TypeRestriction")));
    ptrs[3] = J_PROP(J_ASCIZ("type"), typeExprJson(pp->typeRestriction.type, a));
    ptrs[4] = J_PROP(J_ASCIZ("has_binding"), J_BOOL_ELEM(pp->typeRestriction.has_binding));
    break;
  }
  case PEK_UnaryOp: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("PEK_UnaryOp")));
    char *peuok;
    switch (pp->unaryOp.operator) {
    case PEUOK_Not: {
      peuok = "PEUOK_Not";
      break;
    }
    }
    ptrs[3] = J_PROP(J_ASCIZ("operator"), J_STR_ELEM(J_ASCIZ(peuok)));
    ptrs[4] = J_PROP(J_ASCIZ("operand"), patternExprJson(pp->unaryOp.operand, a));
    break;
  }
  case PEK_BinaryOp: {
    ptrs_len = 6;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("PEK_BinaryOp")));
    char *pebok;
    switch (pp->binaryOp.operator) {
    case PEBOK_Tuple: {
      pebok = "PEBOK_Tuple";
      break;
    }
    case PEBOK_Union: {
      pebok = "PEBOK_Union";
      break;
    }
    case PEBOK_And: {
      pebok = "PEBOK_And";
      break;
    }
    case PEBOK_Or: {
      pebok = "PEBOK_Or";
      break;
    }
    }
    ptrs[3] = J_PROP(J_ASCIZ("operator"), J_STR_ELEM(J_ASCIZ(pebok)));
    ptrs[4] = J_PROP(J_ASCIZ("left_operand"), patternExprJson(pp->binaryOp.left_operand, a));
    ptrs[5] = J_PROP(J_ASCIZ("right_operand"), patternExprJson(pp->binaryOp.right_operand, a));
    break;
  }
  case PEK_Struct: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("PEK_Struct")));
    size_t members_len = pp->structExpr.members_len;
    j_Elem *members = ALLOC_ARR(a, members_len, j_Elem);
    for(size_t i = 0; i < members_len; i++) {
      members[i] = patternStructMemberExprJson(&pp->structExpr.members[i], a);
    }
    ptrs[3] = J_PROP(J_ASCIZ("value"), J_ARRAY_ELEM(members, members_len));
    break;
  }
  }
  ptrs[1] = J_PROP(J_ASCIZ("span"), spanJson(pp->span, a));
  ptrs[2] =
      J_PROP(J_ASCIZ("comments"), commentsJson(pp->comments, pp->comments_len, a));
  return J_OBJECT_ELEM(ptrs, ptrs_len);
}

static j_Elem pathJson(Path *pp, Allocator *a) {
  if (pp == NULL) {
    return J_NULL_ELEM;
  }

  size_t ptrs_len = 3;
  j_Prop *ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
  ptrs[0] = J_PROP(J_ASCIZ("span"), spanJson(pp->span, a));
  ptrs[1] =
      J_PROP(J_ASCIZ("comments"), commentsJson(pp->comments, pp->comments_len, a));
  j_Elem *path_ptrs = ALLOC_ARR(a, pp->pathSegments_len, j_Elem);
  for (size_t i = 0; i < pp->pathSegments_len; i++) {
    path_ptrs[i] = J_STR_ELEM(J_ASCIZ(pp->pathSegments[i]));
  }
  ptrs[2] =
      J_PROP(J_ASCIZ("path_segments"), J_ARRAY_ELEM(path_ptrs, pp->pathSegments_len));
  return J_OBJECT_ELEM(ptrs, ptrs_len);
}

static j_Elem typeExprJson(TypeExpr *tep, Allocator *a) {
  if (tep == NULL) {
    return J_NULL_ELEM;
  }

  size_t ptrs_len = 0;
  j_Prop *ptrs = NULL;
  // 1 reserved for span
  // 3 reserved for comments
  switch (tep->kind) {
  case TEK_None: {
    ptrs_len = 3;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("TEK_None")));
    break;
  }
  case TEK_Omitted: {
    ptrs_len = 3;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("TEK_Omitted")));
    break;
  }
  case TEK_Builtin: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("TEK_Builtin")));
    ptrs[3] = J_PROP(J_ASCIZ("builtin"), builtinJson(tep->builtinExpr.builtin, a));
    break;
  }
  case TEK_Void: {
    ptrs_len = 3;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("TEK_Void")));
    break;
  }
  case TEK_Reference: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("TEK_Reference")));
    ptrs[3] = J_PROP(J_ASCIZ("path"), pathJson(tep->referenceExpr.path, a));
    break;
  }
  case TEK_Struct: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("TEK_StructExpr")));

    // struct kind
    char *structKind;
    switch (tep->structExpr.kind) {
    case TSEK_Struct: {
      structKind = "TSEK_Struct";
      break;
    }
    case TSEK_Enum: {
      structKind = "TSEK_Enum";
      break;
    }
    }
    ptrs[3] = J_PROP(J_ASCIZ("struct_kind"), J_STR_ELEM(J_ASCIZ(structKind)));

    // Embed array
    size_t len = tep->structExpr.members_len;
    j_Elem *array = ALLOC_ARR(a, len, j_Elem);
    for (size_t i = 0; i < len; i++) {
      array[i] = typeStructMemberExprJson(&tep->structExpr.members[i], a);
    }
    ptrs[4] = J_PROP(J_ASCIZ("members"), J_ARRAY_ELEM(array, len));
    break;
  }
  case TEK_UnaryOp: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("TEK_UnaryOp")));
    char *unOpStr;
    switch (tep->unaryOp.operator) {
    case TEUOK_Ref: {
      unOpStr = "TEUOK_Ref";
      break;
    }
    case TEUOK_Deref: {
      unOpStr = "TEUOK_Deref";
      break;
    }
    }
    ptrs[3] = J_PROP(J_ASCIZ("operator"), J_STR_ELEM(J_ASCIZ(unOpStr)));
    ptrs[4] = J_PROP(J_ASCIZ("operand"), typeExprJson(tep->unaryOp.operand, a));
    break;
  }
  case TEK_BinaryOp: {
    ptrs_len = 6;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("TEK_BinaryOp")));
    char *binOpStr;
    switch (tep->binaryOp.operator) {
    case TEBOK_Tuple: {
      binOpStr = "TEBOK_Tuple";
      break;
    }
    case TEBOK_Union: {
      binOpStr = "TEBOK_Union";
      break;
    }
    }
    ptrs[3] = J_PROP(J_ASCIZ("operator"), J_STR_ELEM(J_ASCIZ(binOpStr)));
    ptrs[4] =
        J_PROP(J_ASCIZ("left_operand"), typeExprJson(tep->binaryOp.left_operand, a));
    ptrs[5] =
        J_PROP(J_ASCIZ("right_operand"), typeExprJson(tep->binaryOp.right_operand, a));
    break;
  }

  case TEK_FieldAccess: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("TEK_FieldAccess")));
    ptrs[3] = J_PROP(J_ASCIZ("value"), typeExprJson(tep->fieldAccess.value, a));
    ptrs[4] = J_PROP(J_ASCIZ("field"), J_STR_ELEM(J_ASCIZ(tep->fieldAccess.field)));
    break;
  }
  case TEK_Fn: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("TEK_Fn")));

    // Embed array
    size_t len = tep->fnExpr.parameters_len;
    j_Elem *array = ALLOC_ARR(a, len, j_Elem);
    for (size_t i = 0; i < len; i++) {
      array[i] = typeExprJson(&tep->fnExpr.parameters[i], a);
    }
    ptrs[3] = J_PROP(J_ASCIZ("parameters"), J_ARRAY_ELEM(array, len));
    ptrs[4] = J_PROP(J_ASCIZ("result"), typeExprJson(tep->fnExpr.type, a));
    break;
  }
  }
  ptrs[1] = J_PROP(J_ASCIZ("span"), spanJson(tep->span, a));
  ptrs[2] =
      J_PROP(J_ASCIZ("comments"), commentsJson(tep->comments, tep->comments_len, a));
  return J_OBJECT_ELEM(ptrs, ptrs_len);
}

static j_Elem valueStructMemberExprJson(struct ValueStructMemberExpr_s *vsmep,
                                          Allocator *a) {
  if (vsmep == NULL) {
    return J_NULL_ELEM;
  }
  size_t ptrs_len = 4;
  j_Prop *ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
  ptrs[0] = J_PROP(J_ASCIZ("span"), spanJson(vsmep->span, a));
  ptrs[1] = J_PROP(J_ASCIZ("name"), J_STR_ELEM(J_ASCIZ(vsmep->name)));
  ptrs[2] = J_PROP(J_ASCIZ("comments"),
                   commentsJson(vsmep->comments, vsmep->comments_len, a));
  ptrs[3] = J_PROP(J_ASCIZ("value"), valueExprJson(vsmep->value, a));
  return J_OBJECT_ELEM(ptrs, ptrs_len);
}

static j_Elem patternStructMemberExprJson(struct PatternStructMemberExpr_s *psmep,
                                        Allocator *a) {
  if (psmep == NULL) {
    return J_NULL_ELEM;
  }

  j_Prop *ptrs;
  size_t ptrs_len;
  switch (psmep->kind) {
  case PSMEK_Field: {
    ptrs_len = 6;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("PSMEK_Field")));
    ptrs[5] = J_PROP(J_ASCIZ("field"), J_STR_ELEM(J_ASCIZ(psmep->field.field)));
    break;
  }
  case PSMEK_Rest: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("PSMEK_Rest")));
  }
  }
  ptrs[3] = J_PROP(J_ASCIZ("pattern"), patternExprJson(psmep->pattern, a));

  ptrs[1] = J_PROP(J_ASCIZ("span"), spanJson(psmep->span, a));
  ptrs[2] = J_PROP(J_ASCIZ("comments"),
                   commentsJson(psmep->comments, psmep->comments_len, a));
  return J_OBJECT_ELEM(ptrs, ptrs_len);
}

static j_Elem typeStructMemberExprJson(struct TypeStructMemberExpr_s *tsmep,
                                         Allocator *a) {
  if (tsmep == NULL) {
    return J_NULL_ELEM;
  }
  size_t ptrs_len = 4;
  j_Prop *ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
  ptrs[0] = J_PROP(J_ASCIZ("span"), spanJson(tsmep->span, a));
  ptrs[1] = J_PROP(J_ASCIZ("name"), J_STR_ELEM(J_ASCIZ(tsmep->name)));
  ptrs[2] = J_PROP(J_ASCIZ("comments"),
                   commentsJson(tsmep->comments, tsmep->comments_len, a));
  ptrs[3] = J_PROP(J_ASCIZ("type"), typeExprJson(tsmep->type, a));
  return J_OBJECT_ELEM(ptrs, ptrs_len);
}

static j_Elem matchCaseExprJson(struct MatchCaseExpr_s *mcep, Allocator *a) {
  if (mcep == NULL) {
    return J_NULL_ELEM;
  }
  size_t ptrs_len = 5;
  j_Prop *ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
  ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("MatchCaseExpr")));
  ptrs[1] = J_PROP(J_ASCIZ("span"), spanJson(mcep->span, a));
  ptrs[2] =
      J_PROP(J_ASCIZ("comments"), commentsJson(mcep->comments, mcep->comments_len, a));
  ptrs[3] = J_PROP(J_ASCIZ("pattern"), patternExprJson(mcep->pattern, a));
  ptrs[4] = J_PROP(J_ASCIZ("value"), valueExprJson(mcep->value, a));
  return J_OBJECT_ELEM(ptrs, ptrs_len);
}

static j_Elem valueExprJson(ValueExpr *vep, Allocator *a) {
  if (vep == NULL) {
    return J_NULL_ELEM;
  }
  size_t ptrs_len = 0;
  j_Prop *ptrs = NULL;
  // 1 reserved for span
  // 2 reserved for diagnostic
  // 3 reserved for comments
  switch (vep->kind) {
  case VEK_None: {
    ptrs_len = 3;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_None")));
    break;
  }
  case VEK_VoidLiteral: {
    ptrs_len = 3;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_VoidLiteral")));
    break;
  }
  case VEK_BoolLiteral: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_BoolLiteral")));
    ptrs[3] = J_PROP(J_ASCIZ("value"), J_BOOL_ELEM(vep->boolLiteral.value));
    break;
  }
  case VEK_IntLiteral: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_IntLiteral")));
    ptrs[3] = J_PROP(J_ASCIZ("value"), J_INT_ELEM(J_UINT(vep->intLiteral.value)));
    break;
  }
  case VEK_CharLiteral: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_CharLiteral")));
    ptrs[3] = J_PROP(J_ASCIZ("value"), J_INT_ELEM(J_SINT(vep->charLiteral.value)));
    break;
  }
  case VEK_FloatLiteral: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_FloatLiteral")));
    ptrs[3] = J_PROP(J_ASCIZ("value"), J_NUM_ELEM(vep->floatLiteral.value));
    break;
  }
  case VEK_StringLiteral: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_StringLiteral")));
    ptrs[3] =
        J_PROP(J_ASCIZ("value"), J_STR_ELEM(J_ASCIZ(vep->stringLiteral.value)));
    break;
  }
  case VEK_StructLiteral: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_StructLiteral")));

    // Embed array
    size_t len = vep->structExpr.members_len;
    j_Elem *array = ALLOC_ARR(a, len, j_Elem);
    for (size_t i = 0; i < len; i++) {
      array[i] = valueStructMemberExprJson(&vep->structExpr.members[i], a);
    }
    ptrs[3] = J_PROP(J_ASCIZ("members"), J_ARRAY_ELEM(array, len));
    break;
  }
  case VEK_BinaryOp: {
    ptrs_len = 6;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_BinaryOp")));
    char *binOpStr;
    switch (vep->binaryOp.operator) {
    case VEBOK_Add: {
      binOpStr = "VEBOK_Add";
      break;
    }
    case VEBOK_Sub: {
      binOpStr = "VEBOK_Sub";
      break;
    }
    case VEBOK_Mul: {
      binOpStr = "VEBOK_Mul";
      break;
    }
    case VEBOK_Div: {
      binOpStr = "VEBOK_Div";
      break;
    }
    case VEBOK_Mod: {
      binOpStr = "VEBOK_Mod";
      break;
    }
    case VEBOK_And: {
      binOpStr = "VEBOK_And";
      break;
    }
    case VEBOK_Or: {
      binOpStr = "VEBOK_Or";
      break;
    }
    case VEBOK_Tuple: {
      binOpStr = "VEBOK_Tuple";
      break;
    }
    case VEBOK_CompEqual: {
      binOpStr = "VEBOK_CompEqual";
      break;
    }
    case VEBOK_CompNotEqual: {
      binOpStr = "VEBOK_CompNotEqual";
      break;
    }
    case VEBOK_CompLess: {
      binOpStr = "VEBOK_CompLess";
      break;
    }
    case VEBOK_CompLessEqual: {
      binOpStr = "VEBOK_CompLessEqual";
      break;
    }
    case VEBOK_CompGreater: {
      binOpStr = "VEBOK_CompGreater";
      break;
    }
    case VEBOK_CompGreaterEqual: {
      binOpStr = "VEBOK_CompGreaterEqual";
      break;
    }
    case VEBOK_Pipeline: {
      binOpStr = "VEBOK_Pipeline";
      break;
    }
    case VEBOK_Assign: {
      binOpStr = "VEBOK_Assign";
      break;
    }
    case VEBOK_AssignAdd: {
      binOpStr = "VEBOK_AssignAdd";
      break;
    }
    case VEBOK_AssignSub: {
      binOpStr = "VEBOK_AssignSub";
      break;
    }
    case VEBOK_AssignMul: {
      binOpStr = "VEBOK_AssignMul";
      break;
    }
    case VEBOK_AssignDiv: {
      binOpStr = "VEBOK_AssignDiv";
      break;
    }
    case VEBOK_AssignMod: {
      binOpStr = "VEBOK_AssignMod";
      break;
    }
    }
    ptrs[3] = J_PROP(J_ASCIZ("operator"), J_STR_ELEM(J_ASCIZ(binOpStr)));
    ptrs[4] =
        J_PROP(J_ASCIZ("left_operand"), valueExprJson(vep->binaryOp.left_operand, a));
    ptrs[5] =
        J_PROP(J_ASCIZ("right_operand"), valueExprJson(vep->binaryOp.right_operand, a));
    break;
  }
  case VEK_UnaryOp: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_UnaryOp")));
    char *unOpStr;
    switch (vep->unaryOp.operator) {
    case VEUOK_Negate: {
      unOpStr = "VEUOK_Negate";
      break;
    }
    case VEUOK_Posit: {
      unOpStr = "VEUOK_Posit";
      break;
    }
    case VEUOK_Not: {
      unOpStr = "VEUOK_LogicalNot";
      break;
    }
    case VEUOK_Ref: {
      unOpStr = "VEUOK_Ref";
      break;
    }
    case VEUOK_Deref: {
      unOpStr = "VEUOK_Deref";
      break;
    }
    }
    ptrs[3] = J_PROP(J_ASCIZ("operator"), J_STR_ELEM(J_ASCIZ(unOpStr)));
    ptrs[4] = J_PROP(J_ASCIZ("operand"), valueExprJson(vep->unaryOp.operand, a));
    break;
  }
  case VEK_Fn: {
    ptrs_len = 6;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_Fn")));

    // Embed array
    size_t len = vep->fnExpr.parameters_len;
    j_Elem *array = ALLOC_ARR(a, len, j_Elem);
    for (size_t i = 0; i < len; i++) {
      array[i] = patternExprJson(&vep->fnExpr.parameters[i], a);
    }
    ptrs[3] = J_PROP(J_ASCIZ("parameters"), J_ARRAY_ELEM(array, len));
    ptrs[4] = J_PROP(J_ASCIZ("type"), typeExprJson(vep->fnExpr.type, a));
    ptrs[5] = J_PROP(J_ASCIZ("body"), valueExprJson(vep->fnExpr.body, a));
    break;
  }
  case VEK_Call: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_Call")));
    ptrs[3] = J_PROP(J_ASCIZ("fn"), valueExprJson(vep->callExpr.function, a));
    // Embed array
    size_t len = vep->callExpr.parameters_len;
    j_Elem *array = ALLOC_ARR(a, len, j_Elem);
    for (size_t i = 0; i < len; i++) {
      array[i] = valueExprJson(&vep->callExpr.parameters[i], a);
    }
    ptrs[4] = J_PROP(J_ASCIZ("arguments"), J_ARRAY_ELEM(array, len));
    break;
  }
  case VEK_Loop: {
    ptrs_len = 6;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_Loop")));
    ptrs[3] = J_PROP(J_ASCIZ("value"), valueExprJson(vep->loopExpr.value, a));
    ptrs[4] = J_PROP(J_ASCIZ("has_label"), J_BOOL_ELEM(vep->loopExpr.has_label));
    if(vep->loopExpr.has_label) {
      ptrs[5] = J_PROP(J_ASCIZ("label"), J_STR_ELEM(J_ASCIZ(vep->loopExpr.label)));
    } else {
      ptrs[5] = J_PROP(J_ASCIZ("label"), J_NULL_ELEM);
    }
    break;
  }
  case VEK_Builtin: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_Builtin")));
    ptrs[3] = J_PROP(J_ASCIZ("value"), builtinJson(vep->builtinExpr.builtin, a));
    break;
  }
  case VEK_Defer: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_Defer")));
    ptrs[3] = J_PROP(J_ASCIZ("value"), valueExprJson(vep->deferExpr.value, a));
    break;
  }
  case VEK_As: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_As")));
    ptrs[3] = J_PROP(J_ASCIZ("value"), valueExprJson(vep->asExpr.value, a));
    ptrs[4] = J_PROP(J_ASCIZ("type"), typeExprJson(vep->asExpr.type, a));
    break;
  }
  case VEK_Continue: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_Continue")));
    ptrs[3] = J_PROP(J_ASCIZ("label"), J_STR_ELEM(J_ASCIZ(vep->continueExpr.label)));
    break;
  }
  case VEK_Return: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_Return")));
    ptrs[3] = J_PROP(J_ASCIZ("label"), J_STR_ELEM(J_ASCIZ(vep->continueExpr.label)));
    ptrs[4] = J_PROP(J_ASCIZ("value"), valueExprJson(vep->returnExpr.value, a));
    break;
  }
  case VEK_Match: {
    ptrs_len = 7;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_Match")));
    ptrs[3] = J_PROP(J_ASCIZ("value"), valueExprJson(vep->matchExpr.value, a));
    // Embed array
    size_t len = vep->matchExpr.cases_len;
    j_Elem *array = ALLOC_ARR(a, len, j_Elem);
    for (size_t i = 0; i < len; i++) {
      array[i] = matchCaseExprJson(&vep->matchExpr.cases[i], a);
    }
    ptrs[4] = J_PROP(J_ASCIZ("cases"), J_ARRAY_ELEM(array, len));
    
    ptrs[5] = J_PROP(J_ASCIZ("has_label"), J_BOOL_ELEM(vep->matchExpr.has_label));
    if(vep->matchExpr.has_label) {
      ptrs[6] = J_PROP(J_ASCIZ("label"), J_STR_ELEM(J_ASCIZ(vep->matchExpr.label)));
    } else {
      ptrs[6] = J_PROP(J_ASCIZ("label"), J_NULL_ELEM);
    }
    break;
  }
  case VEK_Block: {
    ptrs_len = 8;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_Block")));
    // Embed array
    size_t len = vep->blockExpr.statements_len;
    j_Elem *array = ALLOC_ARR(a, len, j_Elem);
    for (size_t i = 0; i < len; i++) {
      array[i] = stmntJson(&vep->blockExpr.statements[i], a);
    }
    ptrs[3] = J_PROP(J_ASCIZ("statements"), J_ARRAY_ELEM(array, len));
    ptrs[4] = J_PROP(J_ASCIZ("has_label"), J_BOOL_ELEM(vep->blockExpr.has_label));
    if(vep->matchExpr.has_label) {
      ptrs[5] = J_PROP(J_ASCIZ("label"), J_STR_ELEM(J_ASCIZ(vep->blockExpr.label)));
    } else {
      ptrs[5] = J_PROP(J_ASCIZ("label"), J_NULL_ELEM);
    }
    break;
  }
  case VEK_FieldAccess: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_FieldAccess")));
    ptrs[3] = J_PROP(J_ASCIZ("value"), valueExprJson(vep->fieldAccess.value, a));
    ptrs[4] = J_PROP(J_ASCIZ("field"), J_STR_ELEM(J_ASCIZ(vep->fieldAccess.field)));
    break;
  }
  case VEK_Reference: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("VEK_Reference")));
    ptrs[3] = J_PROP(J_ASCIZ("path"), pathJson(vep->reference.path, a));
    break;
  }
  }
  ptrs[1] = J_PROP(J_ASCIZ("span"), spanJson(vep->span, a));
  ptrs[2] =
      J_PROP(J_ASCIZ("comments"), commentsJson(vep->comments, vep->comments_len, a));
  // create final json object
  return J_OBJECT_ELEM(ptrs, ptrs_len);
}

j_Elem stmntJson(Stmnt *sp, Allocator *a) {
  if (sp == NULL) {
    return J_NULL_ELEM;
  }
  size_t ptrs_len;
  j_Prop *ptrs;
  // 1 reserved for span
  // 2 reserved for diagnostic
  // 3 reserved for comments
  switch (sp->kind) {
  case SK_None: {
    ptrs_len = 3;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("SK_None")));
    break;
  }
  // Misc
  case SK_Use: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("SK_Use")));
    ptrs[3] = J_PROP(J_ASCIZ("path"), pathJson(sp->useStmnt.path, a));
    break;
  }
  case SK_Namespace: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("SK_Namespace")));
    ptrs[3] = J_PROP(J_ASCIZ("path"), pathJson(sp->namespaceStmnt.path, a));
    ptrs[4] = J_PROP(J_ASCIZ("stmnt"), stmntJson(sp->namespaceStmnt.stmnt, a));
    break;
  }
  case SK_Macro: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("SK_Macro")));
    ptrs[3] = J_PROP(J_ASCIZ("macro"), J_STR_ELEM(J_ASCIZ(sp->macroStmnt.name)));
    break;
  }
  // Decls
  case SK_ValDecl: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("SK_ValDecl")));
    ptrs[3] = J_PROP(J_ASCIZ("pattern"), patternExprJson(sp->valDecl.pattern, a));
    ptrs[4] = J_PROP(J_ASCIZ("value"), valueExprJson(sp->valDecl.value, a));
    break;
  }
  case SK_TypeDecl: {
    ptrs_len = 5;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("SK_TypeDecl")));
    ptrs[3] = J_PROP(J_ASCIZ("type"), typeExprJson(sp->typeDecl.type, a));
    ptrs[4] = J_PROP(J_ASCIZ("name"), J_STR_ELEM(J_ASCIZ(sp->typeDecl.name)));
    break;
  }
  // Exprs
  case SK_ValExpr: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("SK_ValExpr")));
    ptrs[3] = J_PROP(J_ASCIZ("value"), valueExprJson(sp->valExpr.value, a));
    break;
  }
  case SK_TypeExpr: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("SK_TypeExpr")));
    ptrs[3] = J_PROP(J_ASCIZ("type"), typeExprJson(sp->typeExpr.type, a));
    break;
  }
  case SK_PatExpr: {
    ptrs_len = 4;
    ptrs = ALLOC_ARR(a, ptrs_len, j_Prop);
    ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("SK_PatExpr")));
    ptrs[3] = J_PROP(J_ASCIZ("pat"), patternExprJson(sp->patExpr.pattern, a));
    break;
  }
  }
  ptrs[1] = J_PROP(J_ASCIZ("span"), spanJson(sp->span, a));
  ptrs[2] =
      J_PROP(J_ASCIZ("comments"), commentsJson(sp->comments, sp->comments_len, a));
  // create final json object
  j_Elem je = J_OBJECT_ELEM(ptrs, ptrs_len);
  return je;
}

void print_stream(Parser *parser, FILE *file) {
  bool eof = false;
  while(!eof) {
    Allocator a = arena_a_create();

    // Parse the next statment
    Stmnt stmnt;
    Vector diagnostics =  vec_create(&a);
    parseStmnt(&stmnt, &diagnostics, parser);

    // print the json
    j_Elem sjson = stmntJson(&stmnt, &a);
    fputs(j_stringify(&sjson, &a), file);
    fputs("\n", file);


    for(size_t i = 0; i < VEC_LEN(&diagnostics, Diagnostic); i--) {
        Diagnostic d = *VEC_GET(&diagnostics, i, Diagnostic);
        j_Elem djson = diagnosticJson(d, &a);
        fputs(j_stringify(&djson, &a), file);
        fputs("\n", file);
        if(d.kind == DK_EOF) {
          eof = true;
        }
    }
    // Clean up
    vec_destroy(&diagnostics);
    a_destroy(&a);
  }
}
