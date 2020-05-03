#include "printAst.h"

#include "arena.h"
#include "ast.h"
#include "json.h"
#include "token.h"

static JsonElem lnColJson(LnCol lncol, Arena *ja) {
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 2);
  ptrs[0] = KVJson("ln", intJson(lncol.ln));
  ptrs[1] = KVJson("col", intJson(lncol.col));
  return objDefJson(ptrs, 2);
}

static JsonElem spanJson(Span span, Arena *ja) {
  return nullJson();
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 2);
  ptrs[0] = KVJson("start", lnColJson(span.start, ja));
  ptrs[1] = KVJson("end", lnColJson(span.end, ja));
  return objDefJson(ptrs, 2);
}

static JsonElem diagnosticJson(Diagnostic diagnostic, Arena *ja) {
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 2);
  ptrs[0] = KVJson("kind", strJson(strDiagnosticKind(diagnostic.kind)));
  ptrs[1] = KVJson("span", spanJson(diagnostic.span, ja));
  return objDefJson(ptrs, 2);
}

static JsonElem diagnosticsJson(Diagnostic *diagnostics,
                                size_t diagnostics_length, Arena *ja) {
  JsonElem *ptrs = allocArena(ja, sizeof(JsonElem) * diagnostics_length);
  for (size_t i = 0; i < diagnostics_length; i++) {
    ptrs[i] = diagnosticJson(diagnostics[i], ja);
  }
  return arrDefJson(ptrs, diagnostics_length);
}

static JsonElem commentJson(Comment comment, Arena *ja) {
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 3);
  ptrs[0] = KVJson("scope", strJson(comment.scope));
  ptrs[1] = KVJson("comment", strJson(comment.data));
  ptrs[2] = KVJson("span", spanJson(comment.span, ja));
  return objDefJson(ptrs, 3);
}

static JsonElem commentsJson(Comment *comments, size_t comments_length,
                             Arena *ja) {
  JsonElem *ptrs = allocArena(ja, sizeof(JsonElem) * comments_length);
  for (size_t i = 0; i < comments_length; i++) {
    ptrs[i] = commentJson(comments[i], ja);
  }
  return arrDefJson(ptrs, comments_length);
}

static JsonElem stmntJson(Stmnt *s, Arena *ja);
static JsonElem typeExprJson(TypeExpr *tep, Arena *ja);
static JsonElem valueExprJson(ValueExpr *vep, Arena *ja);
static JsonElem matchCaseExprJson(struct MatchCaseExpr_s *mcep, Arena *ja);
static JsonElem bindingJson(Binding *bp, Arena *ja);

static JsonElem pathJson(Path *pp, Arena *ja) {
  size_t ptrs_len = 4;
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
  ptrs[0] = KVJson("span", spanJson(pp->span, ja));
  ptrs[1] = KVJson("diagnostics", diagnosticsJson(pp->diagnostics,
                                                  pp->diagnostics_length, ja));
  ptrs[2] = KVJson("comments", commentsJson(pp->comments,
                                                  pp->comments_length, ja));
  JsonElem *path_ptrs =
      allocArena(ja, sizeof(JsonElem) * pp->pathSegments_length);
  for (size_t i = 0; i < pp->pathSegments_length; i++) {
    path_ptrs[i] = strJson(internArena(pp->pathSegments[i], ja));
  }
  ptrs[3] =
      KVJson("path_segments", arrDefJson(path_ptrs, pp->pathSegments_length));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem typeExprJson(TypeExpr *tep, Arena *ja) {
  if (tep == NULL) {
    return nullJson();
  }

  size_t ptrs_len = 0;
  JsonKV *ptrs = NULL;
  // 1 reserved for span
  // 2 reserved for diagnostic
  // 3 reserved for comments
  switch (tep->kind) {
  case TEK_None: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_None"));
    break;
  }
  case TEK_Omitted: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_Omitted"));
    break;
  }
  case TEK_Void: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_Void"));
    break;
  }
  case TEK_Reference: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_Reference"));
    ptrs[4] = KVJson("path", pathJson(tep->referenceExpr.path, ja));
    break;
  }
  case TEK_Typeof: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_Typeof"));
    ptrs[4] = KVJson("value", valueExprJson(tep->typeofExpr.value, ja));
    break;
  }
  case TEK_Struct: {
    ptrs_len = 7;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_StructExpr"));

    // struct kind
    char *structKind;
    switch (tep->structExpr.kind) {
    case TESK_Struct: {
      structKind = "TESK_Struct";
      break;
    }
    case TESK_Pack: {
      structKind = "TESK_Pack";
      break;
    }
    case TESK_Union: {
      structKind = "TESK_Union";
      break;
    }
    case TESK_Enum: {
      structKind = "TESK_Enum";
      break;
    }
    }
    ptrs[4] = KVJson("struct_kind", strJson(structKind));

    // Embed array
    size_t len = tep->structExpr.members_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = bindingJson(&tep->structExpr.members[i], ja);
    }
    ptrs[5] = KVJson("members", arrDefJson(array, len));
    ptrs[6] =
        KVJson("trailing_comma", boolJson(tep->structExpr.trailing_comma));
    break;
  }
  case TEK_UnaryOp: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_UnaryOp"));
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
    ptrs[4] = KVJson("operator", strJson(unOpStr));
    ptrs[5] = KVJson("operand", typeExprJson(tep->unaryOp.operand, ja));
    break;
  }
  case TEK_FieldAccess: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_FieldAccess"));
    ptrs[4] = KVJson("value", typeExprJson(tep->fieldAccess.value, ja));
    ptrs[5] = KVJson("field", strJson(internArena(tep->fieldAccess.field, ja)));
    break;
  }
  case TEK_Tuple: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_Tuple"));
    // Embed array
    size_t len = tep->tupleExpr.members_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = typeExprJson(&tep->tupleExpr.members[i], ja);
    }
    ptrs[4] = KVJson("members", arrDefJson(array, len));
    ptrs[5] = KVJson("trailing_comma", boolJson(tep->tupleExpr.trailing_comma));
    break;
  }
  case TEK_Fn: {
    ptrs_len = 7;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_Fn"));

    // Embed array
    size_t len = tep->fnExpr.parameters_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = typeExprJson(&tep->fnExpr.parameters[i], ja);
    }
    ptrs[4] = KVJson("parameters", arrDefJson(array, len));
    ptrs[5] = KVJson("trailing_comma",
                     boolJson(tep->fnExpr.parameters_trailing_comma));
    ptrs[6] = KVJson("result", typeExprJson(tep->fnExpr.result, ja));
    break;
  }
  }
  ptrs[1] = KVJson("span", spanJson(tep->span, ja));
  ptrs[2] = KVJson("diagnostics", diagnosticsJson(tep->diagnostics,
                                                  tep->diagnostics_length, ja));
  ptrs[3] =
      KVJson("comments", commentsJson(tep->comments, tep->comments_length, ja));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem bindingJson(Binding *bp, Arena *ja) {
  if (bp == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 5;
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
  ptrs[0] = KVJson("span", spanJson(bp->span, ja));
  ptrs[1] = KVJson("diagnostics", diagnosticsJson(bp->diagnostics,
                                                  bp->diagnostics_length, ja));
  ptrs[2] = KVJson("name", strJson(internArena(bp->name, ja)));
  ptrs[3] =
      KVJson("comments", commentsJson(bp->comments, bp->comments_length, ja));
  ptrs[4] = KVJson("type", typeExprJson(bp->type, ja));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem matchCaseExprJson(struct MatchCaseExpr_s *mcep, Arena *ja) {
  if (mcep == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 6;
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
  ptrs[0] = KVJson("kind", strJson("MatchCaseExpr"));
  ptrs[1] = KVJson("span", spanJson(mcep->span, ja));
  ptrs[2] =
      KVJson("diagnostics",
             diagnosticsJson(mcep->diagnostics, mcep->diagnostics_length, ja));
  ptrs[3] = KVJson("comments",
                   commentsJson(mcep->comments, mcep->comments_length, ja));
  ptrs[3] = KVJson("pattern", valueExprJson(mcep->pattern, ja));
  ptrs[4] = KVJson("name", valueExprJson(mcep->value, ja));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem valueExprJson(ValueExpr *vep, Arena *ja) {
  if (vep == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 0;
  JsonKV *ptrs = NULL;
  // 1 reserved for span
  // 2 reserved for diagnostic
  // 3 reserved for comments
  switch (vep->kind) {
  case VEK_None: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_None"));
    break;
  }
  case VEK_IntLiteral: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_IntLiteral"));
    ptrs[4] = KVJson("value", intJson(vep->intLiteral.value));
    break;
  }
  case VEK_BoolLiteral: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_BoolLiteral"));
    ptrs[4] = KVJson("value", boolJson(vep->boolLiteral.value));
    break;
  }
  case VEK_FloatLiteral: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_FloatLiteral"));
    ptrs[4] = KVJson("value", numJson(vep->floatLiteral.value));
    break;
  }
  case VEK_CharLiteral: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_CharLiteral"));
    ptrs[4] = KVJson("value", intJson((uint64_t)vep->charLiteral.value));
    break;
  }
  case VEK_StringLiteral: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_StringLiteral"));
    ptrs[4] =
        KVJson("value", strJson(internArena(vep->stringLiteral.value, ja)));
    break;
  }
  case VEK_StructLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_StructLiteral"));
    // TODO
    break;
  }
  case VEK_BinaryOp: {
    ptrs_len = 7;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_BinaryOp"));
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
    case VEBOK_BitAnd: {
      binOpStr = "VEBOK_BitAnd";
      break;
    }
    case VEBOK_BitOr: {
      binOpStr = "VEBOK_BitOr";
      break;
    }
    case VEBOK_BitXor: {
      binOpStr = "VEBOK_BitXor";
      break;
    }
    case VEBOK_BitShl: {
      binOpStr = "VEBOK_BitShl";
      break;
    }
    case VEBOK_BitShr: {
      binOpStr = "VEBOK_BitShr";
      break;
    }
    case VEBOK_LogicalAnd: {
      binOpStr = "VEBOK_LogicalAnd";
      break;
    }
    case VEBOK_LogicalOr: {
      binOpStr = "VEBOK_LogicalOr";
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
    case VEBOK_AssignBitAnd: {
      binOpStr = "VEBOK_AssignBitAnd";
      break;
    }
    case VEBOK_AssignBitOr: {
      binOpStr = "VEBOK_AssignBitOr";
      break;
    }
    }
    ptrs[4] = KVJson("operator", strJson(binOpStr));
    ptrs[5] =
        KVJson("left_operand", valueExprJson(vep->binaryOp.left_operand, ja));
    ptrs[6] =
        KVJson("right_operand", valueExprJson(vep->binaryOp.right_operand, ja));
    break;
  }
  case VEK_UnaryOp: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_UnaryOp"));
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
    case VEUOK_LogicalNot: {
      unOpStr = "VEUOK_LogicalNot";
      break;
    }
    case VEUOK_BitNot: {
      unOpStr = "VEUOK_BitNot";
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
    ptrs[4] = KVJson("operator", strJson(unOpStr));
    ptrs[5] = KVJson("operand", valueExprJson(vep->unaryOp.operand, ja));
    break;
  }
  case VEK_Call: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Call"));
    ptrs[4] = KVJson("fn", valueExprJson(vep->callExpr.function, ja));
    // Embed array
    size_t len = vep->callExpr.arguments_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = valueExprJson(&vep->callExpr.arguments[i], ja);
    }
    ptrs[5] = KVJson("arguments", arrDefJson(array, len));
    break;
  }
  case VEK_If: {
    ptrs_len = 8;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_If"));
    ptrs[4] = KVJson("condition", valueExprJson(vep->ifExpr.condition, ja));
    ptrs[5] = KVJson("body", valueExprJson(vep->ifExpr.body, ja));
    ptrs[6] = KVJson("has_else", boolJson(vep->ifExpr.has_else));
    if (vep->ifExpr.has_else) {
      ptrs[7] = KVJson("else_body", valueExprJson(vep->ifExpr.else_body, ja));
    } else {
      ptrs[7] = KVJson("else_body", nullJson());
    }
    break;
  }
  case VEK_While: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_While"));
    ptrs[4] = KVJson("condition", valueExprJson(vep->ifExpr.condition, ja));
    ptrs[5] = KVJson("body", valueExprJson(vep->ifExpr.body, ja));
    break;
  }
  case VEK_With: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_With"));
    // TODO
    break;
  }
  case VEK_Pass: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Pass"));
    break;
  }
  case VEK_Break: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Break"));
    break;
  }
  case VEK_Continue: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Continue"));
    break;
  }
  case VEK_Return: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Return"));
    ptrs[4] = KVJson("value", valueExprJson(vep->returnExpr.value, ja));
    break;
  }
  case VEK_Match: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Match"));
    ptrs[4] = KVJson("value", valueExprJson(vep->matchExpr.value, ja));
    // Embed array
    size_t len = vep->matchExpr.cases_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = matchCaseExprJson(&vep->matchExpr.cases[i], ja);
    }
    ptrs[5] = KVJson("cases", arrDefJson(array, len));
    break;
  }
  case VEK_Block: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Block"));
    // Embed array
    size_t len = vep->blockExpr.statements_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = stmntJson(&vep->blockExpr.statements[i], ja);
    }
    ptrs[4] = KVJson("statements", arrDefJson(array, len));
    ptrs[5] = KVJson("suppress_value", boolJson(vep->blockExpr.suppress_value));
    break;
  }
  case VEK_Group: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Group"));
    ptrs[4] = KVJson("value", valueExprJson(vep->groupExpr.value, ja));
    break;
  }
  case VEK_FieldAccess: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_FieldAccess"));
    ptrs[4] = KVJson("value", valueExprJson(vep->fieldAccess.value, ja));
    ptrs[5] = KVJson("field", strJson(internArena(vep->fieldAccess.field, ja)));
    break;
  }
  case VEK_Reference: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Reference"));
    ptrs[4] = KVJson("path", pathJson(vep->reference.path, ja));
    break;
  }
  }
  ptrs[1] = KVJson("span", spanJson(vep->span, ja));
  ptrs[2] = KVJson("diagnostics", diagnosticsJson(vep->diagnostics,
                                                  vep->diagnostics_length, ja));
  ptrs[3] =
      KVJson("comments", commentsJson(vep->comments, vep->comments_length, ja));
  // create final json object
  return objDefJson(ptrs, ptrs_len);
}

JsonElem stmntJson(Stmnt *sp, Arena *ja) {
  if (sp == NULL) {
    return nullJson();
  }
  size_t ptrs_len;
  JsonKV *ptrs;
  // 1 reserved for span
  // 2 reserved for diagnostic
  // 3 reserved for comments
  switch (sp->kind) {
  case SK_None: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("SK_None"));
    break;
  }
  case SK_FnDecl: {
    ptrs_len = 8;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("SK_FnDecl"));
    ptrs[4] = KVJson("name", strJson(internArena(sp->fnDecl.name, ja)));
    // Embed array
    size_t len = sp->fnDecl.params_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = bindingJson(&sp->fnDecl.params[i], ja);
    }
    ptrs[5] = KVJson("params", arrDefJson(array, len));
    ptrs[6] = KVJson("type", typeExprJson(sp->fnDecl.type, ja));
    ptrs[7] = KVJson("body", valueExprJson(sp->fnDecl.body, ja));
    break;
  }
  case SK_VarDecl: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("SK_VarDecl"));
    ptrs[4] = KVJson("binding", bindingJson(sp->varDecl.binding, ja));
    ptrs[5] = KVJson("value", valueExprJson(sp->varDecl.value, ja));
    break;
  }
  case SK_TypeAliasStmnt: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("SK_TypeAliasDecl"));
    ptrs[4] = KVJson("type", typeExprJson(sp->typeAliasStmnt.type, ja));
    ptrs[5] = KVJson("name", strJson(internArena(sp->typeAliasStmnt.name, ja)));
    break;
  }
  case SK_Expr: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("SK_Expr"));
    ptrs[4] = KVJson("value", valueExprJson(sp->exprStmnt.value, ja));
    break;
  }
  }
  ptrs[1] = KVJson("span", spanJson(sp->span, ja));
  ptrs[2] = KVJson("diagnostics", diagnosticsJson(sp->diagnostics,
                                                  sp->diagnostics_length, ja));
  ptrs[3] =
      KVJson("comments", commentsJson(sp->comments, sp->comments_length, ja));
  // create final json object
  JsonElem je = objDefJson(ptrs, ptrs_len);
  return je;
}

static JsonElem jsonTranslationUnit(TranslationUnit *tup, Arena *ja) {
  if (tup == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 5;
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);

  ptrs[0] = KVJson("kind", strJson("TranslationUnit"));
  ptrs[1] = KVJson("span", spanJson(tup->span, ja));
  ptrs[2] = KVJson("diagnostics", diagnosticsJson(tup->diagnostics,
                                                  tup->diagnostics_length, ja));
  ptrs[3] =
      KVJson("comments", commentsJson(tup->comments, tup->comments_length, ja));
  // Embed array
  size_t len = tup->statements_length;
  JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
  for (size_t i = 0; i < len; i++) {
    array[i] = stmntJson(&tup->statements[i], ja);
  }
  ptrs[4] = KVJson("statements", arrDefJson(array, len));
  return objDefJson(ptrs, ptrs_len);
}

void createPrinter(Printer *printer, Parser *parser, Arena *arena) {
  printer->parser = parser;
  printer->arena = arena;
}

Arena *releasePrinter(Printer *printer) { return printer->arena; }

void printJsonPrinter(Printer *printer, FILE *file) {
  TranslationUnit tu;
  parseTranslationUnitParser(printer->parser, &tu);
  JsonElem jsonElem = jsonTranslationUnit(&tu, printer->arena);
  char *str = toStringJsonElem(&jsonElem);
  fputs(str, file);
  free(str);
}
