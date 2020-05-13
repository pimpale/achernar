#include "printAst.h"

#include "arena.h"
#include "ast.h"
#include "json.h"
#include "token.h"

static JsonElem lnColJson(LnCol lncol, Arena *ja) {
  JsonKV *ptrs = RALLOC_ARR(ja, 2, JsonKV);
  ptrs[0] = KVJson("ln", intJson(lncol.ln));
  ptrs[1] = KVJson("col", intJson(lncol.col));
  return objDefJson(ptrs, 2);
}

static JsonElem spanJson(Span span, Arena *ja) {
  return nullJson();
  JsonKV *ptrs = RALLOC_ARR(ja, 2, JsonKV);

  ptrs[0] = KVJson("start", lnColJson(span.start, ja));
  ptrs[1] = KVJson("end", lnColJson(span.end, ja));
  return objDefJson(ptrs, 2);
}

static JsonElem diagnosticJson(Diagnostic diagnostic, Arena *ja) {
  JsonKV *ptrs = RALLOC_ARR(ja, 2, JsonKV);

  ptrs[0] = KVJson("kind", strJson(strDiagnosticKind(diagnostic.kind)));
  ptrs[1] = KVJson("span", spanJson(diagnostic.span, ja));
  return objDefJson(ptrs, 2);
}

static JsonElem diagnosticsJson(Diagnostic *diagnostics, size_t diagnostics_len,
                                Arena *ja) {
  JsonElem *ptrs = RALLOC_ARR(ja, diagnostics_len, JsonElem);

  for (size_t i = 0; i < diagnostics_len; i++) {
    ptrs[i] = diagnosticJson(diagnostics[i], ja);
  }
  return arrDefJson(ptrs, diagnostics_len);
}

static JsonElem commentJson(Comment comment, Arena *ja) {
  JsonKV *ptrs = RALLOC_ARR(ja, 3, JsonKV);
  ptrs[0] = KVJson("scope", strJson(comment.scope));
  ptrs[1] = KVJson("comment", strJson(comment.data));
  ptrs[2] = KVJson("span", spanJson(comment.span, ja));
  return objDefJson(ptrs, 3);
}

static JsonElem commentsJson(Comment *comments, size_t comments_len,
                             Arena *ja) {
  JsonElem *ptrs = RALLOC_ARR(ja, comments_len, JsonElem);
  for (size_t i = 0; i < comments_len; i++) {
    ptrs[i] = commentJson(comments[i], ja);
  }
  return arrDefJson(ptrs, comments_len);
}

static JsonElem stmntJson(Stmnt *s, Arena *ja);
static JsonElem typeExprJson(TypeExpr *tep, Arena *ja);
static JsonElem valueExprJson(ValueExpr *vep, Arena *ja);
static JsonElem constExprJson(ConstExpr *cep, Arena *ja); // TODO
static JsonElem patternExprJson(PatternExpr *pep, Arena *ja); // TODO
static JsonElem
patternStructMemberExprJson(struct PatternStructMemberExpr_s *psmep, Arena *ja);
static JsonElem typeStructMemberExprJson(struct TypeStructMemberExpr_s *tsmep,
                                         Arena *ja);
static JsonElem valueStructMemberExprJson(struct ValueStructMemberExpr_s *vsmep,
                                          Arena *ja);
static JsonElem matchCaseExprJson(struct MatchCaseExpr_s *mcep, Arena *ja);

static JsonElem pathJson(Path *pp, Arena *ja) {
  size_t ptrs_len = 4;
  JsonKV *ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
  ptrs[0] = KVJson("span", spanJson(pp->span, ja));
  ptrs[1] = KVJson("diagnostics",
                   diagnosticsJson(pp->diagnostics, pp->diagnostics_len, ja));
  ptrs[2] =
      KVJson("comments", commentsJson(pp->comments, pp->comments_len, ja));
  JsonElem *path_ptrs = RALLOC_ARR(ja, pp->pathSegments_len, JsonElem);
  for (size_t i = 0; i < pp->pathSegments_len; i++) {
    path_ptrs[i] = strJson(internArena(ja, pp->pathSegments[i]));
  }
  ptrs[3] =
      KVJson("path_segments", arrDefJson(path_ptrs, pp->pathSegments_len));
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
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("TEK_None"));
    break;
  }
  case TEK_Omitted: {
    ptrs_len = 4;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("TEK_Omitted"));
    break;
  }
  case TEK_Void: {
    ptrs_len = 4;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("TEK_Void"));
    break;
  }
  case TEK_Reference: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("TEK_Reference"));
    ptrs[4] = KVJson("path", pathJson(tep->referenceExpr.path, ja));
    break;
  }
  case TEK_Struct: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("TEK_StructExpr"));

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
    ptrs[4] = KVJson("struct_kind", strJson(structKind));

    // Embed array
    size_t len = tep->structExpr.members_len;
    JsonElem *array = RALLOC_ARR(ja, len, JsonElem);
    for (size_t i = 0; i < len; i++) {
      array[i] = typeStructMemberExpr(&tep->structExpr.members[i], ja);
    }
    ptrs[5] = KVJson("members", arrDefJson(array, len));
    break;
  }
  case TEK_UnaryOp: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
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
  case TEK_BinaryOp: {
    ptrs_len = 7;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("TEK_BinaryOp"));
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
    ptrs[4] = KVJson("operator", strJson(binOpStr));
    ptrs[5] =
        KVJson("left_operand", typeExprJson(tep->binaryOp.left_operand, ja));
    ptrs[6] =
        KVJson("right_operand", typeExprJson(tep->binaryOp.right_operand, ja));
    break;
  }

  case TEK_FieldAccess: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("TEK_FieldAccess"));
    ptrs[4] = KVJson("value", typeExprJson(tep->fieldAccess.value, ja));
    ptrs[5] = KVJson("field", strJson(internArena(ja, tep->fieldAccess.field)));
    break;
  }
  case TEK_Fn: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("TEK_Fn"));

    // Embed array
    size_t len = tep->fnExpr.parameters_len;
    JsonElem *array = RALLOC_ARR(ja, len, JsonElem);
    for (size_t i = 0; i < len; i++) {
      array[i] = typeExprJson(&tep->fnExpr.parameters[i], ja);
    }
    ptrs[4] = KVJson("parameters", arrDefJson(array, len));
    ptrs[5] = KVJson("result", typeExprJson(tep->fnExpr.type, ja));
    break;
  }
  }
  ptrs[1] = KVJson("span", spanJson(tep->span, ja));
  ptrs[2] = KVJson("diagnostics",
                   diagnosticsJson(tep->diagnostics, tep->diagnostics_len, ja));
  ptrs[3] =
      KVJson("comments", commentsJson(tep->comments, tep->comments_len, ja));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem valueStructMemberExprJson(struct ValueStructMemberExpr_s *vsmep,
                                          Arena *ja) {
  if (vsmep == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 5;
  JsonKV *ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
  ptrs[0] = KVJson("span", spanJson(vsmep->span, ja));
  ptrs[1] = KVJson("diagnostics", diagnosticsJson(vsmep->diagnostics,
                                                  vsmep->diagnostics_len, ja));
  ptrs[2] = KVJson("name", strJson(internArena(ja, vsmep->name)));
  ptrs[3] = KVJson("comments",
                   commentsJson(vsmep->comments, vsmep->comments_len, ja));
  ptrs[4] = KVJson("value", valueExprJson(vsmep->value, ja));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem patternStructMemberJson(struct PatternStructMemberExpr_s *psmep,
                                        Arena *ja) {
  if (psmep == NULL) {
    return nullJson();
  }

  JsonKV *ptrs;
  size_t ptrs_len;
  switch (psmep->kind) {
  case PSMEK_Field: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("PSMEK_Field"));
    ptrs[4] = KVJson("field", strJson(internArena(ja, psmep->field.field)));
    ptrs[5] = KVJson("pattern", patternExprJson(psmep->field.pattern, ja));
  }
  case PSMEK_Rest: {
    ptrs_len = 4;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("PSMEK_Rest"));
  }
  }
  ptrs[1] = KVJson("span", spanJson(psmep->span, ja));
  ptrs[2] = KVJson("diagnostics", diagnosticsJson(psmep->diagnostics,
                                                  psmep->diagnostics_len, ja));
  ptrs[3] = KVJson("comments",
                   commentsJson(psmep->comments, psmep->comments_len, ja));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem typeStructMemberExprJson(struct TypeStructMemberExpr_s *tsmep,
                                         Arena *ja) {
  if (tsmep == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 5;
  JsonKV *ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
  ptrs[0] = KVJson("span", spanJson(tsmep->span, ja));
  ptrs[1] = KVJson("diagnostics", diagnosticsJson(tsmep->diagnostics,
                                                  tsmep->diagnostics_len, ja));
  ptrs[2] = KVJson("name", strJson(internArena(ja, tsmep->name)));
  ptrs[3] = KVJson("comments",
                   commentsJson(tsmep->comments, tsmep->comments_len, ja));
  ptrs[4] = KVJson("type", typeExprJson(tsmep->type, ja));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem matchCaseExprJson(struct MatchCaseExpr_s *mcep, Arena *ja) {
  if (mcep == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 6;
  JsonKV *ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
  ptrs[0] = KVJson("kind", strJson("MatchCaseExpr"));
  ptrs[1] = KVJson("span", spanJson(mcep->span, ja));
  ptrs[2] = KVJson("diagnostics", diagnosticsJson(mcep->diagnostics,
                                                  mcep->diagnostics_len, ja));
  ptrs[3] =
      KVJson("comments", commentsJson(mcep->comments, mcep->comments_len, ja));
  ptrs[4] = KVJson("pattern", patternExprJson(mcep->pattern, ja));
  ptrs[5] = KVJson("name", valueExprJson(mcep->value, ja));
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
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_None"));
    break;
  }
  case VEK_ConstExpr: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_ConstExpr"));
    ptrs[4] = KVJson("value", constExprJson(vep->constExpr.constExpr, ja));
    break;
  }
  case VEK_StringLiteral: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_StringLiteral"));
    ptrs[4] =
        KVJson("value", strJson(internArena(ja, vep->stringLiteral.value)));
    break;
  }
  case VEK_StructLiteral: {
    ptrs_len = 4;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_StructLiteral"));
    // TODO
    break;
  }
  case VEK_BinaryOp: {
    ptrs_len = 7;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
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
    ptrs[4] = KVJson("operator", strJson(binOpStr));
    ptrs[5] =
        KVJson("left_operand", valueExprJson(vep->binaryOp.left_operand, ja));
    ptrs[6] =
        KVJson("right_operand", valueExprJson(vep->binaryOp.right_operand, ja));
    break;
  }
  case VEK_UnaryOp: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
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
    ptrs[4] = KVJson("operator", strJson(unOpStr));
    ptrs[5] = KVJson("operand", valueExprJson(vep->unaryOp.operand, ja));
    break;
  }
  case VEK_Call: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Call"));
    ptrs[4] = KVJson("fn", valueExprJson(vep->callExpr.function, ja));
    // Embed array
    size_t len = vep->callExpr.parameters_len;
    JsonElem *array = RALLOC_ARR(ja, len, JsonElem);
    for (size_t i = 0; i < len; i++) {
      array[i] = valueExprJson(&vep->callExpr.parameters[i], ja);
    }
    ptrs[5] = KVJson("arguments", arrDefJson(array, len));
    break;
  }
  case VEK_If: {
    ptrs_len = 8;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
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
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_While"));
    ptrs[4] = KVJson("condition", valueExprJson(vep->ifExpr.condition, ja));
    ptrs[5] = KVJson("body", valueExprJson(vep->ifExpr.body, ja));
    break;
  }
  case VEK_With: {
    ptrs_len = 4;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_With"));
    // TODO
    break;
  }
  case VEK_Pass: {
    ptrs_len = 4;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Pass"));
    break;
  }
  case VEK_Break: {
    ptrs_len = 4;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Break"));
    break;
  }
  case VEK_Continue: {
    ptrs_len = 4;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Continue"));
    break;
  }
  case VEK_Return: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Return"));
    ptrs[4] = KVJson("value", valueExprJson(vep->returnExpr.value, ja));
    break;
  }
  case VEK_Match: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Match"));
    ptrs[4] = KVJson("value", valueExprJson(vep->matchExpr.value, ja));
    // Embed array
    size_t len = vep->matchExpr.cases_len;
    JsonElem *array = RALLOC_ARR(ja, len, JsonElem);
    for (size_t i = 0; i < len; i++) {
      array[i] = matchCaseExprJson(&vep->matchExpr.cases[i], ja);
    }
    ptrs[5] = KVJson("cases", arrDefJson(array, len));
    break;
  }
  case VEK_Block: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Block"));
    // Embed array
    size_t len = vep->blockExpr.statements_len;
    JsonElem *array = RALLOC_ARR(ja, len, JsonElem);
    for (size_t i = 0; i < len; i++) {
      array[i] = stmntJson(&vep->blockExpr.statements[i], ja);
    }
    ptrs[4] = KVJson("statements", arrDefJson(array, len));
    ptrs[5] = KVJson("suppress_value", boolJson(vep->blockExpr.suppress_value));
    break;
  }
  case VEK_FieldAccess: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_FieldAccess"));
    ptrs[4] = KVJson("value", valueExprJson(vep->fieldAccess.value, ja));
    ptrs[5] = KVJson("field", strJson(internArena(ja, vep->fieldAccess.field)));
    break;
  }
  case VEK_Reference: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Reference"));
    ptrs[4] = KVJson("path", pathJson(vep->reference.path, ja));
    break;
  }
  }
  ptrs[1] = KVJson("span", spanJson(vep->span, ja));
  ptrs[2] = KVJson("diagnostics",
                   diagnosticsJson(vep->diagnostics, vep->diagnostics_len, ja));
  ptrs[3] =
      KVJson("comments", commentsJson(vep->comments, vep->comments_len, ja));
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
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("SK_None"));
    break;
  }
  case SK_VarDecl: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("SK_VarDecl"));
    ptrs[4] = KVJson("pattern", patternExpr(sp->varDecl.pattern, ja));
    ptrs[5] = KVJson("value", valueExprJson(sp->varDecl.value, ja));
    break;
  }
  case SK_TypeDecl: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("SK_TypeDecl"));
    ptrs[4] = KVJson("type", typeExprJson(sp->typeDecl.type, ja));
    ptrs[5] = KVJson("name", strJson(internArena(ja, sp->typeDecl.name)));
    break;
  }
  case SK_Expr: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("SK_Expr"));
    ptrs[4] = KVJson("value", valueExprJson(sp->exprStmnt.value, ja));
    break;
  }
  }
  ptrs[1] = KVJson("span", spanJson(sp->span, ja));
  ptrs[2] = KVJson("diagnostics",
                   diagnosticsJson(sp->diagnostics, sp->diagnostics_len, ja));
  ptrs[3] =
      KVJson("comments", commentsJson(sp->comments, sp->comments_len, ja));
  // create final json object
  JsonElem je = objDefJson(ptrs, ptrs_len);
  return je;
}

static JsonElem jsonTranslationUnit(TranslationUnit *tup, Arena *ja) {
  if (tup == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 5;
  JsonKV *ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);

  ptrs[0] = KVJson("kind", strJson("TranslationUnit"));
  ptrs[1] = KVJson("span", spanJson(tup->span, ja));
  ptrs[2] = KVJson("diagnostics",
                   diagnosticsJson(tup->diagnostics, tup->diagnostics_len, ja));
  ptrs[3] =
      KVJson("comments", commentsJson(tup->comments, tup->comments_len, ja));
  // Embed array
  size_t len = tup->statements_len;
  JsonElem *array = RALLOC_ARR(ja, len, JsonElem);
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
