#include "printAst.h"

#include "arena.h"
#include "ast.h"
#include "json.h"
#include "token.h"

static JsonElem jsonLnCol(LnCol lncol, Arena *ja) {
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 2);
  ptrs[0] = KVJson("ln", intJson(lncol.ln));
  ptrs[1] = KVJson("col", intJson(lncol.col));
  return objDefJson(ptrs, 2);
}

static JsonElem jsonSpan(Span span, Arena *ja) {
  return nullJson();
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 2);
  ptrs[0] = KVJson("start", jsonLnCol(span.start, ja));
  ptrs[1] = KVJson("end", jsonLnCol(span.end, ja));
  return objDefJson(ptrs, 2);
}

static JsonElem jsonDiagnostic(Diagnostic diagnostic, Arena *ja) {
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 2);
  ptrs[0] = KVJson("kind", strJson(strDiagnosticKind(diagnostic.kind)));
  ptrs[1] = KVJson("span", jsonSpan(diagnostic.span, ja));
  return objDefJson(ptrs, 2);
}

static JsonElem jsonDiagnostics(Diagnostic* diagnostics, size_t diagnostics_length, Arena *ja) {
  JsonElem *ptrs = allocArena(ja, sizeof(JsonElem) * diagnostics_length);
  for(size_t i = 0; i < diagnostics_length; i++) {
    ptrs[i] = jsonDiagnostic(diagnostics[i], ja);
  }
  return arrDefJson(ptrs, diagnostics_length);
}

static JsonElem jsonTypeExpr(TypeExpr *tep, Arena *ja);
static JsonElem jsonValueExpr(ValueExpr *vep, Arena *ja);
static JsonElem jsonStmnt(Stmnt *sp, Arena *ja);
static JsonElem jsonMatchCaseExpr(MatchCaseExpr *mcep, Arena *ja);
static JsonElem jsonBinding(Binding *bp, Arena *ja);

static JsonElem jsonPath(Path* pp, Arena *ja) {
  size_t ptrs_len = 4;
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
  ptrs[0] = KVJson("kind", strJson("path"));
  ptrs[1] = KVJson("span", jsonSpan(pp->span, ja));
  ptrs[2] = KVJson("diagnostics", jsonDiagnostics(pp->diagnostics, pp->diagnostics_length, ja));
  JsonElem *path_ptrs = allocArena(ja, sizeof(JsonElem) * pp->pathSegments_length);
  for(size_t i = 0; i < pp->pathSegments_length; i++) {
    path_ptrs[i] = strJson(INTERN(pp->pathSegments[i], ja));
  }
  ptrs[3] = KVJson("path_segments", arrDefJson(path_ptrs, pp->pathSegments_length));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem jsonTypeExpr(TypeExpr *tep, Arena *ja) {
  if (tep == NULL) {
    return nullJson();
  }

  size_t ptrs_len = 0;
  JsonKV *ptrs = NULL;
  // 1 reserved for span
  // 2 reserved for diagnostic
  switch (tep->kind) {
  case TEK_None: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_None"));
    break;
  }
  case TEK_Omitted: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_Omitted"));
    break;
  }
  case TEK_Reference: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_Reference"));
    ptrs[3] = KVJson("path", jsonPath(tep->referenceExpr.path, ja));
    break;
  }
  case TEK_Typeof: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_Typeof"));
    ptrs[3] = KVJson("value", jsonValueExpr(tep->typeofExpr.value, ja));
    break;
  }
  case TEK_Struct: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_StructExpr"));
    // Embed array
    size_t len = tep->structExpr.members_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonBinding(&tep->structExpr.members[i], ja);
    }
    ptrs[3] = KVJson("members", arrDefJson(array, len));
  }
  case TEK_UnaryOp: {
    ptrs_len = 5;
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
    ptrs[3] = KVJson("operator", strJson(unOpStr));
    ptrs[4] = KVJson("operand", jsonTypeExpr(tep->unaryOp.operand, ja));
    break;
  }
  case TEK_FieldAccess: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_FieldAccess"));
    ptrs[3] = KVJson("value", jsonTypeExpr(tep->fieldAccess.value, ja));
    ptrs[4] = KVJson("field", strJson(INTERN(tep->fieldAccess.field, ja)));
    break;
  }
  }
  ptrs[1] = KVJson("span", jsonSpan(tep->span, ja));
  ptrs[2] = KVJson("diagnostics", jsonDiagnostics(tep->diagnostics, tep->diagnostics_length, ja));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem jsonBinding(Binding *bp, Arena *ja) {
  if (bp == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 5;
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
  ptrs[0] = KVJson("kind", strJson("binding"));
  ptrs[1] = KVJson("span", jsonSpan(bp->span, ja));
  ptrs[2] = KVJson("diagnostics", jsonDiagnostics(bp->diagnostics, bp->diagnostics_length, ja));
  ptrs[3] = KVJson("name", strJson(INTERN(bp->name, ja)));
  ptrs[4] = KVJson("type", jsonTypeExpr(bp->type, ja));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem jsonMatchCaseExpr(MatchCaseExpr *mcep, Arena *ja) {
  if (mcep == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 5;
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
  ptrs[0] = KVJson("kind", strJson("matchCaseExpr"));
  ptrs[1] = KVJson("span", jsonSpan(mcep->span, ja));
  ptrs[2] = KVJson("diagnostics", jsonDiagnostics(mcep->diagnostics, mcep->diagnostics_length, ja));
  ptrs[3] = KVJson("pattern", jsonValueExpr(mcep->pattern, ja));
  ptrs[4] = KVJson("name", jsonValueExpr(mcep->value, ja));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem jsonValueExpr(ValueExpr *vep, Arena *ja) {
  if (vep == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 0;
  JsonKV *ptrs = NULL;
  // 1 reserved for span
  // 2 reserved for diagnostic
  switch (vep->kind) {
  case VEK_None: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_None"));
    break;
  }
  case VEK_IntLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_IntLiteral"));
    ptrs[3] = KVJson("value", intJson(vep->intLiteral.value));
    break;
  }
  case VEK_BoolLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_BoolLiteral"));
    ptrs[3] = KVJson("value", boolJson(vep->boolLiteral.value));
    break;
  }
  case VEK_FloatLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_FloatLiteral"));
    ptrs[3] = KVJson("value", numJson(vep->floatLiteral.value));
    break;
  }
  case VEK_CharLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_CharLiteral"));
    ptrs[3] = KVJson("value", intJson((uint64_t)vep->charLiteral.value));
    break;
  }
  case VEK_StringLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_StringLiteral"));
    ptrs[3] = KVJson("value", strJson(INTERN(vep->stringLiteral.value, ja)));
    break;
  }
  case VEK_ArrayLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * 4);
    ptrs[0] = KVJson("kind", strJson("VEK_ArrayLiteral"));
    // Embed array
    size_t len = vep->arrayLiteral.elements_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonValueExpr(&vep->arrayLiteral.elements[i], ja);
    }
    ptrs[3] = KVJson("elements", arrDefJson(array, len));
    break;
  }
  case VEK_StructLiteral: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_StructLiteral"));
    // TODO
    break;
  }
  case VEK_BinaryOp: {
    ptrs_len = 6;
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
    case VEBOK_ArrayAccess: {
      binOpStr = "VEBOK_ArrayAccess";
      break;
    }
    case VEBOK_Pipeline: {
      binOpStr = "VEBOK_Pipeline";
      break;
    }
    }
    ptrs[3] = KVJson("operator", strJson(binOpStr));
    ptrs[4] =
        KVJson("left_operand", jsonValueExpr(vep->binaryOp.left_operand, ja));
    ptrs[5] =
        KVJson("right_operand", jsonValueExpr(vep->binaryOp.right_operand, ja));
    break;
  }
  case VEK_UnaryOp: {
    ptrs_len = 5;
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
    ptrs[3] = KVJson("operator", strJson(unOpStr));
    ptrs[4] = KVJson("operand", jsonValueExpr(vep->unaryOp.operand, ja));
    break;
  }
  case VEK_Call: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Call"));
    ptrs[3] = KVJson("fn", jsonValueExpr(vep->callExpr.function, ja));
    // Embed array
    size_t len = vep->callExpr.arguments_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonValueExpr(&vep->callExpr.arguments[i], ja);
    }
    ptrs[4] = KVJson("arguments", arrDefJson(array, len));
    break;
  }
  case VEK_If: {
    ptrs_len = 7;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_If"));
    ptrs[3] = KVJson("condition", jsonValueExpr(vep->ifExpr.condition, ja));
    ptrs[4] = KVJson("body", jsonValueExpr(vep->ifExpr.body, ja));
    ptrs[5] = KVJson("has_else", boolJson(vep->ifExpr.has_else));
    if (vep->ifExpr.has_else) {
      ptrs[6] = KVJson("else_body", jsonValueExpr(vep->ifExpr.else_body, ja));
    } else {
      ptrs[6] = KVJson("else_body", nullJson());
    }
    break;
  }
  case VEK_While: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_While"));
    ptrs[3] = KVJson("condition", jsonValueExpr(vep->ifExpr.condition, ja));
    ptrs[4] = KVJson("body", jsonValueExpr(vep->ifExpr.body, ja));
    break;
  }
  case VEK_For: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_For"));
    // TODO
    break;
  }
  case VEK_With: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_With"));
    // TODO
    break;
  }
  case VEK_Pass: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Pass"));
    break;
  }
  case VEK_Break: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Break"));
    break;
  }
  case VEK_Continue: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Continue"));
    break;
  }
  case VEK_Return: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Return"));
    ptrs[3] = KVJson("value", jsonValueExpr(vep->returnExpr.value, ja));
    break;
  }
  case VEK_Match: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Match"));
    ptrs[3] = KVJson("value", jsonValueExpr(vep->matchExpr.value, ja));
    // Embed array
    size_t len = vep->matchExpr.cases_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonMatchCaseExpr(&vep->matchExpr.cases[i], ja);
    }
    ptrs[4] = KVJson("cases", arrDefJson(array, len));
    break;
  }
  case VEK_Block: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Block"));
    // Embed array
    size_t len = vep->blockExpr.statements_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonStmnt(&vep->blockExpr.statements[i], ja);
    }
    ptrs[3] = KVJson("statements", arrDefJson(array, len));
    ptrs[4] = KVJson("suppress_value", boolJson(vep->blockExpr.suppress_value));
    break;
  }
  case VEK_Group: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Group"));
    ptrs[3] = KVJson("value", jsonValueExpr(vep->groupExpr.value, ja));
    break;
  }
  case VEK_FieldAccess: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_FieldAccess"));
    ptrs[3] = KVJson("value", jsonValueExpr(vep->fieldAccess.value, ja));
    ptrs[4] = KVJson("field", strJson(INTERN(vep->fieldAccess.field, ja)));
    break;
  }
  case VEK_Reference: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Reference"));
    ptrs[3] = KVJson("path", jsonPath(vep->reference.path, ja));
    break;
  }
  }
  ptrs[1] = KVJson("span", jsonSpan(vep->span, ja));
  ptrs[2] = KVJson("diagnostics", jsonDiagnostics(vep->diagnostics, vep->diagnostics_length, ja));
  // create final json object
  return objDefJson(ptrs, ptrs_len);
}

JsonElem jsonStmnt(Stmnt *sp, Arena *ja) {
  if (sp == NULL) {
    return nullJson();
  }
  size_t ptrs_len;
  JsonKV *ptrs;
  // 1 reserved for span
  // 2 reserved for diagnostic
  switch (sp->kind) {
  case SK_None: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("SK_None"));
    break;
  }
  case SK_FnDecl: {
    ptrs_len = 7;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("SK_FnDecl"));
    ptrs[3] = KVJson("name", strJson(INTERN(sp->fnDecl.name, ja)));
    // Embed array
    size_t len = sp->fnDecl.params_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonBinding(&sp->fnDecl.params[i], ja);
    }
    ptrs[4] = KVJson("params", arrDefJson(array, len));
    ptrs[5] = KVJson("type", jsonTypeExpr(sp->fnDecl.type, ja));
    ptrs[6] = KVJson("body", jsonValueExpr(sp->fnDecl.body, ja));
    break;
  }
  case SK_VarDecl: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("SK_VarDecl"));
    ptrs[3] = KVJson("binding", jsonBinding(sp->varDecl.binding, ja));
    ptrs[4] = KVJson("value", jsonValueExpr(sp->varDecl.value, ja));
    break;
  }
  case SK_TypeAliasDecl: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("SK_TypeAliasDecl"));
    ptrs[3] = KVJson("type", jsonTypeExpr(sp->aliasStmnt.type, ja));
    ptrs[4] = KVJson("name", strJson(INTERN(sp->aliasStmnt.name, ja)));
    break;
  }
  case SK_Expr: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("SK_Expr"));
    ptrs[3] = KVJson("value", jsonValueExpr(sp->exprStmnt.value, ja));
    break;
  }
  }
  ptrs[1] = KVJson("span", jsonSpan(sp->span, ja));
  ptrs[2] = KVJson("diagnostics", jsonDiagnostics(sp->diagnostics, sp->diagnostics_length, ja));
  // create final json object
  JsonElem je = objDefJson(ptrs, ptrs_len);
  return je;
}

static JsonElem jsonTranslationUnit(TranslationUnit *tup, Arena *ja) {
  if (tup == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 4;
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);

  ptrs[0] = KVJson("kind", strJson("TranslationUnit"));
  ptrs[1] = KVJson("span", jsonSpan(tup->span, ja));
  ptrs[2] = KVJson("diagnostics", jsonDiagnostics(tup->diagnostics, tup->diagnostics_length, ja));
  // Embed array
  size_t len = tup->statements_length;
  JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
  for (size_t i = 0; i < len; i++) {
    array[i] = jsonStmnt(&tup->statements[i], ja);
  }
  ptrs[3] = KVJson("statements", arrDefJson(array, len));
  return objDefJson(ptrs, ptrs_len);
}

char *printTranslationUnit(TranslationUnit *tup) {
  Arena ja;
  createArena(&ja);
  JsonElem json = jsonTranslationUnit(tup, &ja);
  char *str = toStringJsonElem(&json);
  destroyArena(&ja);
  return str;
}
