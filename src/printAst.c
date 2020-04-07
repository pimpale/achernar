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

static JsonElem jsonTypeExpr(TypeExpr *tep, Arena *ja);
static JsonElem jsonValueExpr(ValueExpr *vep, Arena *ja);
static JsonElem jsonStmnt(Stmnt *sp, Arena *ja);
static JsonElem jsonMatchCaseExpr(MatchCaseExpr *mcep, Arena *ja);
static JsonElem jsonBinding(Binding *bp, Arena *ja);

static JsonElem jsonTypeExpr(TypeExpr *tep, Arena *ja) {
  if(tep == NULL) {
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
  case TEK_Type: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_Type"));
    ptrs[3] = KVJson("name", strJson(tep->type.name));
    ptrs[4] = KVJson("ptrCount", intJson(tep->type.ptrCount));
    break;
  }
  case TEK_Typeof: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("TEK_Typeof"));
    ptrs[3] = KVJson("value", jsonValueExpr(tep->typeofExpr.value, ja));
    break;
  }
  }
  ptrs[1] = KVJson("span", jsonSpan(tep->span, ja));
  ptrs[2] = KVJson("diagnostic", jsonDiagnostic(tep->diagnostic, ja));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem jsonBinding(Binding *bp, Arena *ja) {
  if(bp == NULL) {
    return nullJson();
  }
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
  ptrs[1] = KVJson("span", jsonSpan(bp->span, ja));
  ptrs[2] = KVJson("diagnostic", jsonDiagnostic(bp->diagnostic, ja));
  ptrs[3] = KVJson("name", strJson(bp->name));
  ptrs[4] = KVJson("type", jsonTypeExpr(bp->type, ja));
  return objDefJson(ptrs, 5);
}

static JsonElem jsonMatchCaseExpr(MatchCaseExpr *mcep, Arena *ja) {
  if(mcep == NULL) {
    return nullJson();
  }
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
  ptrs[0] = KVJson("kind", strJson("matchCaseExpr"));
  ptrs[1] = KVJson("span", jsonSpan(mcep->span, ja));
  ptrs[2] = KVJson("diagnostic", jsonDiagnostic(mcep->diagnostic, ja));
  ptrs[3] = KVJson("pattern", jsonValueExpr(mcep->pattern, ja));
  ptrs[4] = KVJson("name", jsonValueExpr(mcep->value, ja));
  return objDefJson(ptrs, 5);
}

static JsonElem jsonValueExpr(ValueExpr *vep, Arena *ja) {
  if(vep == NULL) {
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
    ptrs[3] = KVJson("value", strJson(vep->stringLiteral.value));
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
    case BOK_Add: {
      binOpStr = "BOK_Add";
      break;
    }
    case BOK_Sub: {
      binOpStr = "BOK_Sub";
      break;
    }
    case BOK_Mul: {
      binOpStr = "BOK_Mul";
      break;
    }
    case BOK_Div: {
      binOpStr = "BOK_Div";
      break;
    }
    case BOK_Mod: {
      binOpStr = "BOK_Mod";
      break;
    }
    case BOK_BitAnd: {
      binOpStr = "BOK_BitAnd";
      break;
    }
    case BOK_BitOr: {
      binOpStr = "BOK_BitOr";
      break;
    }
    case BOK_BitXor: {
      binOpStr = "BOK_BitXor";
      break;
    }
    case BOK_BitShl: {
      binOpStr = "BOK_BitShl";
      break;
    }
    case BOK_BitShr: {
      binOpStr = "BOK_BitShr";
      break;
    }
    case BOK_LogicalAnd: {
      binOpStr = "BOK_LogicalAnd";
      break;
    }
    case BOK_LogicalOr: {
      binOpStr = "BOK_LogicalOr";
      break;
    }
    case BOK_CompEqual: {
      binOpStr = "BOK_CompEqual";
      break;
    }
    case BOK_CompNotEqual: {
      binOpStr = "BOK_CompNotEqual";
      break;
    }
    case BOK_CompLess: {
      binOpStr = "BOK_CompLess";
      break;
    }
    case BOK_CompLessEqual: {
      binOpStr = "BOK_CompLessEqual";
      break;
    }
    case BOK_CompGreater: {
      binOpStr = "BOK_CompGreater";
      break;
    }
    case BOK_CompGreaterEqual: {
      binOpStr = "BOK_CompGreaterEqual";
      break;
    }
    case BOK_ArrayAccess: {
      binOpStr = "BOK_ArrayAccess";
      break;
    }
    case BOK_Pipeline: {
      binOpStr = "BOK_Pipeline";
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
    case UOK_Negate: {
      unOpStr = "UOK_Negate";
      break;
    }
    case UOK_Posit: {
      unOpStr = "UOK_Posit";
      break;
    }
    case UOK_LogicalNot: {
      unOpStr = "UOK_LogicalNot";
      break;
    }
    case UOK_BitNot: {
      unOpStr = "UOK_BitNot";
      break;
    }
    case UOK_Ref: {
      unOpStr = "UOK_Ref";
      break;
    }
    case UOK_Deref: {
      unOpStr = "UOK_Deref";
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
    ptrs[4] = KVJson("field", strJson(vep->fieldAccess.field));
    break;
  }
  case VEK_Reference: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("VEK_Reference"));
    ptrs[3] = KVJson("identifier", strJson(vep->reference.identifier));
    break;
  }
  }
  ptrs[1] = KVJson("span", jsonSpan(vep->span, ja));
  ptrs[2] = KVJson("diagnostic", jsonDiagnostic(vep->diagnostic, ja));
  // create final json object
  return objDefJson(ptrs, ptrs_len);
}

JsonElem jsonStmnt(Stmnt *sp, Arena *ja) {
  if(sp == NULL) {
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
    ptrs[3] = KVJson("name", strJson(sp->fnDecl.name));
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
  case SK_StructDecl: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("SK_StructDecl"));
    ptrs[3] = KVJson("has_name", boolJson(sp->structDecl.has_name));
    if (sp->structDecl.has_name) {
      ptrs[4] = KVJson("name", strJson(sp->fnDecl.name));
    } else {
      ptrs[4] = KVJson("name", nullJson());
    }
    // Embed array
    size_t len = sp->structDecl.members_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonBinding(&sp->structDecl.members[i], ja);
    }
    ptrs[5] = KVJson("members", arrDefJson(array, len));
    break;
  }
  case SK_TypeAliasDecl: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = KVJson("kind", strJson("SK_TypeAliasDecl"));
    ptrs[3] = KVJson("type", jsonTypeExpr(sp->aliasStmnt.type, ja));
    ptrs[4] = KVJson("name", strJson(sp->aliasStmnt.name));
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
  ptrs[2] = KVJson("diagnostic", jsonDiagnostic(sp->diagnostic, ja));
  // create final json object
  JsonElem je = objDefJson(ptrs, ptrs_len);
  return je;
}

static JsonElem jsonTranslationUnit(TranslationUnit *tup, Arena *ja) {
  if(tup == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 4;
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);

  ptrs[0] = KVJson("kind", strJson("TranslationUnit"));
  ptrs[1] = KVJson("span", jsonSpan(tup->span, ja));
  ptrs[2] = KVJson("diagnostic", jsonDiagnostic(tup->diagnostic, ja));
  // Embed array
  size_t len = tup->statements_length;
  JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
  for (size_t i = 0; i < len; i++) {
    JsonElem je = jsonStmnt(&tup->statements[i], ja);
    array[i] = je;
    // TODO array[i] = jsonStmnt(&tup->statements[i], ja);
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
