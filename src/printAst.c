#include "printAst.h"

#include "arena.h"
#include "ast.h"
#include "json.h"
#include "token.h"

static JsonElem jsonLnCol(LnCol lncol, Arena *ja) {
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 2);
  ptrs[0] = JKV("ln", J_INT(lncol.ln));
  ptrs[1] = JKV("col", J_INT(lncol.col));
  return J_OBJ_DEF(ptrs, 2);
}

static JsonElem jsonSpan(Span span, Arena *ja) {
  return J_NULL;
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 2);
  ptrs[0] = JKV("start", jsonLnCol(span.start, ja));
  ptrs[1] = JKV("end", jsonLnCol(span.end, ja));
  return J_OBJ_DEF(ptrs, 2);
}

static JsonElem jsonDiagnostic(Diagnostic diagnostic, Arena *ja) {
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 2);
  ptrs[0] = JKV("kind", J_STR(strDiagnosticKind(diagnostic.kind)));
  ptrs[1] = JKV("span", jsonSpan(diagnostic.span, ja));
  return J_OBJ_DEF(ptrs, 2);
}

static JsonElem jsonTypeExpr(TypeExpr *tep, Arena *ja);
static JsonElem jsonValueExpr(ValueExpr *vep, Arena *ja);
static JsonElem jsonStmnt(Stmnt *sp, Arena *ja);
static JsonElem jsonMatchCaseExpr(MatchCaseExpr *mcep, Arena *ja);
static JsonElem jsonBinding(Binding *bp, Arena *ja);

static JsonElem jsonTypeExpr(TypeExpr *tep, Arena *ja) {
  switch (tep->kind) {
  case TEK_Type: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    ptrs[0] = JKV("kind", J_STR("TEK_Type"));
    ptrs[1] = JKV("span", jsonSpan(tep->span, ja));
    ptrs[2] = JKV("diagnostic", jsonDiagnostic(tep->diagnostic, ja));
    ptrs[3] = JKV("name", J_STR(tep->type.name));
    ptrs[4] = JKV("ptrCount", J_INT(tep->type.ptrCount));
    return J_OBJ_DEF(ptrs, 5);
  }
  case TEK_Typeof: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 4);
    ptrs[0] = JKV("kind", J_STR("TEK_Typeof"));
    ptrs[1] = JKV("span", jsonSpan(tep->span, ja));
    ptrs[2] = JKV("diagnostic", jsonDiagnostic(tep->diagnostic, ja));
    ptrs[3] = JKV("value", jsonValueExpr(tep->typeofExpr.value, ja));
    return J_OBJ_DEF(ptrs, 4);
  }
  }
}

static JsonElem jsonBinding(Binding *bp, Arena *ja) {
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
  ptrs[0] = JKV("kind", J_STR("binding"));
  ptrs[1] = JKV("span", jsonSpan(bp->span, ja));
  ptrs[2] = JKV("diagnostic", jsonDiagnostic(bp->diagnostic, ja));
  ptrs[3] = JKV("name", J_STR(bp->name));
  ptrs[4] = JKV("type", jsonTypeExpr(bp->type, ja));
  return J_OBJ_DEF(ptrs, 5);
}

static JsonElem jsonMatchCaseExpr(MatchCaseExpr *mcep, Arena *ja) {
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
  ptrs[0] = JKV("kind", J_STR("matchCaseExpr"));
  ptrs[1] = JKV("span", jsonSpan(mcep->span, ja));
  ptrs[2] = JKV("diagnostic", jsonDiagnostic(mcep->diagnostic, ja));
  ptrs[3] = JKV("pattern", jsonValueExpr(mcep->pattern, ja));
  ptrs[4] = JKV("name", jsonValueExpr(mcep->value, ja));
  return J_OBJ_DEF(ptrs, 5);
}

static JsonElem jsonValueExpr(ValueExpr *vep, Arena *ja) {
  size_t ptrs_len = 0;
  JsonKV *ptrs = NULL;
  // 1 reserved for span
  // 2 reserved for diagnostic
  switch (vep->kind) {
  case VEK_IntLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_IntLiteral"));
    ptrs[3] = JKV("value", J_INT(vep->intLiteral.value));
    break;
  }
  case VEK_FloatLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_FloatLiteral"));
    ptrs[3] = JKV("value", J_NUM(vep->floatLiteral.value));
    break;
  }
  case VEK_CharLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_CharLiteral"));
    ptrs[3] = JKV("value", J_INT((uint64_t)vep->charLiteral.value));
    break;
  }
  case VEK_StringLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_StringLiteral"));
    ptrs[3] = JKV("value", J_STR(vep->stringLiteral.value));
    break;
  }
  case VEK_ArrayLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * 4);
    ptrs[0] = JKV("kind", J_STR("VEK_ArrayLiteral"));
    // Embed array
    size_t len = vep->arrayLiteral.elements_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonValueExpr(&vep->arrayLiteral.elements[i], ja);
    }
    ptrs[3] = JKV("elements", J_ARR_DEF(array, len));
    break;
  }
  case VEK_StructLiteral: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_StructLiteral"));
    // TODO
    break;
  }
  case VEK_BinaryOp: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_BinaryOp"));
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
    ptrs[3] = JKV("operator", J_STR(binOpStr));
    ptrs[4] = JKV("left_operand", jsonValueExpr(vep->binaryOp.left_operand, ja));
    ptrs[5] = JKV("right_operand", jsonValueExpr(vep->binaryOp.right_operand, ja));
    break;
  }
  case VEK_UnaryOp: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_UnaryOp"));
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
    ptrs[3] = JKV("operator", J_STR(unOpStr));
    ptrs[4] = JKV("operand", jsonValueExpr(vep->unaryOp.operand, ja));
    break;
  }
  case VEK_Call: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_Call"));
    ptrs[3] = JKV("fn", jsonValueExpr(vep->callExpr.function, ja));
    // Embed array
    size_t len = vep->callExpr.arguments_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonValueExpr(&vep->callExpr.arguments[i], ja);
    }
    ptrs[4] = JKV("arguments", J_ARR_DEF(array, len));
    break;
  }
  case VEK_If: {
    ptrs_len = 7;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_If"));
    ptrs[3] = JKV("condition", jsonValueExpr(vep->ifExpr.condition, ja));
    ptrs[4] = JKV("body", jsonValueExpr(vep->ifExpr.body, ja));
    ptrs[5] = JKV("has_else", J_BOOL(vep->ifExpr.has_else));
    if (vep->ifExpr.has_else) {
      ptrs[6] = JKV("else_body", jsonValueExpr(vep->ifExpr.else_body, ja));
    } else {
      ptrs[6] = JKV("else_body", J_NULL);
    }
    break;
  }
  case VEK_While: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_While"));
    ptrs[3] = JKV("condition", jsonValueExpr(vep->ifExpr.condition, ja));
    ptrs[4] = JKV("body", jsonValueExpr(vep->ifExpr.body, ja));
    break;
  }
  case VEK_For: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_For"));
    // TODO
    break;
  }
  case VEK_With: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_With"));
    // TODO
    break;
  }
  case VEK_Pass: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_Pass"));
    break;
  }
  case VEK_Break: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_Break"));
    break;
  }
  case VEK_Continue: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_Continue"));
    break;
  }
  case VEK_Return: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_Return"));
    ptrs[3] = JKV("value", jsonValueExpr(vep->returnExpr.value, ja));
    break;
  }
  case VEK_Match: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_Match"));
    ptrs[3] = JKV("value", jsonValueExpr(vep->matchExpr.value, ja));
    // Embed array
    size_t len = vep->matchExpr.cases_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonMatchCaseExpr(&vep->matchExpr.cases[i], ja);
    }
    ptrs[4] = JKV("cases", J_ARR_DEF(array, len));
    break;
  }
  case VEK_Block: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_Block"));
    // Embed array
    size_t len = vep->blockExpr.statements_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonStmnt(&vep->blockExpr.statements[i], ja);
    }
    ptrs[3] = JKV("statements", J_ARR_DEF(array, len));
    ptrs[4] =
        JKV("suppress_value", J_BOOL(vep->blockExpr.suppress_value));
    break;
  }
  case VEK_Group: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_Group"));
    ptrs[3] = JKV("value", jsonValueExpr(vep->groupExpr.value, ja));
    break;
  }
  case VEK_FieldAccess: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_FieldAccess"));
    ptrs[3] = JKV("value", jsonValueExpr(vep->fieldAccess.value, ja));
    ptrs[4] = JKV("field", J_STR(vep->fieldAccess.field));
    break;
  }
  case VEK_Reference: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VEK_Reference"));
    ptrs[3] = JKV("identifier", J_STR(vep->reference.identifier));
    break;
  }
  }
  ptrs[1] = JKV("span", jsonSpan(vep->span, ja));
  ptrs[2] = JKV("diagnostic", jsonDiagnostic(vep->diagnostic, ja));
  // create final json object
  return J_OBJ_DEF(ptrs, ptrs_len);
}

JsonElem jsonStmnt(Stmnt *sp, Arena *ja) {
  size_t ptrs_len;
  JsonKV *ptrs;
  // 1 reserved for span
  // 2 reserved for diagnostic
  switch (sp->kind) {
  case SK_FnDecl: {
    ptrs_len = 7;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("SK_FnDecl"));
    ptrs[3] = JKV("name", J_STR(sp->fnDecl.name));
    // Embed array
    size_t len = sp->fnDecl.params_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonBinding(&sp->fnDecl.params[i], ja);
    }
    ptrs[4] = JKV("params", J_ARR_DEF(array, len));
    ptrs[5] = JKV("type", jsonTypeExpr(sp->fnDecl.type, ja));
    ptrs[6] = JKV("body", jsonValueExpr(sp->fnDecl.body, ja));
    break;
  }
  case SK_VarDecl: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("SK_VarDecl"));
    ptrs[3] = JKV("binding", jsonBinding(sp->varDecl.binding, ja));
    ptrs[4] = JKV("value", jsonValueExpr(sp->varDecl.value, ja));
    break;
  }
  case SK_StructDecl: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("SK_StructDecl"));
    ptrs[3] = JKV("has_name", J_BOOL(sp->structDecl.has_name));
    if (sp->structDecl.has_name) {
      ptrs[4] = JKV("name", J_STR(sp->fnDecl.name));
    } else {
      ptrs[4] = JKV("name", J_NULL);
    }
    // Embed array
    size_t len = sp->structDecl.members_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonBinding(&sp->structDecl.members[i], ja);
    }
    ptrs[5] = JKV("members", J_ARR_DEF(array, len));
    break;
  }
  case SK_TypeAliasDecl: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("SK_TypeAliasDecl"));
    ptrs[3] = JKV("type", jsonTypeExpr(sp->aliasStmnt.type, ja));
    ptrs[4] = JKV("name", J_STR(sp->aliasStmnt.name));
    break;
  }
  case SK_Expr: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("SK_Expr"));
    ptrs[3] = JKV("value", jsonValueExpr(sp->exprStmnt.value, ja));
    break;
  }
  }
  ptrs[1] = JKV("span", jsonSpan(sp->span, ja));
  ptrs[2] = JKV("diagnostic", jsonDiagnostic(sp->diagnostic, ja));
  // create final json object
  JsonElem je = J_OBJ_DEF(ptrs, ptrs_len);
  return je;
}

static JsonElem jsonTranslationUnit(TranslationUnit *tup, Arena *ja) {
  size_t ptrs_len = 4;
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
  ptrs[0] = JKV("kind", J_STR("TranslationUnit"));
  ptrs[1] = JKV("span", jsonSpan(tup->span, ja));
  ptrs[2] = JKV("diagnostic", jsonDiagnostic(tup->diagnostic, ja));
  // Embed array
  size_t len = tup->statements_length;
  JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
  for (size_t i = 0; i < len; i++) {
    JsonElem je = jsonStmnt(&tup->statements[i], ja);
    array[i] = je;
    // TODO array[i] = jsonStmnt(&tup->statements[i], ja);
  }
  ptrs[3] = JKV("statements", J_ARR_DEF(array, len));
  return J_OBJ_DEF(ptrs, ptrs_len);
}

char *printTranslationUnit(TranslationUnit *tup) {
  Arena ja;
  createArena(&ja);
  JsonElem json = jsonTranslationUnit(tup, &ja);
  char *str = toStringJsonElem(&json);
  destroyArena(&ja);
  return str;
}
