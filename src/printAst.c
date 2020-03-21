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
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 2);
  ptrs[0] = JKV("start", jsonLnCol(span.start, ja));
  ptrs[1] = JKV("end", jsonLnCol(span.end, ja));
  return J_OBJ_DEF(ptrs, 2);
}

static JsonElem jsonDiagnostic(Diagnostic diagnostic, Arena *ja) {
  // TODO
  return J_NULL;
}

static JsonElem jsonTypeExpr(TypeExpr *tep, Arena *ja);
static JsonElem jsonValueExpr(ValueExpr *vep, Arena *ja);
static JsonElem jsonStmnt(Stmnt *sp, Arena *ja);
static JsonElem jsonMatchCaseExpr(MatchCaseExpr *mcep, Arena *ja);
static JsonElem jsonBinding(Binding *bp, Arena *ja);

static JsonElem jsonTypeExpr(TypeExpr *tep, Arena *ja) {
  switch (tep->kind) {
  case TE_Type: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    ptrs[0] = JKV("kind", J_STR("TE_Type"));
    ptrs[1] = JKV("span", jsonSpan(tep->span, ja));
    ptrs[2] = JKV("diagnostic", jsonDiagnostic(tep->diagnostic, ja));
    ptrs[3] = JKV("name", J_STR(tep->type.name));
    ptrs[4] = JKV("ptrCount", J_INT(tep->type.ptrCount));
    return J_OBJ_DEF(ptrs, 5);
  }
  case TE_Typeof: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 4);
    ptrs[0] = JKV("kind", J_STR("TE_Typeof"));
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
  size_t ptrs_len;
  JsonKV *ptrs;
  // 1 reserved for span
  // 2 reserved for diagnostic
  switch (vep->kind) {
  case VE_IntLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_IntLiteral"));
    ptrs[3] = JKV("value", J_INT(vep->intLiteral.value));
    break;
  }
  case VE_FloatLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_FloatLiteral"));
    ptrs[3] = JKV("value", J_NUM(vep->floatLiteral.value));
    break;
  }
  case VE_CharLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_CharLiteral"));
    ptrs[3] = JKV("value", J_INT((uint64_t)vep->charLiteral.value));
    break;
  }
  case VE_StringLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_StringLiteral"));
    ptrs[3] = JKV("value", J_STR(vep->stringLiteral.value));
    break;
  }
  case VE_ArrayLiteral: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * 4);
    ptrs[0] = JKV("kind", J_STR("VE_ArrayLiteral"));
    // Embed array
    size_t len = vep->arrayLiteral.elements_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonValueExpr(&vep->arrayLiteral.elements[i], ja);
    }
    ptrs[3] = JKV("elements", J_ARR_DEF(array, len));
    break;
  }
  case VE_StructLiteral: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_StructLiteral"));
    // TODO
    break;
  }
  case VE_BinaryOp: {
    ptrs_len = 6;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_BinaryOp"));
    char *binOpStr;
    switch (vep->binaryOp.operator) {
    case EBO_Add: {
      binOpStr = "EBO_Add";
      break;
    }
    case EBO_Sub: {
      binOpStr = "EBO_Sub";
      break;
    }
    case EBO_Mul: {
      binOpStr = "EBO_Mul";
      break;
    }
    case EBO_Div: {
      binOpStr = "EBO_Div";
      break;
    }
    case EBO_Mod: {
      binOpStr = "EBO_Mod";
      break;
    }
    case EBO_BitAnd: {
      binOpStr = "EBO_BitAnd";
      break;
    }
    case EBO_BitOr: {
      binOpStr = "EBO_BitOr";
      break;
    }
    case EBO_BitXor: {
      binOpStr = "EBO_BitXor";
      break;
    }
    case EBO_BitShl: {
      binOpStr = "EBO_BitShl";
      break;
    }
    case EBO_BitShr: {
      binOpStr = "EBO_BitShr";
      break;
    }
    case EBO_LogicalAnd: {
      binOpStr = "EBO_LogicalAnd";
      break;
    }
    case EBO_LogicalOr: {
      binOpStr = "EBO_LogicalOr";
      break;
    }
    case EBO_CompEqual: {
      binOpStr = "EBO_CompEqual";
      break;
    }
    case EBO_CompNotEqual: {
      binOpStr = "EBO_CompNotEqual";
      break;
    }
    case EBO_CompLess: {
      binOpStr = "EBO_CompLess";
      break;
    }
    case EBO_CompLessEqual: {
      binOpStr = "EBO_CompLessEqual";
      break;
    }
    case EBO_CompGreater: {
      binOpStr = "EBO_CompGreater";
      break;
    }
    case EBO_CompGreaterEqual: {
      binOpStr = "EBO_CompGreaterEqual";
      break;
    }
    case EBO_ArrayAccess: {
      binOpStr = "EBO_ArrayAccess";
      break;
    }
    case EBO_Pipeline: {
      binOpStr = "EBO_Pipeline";
      break;
    }
    }
    ptrs[3] = JKV("binOpKind", J_STR(binOpStr));
    ptrs[4] = JKV("operand_1", jsonValueExpr(vep->binaryOp.operand_1, ja));
    ptrs[5] = JKV("operand_2", jsonValueExpr(vep->binaryOp.operand_2, ja));
    break;
  }
  case VE_UnaryOp: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_UnaryOp"));
    char *unOpStr;
    switch (vep->unaryOp.operator) {
    case EUO_Negate: {
      unOpStr = "EUO_Negate";
      break;
    }
    case EUO_Posit: {
      unOpStr = "EUO_Posit";
      break;
    }
    case EUO_LogicalNot: {
      unOpStr = "EUO_LogicalNot";
      break;
    }
    case EUO_BitNot: {
      unOpStr = "EUO_BitNot";
      break;
    }
    case EUO_Ref: {
      unOpStr = "EUO_Ref";
      break;
    }
    case EUO_Deref: {
      unOpStr = "EUO_Deref";
      break;
    }
    }
    ptrs[3] = JKV("unOpKind", J_STR(unOpStr));
    ptrs[4] = JKV("operand", jsonValueExpr(vep->unaryOp.operand, ja));
    break;
  }
  case VE_Call: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_Call"));
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
  case VE_If: {
    ptrs_len = 7;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_If"));
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
  case VE_While: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_While"));
    ptrs[3] = JKV("condition", jsonValueExpr(vep->ifExpr.condition, ja));
    ptrs[4] = JKV("body", jsonValueExpr(vep->ifExpr.body, ja));
    break;
  }
  case VE_For: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_For"));
    // TODO
    break;
  }
  case VE_With: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_With"));
    // TODO
    break;
  }
  case VE_Pass: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_Pass"));
    break;
  }
  case VE_Break: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_Break"));
    break;
  }
  case VE_Continue: {
    ptrs_len = 3;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_Continue"));
    break;
  }
  case VE_Return: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_Return"));
    ptrs[3] = JKV("value", jsonValueExpr(vep->returnExpr.value, ja));
    break;
  }
  case VE_Match: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_Match"));
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
  case VE_Block: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_Block"));
    // Embed array
    size_t len = vep->blockExpr.statements_length;
    JsonElem *array = allocArena(ja, len * sizeof(JsonElem));
    for (size_t i = 0; i < len; i++) {
      array[i] = jsonStmnt(&vep->blockExpr.statements[i], ja);
    }
    ptrs[3] = JKV("statements", J_ARR_DEF(array, len));
    ptrs[4] =
        JKV("trailing_semicolon", J_BOOL(vep->blockExpr.trailing_semicolon));
    break;
  }
  case VE_Group: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_Group"));
    ptrs[3] = JKV("value", jsonValueExpr(vep->groupExpr.value, ja));
    break;
  }
  case VE_FieldAccess: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_FieldAccess"));
    ptrs[3] = JKV("value", jsonValueExpr(vep->fieldAccess.value, ja));
    ptrs[4] = JKV("field", J_STR(vep->fieldAccess.field));
    break;
  }
  case VE_Reference: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("VE_Reference"));
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
  case S_FnDecl: {
    ptrs_len = 7;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("S_FnDecl"));
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
  case S_VarDecl: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("S_VarDecl"));
    ptrs[3] = JKV("binding", jsonBinding(sp->varDecl.binding, ja));
    ptrs[4] = JKV("value", jsonValueExpr(sp->varDecl.value, ja));
    break;
  }
  case S_StructDecl: {
    ptrs_len = 7;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("S_StructDecl"));
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
    ptrs[6] = JKV("trailing_comma", J_BOOL(sp->structDecl.trailing_comma));
    break;
  }
  case S_TypeAliasDecl: {
    ptrs_len = 5;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("S_TypeAliasDecl"));
    ptrs[3] = JKV("type", jsonTypeExpr(sp->aliasStmnt.type, ja));
    ptrs[4] = JKV("name", J_STR(sp->aliasStmnt.name));
    break;
  }
  case S_Expr: {
    ptrs_len = 4;
    ptrs = allocArena(ja, sizeof(JsonKV) * ptrs_len);
    ptrs[0] = JKV("kind", J_STR("S_Expr"));
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
