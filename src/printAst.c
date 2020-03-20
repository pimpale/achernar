#include "printAst.h"

#include "ast.h"
#include "json.h"
#include "token.h"
#include "arena.h"

static JsonElem jsonLnCol(LnCol lncol, Arena *ja) {
  JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 2);
  ptrs[0] = JKV("ln", J_NUM(lncol.ln));
  ptrs[1] = JKV("col", J_NUM(lncol.col));
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
  switch (vep->kind) {
  case VE_IntLiteral: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_FloatLiteral: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_CharLiteral: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_StringLiteral: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_ArrayLiteral: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_StructLiteral: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_BinaryOp: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_UnaryOp: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_Call: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_If: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_While: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_For: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_With: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_Pass: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_Break: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_Continue: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_Return: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_Match: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_Block: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_Group: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_FieldAccess: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  case VE_Reference: {
    JsonKV *ptrs = allocArena(ja, sizeof(JsonKV) * 5);
    return J_OBJ_DEF(ptrs, 5);
  }
  }
}

char *printTranslationUnit(TranslationUnit *tup) {
  Arena ja;
  createArena(&ja);
  JsonElem json = jsonTranslationUnit(TranslationUnit tup)

      return "yeet";
}
