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
static JsonElem patternExprJson(PatternExpr *pep, Arena *ja);
static JsonElem builtinJson(Builtin *bp, Arena *ja);
static JsonElem
patternStructMemberExprJson(struct PatternStructMemberExpr_s *psmep, Arena *ja);
static JsonElem typeStructMemberExprJson(struct TypeStructMemberExpr_s *tsmep,
                                         Arena *ja);
static JsonElem valueStructMemberExprJson(struct ValueStructMemberExpr_s *vsmep,
                                          Arena *ja);
static JsonElem matchCaseExprJson(struct MatchCaseExpr_s *mcep, Arena *ja);

static JsonElem builtinJson(Builtin *bp, Arena *ja) {
  if (bp == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 5;
  JsonKV *ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
  ptrs[0] = KVJson("span", spanJson(bp->span, ja));
  ptrs[1] =
      KVJson("comments", commentsJson(bp->comments, bp->comments_len, ja));
  ptrs[2] = KVJson("diagnostics",
                   diagnosticsJson(bp->diagnostics, bp->diagnostics_len, ja));
  ptrs[3] = KVJson("name", strJson(bp->name));

  JsonElem *parameter_ptrs = RALLOC_ARR(ja, bp->parameters_len, JsonElem);
  for (size_t i = 0; i < bp->parameters_len; i++) {
    parameter_ptrs[i] = stmntJson(&bp->parameters[i], ja);
  }
  ptrs[4] =
      KVJson("parameters", arrDefJson(parameter_ptrs, bp->parameters_len));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem patternExprJson(PatternExpr *pp, Arena *ja) {
  if (pp == NULL) {
    return nullJson();
  }
  size_t ptrs_len = 0;
  JsonKV *ptrs = NULL;
  // 1 reserved for span
  // 2 reserved for diagnostic
  // 3 reserved for comments
  switch (pp->kind) {
  case PEK_None: {
    ptrs_len = 4;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("PEK_None"));
    break;
  }
  case PEK_Group: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("PEK_Group"));
    ptrs[4] = KVJson("value", patternExprJson(pp->groupExpr.value, ja));
    break;
  }
  case PEK_ValueRestriction: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("PEK_ValueRestriction"));
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
    ptrs[4] = KVJson("restriction", strJson(pevrk));
    ptrs[5] =
        KVJson("value", valueExprJson(pp->valueRestriction.valueExpr, ja));
    break;
  }
  case PEK_TypeRestriction: {
    if (pp->typeRestriction.has_binding) {
      ptrs_len = 7;
      ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
      ptrs[6] = KVJson("binding",
                       strJson(internArena(ja, pp->typeRestriction.binding)));
    } else {
      ptrs_len = 6;
      ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    }

    ptrs[0] = KVJson("kind", strJson("PEK_TypeRestriction"));
    ptrs[4] = KVJson("type", typeExprJson(pp->typeRestriction.type, ja));
    ptrs[5] = KVJson("has_binding", boolJson(pp->typeRestriction.has_binding));
    break;
  }
  case PEK_UnaryOp: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("PEK_UnaryOp"));
    char *peuok;
    switch (pp->unaryOp.operator) {
    case PEUOK_Not: {
      peuok = "PEUOK_Not";
      break;
    }
    }
    ptrs[4] = KVJson("operator", strJson(peuok));
    ptrs[5] = KVJson("operand", patternExprJson(pp->unaryOp.operand, ja));
    break;
  }
  case PEK_BinaryOp: {
    ptrs_len = 7;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("PEK_BinaryOp"));
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
    ptrs[4] = KVJson("operator", strJson(pebok));
    ptrs[5] = KVJson("left_operand", patternExprJson(pp->binaryOp.left_operand, ja));
    ptrs[6] = KVJson("right_operand", patternExprJson(pp->binaryOp.right_operand, ja));
    break;
  }
  case PEK_Struct: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("PEK_Struct"));
    size_t members_len = pp->structExpr.members_len;
    JsonElem *members = RALLOC_ARR(ja, members_len, JsonElem);
    for(size_t i = 0; i < members_len; i++) {
      members[i] = patternStructMemberExprJson(&pp->structExpr.members[i], ja);
    }
    ptrs[4] = KVJson("value", arrDefJson(members, members_len));
    break;
  }
  }
  ptrs[1] = KVJson("span", spanJson(pp->span, ja));
  ptrs[2] = KVJson("diagnostics",
                   diagnosticsJson(pp->diagnostics, pp->diagnostics_len, ja));
  ptrs[3] =
      KVJson("comments", commentsJson(pp->comments, pp->comments_len, ja));
  return objDefJson(ptrs, ptrs_len);
}

static JsonElem pathJson(Path *pp, Arena *ja) {
  if (pp == NULL) {
    return nullJson();
  }

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
  case TEK_Builtin: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("TEK_Builtin"));
    ptrs[4] = KVJson("builtin", builtinJson(tep->builtinExpr.builtin, ja));
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
      array[i] = typeStructMemberExprJson(&tep->structExpr.members[i], ja);
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

static JsonElem patternStructMemberExprJson(struct PatternStructMemberExpr_s *psmep,
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
    ptrs[5] = KVJson("field", strJson(internArena(ja, psmep->field.field)));
    break;
  }
  case PSMEK_Rest: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("PSMEK_Rest"));
  }
  }
  ptrs[4] = KVJson("pattern", patternExprJson(psmep->pattern, ja));

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
  ptrs[5] = KVJson("value", valueExprJson(mcep->value, ja));
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
  case VEK_VoidLiteral: {
    ptrs_len = 4;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_VoidLiteral"));
    break;
  }
  case VEK_BoolLiteral: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_BoolLiteral"));
    ptrs[4] = KVJson("value", boolJson(vep->boolLiteral.value));
    break;
  }
  case VEK_IntLiteral: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_IntLiteral"));
    ptrs[4] = KVJson("value", intJson(vep->intLiteral.value));
    break;
  }
  case VEK_CharLiteral: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_CharLiteral"));
    ptrs[4] = KVJson("value", intJson((uint64_t)vep->charLiteral.value));
    break;
  }
  case VEK_FloatLiteral: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_FloatLiteral"));
    ptrs[4] = KVJson("value", numJson(vep->floatLiteral.value));
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
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_StructLiteral"));

    // Embed array
    size_t len = vep->structExpr.members_len;
    JsonElem *array = RALLOC_ARR(ja, len, JsonElem);
    for (size_t i = 0; i < len; i++) {
      array[i] = valueStructMemberExprJson(&vep->structExpr.members[i], ja);
    }
    ptrs[4] = KVJson("members", arrDefJson(array, len));
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
  case VEK_Fn: {
    ptrs_len = 7;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Fn"));

    // Embed array
    size_t len = vep->fnExpr.parameters_len;
    JsonElem *array = RALLOC_ARR(ja, len, JsonElem);
    for (size_t i = 0; i < len; i++) {
      array[i] = patternExprJson(&vep->fnExpr.parameters[i], ja);
    }
    ptrs[4] = KVJson("parameters", arrDefJson(array, len));
    ptrs[5] = KVJson("type", typeExprJson(vep->fnExpr.type, ja));
    ptrs[6] = KVJson("body", valueExprJson(vep->fnExpr.body, ja));
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
  case VEK_Loop: {
    ptrs_len = 7;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Loop"));
    ptrs[4] = KVJson("value", valueExprJson(vep->loopExpr.value, ja));
    ptrs[5] = KVJson("has_label", boolJson(vep->loopExpr.has_label));
    if(vep->loopExpr.has_label) {
      ptrs[6] = KVJson("label", strJson(internArena(ja, vep->loopExpr.label)));
    } else {
      ptrs[6] = KVJson("label", nullJson());
    }
    break;
  }
  case VEK_Builtin: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Builtin"));
    ptrs[4] = KVJson("value", builtinJson(vep->builtinExpr.builtin, ja));
    break;
  }
  case VEK_Defer: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Defer"));
    ptrs[4] = KVJson("value", valueExprJson(vep->deferExpr.value, ja));
    break;
  }
  case VEK_As: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_As"));
    ptrs[4] = KVJson("value", valueExprJson(vep->asExpr.value, ja));
    ptrs[5] = KVJson("type", typeExprJson(vep->asExpr.type, ja));
    break;
  }
  case VEK_Continue: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Continue"));
    ptrs[4] = KVJson("label", strJson(internArena(ja, vep->continueExpr.label)));
    break;
  }
  case VEK_Return: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Return"));
    ptrs[4] = KVJson("label", strJson(internArena(ja, vep->continueExpr.label)));
    ptrs[5] = KVJson("value", valueExprJson(vep->returnExpr.value, ja));
    break;
  }
  case VEK_Match: {
    ptrs_len = 8;
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
    
    ptrs[6] = KVJson("has_label", boolJson(vep->matchExpr.has_label));
    if(vep->matchExpr.has_label) {
      ptrs[7] = KVJson("label", strJson(internArena(ja, vep->matchExpr.label)));
    } else {
      ptrs[7] = KVJson("label", nullJson());
    }
    break;
  }
  case VEK_Block: {
    ptrs_len = 7;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("VEK_Block"));
    // Embed array
    size_t len = vep->blockExpr.statements_len;
    JsonElem *array = RALLOC_ARR(ja, len, JsonElem);
    for (size_t i = 0; i < len; i++) {
      array[i] = stmntJson(&vep->blockExpr.statements[i], ja);
    }
    ptrs[4] = KVJson("statements", arrDefJson(array, len));
    ptrs[5] = KVJson("has_label", boolJson(vep->blockExpr.has_label));
    if(vep->matchExpr.has_label) {
      ptrs[6] = KVJson("label", strJson(internArena(ja, vep->blockExpr.label)));
    } else {
      ptrs[6] = KVJson("label", nullJson());
    }
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
  // Misc
  case SK_Use: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("SK_Use"));
    ptrs[4] = KVJson("path", pathJson(sp->useStmnt.path, ja));
    break;
  }
  case SK_Namespace: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("SK_Namespace"));
    ptrs[4] = KVJson("path", pathJson(sp->namespaceStmnt.path, ja));
    ptrs[5] = KVJson("stmnt", stmntJson(sp->namespaceStmnt.stmnt, ja));
    break;
  }
  case SK_Macro: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("SK_Macro"));
    ptrs[4] = KVJson("macro", strJson(internArena(ja, sp->macroStmnt.name)));
    break;
  }
  // Decls
  case SK_ValDecl: {
    ptrs_len = 6;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("SK_ValDecl"));
    ptrs[4] = KVJson("pattern", patternExprJson(sp->valDecl.pattern, ja));
    ptrs[5] = KVJson("value", valueExprJson(sp->valDecl.value, ja));
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
  // Exprs
  case SK_ValExpr: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("SK_ValExpr"));
    ptrs[4] = KVJson("value", valueExprJson(sp->valExpr.value, ja));
    break;
  }
  case SK_TypeExpr: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("SK_TypeExpr"));
    ptrs[4] = KVJson("type", typeExprJson(sp->typeExpr.type, ja));
    break;
  }
  case SK_PatExpr: {
    ptrs_len = 5;
    ptrs = RALLOC_ARR(ja, ptrs_len, JsonKV);
    ptrs[0] = KVJson("kind", strJson("SK_PatExpr"));
    ptrs[4] = KVJson("pat", patternExprJson(sp->patExpr.pattern, ja));
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