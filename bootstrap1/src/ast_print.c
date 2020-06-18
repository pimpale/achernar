#include "ast_print.h"

#include "allocator.h"
#include "json.h"
#include "std_allocator.h"
#include "vector.h"

#include "ast.h"
#include "token.h"

static inline j_Elem print_str(const char* ptr) {
  if(ptr == NULL) {
    return J_NULL_ELEM;
  }
  return J_STR_ELEM(J_ASCIZ(ptr));
}

static inline j_Elem print_objectify(Vector *props) {
  size_t len = VEC_LEN(props, j_Prop);
  j_Prop *ptrs = vec_release(props);
  return J_OBJECT_ELEM(ptrs, len);
}

static inline j_Elem print_arrayify(Vector *elems) {
  size_t len = VEC_LEN(elems, j_Elem);
  j_Elem *ptrs = vec_release(elems);
  return J_ARRAY_ELEM(ptrs, len);
}

static j_Elem print_LnCol(LnCol lncol, Allocator *a) {
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("lncol")));
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("ln"), J_INT_ELEM(J_UINT(lncol.ln.val)));
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("col"), J_INT_ELEM(J_UINT(lncol.col.val)));
  return print_objectify(&obj);
}

static j_Elem print_Span(Span span, Allocator *a) {
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("span")));
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("start"), print_LnCol(span.start, a));
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("end"), print_LnCol(span.end, a));
  return print_objectify(&obj);
}

static j_Elem print_diagnostic(Diagnostic diagnostic, Allocator *a) {
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("diagnostic")));
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("diagnostic"),
             print_str(strDiagnosticKind(diagnostic.kind)));
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("span"), print_Span(diagnostic.span, a));
  return print_objectify(&obj);
}

static j_Elem print_Comment(Comment comment, Allocator *a) {
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("comment")));
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("scope"), print_str(comment.scope));
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("data"), print_str(comment.data));
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("span"), print_Span(comment.span, a));
  return print_objectify(&obj);
}

// add shared data to the vector
static void print_appendAstNode(AstNode node, Vector *props, Allocator *a) {
  *VEC_PUSH(props, j_Prop) = J_PROP(J_LITSTR("span"), print_Span(node.span, a));
  j_Elem *ptrs = ALLOC_ARR(a, node.comments_len, j_Elem);
  for (size_t i = 0; i < node.comments_len; i++) {
    ptrs[i] = print_Comment(node.comments[i], a);
  }
  *VEC_PUSH(props, j_Prop) =
      J_PROP(J_LITSTR("comments"), J_ARRAY_ELEM(ptrs, node.comments_len));
}

static j_Elem print_Path(Path *path, Allocator *a) {
  if (path == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("path")));
  print_appendAstNode(path->node, &obj, a);

  Vector arr = vec_create(a);
  for (size_t i = 0; i < path->pathSegments_len; i++) {
    *VEC_PUSH(&arr, j_Elem) = print_str(path->pathSegments[i]);
  }

  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("path_segments"), print_arrayify(&arr));
  return print_objectify(&obj);
}

static j_Elem print_LabelExpr(LabelExpr *label, Allocator *a) {
  if (label == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("label")));
  print_appendAstNode(label->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("label_kind"),
             print_str(strLabelExprKind(label->kind)));
  switch (label->kind) {
  case LEK_Label: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("label"), print_str(label->label.label));
    break;
  }
  case LEK_Omitted: {
    // nothing
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_Token(Token *token, Allocator *a) {
  if (token == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("token")));
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("span"), print_Span(token->span, a));
  *VEC_PUSH(&obj, j_Prop) = J_PROP(
      J_LITSTR("token_kind"), print_str(tk_strKind(token->kind)));
  switch (token->kind) {
  case tk_Identifier: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("identifier"),
               print_str(token->identifierToken.data));
    break;
  }
  case tk_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_str(token->macroToken.data));
    break;
  }
  case tk_Label: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("label"), print_str(token->labelToken.data));
    break;
  }
  case tk_Comment: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("comment_scope"),
               print_str(token->commentToken.scope));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("comment_data"),
               print_str(token->commentToken.comment));
    break;
  }
  case tk_Bool: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("bool"), J_BOOL_ELEM(token->boolToken.data));
    break;
  }
  case tk_String: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("string"), J_STR_ELEM(J_STR(token->stringToken.data,
                                             token->stringToken.data_len)));
    break;
  }
  case tk_Int: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("int"), J_INT_ELEM(J_UINT(token->intToken.data)));
    break;
  }
  case tk_Float: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("bool"), J_NUM_ELEM(token->floatToken.data));
    break;
  }
  case tk_Char: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("char"), J_INT_ELEM(J_SINT(token->charToken.data)));
    break;
  }
  default: {
    // nop
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_MacroExpr(MacroExpr *macro, Allocator *a) {
  if (macro == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("macro")));
  print_appendAstNode(macro->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("name"), print_str(macro->name));

  Vector tokens = vec_create(a);
  for (size_t i = 0; i < macro->tokens_len; i++) {
    *VEC_PUSH(&tokens, j_Elem) = print_Token(&macro->tokens[i], a);
  }
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("tokens"), print_arrayify(&tokens));
  return print_objectify(&obj);
}

static j_Elem print_TypeExpr(TypeExpr *type, Allocator *a);

static j_Elem print_TypeStructMemberExpr(TypeStructMemberExpr *tsmep,
                                         Allocator *a) {
  if (tsmep == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("type_struct_member_expr")));
  print_appendAstNode(tsmep->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("type_struct_member_expr_kind"),
             print_str(strTypeStructMemberExprKind(tsmep->kind)));
  switch (tsmep->kind) {
  case TSMEK_None: {
    // nop
    break;
  }
  case TSMEK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_MacroExpr(tsmep->macro.macro, a));
    break;
  }
  case TSMEK_StructMember: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("member_name"), print_str(tsmep->structMember.name));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("member_type"), print_TypeExpr(tsmep->structMember.type, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_TypeExpr(TypeExpr *type, Allocator *a) {
  if (type == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("type")));
  print_appendAstNode(type->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) = J_PROP(
      J_LITSTR("type_kind"), print_str(strTypeExprKind(type->kind)));
  switch (type->kind) {
  case TEK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_MacroExpr(type->macro.macro, a));
    break;
  }
  case TEK_Reference: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("reference"), print_Path(type->referenceExpr.path, a));
    break;
  }
  case TEK_Struct: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("struct_kind"),
        print_str(strTypeStructExprKind(type->structExpr.kind)));
    Vector members = vec_create(a);
    for (size_t i = 0; i < type->structExpr.members_len; i++) {
      *VEC_PUSH(&members, j_Elem) =
          print_TypeStructMemberExpr(&type->structExpr.members[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("struct_members"), print_arrayify(&members));
    break;
  }
  case TEK_Fn: {
    Vector parameters = vec_create(a);
    for (size_t i = 0; i < type->fnExpr.parameters_len; i++) {
      *VEC_PUSH(&parameters, j_Elem) =
          print_TypeExpr(&type->fnExpr.parameters[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("fn_parameters"), print_arrayify(&parameters));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("fn_type"), print_TypeExpr(type->fnExpr.type, a));
    break;
  }
  case TEK_Group: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("group_inner"),
                                     print_TypeExpr(type->groupExpr.inner, a));
    break;
  }
  case TEK_UnaryOp: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("unary_operation"),
               print_str(strTypeExprUnaryOpKind(type->unaryOp.op)));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("unary_operand"),
                                     print_TypeExpr(type->unaryOp.operand, a));
    break;
  }
  case TEK_BinaryOp: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_operation"),
               print_str(strTypeExprBinaryOpKind(type->binaryOp.op)));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_left_operand"),
               print_TypeExpr(type->binaryOp.left_operand, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_right_operand"),
               print_TypeExpr(type->binaryOp.right_operand, a));
    break;
  }
  case TEK_FieldAccess: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("field_name"), print_str(type->fieldAccess.field));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("field_root"),
                                     print_TypeExpr(type->fieldAccess.root, a));
    break;
  }
  default: {
    // noop
    break;
  }
  }

  return print_objectify(&obj);
}

static j_Elem print_PatExpr(PatExpr *pep, Allocator *a);

static j_Elem print_PatStructMemberExpr(PatStructMemberExpr *psmep,
                                        Allocator *a) {
  if (psmep == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("pat_struct_member")));
  print_appendAstNode(psmep->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("pat_struct_member_kind"),
             print_str(strPatStructMemberExprKind(psmep->kind)));
  switch (psmep->kind) {
  case PSMEK_None: {
    // do nothing
    break;
  }
  case PSMEK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_MacroExpr(psmep->macro.macro, a));
    break;
  }
  case PSMEK_Field: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("field_name"), print_str(psmep->field.field));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("field_pattern"),
                                     print_PatExpr(psmep->field.pattern, a));
    break;
  }
  case PSMEK_Rest: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("rest_pattern"), print_PatExpr(psmep->rest.pattern, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_ValExpr(ValExpr *vep, Allocator *a);

static j_Elem print_PatExpr(PatExpr *pep, Allocator *a) {
  if (pep == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("pat")));
  print_appendAstNode(pep->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) = J_PROP(
      J_LITSTR("pat_kind"), print_str(strPatExprKind(pep->kind)));
  switch (pep->kind) {
  case PEK_None: {
    // nop
    break;
  }
  case PEK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_MacroExpr(pep->macro.macro, a));
    break;
  }
  case PEK_ValRestriction: {

    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("value_restriction_kind"),
               print_str(strPatExprValRestrictionKind(
                   pep->valRestriction.restriction)));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("value_restriction_value"),
               print_ValExpr(pep->valRestriction.valExpr, a));
    break;
  }
  case PEK_TypeRestriction: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("type_restriction_type"),
               print_TypeExpr(pep->typeRestriction.type, a));
    break;
  }
  case PEK_TypeRestrictionBinding: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("type_restriction_binding_type"),
               print_TypeExpr(pep->typeRestrictionBinding.type, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("type_restriction_binding_name"),
               print_str(pep->typeRestrictionBinding.name));
    break;
  }
  case PEK_Struct: {
    Vector members = vec_create(a);
    for (size_t i = 0; i < pep->structExpr.members_len; i++) {
      *VEC_PUSH(&members, j_Elem) =
          print_PatStructMemberExpr(&pep->structExpr.members[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("struct_members"), print_arrayify(&members));
    break;
  }
  case PEK_Group: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("group_inner"), print_PatExpr(pep->groupExpr.inner, a));
    break;
  }
  case PEK_UnaryOp: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("unary_operation"),
               print_str(strPatExprUnaryOpKind(pep->unaryOp.op)));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("unary_operand"),
                                     print_PatExpr(pep->unaryOp.operand, a));
    break;
  }
  case PEK_BinaryOp: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_operation"),
               print_str(strPatExprBinaryOpKind(pep->binaryOp.op)));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_left_operand"),
               print_PatExpr(pep->binaryOp.left_operand, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_right_operand"),
               print_PatExpr(pep->binaryOp.right_operand, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_MatchCaseExpr(MatchCaseExpr *mcep, Allocator *a) {
  if (mcep == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("match_case")));
  print_appendAstNode(mcep->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("match_case_kind"),
             print_str(strMatchCaseExprKind(mcep->kind)));
  switch (mcep->kind) {
  case MCEK_None: {
    // nop
    break;
  }
  case MCEK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_MacroExpr(mcep->macro.macro, a));
    break;
  }
  case MCEK_Case: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("case_pat"), print_PatExpr(mcep->matchCase.pattern, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("case_val"), print_ValExpr(mcep->matchCase.val, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_ValStructMemberExpr(ValStructMemberExpr *vsmep,
                                        Allocator *a) {
  if (vsmep == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("val_struct_member_expr")));
  print_appendAstNode(vsmep->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("val_struct_member_expr_kind"),
             print_str(strValStructMemberExprKind(vsmep->kind)));
  switch (vsmep->kind) {
  case VSMEK_None: {
    // nop
    break;
  }
  case VSMEK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_MacroExpr(vsmep->macro.macro, a));
    break;
  }
  case VSMEK_Member: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("member_name"), print_str(vsmep->memberExpr.name));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("member_val"), print_ValExpr(vsmep->memberExpr.val, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_Stmnt(Stmnt *sp, Allocator *a);

static j_Elem print_ValExpr(ValExpr *vep, Allocator *a) {
  if (vep == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("val")));
  print_appendAstNode(vep->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) = J_PROP(
      J_LITSTR("val_kind"), print_str(strValExprKind(vep->kind)));
  switch (vep->kind) {
  case VEK_None: {
    // nop
    break;
  }
  case VEK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_MacroExpr(vep->macro.macro, a));
    break;
  }
  case VEK_NilLiteral: {
    // literally nothing
    break;
  }
  case VEK_BoolLiteral: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("bool"), J_BOOL_ELEM(vep->boolLiteral.value));
    break;
  }
  case VEK_IntLiteral: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("int"), J_INT_ELEM(J_UINT(vep->intLiteral.value)));
    break;
  }
  case VEK_FloatLiteral: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("float"), J_NUM_ELEM(vep->floatLiteral.value));
    break;
  }
  case VEK_CharLiteral: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("char"), J_INT_ELEM(J_UINT((uint64_t)vep->charLiteral.value)));
    break;
  }
  case VEK_StringLiteral: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("string"), J_STR_ELEM(J_STR(vep->stringLiteral.value,
                                             vep->stringLiteral.value_len)));
    break;
  }
  case VEK_StructLiteral: {
    Vector members = vec_create(a);
    for (size_t i = 0; i < vep->structExpr.members_len; i++) {
      *VEC_PUSH(&members, j_Elem) =
          print_ValStructMemberExpr(&vep->structExpr.members[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("struct_members"), print_arrayify(&members));
    break;
  }
  case VEK_As: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("as_root"), print_ValExpr(vep->asExpr.root, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("as_type"), print_TypeExpr(vep->asExpr.type, a));
    break;
  }
  case VEK_Loop: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("loop_label"), print_LabelExpr(vep->loopExpr.label, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("loop_body"), print_ValExpr(vep->loopExpr.body, a));
    break;
  }
  case VEK_FieldAccess: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("field_root"), print_ValExpr(vep->fieldAccess.root, a));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("field_name"), print_str(vep->fieldAccess.name));
    break;
  }
  case VEK_Reference: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("reference"), print_Path(vep->reference.path, a));
    break;
  }
  case VEK_UnaryOp: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("unary_operation"),
               print_str(strValExprUnaryOpKind(vep->unaryOp.op)));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("unary_operand"),
                                     print_ValExpr(vep->unaryOp.operand, a));
    break;
  }
  case VEK_BinaryOp: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_operation"),
               print_str(strValExprBinaryOpKind(vep->binaryOp.op)));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_left_operand"),
               print_ValExpr(vep->binaryOp.left_operand, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_right_operand"),
               print_ValExpr(vep->binaryOp.right_operand, a));
    break;
  }
  case TEK_Fn: {

    Vector parameters = vec_create(a);
    for (size_t i = 0; i < vep->fnExpr.parameters_len; i++) {
      *VEC_PUSH(&parameters, j_Elem) =
          print_PatExpr(&vep->fnExpr.parameters[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("fn_parameters"), print_arrayify(&parameters));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("fn_type"), print_TypeExpr(vep->fnExpr.type, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("fn_body"), print_ValExpr(vep->fnExpr.body, a));
    break;
  }
  case VEK_Call: {
    Vector parameters = vec_create(a);
    for (size_t i = 0; i < vep->callExpr.parameters_len; i++) {
      *VEC_PUSH(&parameters, j_Elem) =
          print_ValExpr(&vep->callExpr.parameters[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("call_parameters"), print_arrayify(&parameters));
    break;
  }
  case VEK_Return: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("return_label"),
                                     print_LabelExpr(vep->returnExpr.label, a));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("return_value"),
                                     print_ValExpr(vep->returnExpr.value, a));
    break;
  }
  case VEK_Match: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("match_root"), print_ValExpr(vep->matchExpr.root, a));
    Vector cases = vec_create(a);
    for (size_t i = 0; i < vep->matchExpr.cases_len; i++) {
      *VEC_PUSH(&cases, j_Elem) =
          print_MatchCaseExpr(&vep->matchExpr.cases[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("match_cases"), print_arrayify(&cases));
    break;
  }
  case VEK_Block: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("block_label"),
                                     print_LabelExpr(vep->blockExpr.label, a));
    Vector stmnts = vec_create(a);
    for (size_t i = 0; i < vep->blockExpr.stmnts_len; i++) {
      *VEC_PUSH(&stmnts, j_Elem) = print_Stmnt(&vep->blockExpr.stmnts[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("block_stmnts"), print_arrayify(&stmnts));
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_Stmnt(Stmnt *sp, Allocator *a) {
  if (sp == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("stmnt")));
  print_appendAstNode(sp->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("stmnt_kind"),
                                   print_str(strStmntKind(sp->kind)));
  switch (sp->kind) {
  case SK_None: {
    // nop
    break;
  }
  case SK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_MacroExpr(sp->macro.macro, a));
    break;
  }
  case SK_Use: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("use"), print_Path(sp->useStmnt.path, a));
    break;
  }
  case SK_Namespace: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("namespace_name"),
               print_str(sp->namespaceStmnt.name));
    Vector stmnts = vec_create(a);
    for (size_t i = 0; i < sp->namespaceStmnt.stmnts_len; i++) {
      *VEC_PUSH(&stmnts, j_Elem) =
          print_Stmnt(&sp->namespaceStmnt.stmnts[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("namespace_stmnts"), print_arrayify(&stmnts));
    break;
  }
  case SK_ValDecl: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("val_decl_pat"), print_PatExpr(sp->valDecl.pat, a));
    break;
  }
  case SK_ValDeclDefine: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("val_decl_define_pat"),
                                     print_PatExpr(sp->valDeclDefine.pat, a));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("val_decl_define_val"),
                                     print_ValExpr(sp->valDeclDefine.val, a));
    break;
  }
  case SK_TypeDecl: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("type_decl_pat"),
                                     print_str(sp->typeDecl.name));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("type_decl_type"),
               print_TypeExpr(sp->typeDecl.type, a));
    break;
  }
  case SK_DeferStmnt: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("defer_label"),
                                     print_LabelExpr(sp->deferStmnt.label, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("defer_val"), print_ValExpr(sp->deferStmnt.val, a));
    break;
  }
  case SK_ValExpr: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("val"), print_ValExpr(sp->valExpr.val, a));
    break;
  }
  }
  return print_objectify(&obj);
}

void print_stream(Parser *parser, FILE *file) {
  while (true) {
    Allocator a = std_allocator();

    // Parse the next statement
    Stmnt stmnt;
    Vector diagnostics = vec_create(&a);
    bool eof = !parse_nextStmntAndCheckNext(&stmnt, &diagnostics, parser);

    if (eof) {
      vec_destroy(&diagnostics);
      a_destroy(&a);
      break;
    }

    // print the json
    j_Elem sjson = print_Stmnt(&stmnt, &a);
    fputs(j_stringify(&sjson, &a), file);
    fputc('\n', file);

    for (size_t i = 0; i < VEC_LEN(&diagnostics, Diagnostic); i--) {
      Diagnostic d = *VEC_GET(&diagnostics, i, Diagnostic);
      j_Elem djson = print_diagnostic(d, &a);
      fputs(j_stringify(&djson, &a), file);
      fputc('\n', file);
    }
    fflush(file);

    // Clean up
    vec_destroy(&diagnostics);
    a_destroy(&a);
  }
}
