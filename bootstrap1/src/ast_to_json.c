#include "ast_to_json.h"

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

static j_Elem print_Comment(ast_Comment comment, Allocator *a) {
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
static void print_appendAstNode(ast_Node node, Vector *props, Allocator *a) {
  *VEC_PUSH(props, j_Prop) = J_PROP(J_LITSTR("span"), print_Span(node.span, a));
  j_Elem *ptrs = ALLOC_ARR(a, node.comments_len, j_Elem);
  for (size_t i = 0; i < node.comments_len; i++) {
    ptrs[i] = print_Comment(node.comments[i], a);
  }
  *VEC_PUSH(props, j_Prop) =
      J_PROP(J_LITSTR("comments"), J_ARRAY_ELEM(ptrs, node.comments_len));
}

static j_Elem print_Path(ast_Path *path, Allocator *a) {
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

static j_Elem print_Label(ast_Label *label, Allocator *a) {
  if (label == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("label")));
  print_appendAstNode(label->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("label_kind"),
             print_str(ast_strLabelKind(label->kind)));
  switch (label->kind) {
  case ast_LK_Label: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("label"), print_str(label->label.label));
    break;
  }
  case ast_LK_Omitted: {
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

static j_Elem print_Macro(ast_Macro *macro, Allocator *a) {
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

static j_Elem print_Type(ast_Type *type, Allocator *a);

static j_Elem print_TypeStructMember(ast_TypeStructMember *tsmep,
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
             print_str(ast_strTypeStructMemberKind(tsmep->kind)));
  switch (tsmep->kind) {
  case ast_TSMK_None: {
    // nop
    break;
  }
  case ast_TSMK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_Macro(tsmep->macro.macro, a));
    break;
  }
  case ast_TSMK_StructMember: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("member_name"), print_str(tsmep->structMember.name));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("member_type"), print_Type(tsmep->structMember.type, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_Type(ast_Type *type, Allocator *a) {
  if (type == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("type")));
  print_appendAstNode(type->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) = J_PROP(
      J_LITSTR("type_kind"), print_str(ast_strTypeKind(type->kind)));
  switch (type->kind) {
  case ast_TK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_Macro(type->macro.macro, a));
    break;
  }
  case ast_TK_Reference: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("reference"), print_Path(type->reference.path, a));
    break;
  }
  case ast_TK_Struct: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("struct_kind"),
        print_str(ast_strTypeStructKind(type->structExpr.kind)));
    Vector members = vec_create(a);
    for (size_t i = 0; i < type->structExpr.members_len; i++) {
      *VEC_PUSH(&members, j_Elem) =
          print_TypeStructMember(&type->structExpr.members[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("struct_members"), print_arrayify(&members));
    break;
  }
  case ast_TK_Fn: {
    Vector parameters = vec_create(a);
    for (size_t i = 0; i < type->fn.parameters_len; i++) {
      *VEC_PUSH(&parameters, j_Elem) =
          print_Type(&type->fn.parameters[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("fn_parameters"), print_arrayify(&parameters));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("fn_type"), print_Type(type->fn.type, a));
    break;
  }
  case ast_TK_Group: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("group_inner"),
                                     print_Type(type->group.inner, a));
    break;
  }
  case ast_TK_UnaryOp: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("unary_operation"),
               print_str(ast_strTypeUnaryOpKind(type->unaryOp.op)));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("unary_operand"),
                                     print_Type(type->unaryOp.operand, a));
    break;
  }
  case ast_TK_BinaryOp: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_operation"),
               print_str(ast_strTypeBinaryOpKind(type->binaryOp.op)));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_left_operand"),
               print_Type(type->binaryOp.left_operand, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_right_operand"),
               print_Type(type->binaryOp.right_operand, a));
    break;
  }
  case ast_TK_FieldAccess: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("field_name"), print_str(type->fieldAccess.field));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("field_root"),
                                     print_Type(type->fieldAccess.root, a));
    break;
  }
  default: {
    // noop
    break;
  }
  }

  return print_objectify(&obj);
}

static j_Elem print_Pat(ast_Pat *pep, Allocator *a);

static j_Elem print_PatStructMember(ast_PatStructMember *psmep,
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
             print_str(ast_strPatStructMemberKind(psmep->kind)));
  switch (psmep->kind) {
  case ast_PSMK_None: {
    // do nothing
    break;
  }
  case ast_PSMK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_Macro(psmep->macro.macro, a));
    break;
  }
  case ast_PSMK_Field: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("field_name"), print_str(psmep->field.field));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("field_pattern"),
                                     print_Pat(psmep->field.pattern, a));
    break;
  }
  case ast_PSMK_Rest: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("rest_pattern"), print_Pat(psmep->rest.pattern, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_Val(ast_Val *vep, Allocator *a);

static j_Elem print_Pat(ast_Pat *pep, Allocator *a) {
  if (pep == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("pat")));
  print_appendAstNode(pep->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) = J_PROP(
      J_LITSTR("pat_kind"), print_str(ast_strPatKind(pep->kind)));
  switch (pep->kind) {
  case ast_PK_None: {
    // nop
    break;
  }
  case ast_PK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_Macro(pep->macro.macro, a));
    break;
  }
  case ast_PK_ValRestriction: {

    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("value_restriction_kind"),
               print_str(ast_strPatValRestrictionKind(
                   pep->valRestriction.restriction)));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("value_restriction_value"),
               print_Val(pep->valRestriction.val, a));
    break;
  }
  case ast_PK_TypeRestriction: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("type_restriction_type"),
               print_Type(pep->typeRestriction.type, a));
    break;
  }
  case ast_PK_TypeRestrictionBinding: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("type_restriction_binding_type"),
               print_Type(pep->typeRestrictionBinding.type, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("type_restriction_binding_name"),
               print_str(pep->typeRestrictionBinding.name));
    break;
  }
  case ast_PK_Struct: {
    Vector members = vec_create(a);
    for (size_t i = 0; i < pep->structExpr.members_len; i++) {
      *VEC_PUSH(&members, j_Elem) =
          print_PatStructMember(&pep->structExpr.members[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("struct_members"), print_arrayify(&members));
    break;
  }
  case ast_PK_Group: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("group_inner"), print_Pat(pep->group.inner, a));
    break;
  }
  case ast_PK_UnaryOp: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("unary_operation"),
               print_str(ast_strPatUnaryOpKind(pep->unaryOp.op)));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("unary_operand"),
                                     print_Pat(pep->unaryOp.operand, a));
    break;
  }
  case ast_PK_BinaryOp: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_operation"),
               print_str(ast_strPatBinaryOpKind(pep->binaryOp.op)));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_left_operand"),
               print_Pat(pep->binaryOp.left_operand, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_right_operand"),
               print_Pat(pep->binaryOp.right_operand, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_MatchCase(ast_MatchCase *mcep, Allocator *a) {
  if (mcep == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("match_case")));
  print_appendAstNode(mcep->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("match_case_kind"),
             print_str(ast_strMatchCaseKind(mcep->kind)));
  switch (mcep->kind) {
  case ast_MCK_None: {
    // nop
    break;
  }
  case ast_MCK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_Macro(mcep->macro.macro, a));
    break;
  }
  case ast_MCK_Case: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("case_pat"), print_Pat(mcep->matchCase.pattern, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("case_val"), print_Val(mcep->matchCase.val, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_ValStructMember(ast_ValStructMember *vsmep,
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
             print_str(ast_strValStructMemberKind(vsmep->kind)));
  switch (vsmep->kind) {
  case ast_VSMK_None: {
    // nop
    break;
  }
  case ast_VSMK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_Macro(vsmep->macro.macro, a));
    break;
  }
  case ast_VSMK_Member: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("member_name"), print_str(vsmep->member.name));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("member_val"), print_Val(vsmep->member.val, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_Stmnt(ast_Stmnt *sp, Allocator *a);

static j_Elem print_Val(ast_Val *vep, Allocator *a) {
  if (vep == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("val")));
  print_appendAstNode(vep->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) = J_PROP(
      J_LITSTR("val_kind"), print_str(ast_strValKind(vep->kind)));
  switch (vep->kind) {
  case ast_VK_None: {
    // nop
    break;
  }
  case ast_VK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_Macro(vep->macro.macro, a));
    break;
  }
  case ast_VK_NilLiteral: {
    // literally nothing
    break;
  }
  case ast_VK_BoolLiteral: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("bool"), J_BOOL_ELEM(vep->boolLiteral.value));
    break;
  }
  case ast_VK_IntLiteral: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("int"), J_INT_ELEM(J_UINT(vep->intLiteral.value)));
    break;
  }
  case ast_VK_FloatLiteral: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("float"), J_NUM_ELEM(vep->floatLiteral.value));
    break;
  }
  case ast_VK_CharLiteral: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("char"), J_INT_ELEM(J_UINT((uint64_t)vep->charLiteral.value)));
    break;
  }
  case ast_VK_StringLiteral: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("string"), J_STR_ELEM(J_STR(vep->stringLiteral.value,
                                             vep->stringLiteral.value_len)));
    break;
  }
  case ast_VK_StructLiteral: {
    Vector members = vec_create(a);
    for (size_t i = 0; i < vep->structExpr.members_len; i++) {
      *VEC_PUSH(&members, j_Elem) =
          print_ValStructMember(&vep->structExpr.members[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("struct_members"), print_arrayify(&members));
    break;
  }
  case ast_VK_As: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("as_root"), print_Val(vep->as.root, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("as_type"), print_Type(vep->as.type, a));
    break;
  }
  case ast_VK_Loop: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("loop_label"), print_Label(vep->loop.label, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("loop_body"), print_Val(vep->loop.body, a));
    break;
  }
  case ast_VK_FieldAccess: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("field_root"), print_Val(vep->fieldAccess.root, a));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(
        J_LITSTR("field_name"), print_str(vep->fieldAccess.name));
    break;
  }
  case ast_VK_Reference: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("reference"), print_Path(vep->reference.path, a));
    break;
  }
  case ast_VK_UnaryOp: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("unary_operation"),
               print_str(ast_strValUnaryOpKind(vep->unaryOp.op)));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("unary_operand"),
                                     print_Val(vep->unaryOp.operand, a));
    break;
  }
  case ast_VK_BinaryOp: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_operation"),
               print_str(ast_strValBinaryOpKind(vep->binaryOp.op)));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_left_operand"),
               print_Val(vep->binaryOp.left_operand, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("binary_right_operand"),
               print_Val(vep->binaryOp.right_operand, a));
    break;
  }
  case ast_VK_Fn: {
    Vector parameters = vec_create(a);
    for (size_t i = 0; i < vep->fn.parameters_len; i++) {
      *VEC_PUSH(&parameters, j_Elem) =
          print_Pat(&vep->fn.parameters[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("fn_parameters"), print_arrayify(&parameters));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("fn_type"), print_Type(vep->fn.type, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("fn_body"), print_Val(vep->fn.body, a));
    break;
  }
  case ast_VK_Call: {
    Vector parameters = vec_create(a);
    for (size_t i = 0; i < vep->call.parameters_len; i++) {
      *VEC_PUSH(&parameters, j_Elem) =
          print_Val(&vep->call.parameters[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("call_parameters"), print_arrayify(&parameters));
    break;
  }
  case ast_VK_Return: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("return_label"),
                                     print_Label(vep->returnExpr.label, a));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("return_value"),
                                     print_Val(vep->returnExpr.value, a));
    break;
  }
  case ast_VK_Match: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("match_root"), print_Val(vep->match.root, a));
    Vector cases = vec_create(a);
    for (size_t i = 0; i < vep->match.cases_len; i++) {
      *VEC_PUSH(&cases, j_Elem) =
          print_MatchCase(&vep->match.cases[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("match_cases"), print_arrayify(&cases));
    break;
  }
  case ast_VK_Block: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("block_label"),
                                     print_Label(vep->block.label, a));
    Vector stmnts = vec_create(a);
    for (size_t i = 0; i < vep->block.stmnts_len; i++) {
      *VEC_PUSH(&stmnts, j_Elem) = print_Stmnt(&vep->block.stmnts[i], a);
    }
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("block_stmnts"), print_arrayify(&stmnts));
    break;
  }
  }
  return print_objectify(&obj);
}

static j_Elem print_Stmnt(ast_Stmnt *sp, Allocator *a) {
  if (sp == NULL) {
    return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) =
      J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("stmnt")));
  print_appendAstNode(sp->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("stmnt_kind"),
                                   print_str(ast_strStmntKind(sp->kind)));
  switch (sp->kind) {
  case ast_SK_None: {
    // nop
    break;
  }
  case ast_SK_Macro: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("macro"), print_Macro(sp->macro.macro, a));
    break;
  }
  case ast_SK_Use: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("use_path"), print_Path(sp->useStmnt.path, a));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("use_alias"),
                                     print_str(sp->useStmnt.name));
    break;
  }
  case ast_SK_Namespace: {
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
  case ast_SK_ValDecl: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("val_decl_pat"), print_Pat(sp->valDecl.pat, a));
    break;
  }
  case ast_SK_ValDeclDefine: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("val_decl_define_pat"),
                                     print_Pat(sp->valDeclDefine.pat, a));
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("val_decl_define_val"),
                                     print_Val(sp->valDeclDefine.val, a));
    break;
  }
  case ast_SK_TypeDecl: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("type_decl_pat"),
                                     print_str(sp->typeDecl.name));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("type_decl_type"),
               print_Type(sp->typeDecl.type, a));
    break;
  }
  case ast_SK_DeferStmnt: {
    *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("defer_label"),
                                     print_Label(sp->deferStmnt.label, a));
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("defer_val"), print_Val(sp->deferStmnt.val, a));
    break;
  }
  case ast_SK_Val: {
    *VEC_PUSH(&obj, j_Prop) =
        J_PROP(J_LITSTR("val"), print_Val(sp->val.val, a));
    break;
  }
  }
  return print_objectify(&obj);
}

void print_stream(AstFromCodeConstructor *parser, FILE *file) {
  while (true) {
    Allocator a = std_allocator();

    // Parse the next statement
    ast_Stmnt stmnt;
    Vector diagnostics = vec_create(&a);
    bool eof = !ast_nextStmntAndCheckNext(&stmnt, &diagnostics, parser);

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
