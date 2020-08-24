#include "ast_to_json.h"

#include "com_allocator.h"
#include "com_json.h"
#include "com_vec.h"
#include "com_loc.h"

#include "ast.h"
#include "token.h"

static inline com_json_Elem print_objectify(com_vec *props) {
  usize len = com_vec_len_m(props, com_json_Prop);
  com_json_Prop *ptrs = com_vec_release(props);
  return com_json_obj_m(ptrs, len);
}

static inline com_json_Elem print_arrayify(com_vec *elems) {
  usize len = com_vec_len_m(elems, com_json_Elem);
  com_json_Elem *ptrs = com_vec_release(elems);
  return com_json_array_m(ptrs, len);
}

static com_json_Elem print_com_loc_LnCol(com_loc_LnCol lncol, com_allocator *a) {
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("lncol")));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("ln"), J_INT_ELEM(J_UINT(lncol.ln.val)));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("col"), J_INT_ELEM(J_UINT(lncol.col.val)));
  return print_objectify(&obj);
}

static com_json_Elem print_com_loc_Span(com_loc_Span span, com_allocator *a) {
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("span")));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("start"), print_com_loc_LnCol(span.start, a));
  *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("end"), print_com_loc_LnCol(span.end, a));
  return print_objectify(&obj);
}

static com_json_Elem print_diagnostic(Diagnostic diagnostic, com_allocator *a) {
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("diagnostic")));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("severity"),
             print_str(strDiagnosticSeverityKind(diagnostic.severity)));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("message"), print_str(diagnostic.message));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("range"), print_com_loc_Span(diagnostic.range, a));
  return print_objectify(&obj);
}

static com_json_Elem print_Metadata(ast_Metadata comment, com_allocator *a) {
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("comment")));
  *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("scope"), print_str(comment.scope));
  *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("data"), print_str(comment.data));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("span"), print_com_loc_Span(comment.span, a));
  return print_objectify(&obj);
}

// add shared data to the vector
static void print_appendCommon(ast_Common node, com_vec *props, com_allocator *a) {
  *com_vec_push_m(props, com_json_Prop) = com_json_prop_m(com_str_lit_m("span"), print_com_loc_Span(node.span, a));
  com_json_Elem *ptrs = ALLOC_ARR(a, node.comments_len, com_json_Elem);
  for (usize i = 0; i < node.comments_len; i++) {
    ptrs[i] = print_Metadata(node.comments[i], a);
  }
  *com_vec_push_m(props, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("comments"), com_json_array_m(ptrs, node.comments_len));
}

static com_json_Elem print_Reference(ast_Reference *reference, com_allocator *a) {
  if (reference == NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("reference")));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("span"), print_com_loc_Span(reference->span, a));

  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("reference_kind"),
             print_str(ast_strReferenceKind(reference->kind)));
  switch (reference->kind) {
  case ast_RK_None: {
    // nop
    break;
  }
  case ast_RK_Path: {
    com_vec arr = vec_create(a);
    for (usize i = 0; i < reference->path.segments_len; i++) {
      *com_vec_push_m(&arr, com_json_Elem) = print_str(reference->path.segments[i]);
    }
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("reference_segments"), print_arrayify(&arr));

    break;
  }
  }

  return print_objectify(&obj);
}

static com_json_Elem print_LabelBinding(ast_LabelBinding *label, com_allocator *a) {
  if (label == NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("label")));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("span"), print_com_loc_Span(label->span, a));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("label_kind"), print_str(ast_strLabelBindingKind(label->kind)));
  switch (label->kind) {
  case ast_LBK_Label: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("label"), print_str(label->label.label));
    break;
  }
  case ast_LBK_Omitted: {
    // nothing
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_LabelReference(ast_LabelReference *label, com_allocator *a) {
  if (label == NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("label")));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("span"), print_com_loc_Span(label->span, a));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("label_kind"), print_str(ast_strLabelReferenceKind(label->kind)));
  switch (label->kind) {
  case ast_LRK_Label: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("label"), print_str(label->label.label));
    break;
  }
  case ast_LRK_None: {
    // nothing
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_Field(ast_Field *field, com_allocator *a) {
  if (field== NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("field")));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("span"), print_com_loc_Span(field->span, a));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("field_kind"), print_str(ast_strFieldKind(field->kind)));
  switch (field->kind) {
  case ast_FK_Field: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("field"), print_str(field->field.name));
    break;
  }
  case ast_FK_None: {
    // nothing
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_Binding(ast_Binding *ptr, com_allocator *a) {
  if (ptr== NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("binding")));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("span"), print_com_loc_Span(ptr->span, a));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("binding_kind"), print_str(ast_strBindingKind(ptr->kind)));
  switch (ptr->kind) {
  case ast_BK_Bind: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("binding"), print_str(ptr->bind.val));
    break;
  }
  case ast_BK_Ignore: {
    // nothing
    break;
  }
  case ast_BK_None: {
    // nothing
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_Token(Token *token, com_allocator *a) {
  if (token == NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("token")));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("span"), print_com_loc_Span(token->span, a));
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("token_kind"), print_str(tk_strKind(token->kind)));
  switch (token->kind) {
  case tk_Identifier: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("identifier"), print_str(token->identifierToken.data));
    break;
  }
  case tk_Macro: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("macro"), print_str(token->macroToken.data));
    break;
  }
  case tk_Label: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("label"), print_str(token->labelToken.data));
    break;
  }
  case tk_Metadata: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("comment_scope"), print_str(token->commentToken.scope));
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("comment_data"),
                                     print_str(token->commentToken.comment));
    break;
  }
  case tk_Bool: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("bool"), J_BOOL_ELEM(token->boolToken.data));
    break;
  }
  case tk_String: {
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(
        com_str_lit_m("string"), J_STR_ELEM(J_STR(token->stringToken.data,
                                             token->stringToken.data_len)));
    break;
  }
  case tk_Int: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("int"), J_INT_ELEM(J_UINT(token->intToken.data)));
    break;
  }
  case tk_Float: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("bool"), J_NUM_ELEM(token->floatToken.data));
    break;
  }
  case tk_Char: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("char"), J_INT_ELEM(J_SINT(token->charToken.data)));
    break;
  }
  default: {
    // nop
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_Macro(ast_Macro *macro, com_allocator *a) {
  if (macro == NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("macro")));
  *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("span"), print_com_loc_Span(macro->span, a));

  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("macro")));
  *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("name"), print_str(macro->name));

  com_vec tokens = vec_create(a);
  for (usize i = 0; i < macro->tokens_len; i++) {
    *com_vec_push_m(&tokens, com_json_Elem) = print_Token(&macro->tokens[i], a);
  }
  *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("tokens"), print_arrayify(&tokens));
  return print_objectify(&obj);
}

static com_json_Elem print_Type(ast_Type *type, com_allocator *a);

static com_json_Elem print_TypeStructMember(ast_TypeStructMember *tsmep,
                                     com_allocator *a) {
  if (tsmep == NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("type_struct_member_expr")));
  print_appendCommon(tsmep->common, &obj, a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("type_struct_member_expr_kind"),
             print_str(ast_strTypeStructMemberKind(tsmep->kind)));
  switch (tsmep->kind) {
  case ast_TSMK_None: {
    // nop
    break;
  }
  case ast_TSMK_Macro: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("macro"), print_Macro(tsmep->macro.macro, a));
    break;
  }
  case ast_TSMK_StructMember: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("member_field"), print_Field(tsmep->structMember.field, a));
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("member_type"),
                                     print_Type(tsmep->structMember.type, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_Type(ast_Type *type, com_allocator *a) {
  if (type == NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("type")));
  print_appendCommon(type->common, &obj, a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("type_kind"), print_str(ast_strTypeKind(type->kind)));
  switch (type->kind) {
  case ast_TK_Macro: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("macro"), print_Macro(type->macro.macro, a));
    break;
  }
  case ast_TK_Reference: {
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(
        com_str_lit_m("reference"), print_Reference(type->reference.path, a));
    break;
  }
  case ast_TK_Struct: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("struct_kind"),
               print_str(ast_strTypeStructKind(type->structExpr.kind)));
    com_vec members = vec_create(a);
    for (usize i = 0; i < type->structExpr.members_len; i++) {
      *com_vec_push_m(&members, com_json_Elem) =
          print_TypeStructMember(&type->structExpr.members[i], a);
    }
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("struct_members"), print_arrayify(&members));
    break;
  }
  case ast_TK_Fn: {
    com_vec parameters = vec_create(a);
    for (usize i = 0; i < type->fn.parameters_len; i++) {
      *com_vec_push_m(&parameters, com_json_Elem) = print_Type(&type->fn.parameters[i], a);
    }
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("fn_parameters"), print_arrayify(&parameters));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("fn_type"), print_Type(type->fn.type, a));
    break;
  }
  case ast_TK_Group: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("group_inner"), print_Type(type->group.inner, a));
    break;
  }
  case ast_TK_UnaryOp: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("unary_operation"),
               print_str(ast_strTypeUnaryOpKind(type->unaryOp.op)));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("unary_operand"), print_Type(type->unaryOp.operand, a));
    break;
  }
  case ast_TK_BinaryOp: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("binary_operation"),
               print_str(ast_strTypeBinaryOpKind(type->binaryOp.op)));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("binary_left_operand"),
               print_Type(type->binaryOp.left_operand, a));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("binary_right_operand"),
               print_Type(type->binaryOp.right_operand, a));
    break;
  }
  case ast_TK_FieldAccess: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("field_name"), print_Field(type->fieldAccess.field, a));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("field_root"), print_Type(type->fieldAccess.root, a));
    break;
  }
  default: {
    // noop
    break;
  }
  }

  return print_objectify(&obj);
}

static com_json_Elem print_Pat(ast_Pat *pep, com_allocator *a);

static com_json_Elem print_PatStructMember(ast_PatStructMember *psmep, com_allocator *a) {
  if (psmep == NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("pat_struct_member")));
  print_appendCommon(psmep->common, &obj, a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("pat_struct_member_kind"),
             print_str(ast_strPatStructMemberKind(psmep->kind)));
  switch (psmep->kind) {
  case ast_PSMK_None: {
    // do nothing
    break;
  }
  case ast_PSMK_Macro: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("macro"), print_Macro(psmep->macro.macro, a));
    break;
  }
  case ast_PSMK_Field: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("field_name"), print_Field(psmep->field.field, a));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("field_pattern"), print_Pat(psmep->field.pat, a));
    break;
  }

  }
  return print_objectify(&obj);
}

static com_json_Elem print_Val(ast_Val *vep, com_allocator *a);

static com_json_Elem print_Pat(ast_Pat *pep, com_allocator *a) {
  if (pep == NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("pat")));
  print_appendCommon(pep->common, &obj, a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("pat_kind"), print_str(ast_strPatKind(pep->kind)));
  switch (pep->kind) {
  case ast_PK_None: {
    // nop
    break;
  }
  case ast_PK_Macro: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("macro"), print_Macro(pep->macro.macro, a));
    break;
  }
  case ast_PK_ValRestriction: {

    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("value_restriction_kind"),
                                     print_str(ast_strPatValRestrictionKind(
                                         pep->valRestriction.restriction)));
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("value_restriction_value"),
                                     print_Val(pep->valRestriction.val, a));
    break;
  }
  case ast_PK_TypeRestriction: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("type_restriction_type"),
               print_Type(pep->typeRestriction.type, a));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("type_restriction_name"),
               print_Binding(pep->typeRestriction.name, a));
    break;
  }
  case ast_PK_Struct: {
    com_vec members = vec_create(a);
    for (usize i = 0; i < pep->structExpr.members_len; i++) {
      *com_vec_push_m(&members, com_json_Elem) =
          print_PatStructMember(&pep->structExpr.members[i], a);
    }
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("struct_members"), print_arrayify(&members));
    break;
  }
  case ast_PK_Group: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("group_inner"), print_Pat(pep->group.inner, a));
    break;
  }
  case ast_PK_UnaryOp: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("unary_operation"),
               print_str(ast_strPatUnaryOpKind(pep->unaryOp.op)));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("unary_operand"), print_Pat(pep->unaryOp.operand, a));
    break;
  }
  case ast_PK_BinaryOp: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("binary_operation"),
               print_str(ast_strPatBinaryOpKind(pep->binaryOp.op)));
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("binary_left_operand"),
                                     print_Pat(pep->binaryOp.left_operand, a));
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("binary_right_operand"),
                                     print_Pat(pep->binaryOp.right_operand, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_MatchCase(ast_MatchCase *mcep, com_allocator *a) {
  if (mcep == NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("match_case")));
  print_appendCommon(mcep->common, &obj, a);
  *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("match_case_kind"),
                                   print_str(ast_strMatchCaseKind(mcep->kind)));
  switch (mcep->kind) {
  case ast_MCK_None: {
    // nop
    break;
  }
  case ast_MCK_Macro: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("macro"), print_Macro(mcep->macro.macro, a));
    break;
  }
  case ast_MCK_Case: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("case_pat"), print_Pat(mcep->matchCase.pattern, a));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("case_val"), print_Val(mcep->matchCase.val, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_ValStructMember(ast_ValStructMember *vsmep, com_allocator *a) {
  if (vsmep == NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("val_struct_member_expr")));
  print_appendCommon(vsmep->common, &obj, a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("val_struct_member_expr_kind"),
             print_str(ast_strValStructMemberKind(vsmep->kind)));
  switch (vsmep->kind) {
  case ast_VSMK_None: {
    // nop
    break;
  }
  case ast_VSMK_Macro: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("macro"), print_Macro(vsmep->macro.macro, a));
    break;
  }
  case ast_VSMK_Member: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("member_name"), print_Field(vsmep->member.field, a));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("member_val"), print_Val(vsmep->member.val, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_Stmnt(ast_Stmnt *sp, com_allocator *a);

static com_json_Elem print_Val(ast_Val *vep, com_allocator *a) {
  if (vep == NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("val")));
  print_appendCommon(vep->common, &obj, a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("val_kind"), print_str(ast_strValKind(vep->kind)));
  switch (vep->kind) {
  case ast_VK_None: {
    // nop
    break;
  }
  case ast_VK_Macro: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("macro"), print_Macro(vep->macro.macro, a));
    break;
  }
  case ast_VK_NilLiteral: {
    // literally nothing
    break;
  }
  case ast_VK_BoolLiteral: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("bool"), J_BOOL_ELEM(vep->boolLiteral.value));
    break;
  }
  case ast_VK_IntLiteral: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("int"), J_INT_ELEM(J_UINT(vep->intLiteral.value)));
    break;
  }
  case ast_VK_FloatLiteral: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("float"), J_NUM_ELEM(vep->floatLiteral.value));
    break;
  }
  case ast_VK_CharLiteral: {
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(
        com_str_lit_m("char"), J_INT_ELEM(J_UINT((uint64_t)vep->charLiteral.value)));
    break;
  }
  case ast_VK_StringLiteral: {
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(
        com_str_lit_m("string"), J_STR_ELEM(J_STR(vep->stringLiteral.value,
                                             vep->stringLiteral.value_len)));
    break;
  }
  case ast_VK_StructLiteral: {
    com_vec members = vec_create(a);
    for (usize i = 0; i < vep->structExpr.members_len; i++) {
      *com_vec_push_m(&members, com_json_Elem) =
          print_ValStructMember(&vep->structExpr.members[i], a);
    }
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("struct_members"), print_arrayify(&members));
    break;
  }
  case ast_VK_As: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("as_root"), print_Val(vep->as.root, a));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("as_type"), print_Type(vep->as.type, a));
    break;
  }
  case ast_VK_Loop: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("loop_label"), print_LabelBinding(vep->loop.label, a));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("loop_body"), print_Val(vep->loop.body, a));
    break;
  }
  case ast_VK_FieldAccess: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("field_root"), print_Val(vep->fieldAccess.root, a));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("field_name"), print_Field(vep->fieldAccess.field, a));
    break;
  }
  case ast_VK_Reference: {
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(
        com_str_lit_m("reference"), print_Reference(vep->reference.path, a));
    break;
  }
  case ast_VK_UnaryOp: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("unary_operation"),
               print_str(ast_strValUnaryOpKind(vep->unaryOp.op)));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("unary_operand"), print_Val(vep->unaryOp.operand, a));
    break;
  }
  case ast_VK_BinaryOp: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("binary_operation"),
               print_str(ast_strValBinaryOpKind(vep->binaryOp.op)));
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("binary_left_operand"),
                                     print_Val(vep->binaryOp.left_operand, a));
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("binary_right_operand"),
                                     print_Val(vep->binaryOp.right_operand, a));
    break;
  }
  case ast_VK_Fn: {
    com_vec parameters = vec_create(a);
    for (usize i = 0; i < vep->fn.parameters_len; i++) {
      *com_vec_push_m(&parameters, com_json_Elem) = print_Pat(&vep->fn.parameters[i], a);
    }
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("fn_parameters"), print_arrayify(&parameters));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("fn_type"), print_Type(vep->fn.type, a));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("fn_body"), print_Val(vep->fn.body, a));
    break;
  }
  case ast_VK_Call: {
    com_vec parameters = vec_create(a);
    for (usize i = 0; i < vep->call.parameters_len; i++) {
      *com_vec_push_m(&parameters, com_json_Elem) = print_Val(&vep->call.parameters[i], a);
    }
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("call_parameters"), print_arrayify(&parameters));
    break;
  }
  case ast_VK_Return: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("return_label"), print_LabelReference(vep->returnExpr.label, a));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("return_value"), print_Val(vep->returnExpr.value, a));
    break;
  }
  case ast_VK_Match: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("match_root"), print_Val(vep->match.root, a));
    com_vec cases = vec_create(a);
    for (usize i = 0; i < vep->match.cases_len; i++) {
      *com_vec_push_m(&cases, com_json_Elem) = print_MatchCase(&vep->match.cases[i], a);
    }
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("match_cases"), print_arrayify(&cases));
    break;
  }
  case ast_VK_Block: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("block_label"), print_LabelBinding(vep->block.label, a));
    com_vec stmnts = vec_create(a);
    for (usize i = 0; i < vep->block.stmnts_len; i++) {
      *com_vec_push_m(&stmnts, com_json_Elem) = print_Stmnt(&vep->block.stmnts[i], a);
    }
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("block_stmnts"), print_arrayify(&stmnts));
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_Stmnt(ast_Stmnt *sp, com_allocator *a) {
  if (sp == NULL) {
    return com_json_NULL;
  }
  com_vec obj = vec_create(a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("kind"), J_STR_ELEM(com_str_lit_m("stmnt")));
  print_appendCommon(sp->common, &obj, a);
  *com_vec_push_m(&obj, com_json_Prop) =
      com_json_prop_m(com_str_lit_m("stmnt_kind"), print_str(ast_strStmntKind(sp->kind)));
  switch (sp->kind) {
  case ast_SK_None: {
    // nop
    break;
  }
  case ast_SK_Macro: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("macro"), print_Macro(sp->macro.macro, a));
    break;
  }
  case ast_SK_Use: {
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(
        com_str_lit_m("use_reference"), print_Reference(sp->useStmnt.path, a));
    break;
  }
  case ast_SK_Namespace: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("namespace_name"), print_Binding(sp->namespaceStmnt.name, a));
    com_vec stmnts = vec_create(a);
    for (usize i = 0; i < sp->namespaceStmnt.stmnts_len; i++) {
      *com_vec_push_m(&stmnts, com_json_Elem) =
          print_Stmnt(&sp->namespaceStmnt.stmnts[i], a);
    }
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("namespace_stmnts"), print_arrayify(&stmnts));
    break;
  }
  case ast_SK_ValDecl: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("val_decl_pat"), print_Pat(sp->valDecl.pat, a));
    break;
  }
  case ast_SK_ValDeclDefine: {
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("val_decl_define_pat"),
                                     print_Pat(sp->valDeclDefine.pat, a));
    *com_vec_push_m(&obj, com_json_Prop) = com_json_prop_m(com_str_lit_m("val_decl_define_val"),
                                     print_Val(sp->valDeclDefine.val, a));
    break;
  }
  case ast_SK_TypeDecl: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("type_decl_binding"), print_Binding(sp->typeDecl.name, a));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("type_decl_type"), print_Type(sp->typeDecl.type, a));
    break;
  }
  case ast_SK_DeferStmnt: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("defer_label"), print_LabelReference(sp->deferStmnt.label, a));
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("defer_val"), print_Val(sp->deferStmnt.val, a));
    break;
  }
  case ast_SK_Val: {
    *com_vec_push_m(&obj, com_json_Prop) =
        com_json_prop_m(com_str_lit_m("val"), print_Val(sp->val.val, a));
    break;
  }
  }
  return print_objectify(&obj);
}

void print_stream(AstConstructor *parser, FILE *file) {
  while (true) {
    com_allocator a = std_allocator();

    // Parse the next statement
    ast_Stmnt stmnt;
    DiagnosticLogger dlogger = dlogger_create(&a);
    bool eof = !ast_nextStmntAndCheckNext(&stmnt, &dlogger, parser);
    com_vec diagnostics = dlogger_release(&dlogger);

    if (eof) {
      vec_destroy(&diagnostics);
      a_destroy(&a);
      break;
    }

    // print the json
    com_json_Elem sjson = print_Stmnt(&stmnt, &a);
    fputs(j_stringify(&sjson, &a), file);
    fputc('\n', file);

    for (usize i = 0; i < com_vec_len_m(&diagnostics, Diagnostic); i--) {
      Diagnostic d = *VEC_GET(&diagnostics, i, Diagnostic);
      com_json_Elem djson = print_diagnostic(d, &a);
      fputs(j_stringify(&djson, &a), file);
      fputc('\n', file);
    }
    fflush(file);

    // Clean up
    vec_destroy(&diagnostics);
    a_destroy(&a);
  }
}
