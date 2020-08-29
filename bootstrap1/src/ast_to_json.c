#include "ast_to_json.h"

#include "com_allocator.h"
#include "com_imath.h"
#include "com_json.h"
#include "com_loc.h"
#include "com_vec.h"
#include "com_writer.h"

#include "ast.h"
#include "token.h"

// utility method to create a vector
#define print_vec_create_m(a)                                                  \
  com_vec_create(com_allocator_alloc(                                          \
      a, (com_allocator_HandleData){.len = 10,                                 \
                                    .flags = com_allocator_defaults(a) |       \
                                             com_allocator_NOLEAK |            \
                                             com_allocator_REALLOCABLE}))

#define push_prop_m(obj) com_vec_push_m(obj, com_json_Prop)
#define push_elem_m(obj) com_vec_push_m(obj, com_json_Elem)

#define mkprop_m(str, val) com_json_prop_m(com_str_lit_m(str), (val))

static com_json_Elem print_objectify(com_vec *props) {
  usize len = com_vec_len_m(props, com_json_Prop);
  com_json_Prop *ptrs = com_vec_release(props);
  return com_json_obj_m(ptrs, len);
}

static com_json_Elem print_arrayify(com_vec *elems) {
  usize len = com_vec_len_m(elems, com_json_Elem);
  com_json_Elem *ptrs = com_vec_release(elems);
  return com_json_array_m(ptrs, len);
}

static com_json_Elem print_LnCol(com_loc_LnCol lncol, com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("lncol")));
  *push_prop_m(&obj) = mkprop_m("ln", com_json_uint_m(lncol.ln.val));
  *push_prop_m(&obj) = mkprop_m("col", com_json_uint_m(lncol.col.val));
  return print_objectify(&obj);
}

static com_json_Elem print_Span(com_loc_Span span, com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("span")));
  *push_prop_m(&obj) = mkprop_m("start", print_LnCol(span.start, a));
  *push_prop_m(&obj) = mkprop_m("end", print_LnCol(span.end, a));
  return print_objectify(&obj);
}

static com_json_Elem print_bigint(com_bigint bigint, com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("bigint")));
  com_vec words = print_vec_create_m(a);
  for (usize i = 0; i < com_bigint_len(&bigint); i++) {
    *push_elem_m(&words) = com_json_uint_m(com_bigint_get_at(&bigint, i));
  }
  *push_prop_m(&obj) = mkprop_m("words", print_arrayify(&words));
  return print_objectify(&obj);
}

static com_json_Elem print_bigdecimal(com_bigdecimal bigdecimal, com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("bigdecimal")));
  *push_prop_m(&obj) = mkprop_m("negative", com_json_bool_m(com_bigdecimal_sign(&bigdecimal) == com_math_NEGATIVE));
  com_vec words = print_vec_create_m(a);
  for (usize i = 0; i < com_bigdecimal_len(&bigdecimal); i++) {
    *push_elem_m(&words) = com_json_uint_m(com_bigdecimal_get_at(&bigdecimal, i));
  }
  *push_prop_m(&obj) = mkprop_m("words", print_arrayify(&words));
  *push_prop_m(&obj) = mkprop_m("precision", com_json_uint_m(com_bigdecimal_get_precision(&bigdecimal)));
  return print_objectify(&obj);
}

static com_json_Elem print_diagnostic(Diagnostic diagnostic, com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(com_str_lit_m("diagnostic")));
  *push_prop_m(&obj) =
      mkprop_m("severity",
               com_json_str_m(strDiagnosticSeverityKind(diagnostic.severity)));
  *push_prop_m(&obj) = mkprop_m("message", com_json_str_m(diagnostic.message));
  *push_prop_m(&obj) = mkprop_m("span", print_Span(diagnostic.span, a));
  com_vec children = print_vec_create_m(a);
  for (usize i = 0; i < diagnostic.children_len; i++) {
    *push_elem_m(&children) = print_diagnostic(diagnostic.children[i], a);
  }
  *push_prop_m(&obj) = mkprop_m("children", print_arrayify(&children));
  return print_objectify(&obj);
}

static com_json_Elem print_Metadata(ast_Metadata metadata, com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(com_str_lit_m("metadata")));
  *push_prop_m(&obj) = mkprop_m("data", com_json_str_m(metadata.data));
  *push_prop_m(&obj) =
      mkprop_m("significant", com_json_bool_m(metadata.significant));
  *push_prop_m(&obj) = mkprop_m("span", print_Span(metadata.span, a));
  return print_objectify(&obj);
}

// add shared data to the vector
static void print_appendCommon(ast_Common node, com_vec *props,
                               com_allocator *a) {
  *push_prop_m(props) = mkprop_m("span", print_Span(node.span, a));
  com_vec metadata = print_vec_create_m(a);
  for (usize i = 0; i < node.metadata_len; i++) {
    *push_elem_m(&metadata) = print_Metadata(node.metadata[i], a);
  }
  *com_vec_push_m(props, com_json_Prop) =
      mkprop_m("metadata", print_arrayify(&metadata));
}

static com_json_Elem print_ModReference(ast_ModReference *mr,
                                        com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(com_str_lit_m("ast_ModReference")));
  *push_prop_m(&obj) = mkprop_m("span", print_Span(mr->span, a));

  *push_prop_m(&obj) =
      mkprop_m("ast_ModReferenceKind",
               com_json_str_m(ast_strModReferenceKind(mr->kind)));
  switch (mr->kind) {
  case ast_MRK_None: {
    // nop
    break;
  }
  case ast_MRK_Omitted: {
    // nop
    break;
  }
  case ast_MRK_Reference: {
    *push_prop_m(&obj) = mkprop_m("name", com_json_str_m(mr->reference.name));
    *push_prop_m(&obj) =
        mkprop_m("mod", print_ModReference(mr->reference.mod, a));
    break;
  }
  }

  return print_objectify(&obj);
}

static com_json_Elem print_ModBinding(ast_ModBinding *mb,
                                        com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(com_str_lit_m("ast_ModBinding")));
  *push_prop_m(&obj) = mkprop_m("span", print_Span(mb->span, a));

  *push_prop_m(&obj) =
      mkprop_m("ast_ModBindingKind",
               com_json_str_m(ast_strModBindingKind(mb->kind)));
  switch (mb->kind) {
  case ast_MBK_None: {
    // nop
    break;
  }
  case ast_MBK_Binding: {
    *push_prop_m(&obj) = mkprop_m("value", com_json_str_m(mb->binding.value));
    break;
  }
  }

  return print_objectify(&obj);
}

static com_json_Elem print_Reference(ast_Reference *reference,
                                     com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(com_str_lit_m("reference")));
  *push_prop_m(&obj) = mkprop_m("span", print_Span(reference->span, a));

  *push_prop_m(&obj) = mkprop_m(
      "reference_kind", com_json_str_m(ast_strReferenceKind(reference->kind)));
  switch (reference->kind) {
  case ast_RK_None: {
    // nop
    break;
  }
  case ast_RK_Reference: {
    *push_prop_m(&obj) = mkprop_m("name", com_json_str_m(reference->reference.name));
    *push_prop_m(&obj) =
        mkprop_m("mod", print_ModReference(reference->reference.mod, a));
    break;
  }
  }

  return print_objectify(&obj);
}

static com_json_Elem print_Binding(ast_Binding *ptr, com_allocator *a) {
  if (ptr == NULL) {
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(com_str_lit_m("binding")));
  *push_prop_m(&obj) = mkprop_m("span", print_Span(ptr->span, a));
  *push_prop_m(&obj) =
      mkprop_m("binding_kind", com_json_str_m(ast_strBindingKind(ptr->kind)));
  switch (ptr->kind) {
  case ast_BK_Bind: {
    *push_prop_m(&obj) = mkprop_m("binding", com_json_str_m(ptr->bind.val));
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

static com_json_Elem print_LabelReference(ast_LabelReference *label,
                                          com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("label")));
  *push_prop_m(&obj) = mkprop_m("span", print_Span(label->span, a));
  *push_prop_m(&obj) = mkprop_m(
      "label_kind", com_json_str_m(ast_strLabelReferenceKind(label->kind)));
  switch (label->kind) {
  case ast_LRK_Label: {
    *push_prop_m(&obj) = mkprop_m("label", com_json_str_m(label->label.label));
    break;
  }
  case ast_LRK_None: {
    // nothing
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_LabelBinding(ast_LabelBinding *label,
                                        com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("label")));
  *push_prop_m(&obj) = mkprop_m("span", print_Span(label->span, a));
  *push_prop_m(&obj) = mkprop_m(
      "label_kind", com_json_str_m(ast_strLabelBindingKind(label->kind)));
  switch (label->kind) {
  case ast_LBK_Label: {
    *push_prop_m(&obj) = mkprop_m("label", com_json_str_m(label->label.label));
    break;
  }
  case ast_LBK_Omitted: {
    // nothing
    break;
  }
  }
  return print_objectify(&obj);
}


static com_json_Elem print_Field(ast_Field *field, com_allocator *a) {
  if (field == NULL) {
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("field")));
  *push_prop_m(&obj) = mkprop_m("span", print_Span(field->span, a));
  *push_prop_m(&obj) =
      mkprop_m("field_kind", com_json_str_m(ast_strFieldKind(field->kind)));
  switch (field->kind) {
  case ast_FK_FieldInt: {
    *push_prop_m(&obj) = mkprop_m("field_int", print_bigint(field->intField.val, a));
    break;
  }
  case ast_FK_FieldStr: {
    *push_prop_m(&obj) = mkprop_m("field_str", com_json_str_m(field->strField.val));
    break;
  }
  case ast_FK_None: {
    // nothing
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_Token(Token *token, com_allocator *a) {
  if (token == NULL) {
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("token")));
  *push_prop_m(&obj) = mkprop_m("span", print_Span(token->span, a));
  *push_prop_m(&obj) =
      mkprop_m("token_kind", com_json_str_m(tk_strKind(token->kind)));
  switch (token->kind) {
  case tk_Identifier: {
    *push_prop_m(&obj) =
        mkprop_m("identifier", com_json_str_m(token->identifierToken.data));
    break;
  }
  case tk_Macro: {
    *push_prop_m(&obj) =
        mkprop_m("macro", com_json_str_m(token->macroToken.data));
    break;
  }
  case tk_Label: {
    *push_prop_m(&obj) =
        mkprop_m("label", com_json_str_m(token->labelToken.data));
    break;
  }
  case tk_Metadata: {
    *push_prop_m(&obj) =
        mkprop_m("metadata_content", com_json_str_m(token->metadataToken.content));
    *push_prop_m(&obj) =
        mkprop_m("metadata_significant", com_json_bool_m(token->metadataToken.significant));
    break;
  }
  case tk_Bool: {
    *push_prop_m(&obj) =
        mkprop_m("bool", com_json_bool_m(token->boolToken.data));
    break;
  }
  case tk_String: {
    *push_prop_m(&obj) = mkprop_m("string", com_json_str_m(token->stringToken.data)); 
    break;
  }
  case tk_Int: {
    *push_prop_m(&obj) =
        mkprop_m("int", print_bigint(token->intToken.data, a));
    break;
  }
  case tk_Float: {
    *push_prop_m(&obj) =
        mkprop_m("float", print_bigdecimal(token->floatToken.data, a));
    break;
  }
  case tk_Char: {
    *push_prop_m(&obj) = mkprop_m("char", com_json_uint_m(token->charToken.data));
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
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("macro")));
  *push_prop_m(&obj) = mkprop_m("span", print_Span(macro->span, a));

  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("macro")));
  *push_prop_m(&obj) = mkprop_m("name", com_json_str_m(macro->name));

  com_vec tokens = print_vec_create_m(a);
  for (usize i = 0; i < macro->tokens_len; i++) {
    *com_vec_push_m(&tokens, com_json_Elem) = print_Token(&macro->tokens[i], a);
  }
  *push_prop_m(&obj) = mkprop_m("tokens", print_arrayify(&tokens));
  return print_objectify(&obj);
}

static com_json_Elem print_Type(ast_Type *type, com_allocator *a);

static com_json_Elem print_TypeStructMember(ast_TypeStructMember *tsmep,
                                            com_allocator *a) {
  if (tsmep == NULL) {
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m(
      "kind", com_json_str_m(com_str_lit_m("type_struct_member_expr")));
  print_appendCommon(tsmep->common, &obj, a);
  *push_prop_m(&obj) =
      mkprop_m("type_struct_member_expr_kind",
               com_json_str_m(ast_strTypeStructMemberKind(tsmep->kind)));
  switch (tsmep->kind) {
  case ast_TSMK_None: {
    // nop
    break;
  }
  case ast_TSMK_Macro: {
    *push_prop_m(&obj) = mkprop_m("macro", print_Macro(tsmep->macro.macro, a));
    break;
  }
  case ast_TSMK_StructMember: {
    *push_prop_m(&obj) =
        mkprop_m("member_field", print_Field(tsmep->structMember.field, a));
    *push_prop_m(&obj) =
        mkprop_m("member_type", print_Type(tsmep->structMember.type, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_Type(ast_Type *type, com_allocator *a) {
  if (type == NULL) {
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("type")));
  print_appendCommon(type->common, &obj, a);
  *push_prop_m(&obj) =
      mkprop_m("type_kind", com_json_str_m(ast_strTypeKind(type->kind)));
  switch (type->kind) {
  case ast_TK_Macro: {
    *push_prop_m(&obj) = mkprop_m("macro", print_Macro(type->macro.macro, a));
    break;
  }
  case ast_TK_Reference: {
    *push_prop_m(&obj) =
        mkprop_m("reference", print_Reference(type->reference.path, a));
    break;
  }
  case ast_TK_Struct: {
    *push_prop_m(&obj) = mkprop_m("struct_kind",
        com_json_str_m(ast_strTypeStructKind(type->structExpr.kind)));
    com_vec members = print_vec_create_m(a);
    for (usize i = 0; i < type->structExpr.members_len; i++) {
      *push_elem_m(&members)  = print_TypeStructMember(&type->structExpr.members[i], a);
    }
    *push_prop_m(&obj) = mkprop_m("struct_members", print_arrayify(&members));
    break;
  }
  case ast_TK_Fn: {
    com_vec parameters = print_vec_create_m(a);
    for (usize i = 0; i < type->fn.parameters_len; i++) {
      *com_vec_push_m(&parameters, com_json_Elem) =
          print_Type(&type->fn.parameters[i], a);
    }
    *push_prop_m(&obj) = mkprop_m("fn_parameters", print_arrayify(&parameters));
    *push_prop_m(&obj) = mkprop_m("fn_type", print_Type(type->fn.type, a));
    break;
  }
  case ast_TK_Group: {
    *push_prop_m(&obj) =
        mkprop_m("group_inner", print_Type(type->group.inner, a));
    break;
  }
  case ast_TK_UnaryOp: {
    *push_prop_m(&obj) = mkprop_m("unary_operation",
        com_json_str_m(ast_strTypeUnaryOpKind(type->unaryOp.op)));
    *push_prop_m(&obj) = mkprop_m("unary_operand",
                                  print_Type(type->unaryOp.operand, a));
    break;
  }
  case ast_TK_BinaryOp: {
    *push_prop_m(&obj) = mkprop_m("binary_operation",
        com_json_str_m(ast_strTypeBinaryOpKind(type->binaryOp.op)));
    *push_prop_m(&obj) = mkprop_m("binary_left_operand",
                                  print_Type(type->binaryOp.left_operand, a));
    *push_prop_m(&obj) = mkprop_m("binary_right_operand",
                                  print_Type(type->binaryOp.right_operand, a));
    break;
  }
  case ast_TK_FieldAccess: {
    *push_prop_m(&obj) =
        mkprop_m("field_name", print_Field(type->fieldAccess.field, a));
    *push_prop_m(&obj) =
        mkprop_m("field_root", print_Type(type->fieldAccess.root, a));
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

static com_json_Elem print_PatStructMember(ast_PatStructMember *psmep,
                                           com_allocator *a) {
  if (psmep == NULL) {
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(com_str_lit_m("pat_struct_member")));
  print_appendCommon(psmep->common, &obj, a);
  *push_prop_m(&obj) =
      mkprop_m("pat_struct_member_kind",
               com_json_str_m(ast_strPatStructMemberKind(psmep->kind)));
  switch (psmep->kind) {
  case ast_PSMK_None: {
    // do nothing
    break;
  }
  case ast_PSMK_Macro: {
    *push_prop_m(&obj) = mkprop_m("macro", print_Macro(psmep->macro.macro, a));
    break;
  }
  case ast_PSMK_Field: {
    *push_prop_m(&obj) =
        mkprop_m("field_name", print_Field(psmep->field.field, a));
    *push_prop_m(&obj) =
        mkprop_m("field_pattern", print_Pat(psmep->field.pat, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_Val(ast_Val *vep, com_allocator *a);

static com_json_Elem print_Pat(ast_Pat *pep, com_allocator *a) {
  if (pep == NULL) {
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("pat")));
  print_appendCommon(pep->common, &obj, a);
  *push_prop_m(&obj) =
      mkprop_m("pat_kind", com_json_str_m(ast_strPatKind(pep->kind)));
  switch (pep->kind) {
  case ast_PK_None: {
    // nop
    break;
  }
  case ast_PK_Macro: {
    *push_prop_m(&obj) = mkprop_m("macro", print_Macro(pep->macro.macro, a));
    break;
  }
  case ast_PK_ValRestriction: {

    *push_prop_m(&obj) = mkprop_m("value_restriction_kind",
                                  com_json_str_m(ast_strPatValRestrictionKind(
                                      pep->valRestriction.restriction)));
    *push_prop_m(&obj) = mkprop_m("value_restriction_value",
                                  print_Val(pep->valRestriction.val, a));
    break;
  }
  case ast_PK_TypeRestriction: {
    *push_prop_m(&obj) = mkprop_m("type_restriction_type",
                                  print_Type(pep->typeRestriction.type, a));
    *push_prop_m(&obj) = mkprop_m("type_restriction_name",
                                  print_Binding(pep->typeRestriction.name, a));
    break;
  }
  case ast_PK_Struct: {
    com_vec members = print_vec_create_m(a);
    for (usize i = 0; i < pep->structExpr.members_len; i++) {
      *com_vec_push_m(&members, com_json_Elem) =
          print_PatStructMember(&pep->structExpr.members[i], a);
    }
    *push_prop_m(&obj) = mkprop_m("struct_members", print_arrayify(&members));
    break;
  }
  case ast_PK_Group: {
    *push_prop_m(&obj) =
        mkprop_m("group_inner", print_Pat(pep->group.inner, a));
    break;
  }
  case ast_PK_UnaryOp: {
    *push_prop_m(&obj) =
        mkprop_m("unary_operation",
                 com_json_str_m(ast_strPatUnaryOpKind(pep->unaryOp.op)));
    *push_prop_m(&obj) =
        mkprop_m("unary_operand", print_Pat(pep->unaryOp.operand, a));
    break;
  }
  case ast_PK_BinaryOp: {
    *push_prop_m(&obj) = mkprop_m("binary_operation",
        com_json_str_m(ast_strPatBinaryOpKind(pep->binaryOp.op)));
    *push_prop_m(&obj) = mkprop_m("binary_left_operand",
                                  print_Pat(pep->binaryOp.left_operand, a));
    *push_prop_m(&obj) = mkprop_m("binary_right_operand",
                                  print_Pat(pep->binaryOp.right_operand, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_MatchCase(ast_MatchCase *mcep, com_allocator *a) {
  if (mcep == NULL) {
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(com_str_lit_m("match_case")));
  print_appendCommon(mcep->common, &obj, a);
  *push_prop_m(&obj) = mkprop_m(
      "match_case_kind", com_json_str_m(ast_strMatchCaseKind(mcep->kind)));
  switch (mcep->kind) {
  case ast_MCK_None: {
    // nop
    break;
  }
  case ast_MCK_Macro: {
    *push_prop_m(&obj) = mkprop_m("macro", print_Macro(mcep->macro.macro, a));
    break;
  }
  case ast_MCK_Case: {
    *push_prop_m(&obj) =
        mkprop_m("case_pat", print_Pat(mcep->matchCase.pattern, a));
    *push_prop_m(&obj) =
        mkprop_m("case_val", print_Val(mcep->matchCase.val, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_ValStructMember(ast_ValStructMember *vsmep,
                                           com_allocator *a) {
  if (vsmep == NULL) {
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(com_str_lit_m("val_struct_member_expr")));
  print_appendCommon(vsmep->common, &obj, a);
  *push_prop_m(&obj) =
      mkprop_m("val_struct_member_expr_kind",
               com_json_str_m(ast_strValStructMemberKind(vsmep->kind)));
  switch (vsmep->kind) {
  case ast_VSMK_None: {
    // nop
    break;
  }
  case ast_VSMK_Macro: {
    *push_prop_m(&obj) = mkprop_m("macro", print_Macro(vsmep->macro.macro, a));
    break;
  }
  case ast_VSMK_Member: {
    *push_prop_m(&obj) =
        mkprop_m("member_name", print_Field(vsmep->member.field, a));
    *push_prop_m(&obj) =
        mkprop_m("member_val", print_Val(vsmep->member.val, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_Stmnt(ast_Stmnt *sp, com_allocator *a);

static com_json_Elem print_Val(ast_Val *vep, com_allocator *a) {
  if (vep == NULL) {
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("val")));
  print_appendCommon(vep->common, &obj, a);
  *push_prop_m(&obj) =
      mkprop_m("val_kind", com_json_str_m(ast_strValKind(vep->kind)));
  switch (vep->kind) {
  case ast_VK_None: {
    // nop
    break;
  }
  case ast_VK_Macro: {
    *push_prop_m(&obj) = mkprop_m("macro", print_Macro(vep->macro.macro, a));
    break;
  }
  case ast_VK_NilLiteral: {
    // nop
    break;
  }
  case ast_VK_BoolLiteral: {
    *push_prop_m(&obj) =
        mkprop_m("bool", com_json_bool_m(vep->boolLiteral.value));
    break;
  }
  case ast_VK_IntLiteral: {
    *push_prop_m(&obj) = mkprop_m("int", print_bigint(vep->intLiteral.value, a));
    break;
  }
  case ast_VK_FloatLiteral: {
    *push_prop_m(&obj) =
        mkprop_m("float", print_bigdecimal(vep->floatLiteral.value, a));
    break;
  }
  case ast_VK_CharLiteral: {
    *push_prop_m(&obj) = mkprop_m("char", com_json_uint_m(vep->charLiteral.value));
    break;
  }
  case ast_VK_StringLiteral: {
    *push_prop_m(&obj) =
        mkprop_m("string", com_json_str_m(vep->stringLiteral.value));
    break;
  }
  case ast_VK_StructLiteral: {
    com_vec members = print_vec_create_m(a);
    for (usize i = 0; i < vep->structExpr.members_len; i++) {
      *com_vec_push_m(&members, com_json_Elem) =
          print_ValStructMember(&vep->structExpr.members[i], a);
    }
    *push_prop_m(&obj) = mkprop_m("struct_members", print_arrayify(&members));
    break;
  }
  case ast_VK_As: {
    *push_prop_m(&obj) = mkprop_m("as_root", print_Val(vep->as.root, a));
    *push_prop_m(&obj) = mkprop_m("as_type", print_Type(vep->as.type, a));
    break;
  }
  case ast_VK_Pipe: {
    *push_prop_m(&obj) = mkprop_m("pipe_root", print_Val(vep->pipe.root, a));
    *push_prop_m(&obj) = mkprop_m("pipe_type", print_Val(vep->pipe.fn, a));
    break;
  }
  case ast_VK_Loop: {
    *push_prop_m(&obj) =
        mkprop_m("loop_label", print_LabelBinding(vep->loop.label, a));
    *push_prop_m(&obj) = mkprop_m("loop_body", print_Val(vep->loop.body, a));
    break;
  }
  case ast_VK_FieldAccess: {
    *push_prop_m(&obj) =
        mkprop_m("field_root", print_Val(vep->fieldAccess.root, a));
    *push_prop_m(&obj) =
        mkprop_m("field_name", print_Field(vep->fieldAccess.field, a));
    break;
  }
  case ast_VK_Reference: {
    *push_prop_m(&obj) =
        mkprop_m("reference", print_Reference(vep->reference.path, a));
    break;
  }
  case ast_VK_UnaryOp: {
    *push_prop_m(&obj) =
        mkprop_m("unary_operation",
                 com_json_str_m(ast_strValUnaryOpKind(vep->unaryOp.op)));
    *push_prop_m(&obj) =
        mkprop_m("unary_operand", print_Val(vep->unaryOp.operand, a));
    break;
  }
  case ast_VK_BinaryOp: {
    *push_prop_m(&obj) = mkprop_m("binary_operation",
        com_json_str_m(ast_strValBinaryOpKind(vep->binaryOp.op)));
    *push_prop_m(&obj) = mkprop_m("binary_left_operand",
                                  print_Val(vep->binaryOp.left_operand, a));
    *push_prop_m(&obj) = mkprop_m("binary_right_operand",
                                  print_Val(vep->binaryOp.right_operand, a));
    break;
  }
  case ast_VK_Fn: {
    com_vec parameters = print_vec_create_m(a);
    for (usize i = 0; i < vep->fn.parameters_len; i++) {
      *com_vec_push_m(&parameters, com_json_Elem) =
          print_Pat(&vep->fn.parameters[i], a);
    }
    *push_prop_m(&obj) = mkprop_m("fn_parameters", print_arrayify(&parameters));
    *push_prop_m(&obj) = mkprop_m("fn_type", print_Type(vep->fn.type, a));
    *push_prop_m(&obj) = mkprop_m("fn_body", print_Val(vep->fn.body, a));
    break;
  }
  case ast_VK_Call: {
    com_vec parameters = print_vec_create_m(a);
    for (usize i = 0; i < vep->call.parameters_len; i++) {
      *com_vec_push_m(&parameters, com_json_Elem) =
          print_Val(&vep->call.parameters[i], a);
    }
    *push_prop_m(&obj) =
        mkprop_m("call_parameters", print_arrayify(&parameters));
    break;
  }
  case ast_VK_Ret: {
    *push_prop_m(&obj) = mkprop_m(
        "return_label", print_LabelReference(vep->returnExpr.label, a));
    *push_prop_m(&obj) =
        mkprop_m("return_value", print_Val(vep->returnExpr.value, a));
    break;
  }
  case ast_VK_Match: {
    *push_prop_m(&obj) = mkprop_m("match_root", print_Val(vep->match.root, a));
    com_vec cases = print_vec_create_m(a);
    for (usize i = 0; i < vep->match.cases_len; i++) {
      *com_vec_push_m(&cases, com_json_Elem) =
          print_MatchCase(&vep->match.cases[i], a);
    }
    *push_prop_m(&obj) = mkprop_m("match_cases", print_arrayify(&cases));
    break;
  }
  case ast_VK_Block: {
    *push_prop_m(&obj) =
        mkprop_m("block_label", print_LabelBinding(vep->block.label, a));
    com_vec stmnts = print_vec_create_m(a);
    for (usize i = 0; i < vep->block.stmnts_len; i++) {
      *com_vec_push_m(&stmnts, com_json_Elem) =
          print_Stmnt(&vep->block.stmnts[i], a);
    }
    *push_prop_m(&obj) = mkprop_m("block_stmnts", print_arrayify(&stmnts));
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_Stmnt(ast_Stmnt *sp, com_allocator *a) {
  if (sp == NULL) {
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("stmnt")));
  print_appendCommon(sp->common, &obj, a);
  *push_prop_m(&obj) =
      mkprop_m("stmnt_kind", com_json_str_m(ast_strStmntKind(sp->kind)));
  switch (sp->kind) {
  case ast_SK_None: {
    // nop
    break;
  }
  case ast_SK_Macro: {
    *push_prop_m(&obj) = mkprop_m("macro", print_Macro(sp->macro.macro, a));
    break;
  }
  case ast_SK_Use: {
    *push_prop_m(&obj) =
        mkprop_m("use_reference", print_ModReference(sp->useStmnt.path, a));
    break;
  }
  case ast_SK_Mod: {
    *push_prop_m(&obj) =
        mkprop_m("mod_name", print_ModBinding(sp->modStmnt.name, a));
    com_vec stmnts = print_vec_create_m(a);
    for (usize i = 0; i < sp->modStmnt.stmnts_len; i++) {
      *push_elem_m(&stmnts) = print_Stmnt(&sp->modStmnt.stmnts[i], a);
    }
    *push_prop_m(&obj) = mkprop_m("mod_stmnts", print_arrayify(&stmnts));
    break;
  }
  case ast_SK_ValDecl: {
    *push_prop_m(&obj) =
        mkprop_m("val_decl_pat", print_Pat(sp->valDecl.pat, a));
    break;
  }
  case ast_SK_ValDeclDefine: {
    *push_prop_m(&obj) =
        mkprop_m("val_decl_define_pat", print_Pat(sp->valDeclDefine.pat, a));
    *push_prop_m(&obj) =
        mkprop_m("val_decl_define_val", print_Val(sp->valDeclDefine.val, a));
    break;
  }
  case ast_SK_TypeDecl: {
    *push_prop_m(&obj) =
        mkprop_m("type_decl_binding", print_Binding(sp->typeDecl.name, a));
    *push_prop_m(&obj) =
        mkprop_m("type_decl_type", print_Type(sp->typeDecl.type, a));
    break;
  }
  case ast_SK_DeferStmnt: {
    *push_prop_m(&obj) =
        mkprop_m("defer_label", print_LabelReference(sp->deferStmnt.label, a));
    *push_prop_m(&obj) =
        mkprop_m("defer_val", print_Val(sp->deferStmnt.val, a));
    break;
  }
  case ast_SK_Val: {
    *push_prop_m(&obj) = mkprop_m("val", print_Val(sp->val.val, a));
    break;
  }
  }
  return print_objectify(&obj);
}

void print_stream(AstConstructor *parser, com_allocator* a, com_writer* writer) {
  while (true) {

    // check for EOF
    DiagnosticLogger dlogger = dlogger_create(a);

    bool eof = ast_eof(parser, &dlogger);

    if(!eof) {
      // Parse the next statement
      ast_Stmnt stmnt;
      ast_parseStmnt(&stmnt, &dlogger, parser);

      // print the json
      com_json_Elem sjson = print_Stmnt(&stmnt, a);
      com_json_serialize(&sjson, writer) ;
      com_writer_append_u8(writer, '\n');
    }

    // print the diagnostics
    com_vec diagnostics = dlogger_release(&dlogger);
    for (usize i = 0; i < com_vec_len_m(&diagnostics, Diagnostic); i--) {
      Diagnostic d = *com_vec_get_m(&diagnostics, i, Diagnostic);
      com_json_Elem djson = print_diagnostic(d, a);
      com_json_serialize(&djson, writer) ;
      com_writer_append_u8(writer, '\n');
    }

    // flush what's been written
    com_writer_flush(writer);

    // Clean up
    com_vec_destroy(&diagnostics);

    if(eof) {
        break;
    }
  }
}
