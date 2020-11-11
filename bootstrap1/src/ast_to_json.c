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
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(com_str_lit_m("bigint")));
  com_vec words = print_vec_create_m(a);
  for (usize i = 0; i < com_bigint_len(&bigint); i++) {
    *push_elem_m(&words) = com_json_uint_m(com_bigint_get_at(&bigint, i));
  }
  *push_prop_m(&obj) = mkprop_m("words", print_arrayify(&words));
  return print_objectify(&obj);
}

static com_json_Elem print_bigdecimal(com_bigdecimal bigdecimal,
                                      com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(com_str_lit_m("bigdecimal")));
  *push_prop_m(&obj) =
      mkprop_m("negative", com_json_bool_m(com_bigdecimal_sign(&bigdecimal) ==
                                           com_math_NEGATIVE));
  com_vec words = print_vec_create_m(a);
  for (usize i = 0; i < com_bigdecimal_len(&bigdecimal); i++) {
    *push_elem_m(&words) =
        com_json_uint_m(com_bigdecimal_get_at(&bigdecimal, i));
  }
  *push_prop_m(&obj) = mkprop_m("words", print_arrayify(&words));
  *push_prop_m(&obj) = mkprop_m(
      "precision", com_json_uint_m(com_bigdecimal_get_precision(&bigdecimal)));
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
  // TODO
  return;
  *push_prop_m(props) = mkprop_m("span", print_Span(node.span, a));
  com_vec metadata = print_vec_create_m(a);
  for (usize i = 0; i < node.metadata_len; i++) {
    *push_elem_m(&metadata) = print_Metadata(node.metadata[i], a);
  }
  *com_vec_push_m(props, com_json_Prop) =
      mkprop_m("metadata", print_arrayify(&metadata));
}

// Forward declare
static com_json_Elem print_Expr(ast_Expr *ep, com_allocator *a);

static com_json_Elem print_Identifier(ast_Identifier *identifier,
                                      com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(com_str_lit_m("identifier")));
  *push_prop_m(&obj) = mkprop_m("span", print_Span(identifier->span, a));

  *push_prop_m(&obj) =
      mkprop_m("identifier_kind",
               com_json_str_m(ast_strIdentifierKind(identifier->kind)));
  switch (identifier->kind) {
  case ast_IK_None: {
    // nop
    break;
  }
  case ast_IK_Identifier: {
    *push_prop_m(&obj) = mkprop_m("name", com_json_str_m(identifier->id.name));
    break;
  }
  }

  return print_objectify(&obj);
}

static com_json_Elem print_Label(ast_Label *label, com_allocator *a) {
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) = mkprop_m("kind", com_json_str_m(com_str_lit_m("label")));
  *push_prop_m(&obj) = mkprop_m("span", print_Span(label->span, a));
  *push_prop_m(&obj) =
      mkprop_m("label_kind", com_json_str_m(ast_strLabelKind(label->kind)));
  switch (label->kind) {
  case ast_LK_Label: {
    *push_prop_m(&obj) = mkprop_m("label", com_json_str_m(label->label.label));
    break;
  }
  case ast_LK_None: {
    // nothing
    break;
  }
  case ast_LK_Omitted: {
    // nothing
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_CompoundElement(ast_CompoundElement *cep,
                                           com_allocator *a) {
  if (cep == NULL) {
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(com_str_lit_m("compound_element")));
  print_appendCommon(cep->common, &obj, a);
  *push_prop_m(&obj) =
      mkprop_m("compound_element_expr_kind",
               com_json_str_m(ast_strCompoundElementKind(cep->kind)));
  switch (cep->kind) {
  case ast_CEK_None: {
    // nop
    break;
  }
  case ast_CEK_Element: {
    *push_prop_m(&obj) =
        mkprop_m("name", print_Identifier(cep->element.name, a));
    *push_prop_m(&obj) = mkprop_m("val", print_Expr(cep->element.val, a));
    break;
  }
  }
  return print_objectify(&obj);
}

static com_json_Elem print_Expr(ast_Expr *vep, com_allocator *a) {
  if (vep == NULL) {
    return com_json_null_m;
  }
  com_vec obj = print_vec_create_m(a);
  print_appendCommon(vep->common, &obj, a);
  *push_prop_m(&obj) =
      mkprop_m("kind", com_json_str_m(ast_strExprKind(vep->kind)));
  switch (vep->kind) {
  case ast_EK_None:
  case ast_EK_BindIgnore: {
    // nop
    break;
  }
  case ast_EK_Bind: {
    *push_prop_m(&obj) =
        mkprop_m("binding", print_Identifier(vep->binding.binding, a));
    break;
  }
  case ast_EK_AtBind: {
    *push_prop_m(&obj) =
        mkprop_m("at_binding_pat", print_Identifier(vep->atBinding.binding, a));
    *push_prop_m(&obj) =
        mkprop_m("at_binding_id", print_Expr(vep->atBinding.pat, a));
    break;
  }
  case ast_EK_Int: {
    *push_prop_m(&obj) =
        mkprop_m("int", print_bigint(vep->intLiteral.value, a));
    break;
  }
  case ast_EK_Real: {
    *push_prop_m(&obj) =
        mkprop_m("real", print_bigdecimal(vep->realLiteral.value, a));
    break;
  }
  case ast_EK_String: {
    *push_prop_m(&obj) =
        mkprop_m("string", com_json_str_m(vep->stringLiteral.value));
    break;
  }
  case ast_EK_Struct: {
    com_vec elements = print_vec_create_m(a);
    for (usize i = 0; i < vep->structLiteral.elements_len; i++) {
      *com_vec_push_m(&elements, com_json_Elem) =
          print_CompoundElement(&vep->structLiteral.elements[i], a);
    }
    *push_prop_m(&obj) = mkprop_m("struct_elements", print_arrayify(&elements));
    break;
  }
  case ast_EK_Loop: {
    *push_prop_m(&obj) = mkprop_m("loop_body", print_Expr(vep->loop.body, a));
    break;
  }
  case ast_EK_ModuleAccess: {
    *push_prop_m(&obj) =
        mkprop_m("module_root", print_Expr(vep->moduleAccess.module, a));
    *push_prop_m(&obj) =
        mkprop_m("module_name", print_Identifier(vep->moduleAccess.field, a));
    break;
  }
  case ast_EK_Reference: {
    *push_prop_m(&obj) =
        mkprop_m("reference", print_Identifier(vep->reference.reference, a));
    break;
  }
  case ast_EK_UnaryOp: {
    *push_prop_m(&obj) =
        mkprop_m("unary_operation",
                 com_json_str_m(ast_strExprUnaryOpKind(vep->unaryOp.op)));
    *push_prop_m(&obj) =
        mkprop_m("unary_operand", print_Expr(vep->unaryOp.operand, a));
    break;
  }
  case ast_EK_BinaryOp: {
    *push_prop_m(&obj) =
        mkprop_m("binary_operation",
                 com_json_str_m(ast_strExprBinaryOpKind(vep->binaryOp.op)));
    *push_prop_m(&obj) = mkprop_m("binary_left_operand",
                                  print_Expr(vep->binaryOp.left_operand, a));
    *push_prop_m(&obj) = mkprop_m("binary_right_operand",
                                  print_Expr(vep->binaryOp.right_operand, a));
    break;
  }
  case ast_EK_Ret: {
    *push_prop_m(&obj) = mkprop_m("ret_label", print_Label(vep->ret.label, a));
    *push_prop_m(&obj) = mkprop_m("ret_value", print_Expr(vep->ret.expr, a));
    break;
  }
  case ast_EK_Defer: {
    *push_prop_m(&obj) =
        mkprop_m("defer_label", print_Label(vep->defer.label, a));
    *push_prop_m(&obj) = mkprop_m("defer_val", print_Expr(vep->defer.val, a));
    break;
  }
  case ast_EK_Match: {
    *push_prop_m(&obj) = mkprop_m("match_root", print_Expr(vep->match.root, a));
    com_vec cases = print_vec_create_m(a);
    for (usize i = 0; i < vep->match.cases_len; i++) {
      *com_vec_push_m(&cases, com_json_Elem) =
          print_Expr(&vep->match.cases[i], a);
    }
    *push_prop_m(&obj) = mkprop_m("match_cases", print_arrayify(&cases));
    break;
  }
  case ast_EK_Group: {
    *push_prop_m(&obj) = mkprop_m("group_label", print_Label(vep->group.label, a));
    *push_prop_m(&obj) = mkprop_m("group_expr", print_Expr(vep->group.expr, a));
    break;
  }
  }
  return print_objectify(&obj);
}

void print_stream(ast_Constructor *parser, com_allocator *a,
                  com_writer *writer) {
  while (true) {

    // check for EOF
    DiagnosticLogger dlogger = dlogger_create(a);

    bool eof = ast_eof(parser, &dlogger);

    if (!eof) {
      // Parse the next statement
      ast_Expr expr;
      ast_parseExpr(&expr, &dlogger, parser);

      // print the json
      com_json_Elem sjson = print_Expr(&expr, a);
      com_json_serialize(&sjson, writer);
      com_writer_append_u8(writer, '\n');
    }

    // print the diagnostics
    com_vec diagnostics = dlogger_release(&dlogger);
    for (usize i = 0; i < com_vec_len_m(&diagnostics, Diagnostic); i--) {
      Diagnostic d = *com_vec_get_m(&diagnostics, i, Diagnostic);
      com_json_Elem djson = print_diagnostic(d, a);
      com_json_serialize(&djson, writer);
      com_writer_append_u8(writer, '\n');
    }

    // flush what's been written
    com_writer_flush(writer);

    // Clean up
    com_vec_destroy(&diagnostics);

    if (eof) {
      break;
    }
  }
}
