#include "ast_print.h"

#include "allocator.h"
#include "ast.h"
#include "json.h"
#include "std_allocator.h"
#include "token.h"

static inline j_Elem print_objectify(Vector *props) {
  size_t len = VEC_LEN(props, j_Prop);
  j_Prop* ptrs = vec_release(props);
  return J_OBJECT_ELEM(ptrs, len);
}

static inline j_Elem print_arrayify(Vector *elems) {
    size_t len = VEC_LEN(elems, j_Elem);
    j_Elem *ptrs = vec_release(elems);
    return J_ARRAY_ELEM(ptrs, len);
}

static j_Elem print_LnCol(LnCol lncol, Allocator *a) {
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop)  = J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("lncol")));
  *VEC_PUSH(&obj, j_Prop)  = J_PROP(J_LITSTR("ln"), J_INT_ELEM(J_UINT(lncol.ln.val)));
  *VEC_PUSH(&obj, j_Prop)  = J_PROP(J_LITSTR("col"), J_INT_ELEM(J_UINT(lncol.col.val)));
  return print_objectify(&obj);
}

static j_Elem print_Span(Span span, Allocator *a) {
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop)  = J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("span")));
  *VEC_PUSH(&obj, j_Prop)  = J_PROP(J_LITSTR("start"), print_LnCol(span.start, a));
  *VEC_PUSH(&obj, j_Prop)  = J_PROP(J_LITSTR("end"), print_LnCol(span.end, a));
  return print_objectify(&obj);
}


static j_Elem print_diagnostic(Diagnostic diagnostic, Allocator *a) {
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("diagnostic")));
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("diagnostic"),
                   J_STR_ELEM(J_ASCIZ(strDiagnosticKind(diagnostic.kind))));
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("span"), print_Span(diagnostic.span, a));
  return print_objectify(&obj);
}

static j_Elem print_Comment(Comment comment, Allocator *a) {
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("comment")));
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_LITSTR("scope"), J_STR_ELEM(J_ASCIZ(comment.scope)));
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_LITSTR("data"), J_STR_ELEM(J_ASCIZ(comment.data)));
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_LITSTR("span"), print_Span(comment.span, a));
  return print_objectify(&obj);
}


// add shared data to the vector
static void print_appendAstNode(AstNode node, Vector* props, Allocator* a) {
  *VEC_PUSH(props, j_Prop) = J_PROP(J_LITSTR("span"), print_Span(node.span, a));
  j_Elem *ptrs = ALLOC_ARR(a, node.comments_len, j_Elem);
  for (size_t i = 0; i < node.comments_len; i++) {
    ptrs[i] = print_Comment(node.comments[i], a);
  }
  *VEC_PUSH(props, j_Prop) = J_PROP(J_LITSTR("comments"), J_ARRAY_ELEM(ptrs, node.comments_len));
}

static j_Elem print_Path(Path* path, Allocator *a) {
  if(path == NULL) {
      return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("path")));
  print_appendAstNode(path->node, &obj, a);

  Vector arr  = vec_create(a);
  for (size_t i = 0; i < path->pathSegments_len; i++) {
     *VEC_PUSH(&arr, j_Elem) = J_STR_ELEM(J_ASCIZ(path->pathSegments[i]));
  }

  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("path_segments"), print_arrayify(&arr));
  return print_objectify(&obj);
}

static j_Elem print_LabelExpr(LabelExpr* label, Allocator *a) {
  if(label == NULL) {
      return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("label")));
  print_appendAstNode(label->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_LITSTR("label_kind"), J_STR_ELEM(J_ASCIZ(strLabelExprKind(label->kind))));
  switch(label->kind) {
      case LEK_Label: {
        *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("label"), J_STR_ELEM(J_ASCIZ(label->label.label)));
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
  if(token == NULL) {
      return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("token")));
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("span"), print_Span(token->span, a));
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("token_kind"), J_STR_ELEM(J_ASCIZ(tk_strKind(token->kind))));
  switch(token->kind) {
    case tk_Identifier: {
      *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("identifier"), J_STR_ELEM(J_ASCIZ(token->identifierToken.data)));
      break;
    }
    case tk_Macro: {
      *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("macro"), J_STR_ELEM(J_ASCIZ(token->macroToken.data)));
      break;
    }
    case tk_Label: {
      *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("label"), J_STR_ELEM(J_ASCIZ(token->labelToken.data)));
      break;
    }
    case tk_Comment: {
      *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("comment_scope"), J_STR_ELEM(J_ASCIZ(token->commentToken.scope)));
      *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("comment_data"), J_STR_ELEM(J_ASCIZ(token->commentToken.comment)));
      break;
    }
    case tk_Bool: {
      *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("bool"), J_BOOL_ELEM(token->boolToken.data));
      break;
    }
    case tk_String: {
      *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("string"), J_STR_ELEM(J_STR(token->stringToken.data, token->stringToken.data_len)));
      break;
    }
    case tk_Int: {
      *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("int"), J_INT_ELEM(J_UINT(token->intToken.data)));
      break;
    }
    case tk_Float: {
      *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("bool"), J_NUM_ELEM(token->floatToken.data));
      break;
    }
    case tk_Char: {
      *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("char"), J_INT_ELEM(J_UINT((char)token->floatToken.data)));
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
  if(macro == NULL) {
      return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_LITSTR("kind"), J_STR_ELEM(J_LITSTR("macro")));
  print_appendAstNode(macro->node, &obj, a);
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("name"), J_STR_ELEM(J_ASCIZ(macro->name)));

  Vector tokens = vec_create(a);
  for (size_t i = 0; i < macro->tokens_len; i++) {
     *VEC_PUSH(&tokens, j_Elem) = print_Token(&macro->tokens[i], a);
  }
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_LITSTR("tokens"), print_arrayify(&tokens));
  return print_objectify(&obj);
}

static j_Elem print_TypeExpr(TypeExpr *type, Allocator *a) {
  if(type == NULL) {
      return J_NULL_ELEM;
  }
  Vector obj = vec_create(a);

  // TODO

  return print_objectify(&obj);
}


void print_stream(Parser *parser, FILE *file) {
  while (true) {
    Allocator a = std_allocator();

    // Parse the next statment
    Stmnt stmnt;
    Vector diagnostics = vec_create(&a);
    bool eof = parse_nextStmntIfExists(&stmnt, &diagnostics, parser);

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

