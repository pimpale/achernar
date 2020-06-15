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
  *VEC_PUSH(&obj, j_Prop)  = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("lncol")));
  *VEC_PUSH(&obj, j_Prop)  = J_PROP(J_ASCIZ("ln"), J_INT_ELEM(J_UINT(lncol.ln.val)));
  *VEC_PUSH(&obj, j_Prop)  = J_PROP(J_ASCIZ("col"), J_INT_ELEM(J_UINT(lncol.col.val)));
  return print_objectify(&obj);
}

static j_Elem print_Span(Span span, Allocator *a) {
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop)  = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("span")));
  *VEC_PUSH(&obj, j_Prop)  = J_PROP(J_ASCIZ("start"), print_LnCol(span.start, a));
  *VEC_PUSH(&obj, j_Prop)  = J_PROP(J_ASCIZ("end"), print_LnCol(span.end, a));
  return print_objectify(&obj);
}


static j_Elem print_diagnostic(Diagnostic diagnostic, Allocator *a) {
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("diagnostic")));
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_ASCIZ("diagnostic"),
                   J_STR_ELEM(J_ASCIZ(strDiagnosticKind(diagnostic.kind))));
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_ASCIZ("span"), print_Span(diagnostic.span, a));
  return print_objectify(&obj);
}

static j_Elem print_Comment(Comment comment, Allocator *a) {
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("comment")));
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_ASCIZ("scope"), J_STR_ELEM(J_ASCIZ(comment.scope)));
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_ASCIZ("data"), J_STR_ELEM(J_ASCIZ(comment.data)));
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_ASCIZ("span"), print_Span(comment.span, a));
  return print_objectify(&obj);
}


static j_Elem print_comments(Comment *comments, size_t comments_len,
                           Allocator *a) {
  j_Elem *ptrs = ALLOC_ARR(a, comments_len, j_Elem);
  for (size_t i = 0; i < comments_len; i++) {
    ptrs[i] = print_Comment(comments[i], a);
  }
  return J_ARRAY_ELEM(ptrs, comments_len);
}

// add shared data to the vector
static void print_appendAstNode(AstNode node, Vector* props, Allocator* a) {
  *VEC_PUSH(props, j_Prop) = J_PROP(J_ASCIZ("span"), print_Span(node.span, a));
  *VEC_PUSH(props, j_Prop) = J_PROP(J_ASCIZ("comments"), print_comments(node.comments, node.comments_len, a));
}

static j_Elem print_Path(Path* path, Allocator *a) {
  assert(path != NULL);
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("path")));
  print_appendAstNode(path->node, &obj, a);

  Vector arr  = vec_create(a);
  for (size_t i = 0; i < path->pathSegments_len; i++) {
     *VEC_PUSH(&arr, j_Elem) = J_STR_ELEM(J_ASCIZ(path->pathSegments[i]));
  }
  *VEC_PUSH(&obj, j_Prop) = J_PROP(J_ASCIZ("path_segments"), print_arrayify(&arr));
  return print_objectify(&obj);
}

static j_Elem print_Label(Label* label, Allocator *a) {
  Vector obj = vec_create(a);
  *VEC_PUSH(&obj, j_Prop)= J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("label")));
  print_appendAstNode(label->node, &obj, a);


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

