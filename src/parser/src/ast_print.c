#include "ast_print.h"

#include "allocator.h"
#include "ast.h"
#include "json.h"
#include "std_allocator.h"
#include "token.h"


static j_Elem print_lnCol(LnCol lncol, Allocator *a) {
  j_Prop *ptrs = ALLOC_ARR(a, 3, j_Prop);
  ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("lncol")));
  ptrs[1] = J_PROP(J_ASCIZ("ln"), J_INT_ELEM(J_UINT(lncol.ln.val)));
  ptrs[2] = J_PROP(J_ASCIZ("col"), J_INT_ELEM(J_UINT(lncol.col.val)));
  return J_OBJECT_ELEM(ptrs, 3);
}

static j_Elem print_span(Span span, Allocator *a) {
  return J_NULL_ELEM;
  j_Prop *ptrs = ALLOC_ARR(a, 3, j_Prop);
  ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("span")));
  ptrs[1] = J_PROP(J_ASCIZ("start"), print_lnCol(span.start, a));
  ptrs[2] = J_PROP(J_ASCIZ("end"), print_lnCol(span.end, a));
  return J_OBJECT_ELEM(ptrs, 3);
}


static j_Elem print_diagnostic(Diagnostic diagnostic, Allocator *a) {
  j_Prop *ptrs = ALLOC_ARR(a, 3, j_Prop);
  ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("diagnostic")));
  ptrs[1] = J_PROP(J_ASCIZ("diagnostic"),
                   J_STR_ELEM(J_ASCIZ(strDiagnosticKind(diagnostic.kind))));
  ptrs[2] = J_PROP(J_ASCIZ("span"), print_span(diagnostic.span, a));
  return J_OBJECT_ELEM(ptrs, 2);
}

static j_Elem print_comment(Comment comment, Allocator *a) {
  j_Prop *ptrs = ALLOC_ARR(a, 5, j_Prop);
  ptrs[0] = J_PROP(J_ASCIZ("kind"), J_STR_ELEM(J_ASCIZ("ast")));
  ptrs[1] = J_PROP(J_ASCIZ("ast_kind"), J_STR_ELEM(J_ASCIZ("comment")));
  ptrs[2] = J_PROP(J_ASCIZ("scope"), J_STR_ELEM(J_ASCIZ(comment.scope)));
  ptrs[3] = J_PROP(J_ASCIZ("data"), J_STR_ELEM(J_ASCIZ(comment.data)));
  ptrs[4] = J_PROP(J_ASCIZ("span"), print_span(comment.span, a));
  return J_OBJECT_ELEM(ptrs, 5);
}

static j_Elem print_comments(Comment *comments, size_t comments_len,
                           Allocator *a) {
  j_Elem *ptrs = ALLOC_ARR(a, comments_len, j_Elem);
  for (size_t i = 0; i < comments_len; i++) {
    ptrs[i] = print_comment(comments[i], a);
  }
  return J_ARRAY_ELEM(ptrs, comments_len);
}

// add shared data to the vector
static void print_sharedNodeData(AstNode node, Vector* props, Allocator* a) {
  *VEC_PUSH(props, j_Prop) = J_PROP("span", print_span(node.span, a));
  *VEC_PUSH(props, j_Prop) = J_PROP("comments", print_comments(node.comments, node.comments_len, a));
}

static 

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
    j_Elem sjson = print_stmnt(&stmnt, &a);
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
