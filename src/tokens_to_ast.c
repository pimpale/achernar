#include "tokens_to_ast.h"

#include "com_allocator.h"
#include "com_assert.h"
#include "com_loc.h"
#include "com_mem.h"
#include "com_queue.h"
#include "com_vec.h"

#include "ast.h"
#include "code_to_tokens.h"
#include "constants.h"
#include "token.h"

// utility method to allocate some noleak memory from the parser
static void *parse_alloc(ast_Constructor *parser, usize len) {
  return com_allocator_handle_get((com_allocator_alloc(
      parser->_a,
      (com_allocator_HandleData){.len = len,
                                 .flags = com_allocator_defaults(parser->_a) |
                                          com_allocator_NOLEAK})));
}

#define parse_alloc_obj_m(parser, type)                                        \
  (type *)parse_alloc((parser), sizeof(type))

// utility method to allocate some noleak memory from the parser
static com_vec parse_alloc_vec(ast_Constructor *parser) {
  return com_vec_create(com_allocator_alloc(
      parser->_a,
      (com_allocator_HandleData){.len = 20,
                                 .flags = com_allocator_defaults(parser->_a) |
                                          com_allocator_NOLEAK |
                                          com_allocator_REALLOCABLE}));
}

// ast_Constructor
ast_Constructor ast_create(com_reader *r, com_allocator *a) {
  return (ast_Constructor){
      ._a = a,      // com_allocator
      ._reader = r, // com_reader Pointer
      ._next_tokens_queue = com_queue_create(com_vec_create(com_allocator_alloc(
          a,
          (com_allocator_HandleData){
              .len = 20,
              .flags = com_allocator_defaults(a) | com_allocator_NOLEAK |
                       com_allocator_REALLOCABLE}))), // Queue of peeked tokens
  };
}

/// gets the next token, ignoring buffering
static Token parse_rawNext(ast_Constructor *parser,
                           DiagnosticLogger *diagnostics) {
  return tk_next(parser->_reader, diagnostics, parser->_a);
}

// Of the peeked token stack is not empty:
//    Ret the first element of the top of the token
//    Pop the first element of the next_metadata_stack
//    For each element in the next metadata stack, push it to the top of the
//    current scope
// Else fetch next raw token
static Token parse_next(ast_Constructor *pp, DiagnosticLogger *diagnostics) {

  // the current scope we aim to push the metadata to
  if (com_queue_len_m(&pp->_next_tokens_queue, Token) > 0) {
    // set the token
    Token ret;
    com_queue_pop_m(&pp->_next_tokens_queue, &ret, Token);
    return ret;
  } else {
    return parse_rawNext(pp, diagnostics);
  }
}

// parses next and ignores the result
static void parse_drop(ast_Constructor *ac, DiagnosticLogger *diagnostics) {
  parse_next(ac, diagnostics);
}

// gets the k'th token
// K must be greater than 0
static Token parse_peek(ast_Constructor *pp, DiagnosticLogger *diagnostics,
                        usize k) {
  com_assert_m(k > 0, "k is not 1 or more");

  for (usize i = com_queue_len_m(&pp->_next_tokens_queue, Token); i < k; i++) {
    // parse the token and enqueue it
    *com_queue_push_m(&pp->_next_tokens_queue, Token) =
        parse_rawNext(pp, diagnostics);
  }

  // return the most recent token added
  return *com_queue_get_m(&pp->_next_tokens_queue,
                          com_queue_len_m(&pp->_next_tokens_queue, Token) - k,
                          Token);
}

void ast_destroy(ast_Constructor *pp) {
  com_queue_destroy(&pp->_next_tokens_queue);
}

// returns a vector containing all the metadata encountered here
static com_vec parse_getMetadata(ast_Constructor *parser,
                                 DiagnosticLogger *diagnostics) {
  com_vec metadata = parse_alloc_vec(parser);
  while (parse_peek(parser, diagnostics, 1).kind == tk_Metadata) {
    Token c = parse_next(parser, diagnostics);
    *com_vec_push_m(&metadata, ast_Metadata) =
        (ast_Metadata){.span = c.span,
                       .significant = c.metadataToken.significant,
                       .data = c.metadataToken.content};
  }
  return metadata;
}

// returns the first nonmetadata token
static Token parse_peekPastMetadata(ast_Constructor *parser,
                                    DiagnosticLogger *diagnostics, usize k) {
  com_assert_m(k > 0, "k is not 1 or more");
  u64 n = 1;
  for (usize i = 0; i < k; i++) {
    while (parse_peek(parser, diagnostics, n).kind == tk_Metadata) {
      n++;
    }
  }
  return parse_peek(parser, diagnostics, n);
}

// type is the type of object that the generated function will parse
// x is the index level of the function
// lower_fn is the name of the function that will be called to evaluate the left
// and right op_det_fn is the name of the function that determines the binary
// operator this function should take a pointer to the type and return a bool if
// successful
#define DEFN_PARSE_R_BINARY(lower_fn, switch_fn, fn_name)                      \
  static void fn_name(ast_Expr *expr, DiagnosticLogger *diagnostics,           \
                      ast_Constructor *parser) {                               \
    ast_Expr v;                                                                \
    lower_fn(&v, diagnostics, parser);                                         \
                                                                               \
    Token t = parse_peekPastMetadata(parser, diagnostics, 1);                  \
    ast_ExprBinaryOpKind opKind = switch_fn(t.kind);                           \
    if (opKind == ast_EBOK_None) {                                             \
      /* there is no level x expression */                                     \
      *expr = v;                                                               \
      return;                                                                  \
    }                                                                          \
    /* this will only execute if the operator exists */                        \
    expr->kind = ast_EK_BinaryOp;                                              \
    expr->binaryOp.op = opKind;                                                \
                                                                               \
    /* set the left side */                                                    \
    expr->binaryOp.left_operand = parse_alloc_obj_m(parser, ast_Expr);         \
    *expr->binaryOp.left_operand = v;                                          \
                                                                               \
    /* first get metadata */                                                   \
    com_vec metadata = parse_getMetadata(parser, diagnostics);                 \
    expr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);        \
    expr->common.metadata = com_vec_release(&metadata);                        \
    /* consume operator */                                                     \
    parse_next(parser, diagnostics);                                           \
                                                                               \
    /* now parse the rest of the expression */                                 \
    /* TODO: recursion can cause stack overflow */                             \
    expr->binaryOp.right_operand = parse_alloc_obj_m(parser, ast_Expr);        \
    fn_name(expr->binaryOp.right_operand, diagnostics, parser);                \
                                                                               \
    /* calculate misc stuff */                                                 \
    expr->common.span =                                                        \
        com_loc_span_m(expr->binaryOp.left_operand->common.span.start,         \
                       expr->binaryOp.right_operand->common.span.end);         \
                                                                               \
    return;                                                                    \
  }

// type is the type of object that the generated function will parse
// x is the index level of the function
// lower_fn is the name of the function that will be called to evaluate the left
// and right op_det_fn is the name of the function that determines the binary
// operator this function should take a pointer to the type and return a bool if
// successful
#define DEFN_PARSE_L_BINARY(lower_fn, switch_fn, fn_name)                      \
  static void fn_name(ast_Expr *expr, DiagnosticLogger *diagnostics,           \
                      ast_Constructor *parser) {                               \
    /* parse lower expr */                                                     \
    lower_fn(expr, diagnostics, parser);                                       \
                                                                               \
    while (true) {                                                             \
      /* get next token */                                                     \
      Token t = parse_peekPastMetadata(parser, diagnostics, 1);                \
      /* if token is invalid we can just return the current expr */            \
      ast_ExprBinaryOpKind opKind = switch_fn(t.kind);                         \
      if (opKind == ast_EBOK_None) {                                           \
        return;                                                                \
      }                                                                        \
                                                                               \
      /* if the operation was sucessful, we make the previous expr the left    \
       * operand */                                                            \
      ast_Expr *left_operand = parse_alloc_obj_m(parser, ast_Expr);            \
      *left_operand = *expr;                                                   \
                                                                               \
      /* now we can mutate the expr */                                         \
      expr->kind = ast_EK_BinaryOp;                                            \
      expr->binaryOp.op = opKind;                                              \
      expr->binaryOp.left_operand = left_operand;                              \
                                                                               \
      /* first get metadata */                                                 \
      com_vec metadata = parse_getMetadata(parser, diagnostics);               \
      expr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);      \
      expr->common.metadata = com_vec_release(&metadata);                      \
                                                                               \
      /* then consume operator */                                              \
      parse_drop(parser, diagnostics);                                         \
                                                                               \
      /* now parse the rest of the expression */                               \
      expr->binaryOp.right_operand = parse_alloc_obj_m(parser, ast_Expr);      \
      /* Note that we parse with the lower expression to prevent recursion */  \
      lower_fn(expr->binaryOp.right_operand, diagnostics, parser);             \
                                                                               \
      /* set our span */                                                       \
      expr->common.span =                                                      \
          com_loc_span_m(left_operand->common.span.start,                      \
                         expr->binaryOp.right_operand->common.span.end);       \
    }                                                                          \
  }

// smallest unit
static void ast_parseTermExpr(ast_Expr *l, DiagnosticLogger *diagnostics,
                              ast_Constructor *parser);

// everything that can be seperated by a semicolon
void ast_parseSequenceable(ast_Expr *expr, DiagnosticLogger *diagnostics,
                           ast_Constructor *parser);

static void ast_parseLabel(ast_Label *ptr, DiagnosticLogger *diagnostics,
                           ast_Constructor *parser) {
  com_mem_zero_obj_m(ptr);
  Token t = parse_next(parser, diagnostics);
  ptr->span = t.span;
  if (t.kind == tk_Label) {
    ptr->kind = ast_LK_Label;
    ptr->label.label = t.labelToken.data;
  } else {
    ptr->kind = ast_LK_None;
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("Expected label"),
                     .children_len = 0};
  }
}

static void ast_parseIdentifier(ast_Identifier *ptr,
                                DiagnosticLogger *diagnostics,
                                ast_Constructor *parser) {
  com_mem_zero_obj_m(ptr);
  Token t = parse_next(parser, diagnostics);
  ptr->span = t.span;
  switch (t.kind) {
  case tk_Identifier: {
    ptr->kind = ast_IK_Identifier;
    ptr->id.name = t.identifierToken.data;
    break;
  }
  default: {
    *dlogger_append(diagnostics) = (Diagnostic){
        .span = t.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("identifier expected an identifier"),
        .children_len = 0};
    ptr->kind = ast_IK_None;
    break;
  }
  }
}

static void ast_certain_parseNilExpr(ast_Expr *ptr,
                                     DiagnosticLogger *diagnostics,
                                     ast_Constructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Nil, "expected a tk_Nil");
  ptr->kind = ast_EK_Nil;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseIntExpr(ast_Expr *ptr,
                                     DiagnosticLogger *diagnostics,
                                     ast_Constructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Int, "expected a tk_Int");
  ptr->kind = ast_EK_Int;
  ptr->intLiteral.value = t.intToken.data;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseRealExpr(ast_Expr *ptr,
                                      DiagnosticLogger *diagnostics,
                                      ast_Constructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Real, "expected tk_Real");
  ptr->kind = ast_EK_Real;
  ptr->realLiteral.value = t.realToken.data;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseStringExpr(ast_Expr *sptr,
                                        DiagnosticLogger *diagnostics,
                                        ast_Constructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_String, "expected a tk_String");
  sptr->kind = ast_EK_String;
  sptr->stringLiteral.value = t.stringToken.data;
  sptr->stringLiteral.kind = t.stringToken.kind;
  sptr->common.span = t.span;
  return;
}

static void ast_certain_parseGroupExpr(ast_Expr *bptr,
                                       DiagnosticLogger *diagnostics,
                                       ast_Constructor *parser) {
  com_mem_zero_obj_m(bptr);
  bptr->kind = ast_EK_Group;

  // Parse leftparen
  Token lparen = parse_next(parser, diagnostics);
  com_assert_m(lparen.kind == tk_ParenLeft, "expected tk_ParenLeft");

  bptr->group.expr = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(bptr->group.expr, diagnostics, parser);

  Token rparen = parse_next(parser, diagnostics);
  if (rparen.kind != tk_ParenRight) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = rparen.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("Expected right paren"),
                     .children_len = 0};
  }

  bptr->common.span = com_loc_span_m(lparen.span.start, rparen.span.end);
  return;
}

static void ast_certain_parseRetExpr(ast_Expr *rep,
                                     DiagnosticLogger *diagnostics,
                                     ast_Constructor *parser) {
  com_mem_zero_obj_m(rep);
  rep->kind = ast_EK_Ret;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Ret, "expected tk_Ret");

  com_loc_LnCol start = t.span.start;

  // return's scope
  rep->ret.label = parse_alloc_obj_m(parser, ast_Label);
  ast_parseLabel(rep->ret.label, diagnostics, parser);

  // value to return
  rep->ret.expr = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseTermExpr(rep->ret.expr, diagnostics, parser);

  // span
  rep->common.span = com_loc_span_m(start, rep->ret.expr->common.span.end);
  return;
}

static void ast_certain_parseLoopExpr(ast_Expr *lep,
                                      DiagnosticLogger *diagnostics,
                                      ast_Constructor *parser) {
  lep->kind = ast_EK_Loop;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Loop, "expected tk_Loop");
  com_loc_LnCol start = t.span.start;

  lep->loop.body = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseTermExpr(lep->loop.body, diagnostics, parser);
  lep->common.span = com_loc_span_m(start, lep->loop.body->common.span.end);
  return;
}

static void ast_certain_parseIdentifierExpr(ast_Expr *rptr,
                                            DiagnosticLogger *diagnostics,
                                            ast_Constructor *parser) {
  com_mem_zero_obj_m(rptr);
  rptr->kind = ast_EK_Reference;
  rptr->reference.reference = parse_alloc_obj_m(parser, ast_Identifier);
  ast_parseIdentifier(rptr->reference.reference, diagnostics, parser);
  rptr->common.span = rptr->reference.reference->span;
  return;
}

// '$' binding
static void ast_certain_parseBindExpr(ast_Expr *vbp,
                                      DiagnosticLogger *diagnostics,
                                      ast_Constructor *parser) {
  com_mem_zero_obj_m(vbp);
  vbp->kind = ast_EK_Bind;

  // ensure token is tk bind
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Bind, "expected tk_Bind");

  // now parse bind
  ast_Identifier *bind = parse_alloc_obj_m(parser, ast_Identifier);
  ast_parseIdentifier(bind, diagnostics, parser);
  vbp->bind.bind = bind;
  vbp->common.span = com_loc_span_m(t.span.start, vbp->bind.bind->span.end);
  return;
}

// $_
static void ast_certain_parseBindIgnoreExpr(ast_Expr *wpp,
                                            DiagnosticLogger *diagnostics,
                                            ast_Constructor *parser) {
  com_mem_zero_obj_m(wpp);
  wpp->kind = ast_EK_BindIgnore;
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Ignore, "expected tk_Ignore");
  wpp->common.span = t.span;
}

static void ast_certain_parseStructExpr(ast_Expr *bptr,
                                        DiagnosticLogger *diagnostics,
                                        ast_Constructor *parser) {
  com_mem_zero_obj_m(bptr);
  bptr->kind = ast_EK_Struct;

  // Parse leftbrace
  Token lbrace = parse_next(parser, diagnostics);
  com_assert_m(lbrace.kind == tk_BraceLeft, "expected tk_BraceLeft");

  bptr->structLiteral.expr = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(bptr->structLiteral.expr, diagnostics, parser);

  Token rbrace = parse_next(parser, diagnostics);
  if (rbrace.kind != tk_BraceRight) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = rbrace.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("Expected right brace"),
                     .children_len = 0};
  }

  bptr->common.span = com_loc_span_m(lbrace.span.start, rbrace.span.end);
  return;
}

static void ast_certain_parseDeferExpr(ast_Expr *dsp,
                                       DiagnosticLogger *diagnostics,
                                       ast_Constructor *parser) {
  dsp->kind = ast_EK_Defer;
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Defer, "expected tk_Defer");

  // label
  dsp->defer.label = parse_alloc_obj_m(parser, ast_Label);
  ast_parseLabel(dsp->defer.label, diagnostics, parser);

  // value
  dsp->defer.val = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(dsp->defer.val, diagnostics, parser);

  // span
  dsp->common.span =
      com_loc_span_m(t.span.start, dsp->defer.val->common.span.end);
  return;
}

static ast_ExprBinaryOpKind ast_opDetCaseOptionExpr(tk_Kind tk) {
  switch (tk) {
  case tk_CaseOption: {
    return ast_EBOK_CaseOption;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseSequenceable, ast_opDetCaseOptionExpr,
                    ast_parseCaseOptionExpr)

static void ast_certain_parseCaseExpr(ast_Expr *mptr,
                                      DiagnosticLogger *diagnostics,
                                      ast_Constructor *parser) {
  // guarantee token exists
  Token mt = parse_next(parser, diagnostics);
  com_assert_m(mt.kind == tk_Case, "expected tk_Case");

  com_mem_zero_obj_m(mptr);
  mptr->kind = ast_EK_CaseOf;

  mptr->caseof.expr = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(mptr->caseof.expr, diagnostics, parser);

  // Expect of
  Token oftk = parse_next(parser, diagnostics);
  if (oftk.kind != tk_BraceLeft) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = oftk.span,
                     .severity = DSK_Information,
                     .message = com_str_lit_m("Case Of expected of"),
                     .children_len = 0};
  }

  // parse CaseOptions
  mptr->caseof.cases = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseCaseOptionExpr(mptr->caseof.cases, diagnostics, parser);

  mptr->common.span =
      com_loc_span_m(mt.span.start, mptr->caseof.cases->common.span.end);
  return;
}

static void ast_certain_parseLabelExpr(ast_Expr *lptr,
                                       DiagnosticLogger *diagnostics,
                                       ast_Constructor *parser) {
  lptr->kind = ast_EK_Label;
  lptr->label.label = parse_alloc_obj_m(parser, ast_Label);
  ast_parseLabel(lptr->label.label, diagnostics, parser);

  lptr->label.val = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseTermExpr(lptr->label.val, diagnostics, parser);

  lptr->common.span = com_loc_span_m(lptr->label.label->span.start,
                                     lptr->label.val->common.span.end);
  return;
}
// paren, literals do block
// LEFT  (function application) match >> @ & . ... [] (postfixes)
// RIGHT neg pos not - + (prefixes)
// LEFT  .. ..=
// LEFT  :
// LEFT  * / %
// LEFT  + -
// LEFT  < <= > >= == !=
// LEFT  and or xor
// LEFT  ++ -- ^ !^
// LEFT  , |
// RIGHT ->
// RIGHT <<

static void ast_parseTermExpr(ast_Expr *l, DiagnosticLogger *diagnostics,
                              ast_Constructor *parser) {

  // value metadata;
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  Token t = parse_peek(parser, diagnostics, 1);
  // Decide which expression it is
  switch (t.kind) {
  // Literals
  case tk_Nil: {
    ast_certain_parseNilExpr(l, diagnostics, parser);
    break;
  }
  case tk_Int: {
    ast_certain_parseIntExpr(l, diagnostics, parser);
    break;
  }
  case tk_Real: {
    ast_certain_parseRealExpr(l, diagnostics, parser);
    break;
  }
  case tk_BraceLeft: {
    ast_certain_parseStructExpr(l, diagnostics, parser);
    break;
  }
  case tk_String: {
    ast_certain_parseStringExpr(l, diagnostics, parser);
    break;
  }
  case tk_ParenLeft: {
    ast_certain_parseGroupExpr(l, diagnostics, parser);
    break;
  }
  case tk_Ret: {
    ast_certain_parseRetExpr(l, diagnostics, parser);
    break;
  }
  case tk_Defer: {
    ast_certain_parseDeferExpr(l, diagnostics, parser);
    break;
  }
  case tk_Loop: {
    ast_certain_parseLoopExpr(l, diagnostics, parser);
    break;
  }
  case tk_Case: {
    ast_certain_parseCaseExpr(l, diagnostics, parser);
    break;
  }
  case tk_Label: {
    ast_certain_parseLabelExpr(l, diagnostics, parser);
    break;
  }
  case tk_Bind: {
    ast_certain_parseBindExpr(l, diagnostics, parser);
    break;
  }
  case tk_Ignore: {
    ast_certain_parseBindIgnoreExpr(l, diagnostics, parser);
    break;
  }
  case tk_Identifier: {
    ast_certain_parseIdentifierExpr(l, diagnostics, parser);
    break;
  }
  default: {
    l->kind = ast_EK_None;
    l->common.span = t.span;
    parse_next(parser, diagnostics);

    Diagnostic *hint = dlogger_alloc(diagnostics, sizeof(Diagnostic));
    *hint = (Diagnostic){.span = t.span,
                         .severity = DSK_Hint,
                         .message = tk_strKind(t.kind),
                         .children_len = 0};

    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_UnexpectedToken"),
                     .children = hint,
                     .children_len = 1};
  }
  }
  l->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  l->common.metadata = com_vec_release(&metadata);
}

static ast_ExprBinaryOpKind ast_opDetModuleAccess(tk_Kind tk) {
  switch (tk) {
  case tk_ModuleAccess: {
    return ast_EBOK_ModuleAccess;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseTermExpr, ast_opDetModuleAccess,
                    ast_parseModuleAccessExpr)

static ast_ExprBinaryOpKind ast_opDetRevApplyExpr(tk_Kind tk) {
  switch (tk) {
  case tk_RevApply: {
    return ast_EBOK_RevApply;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseModuleAccessExpr, ast_opDetRevApplyExpr,
                    ast_parseRevApplyExpr)

static void ast_parseApplyExpr(ast_Expr *aptr, DiagnosticLogger *diagnostics,
                               ast_Constructor *parser) {
  /* parse lower expr */
  ast_parseRevApplyExpr(aptr, diagnostics, parser);

  while (true) {
    /* get next token */
    Token t = parse_peekPastMetadata(parser, diagnostics, 1);
    /* if token is invalid we can just return the current expr */
    switch (t.kind) {
    case tk_Int:
    case tk_Real:
    case tk_BraceLeft:
    case tk_String:
    case tk_ParenLeft:
    case tk_Ret:
    case tk_Nil:
    case tk_Defer:
    case tk_Loop:
    case tk_At:
    case tk_Bind:
    case tk_Ignore:
    case tk_Identifier:
    case tk_Label:
    case tk_Case:
      break;
    default:
      return;
    }

    /* if the operation was sucessful, we make the previous expr the left
     * operand */
    ast_Expr *left_operand = parse_alloc_obj_m(parser, ast_Expr);
    com_mem_swap(aptr, left_operand, sizeof(ast_Expr));

    /* now we can mutate the expr */
    aptr->kind = ast_EK_BinaryOp;
    aptr->binaryOp.op = ast_EBOK_Apply;
    aptr->binaryOp.left_operand = left_operand;

    // no metadata
    aptr->common.metadata_len = 0;

    /* now parse the rest of the expression */
    aptr->binaryOp.right_operand = parse_alloc_obj_m(parser, ast_Expr);
    /* Note that we parse with the lower expression to prevent recursion */
    ast_parseRevApplyExpr(aptr->binaryOp.right_operand, diagnostics, parser);

    /* set our span */
    aptr->common.span =
        com_loc_span_m(left_operand->common.span.start,
                       aptr->binaryOp.right_operand->common.span.end);
  }
}

static ast_ExprBinaryOpKind ast_opDetRangeExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Range: {
    return ast_EBOK_Range;
  }
  case tk_RangeInclusive: {
    return ast_EBOK_RangeInclusive;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseApplyExpr, ast_opDetRangeExpr, ast_parseRangeExpr)


static ast_ExprBinaryOpKind ast_opDetAtExpr(tk_Kind tk) {
  switch (tk) {
  case tk_At: {
    return ast_EBOK_At;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseRangeExpr, ast_opDetAtExpr,
                    ast_parseAtExpr)

static ast_ExprBinaryOpKind ast_opDetConstrainExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Constrain: {
    return ast_EBOK_Constrain;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseAtExpr, ast_opDetConstrainExpr,
                    ast_parseConstrainExpr)

static ast_ExprBinaryOpKind ast_opDetPowExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Pow: {
    return ast_EBOK_Pow;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_R_BINARY(ast_parseConstrainExpr, ast_opDetPowExpr, ast_parsePowExpr)

static ast_ExprBinaryOpKind ast_opDetProductExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Mul: {
    return ast_EBOK_Mul;
  }
  case tk_Div: {
    return ast_EBOK_Div;
  }
  case tk_Rem: {
    return ast_EBOK_Rem;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parsePowExpr, ast_opDetProductExpr,
                    ast_parseProductExpr)

static ast_ExprBinaryOpKind ast_opDetArithExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Add: {
    return ast_EBOK_Add;
  }
  case tk_Sub: {
    return ast_EBOK_Sub;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}

DEFN_PARSE_L_BINARY(ast_parseProductExpr, ast_opDetArithExpr,
                    ast_parseArithExpr)

static ast_ExprBinaryOpKind ast_opDetCompExpr(tk_Kind tk) {
  switch (tk) {
  case tk_CompLess: {
    return ast_EBOK_CompLess;
  }
  case tk_CompGreater: {
    return ast_EBOK_CompGreater;
  }
  case tk_CompLessEqual: {
    return ast_EBOK_CompLessEqual;
  }
  case tk_CompGreaterEqual: {
    return ast_EBOK_CompGreaterEqual;
  }
  case tk_CompEqual: {
    return ast_EBOK_CompEqual;
  }
  case tk_CompNotEqual: {
    return ast_EBOK_CompNotEqual;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseArithExpr, ast_opDetCompExpr, ast_parseCompExpr)

static ast_ExprBinaryOpKind ast_opDetBoolExpr(tk_Kind tk) {
  switch (tk) {
  case tk_And: {
    return ast_EBOK_And;
  }
  case tk_Or: {
    return ast_EBOK_Or;
  }
  case tk_Xor: {
    return ast_EBOK_Xor;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseCompExpr, ast_opDetBoolExpr, ast_parseBoolExpr)

static ast_ExprBinaryOpKind ast_opDetSetExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Union: {
    return ast_EBOK_Union;
  }
  case tk_Intersection: {
    return ast_EBOK_Intersection;
  }
  case tk_Difference: {
    return ast_EBOK_Difference;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseBoolExpr, ast_opDetSetExpr, ast_parseSetExpr)

static ast_ExprBinaryOpKind ast_opDetComposeExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Compose: {
    return ast_EBOK_Compose;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseSetExpr, ast_opDetComposeExpr,
                    ast_parseComposeExpr)

static ast_ExprBinaryOpKind ast_opDetConsExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Cons: {
    return ast_EBOK_Cons;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_R_BINARY(ast_parseComposeExpr, ast_opDetConsExpr, ast_parseConsExpr)

static ast_ExprBinaryOpKind ast_opDetSumExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Sum: {
    return ast_EBOK_Sum;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseConsExpr, ast_opDetSumExpr, ast_parseSumExpr)

static ast_ExprBinaryOpKind ast_opDetPipeBackwardExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Arrow: {
    return ast_EBOK_PipeBackward;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseSumExpr, ast_opDetPipeBackwardExpr,
                    ast_parsePipeBackwardExpr)

static ast_ExprBinaryOpKind ast_opDetPipeForwardExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Arrow: {
    return ast_EBOK_PipeForward;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_R_BINARY(ast_parsePipeBackwardExpr, ast_opDetPipeForwardExpr,
                    ast_parsePipeForwardExpr)

static ast_ExprBinaryOpKind ast_opDetFnExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Arrow: {
    return ast_EBOK_Fn;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_R_BINARY(ast_parsePipeForwardExpr, ast_opDetFnExpr, ast_parseFnExpr)

static ast_ExprBinaryOpKind ast_opDetAssignExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Assign: {
    return ast_EBOK_Assign;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_R_BINARY(ast_parseFnExpr, ast_opDetAssignExpr, ast_parseAssignExpr)

void ast_parseSequenceable(ast_Expr *expr, DiagnosticLogger *diagnostics,
                           ast_Constructor *parser) {
  ast_parseAssignExpr(expr, diagnostics, parser);
}

static ast_ExprBinaryOpKind ast_opDetSequenceExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Sequence: {
    return ast_EBOK_Sequence;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseAssignExpr, ast_opDetSequenceExpr,
                    ast_parseSequenceExpr)

void ast_parseExpr(ast_Expr *expr, DiagnosticLogger *diagnostics,
                   ast_Constructor *parser) {
  ast_parseSequenceExpr(expr, diagnostics, parser);
}

bool ast_eof(ast_Constructor *parser, DiagnosticLogger *d) {
  return parse_peek(parser, d, 1).kind == tk_Eof;
}
