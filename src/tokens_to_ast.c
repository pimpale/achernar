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
  static ast_Expr *fn_name(DiagnosticLogger *diagnostics,                      \
                           ast_Constructor *parser) {                          \
    ast_Expr *v = lower_fn(diagnostics, parser);                               \
    Token t = parse_peekPastMetadata(parser, diagnostics, 1);                  \
    ast_ExprBinaryOpKind opKind = switch_fn(t.kind);                           \
    if (opKind == ast_EBOK_None) {                                             \
      /* there is no level x expression */                                     \
      return v;                                                                \
    }                                                                          \
    /* this will only execute if the operator exists */                        \
    ast_Expr *expr = parse_alloc_obj_m(parser, ast_Expr);                      \
    expr->kind = ast_EK_BinaryOp;                                              \
    expr->binaryOp.op = opKind;                                                \
                                                                               \
    /* set the left side */                                                    \
    expr->binaryOp.left_operand = v;                                           \
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
    expr->binaryOp.right_operand = fn_name(diagnostics, parser);               \
                                                                               \
    /* calculate misc stuff */                                                 \
    expr->common.span =                                                        \
        com_loc_span_m(expr->binaryOp.left_operand->common.span.start,         \
                       expr->binaryOp.right_operand->common.span.end);         \
                                                                               \
    return expr;                                                               \
  }

// type is the type of object that the generated function will parse
// x is the index level of the function
// lower_fn is the name of the function that will be called to evaluate the left
// and right op_det_fn is the name of the function that determines the binary
// operator this function should take a pointer to the type and return a bool if
// successful
#define DEFN_PARSE_L_BINARY(lower_fn, switch_fn, fn_name)                      \
  static ast_Expr *fn_name(DiagnosticLogger *diagnostics,                      \
                           ast_Constructor *parser) {                          \
    /* parse lower expr */                                                     \
    ast_Expr *expr = lower_fn(diagnostics, parser);                            \
                                                                               \
    while (true) {                                                             \
      /* get next token */                                                     \
      Token t = parse_peekPastMetadata(parser, diagnostics, 1);                \
      /* if token is invalid we can just return the current expr */            \
      ast_ExprBinaryOpKind opKind = switch_fn(t.kind);                         \
      if (opKind == ast_EBOK_None) {                                           \
        return expr;                                                           \
      }                                                                        \
                                                                               \
      /* this will only execute if the operator exists */                      \
      /* save the old expr in left operand */                                  \
      ast_Expr *left_operand = expr;                                           \
                                                                               \
      expr = parse_alloc_obj_m(parser, ast_Expr);                              \
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
      /* Note that we parse with the lower expression to prevent recursion */  \
      expr->binaryOp.right_operand = lower_fn(diagnostics, parser);            \
                                                                               \
      /* set our span */                                                       \
      expr->common.span =                                                      \
          com_loc_span_m(left_operand->common.span.start,                      \
                         expr->binaryOp.right_operand->common.span.end);       \
    }                                                                          \
  }

// smallest unit
static ast_Expr *ast_parseTermExpr(DiagnosticLogger *diagnostics,
                                   ast_Constructor *parser);

// everything that can be seperated by a semicolon
static ast_Expr *ast_parseSequenceable(DiagnosticLogger *diagnostics,
                                       ast_Constructor *parser);

static ast_Label *ast_parseLabel(DiagnosticLogger *diagnostics,
                                 ast_Constructor *parser) {
  ast_Label *ptr = parse_alloc_obj_m(parser, ast_Label);
  Token t = parse_next(parser, diagnostics);
  ptr->span = t.span;
  if (t.kind == tk_Label) {
    ptr->kind = ast_LK_Label;
    ptr->label.label = t.labelToken.data;
  } else {
    ptr->kind = ast_LK_None;
    *dlogger_append(diagnostics, true) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("Expected label"),
                     .children_len = 0};
  }
  return ptr;
}

static ast_Identifier *ast_parseIdentifier(DiagnosticLogger *diagnostics,
                                           ast_Constructor *parser) {
  ast_Identifier *ptr = parse_alloc_obj_m(parser, ast_Identifier);
  Token t = parse_next(parser, diagnostics);
  ptr->span = t.span;
  if (t.kind == tk_Identifier) {
    ptr->kind = ast_IK_Identifier;
    ptr->id.name = t.identifierToken.data;
  } else {
    ptr->kind = ast_IK_None;
    *dlogger_append(diagnostics, true) = (Diagnostic){
        .span = t.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("identifier expected an identifier"),
        .children_len = 0};
  }
  return ptr;
}

static ast_Expr *ast_parseSimpleExpr(DiagnosticLogger *diagnostics,
                                     ast_Constructor *parser,
                                     ast_ExprKind kind) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_next(parser, diagnostics);
  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = kind;
  ptr->common.span = t.span;
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parseIntExpr(DiagnosticLogger *diagnostics,
                                          ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Int, "expected a tk_Int");

  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_Int;
  ptr->intLiteral.value = t.intToken.data;
  ptr->common.span = t.span;
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parseBoolExpr(DiagnosticLogger *diagnostics,
                                           ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_True || t.kind == tk_False,
               "expected a tk_True or tk_False");

  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_Bool;
  ptr->boolLiteral.value = t.kind == tk_True;
  ptr->common.span = t.span;
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parseRealExpr(DiagnosticLogger *diagnostics,
                                           ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Real, "expected tk_Real");
  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_Real;
  ptr->realLiteral.value = t.realToken.data;
  ptr->common.span = t.span;
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parseStringExpr(DiagnosticLogger *diagnostics,
                                             ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_String, "expected a tk_String");
  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_String;
  ptr->stringLiteral.value = t.stringToken.data;
  ptr->stringLiteral.kind = t.stringToken.kind;
  ptr->common.span = t.span;
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parseGroupExpr(DiagnosticLogger *diagnostics,
                                            ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_Group;

  // Parse leftparen
  Token lparen = parse_next(parser, diagnostics);
  com_assert_m(lparen.kind == tk_ParenLeft, "expected tk_ParenLeft");

  ptr->group.expr = ast_parseExpr(diagnostics, parser);

  Token rparen = parse_next(parser, diagnostics);
  if (rparen.kind != tk_ParenRight) {
    *dlogger_append(diagnostics, true) =
        (Diagnostic){.span = rparen.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("Expected right paren"),
                     .children_len = 0};
  }

  ptr->common.span = com_loc_span_m(lparen.span.start, rparen.span.end);
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parseRetExpr(DiagnosticLogger *diagnostics,
                                          ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Ret, "expected tk_Ret");

  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_Ret;

  com_loc_LnCol start = t.span.start;

  // return's scope
  ptr->ret.label = ast_parseLabel(diagnostics, parser);

  // value to return
  ptr->ret.expr = ast_parseTermExpr(diagnostics, parser);

  // common
  ptr->common.span = com_loc_span_m(start, ptr->ret.expr->common.span.end);
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parseLoopExpr(DiagnosticLogger *diagnostics,
                                           ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Loop, "expected tk_Loop");
  com_loc_LnCol start = t.span.start;

  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_Loop;
  ptr->loop.body = ast_parseTermExpr(diagnostics, parser);
  // common
  ptr->common.span = com_loc_span_m(start, ptr->loop.body->common.span.end);
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parseValExpr(DiagnosticLogger *diagnostics,
                                          ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Val, "expected tk_Val");
  com_loc_LnCol start = t.span.start;

  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_Val;
  ptr->val.val = ast_parseTermExpr(diagnostics, parser);
  // common
  ptr->common.span = com_loc_span_m(start, ptr->loop.body->common.span.end);
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parsePatExpr(DiagnosticLogger *diagnostics,
                                          ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Pat, "expected tk_Pat");
  com_loc_LnCol start = t.span.start;

  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_Pat;
  ptr->pat.pat = ast_parseTermExpr(diagnostics, parser);
  // common
  ptr->common.span = com_loc_span_m(start, ptr->loop.body->common.span.end);
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parseIdentifierExpr(DiagnosticLogger *diagnostics,
                                                 ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_Reference;
  ptr->reference.reference = ast_parseIdentifier(diagnostics, parser);
  // common
  ptr->common.span = ptr->reference.reference->span;
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

// '$' binding
static ast_Expr *ast_certain_parseBindExpr(DiagnosticLogger *diagnostics,
                                           ast_Constructor *parser) {

  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Bind, "expected tk_Bind");

  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_Bind;
  ptr->bind.bind = ast_parseIdentifier(diagnostics, parser);

  // common
  ptr->common.span = com_loc_span_m(t.span.start, ptr->bind.bind->span.end);
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parseStructExpr(DiagnosticLogger *diagnostics,
                                             ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  // Parse leftbrace
  Token lbrace = parse_next(parser, diagnostics);
  com_assert_m(lbrace.kind == tk_BraceLeft, "expected tk_BraceLeft");

  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_Struct;
  ptr->structLiteral.expr = ast_parseExpr(diagnostics, parser);

  // expect rbrace
  Token rbrace = parse_next(parser, diagnostics);
  if (rbrace.kind != tk_BraceRight) {
    *dlogger_append(diagnostics, true) =
        (Diagnostic){.span = rbrace.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("Expected right brace"),
                     .children_len = 0};
  }

  ptr->common.span = com_loc_span_m(lbrace.span.start, rbrace.span.end);
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parseDeferExpr(DiagnosticLogger *diagnostics,
                                            ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Defer, "expected tk_Defer");

  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_Defer;

  // label
  ptr->defer.label = ast_parseLabel(diagnostics, parser);

  // value
  ptr->defer.val = ast_parseExpr(diagnostics, parser);

  // span
  ptr->common.span =
      com_loc_span_m(t.span.start, ptr->defer.val->common.span.end);
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
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

static ast_Expr *ast_certain_parseCaseExpr(DiagnosticLogger *diagnostics,
                                           ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  // guarantee token exists
  Token mt = parse_next(parser, diagnostics);
  com_assert_m(mt.kind == tk_Case, "expected tk_Case");

  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_CaseOf;

  ptr->caseof.expr = ast_parseExpr(diagnostics, parser);

  // Expect of
  Token oftk = parse_next(parser, diagnostics);
  if (oftk.kind != tk_Of) {
    *dlogger_append(diagnostics, true) =
        (Diagnostic){.span = oftk.span,
                     .severity = DSK_Information,
                     .message = com_str_lit_m("Case Of expected of"),
                     .children_len = 0};
  }

  // parse CaseOptions
  ptr->caseof.cases = ast_parseCaseOptionExpr(diagnostics, parser);

  ptr->common.span =
      com_loc_span_m(mt.span.start, ptr->caseof.cases->common.span.end);
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parseIfExpr(DiagnosticLogger *diagnostics,
                                         ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  // guarantee token exists
  Token mt = parse_next(parser, diagnostics);
  com_assert_m(mt.kind == tk_If, "expected tk_If");

  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_IfThen;
  ptr->ifthen.expr = ast_parseExpr(diagnostics, parser);

  // Expect then
  Token thentk = parse_next(parser, diagnostics);
  if (thentk.kind != tk_Then) {
    *dlogger_append(diagnostics, true) = (Diagnostic){
        .span = thentk.span,
        .severity = DSK_Information,
        .message = com_str_lit_m("If expected then after expression"),
        .children_len = 0};
  }

  // parse Then
  ptr->ifthen.then_expr = ast_parseExpr(diagnostics, parser);

  // expect Else
  Token elsetk = parse_next(parser, diagnostics);
  if (elsetk.kind != tk_Else) {
    *dlogger_append(diagnostics, true) = (Diagnostic){
        .span = elsetk.span,
        .severity = DSK_Information,
        .message = com_str_lit_m("If expected else after then expression"),
        .children_len = 0};
  }

  // parse Else
  ptr->ifthen.else_expr = ast_parseSequenceable(diagnostics, parser);

  ptr->common.span =
      com_loc_span_m(mt.span.start, ptr->caseof.cases->common.span.end);
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
}

static ast_Expr *ast_certain_parseLabelExpr(DiagnosticLogger *diagnostics,
                                            ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  ast_Expr *ptr = parse_alloc_obj_m(parser, ast_Expr);
  ptr->kind = ast_EK_Label;
  ptr->label.label = ast_parseLabel(diagnostics, parser);
  ptr->label.val = ast_parseTermExpr(diagnostics, parser);

  ptr->common.span = com_loc_span_m(ptr->label.label->span.start,
                                    ptr->label.val->common.span.end);
  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);
  return ptr;
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

static ast_Expr *ast_parseTermExpr(DiagnosticLogger *diagnostics,
                                   ast_Constructor *parser) {

  Token t = parse_peekPastMetadata(parser, diagnostics, 1);
  // Decide which expression it is
  switch (t.kind) {
  // Literals
  case tk_Void: {
    return ast_parseSimpleExpr(diagnostics, parser, ast_EK_Void);
  }
  case tk_VoidType: {
    return ast_parseSimpleExpr(diagnostics, parser, ast_EK_VoidType);
  }
  case tk_NeverType: {
    return ast_parseSimpleExpr(diagnostics, parser, ast_EK_NeverType);
  }
  case tk_Ignore: {
    return ast_parseSimpleExpr(diagnostics, parser, ast_EK_BindIgnore);
  }
  case tk_Splat: {
    return ast_parseSimpleExpr(diagnostics, parser, ast_EK_BindSplat);
  }
  case tk_True:
  case tk_False: {
    return ast_certain_parseBoolExpr(diagnostics, parser);
  }
  case tk_Int: {
    return ast_certain_parseIntExpr(diagnostics, parser);
  }
  case tk_Real: {
    return ast_certain_parseRealExpr(diagnostics, parser);
  }
  case tk_BraceLeft: {
    return ast_certain_parseStructExpr(diagnostics, parser);
  }
  case tk_String: {
    return ast_certain_parseStringExpr(diagnostics, parser);
  }
  case tk_ParenLeft: {
    return ast_certain_parseGroupExpr(diagnostics, parser);
  }
  case tk_Ret: {
    return ast_certain_parseRetExpr(diagnostics, parser);
  }
  case tk_Defer: {
    return ast_certain_parseDeferExpr(diagnostics, parser);
  }
  case tk_Val: {
    return ast_certain_parseValExpr(diagnostics, parser);
  }
  case tk_Pat: {
    return ast_certain_parsePatExpr(diagnostics, parser);
  }
  case tk_Loop: {
    return ast_certain_parseLoopExpr(diagnostics, parser);
  }
  case tk_If: {
    return ast_certain_parseIfExpr(diagnostics, parser);
  }
  case tk_Case: {
    return ast_certain_parseCaseExpr(diagnostics, parser);
  }
  case tk_Label: {
    return ast_certain_parseLabelExpr(diagnostics, parser);
  }
  case tk_Bind: {
    return ast_certain_parseBindExpr(diagnostics, parser);
  }
  case tk_Identifier: {
    return ast_certain_parseIdentifierExpr(diagnostics, parser);
  }
  default: {
    // value metadata;
    com_vec metadata = parse_getMetadata(parser, diagnostics);
    ast_Expr *l = parse_alloc_obj_m(parser, ast_Expr);
    l->kind = ast_EK_None;
    l->common.span = t.span;
    l->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
    l->common.metadata = com_vec_release(&metadata);
    parse_next(parser, diagnostics);

    Diagnostic *hint = dlogger_append(diagnostics, false);
    *hint = (Diagnostic){.span = t.span,
                         .severity = DSK_Hint,
                         .message = tk_strKind(t.kind),
                         .children_len = 0};
    *dlogger_append(diagnostics, true) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_UnexpectedToken"),
                     .children = hint,
                     .children_len = 1};
    return l;
  }
  }
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

static ast_Expr *ast_parseApplyExpr(DiagnosticLogger *diagnostics,
                                    ast_Constructor *parser) {
  /* parse lower expr */
  ast_Expr *aptr = ast_parseRevApplyExpr(diagnostics, parser);

  while (true) {
    /* get next token */
    Token t = parse_peekPastMetadata(parser, diagnostics, 1);
    /* if token is invalid we can just return the current expr */
    switch (t.kind) {
    case tk_If:
    case tk_Int:
    case tk_Real:
    case tk_BraceLeft:
    case tk_String:
    case tk_ParenLeft:
    case tk_Ret:
    case tk_Void:
    case tk_VoidType:
    case tk_NeverType:
    case tk_Defer:
    case tk_Loop:
    case tk_Bind:
    case tk_Ignore:
    case tk_Splat:
    case tk_Identifier:
    case tk_Label:
    case tk_Case:
      break;
    default:
      return aptr;
    }
    // the following will only execute if there is a valid operation

    ast_Expr *left_operand = aptr;

    aptr = parse_alloc_obj_m(parser, ast_Expr);
    aptr->kind = ast_EK_BinaryOp;
    aptr->binaryOp.op = ast_EBOK_Apply;
    aptr->binaryOp.left_operand = left_operand;
    /* Note that we parse with the lower expression to prevent recursion */
    aptr->binaryOp.right_operand = ast_parseRevApplyExpr(diagnostics, parser);

    /* set our span */
    aptr->common.span =
        com_loc_span_m(left_operand->common.span.start,
                       aptr->binaryOp.right_operand->common.span.end);
    // no metadata
    aptr->common.metadata_len = 0;
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

static ast_ExprBinaryOpKind ast_opDetAsExpr(tk_Kind tk) {
  switch (tk) {
  case tk_As: {
    return ast_EBOK_As;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_L_BINARY(ast_parseRangeExpr, ast_opDetAsExpr, ast_parseAsExpr)

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
DEFN_PARSE_L_BINARY(ast_parseAsExpr, ast_opDetConstrainExpr,
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

static ast_ExprBinaryOpKind ast_opDetDefunExpr(tk_Kind tk) {
  switch (tk) {
  case tk_Arrow: {
    return ast_EBOK_Defun;
  }
  default: {
    return ast_EBOK_None;
  }
  }
}
DEFN_PARSE_R_BINARY(ast_parsePipeForwardExpr, ast_opDetDefunExpr,
                    ast_parseDefunExpr)

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
DEFN_PARSE_R_BINARY(ast_parseDefunExpr, ast_opDetAssignExpr,
                    ast_parseAssignExpr)

ast_Expr *ast_parseSequenceable(DiagnosticLogger *diagnostics,
                                ast_Constructor *parser) {
  return ast_parseAssignExpr(diagnostics, parser);
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

ast_Expr *ast_parseExpr(DiagnosticLogger *diagnostics,
                        ast_Constructor *parser) {
  return ast_parseSequenceExpr(diagnostics, parser);
}

bool ast_eof(ast_Constructor *parser, DiagnosticLogger *d) {
  return parse_peek(parser, d, 1).kind == tk_Eof;
}
