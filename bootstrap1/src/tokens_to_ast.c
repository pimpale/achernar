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

// convoluted function to save repetitive tasks
#define PARSE_LIST(members_vec_ptr, dlogger_ptr, member_parse_function,        \
                   member_kind, delimiting_token_kind,                         \
                   missing_delimiter_error_msg, end_lncol, parser)             \
                                                                               \
  while (true) {                                                               \
    Token pl_ntk = parse_peek(parser, diagnostics, 1); /* next token kind */   \
    if (pl_ntk.kind == delimiting_token_kind) {                                \
      end_lncol = pl_ntk.span.end;                                             \
      parse_next(parser, dlogger_ptr); /* accept delimiting tk */              \
      break;                                                                   \
    } else if (pl_ntk.kind == tk_Eof) {                                        \
      *dlogger_append(dlogger_ptr) =                                           \
          (Diagnostic){.span = pl_ntk.span,                                    \
                       .severity = DSK_Error,                                  \
                       .message = com_str_lit_m(missing_delimiter_error_msg),  \
                       .children_len = 0};                                     \
      end_lncol = pl_ntk.span.end;                                             \
      break;                                                                   \
    }                                                                          \
    /* if there wasn't an end delimiter, push the last token back */           \
    member_parse_function(com_vec_push_m(members_vec_ptr, member_kind),        \
                          diagnostics, parser);                                \
  }

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

// If the peeked token stack is not empty:
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

// Note that all errors resync at the statement level
static void ast_parseExpr(ast_Expr *ptr, DiagnosticLogger *diagnostics,
                          ast_Constructor *parser);

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
static void ast_certain_parseBlockExpr(ast_Expr *bptr,
                                       DiagnosticLogger *diagnostics,
                                       ast_Constructor *parser) {
  com_mem_zero_obj_m(bptr);
  bptr->kind = ast_EK_Block;

  // Parse leftbrace
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_BraceLeft, "expected tk_BraceLeft");
  com_loc_Span lbracespan = t.span;

  t = parse_peek(parser, diagnostics, 1);
  // blocks may be labeled
  bptr->block.label = parse_alloc_obj_m(parser, ast_Label);
  if (t.kind == tk_Label) {
    ast_parseLabel(bptr->block.label, diagnostics, parser);
  } else {
    *bptr->block.label =
        (ast_Label){.span = lbracespan, .kind = ast_LK_Omitted};
  }

  // Create list of statements
  com_vec statements = parse_alloc_vec(parser);

  com_loc_LnCol end;

  PARSE_LIST(&statements,                  // members_vec_ptr
             diagnostics,                  // dlogger_ptr
             ast_parseStmnt,               // member_parse_function
             ast_Stmnt,                    // member_kind
             tk_BraceRight,                // delimiting_token_kind
             "DK_BlockExpectedRightBrace", // missing_delimiter_error
             end,                          // end_lncol
             parser                        // parser
  )

  bptr->block.stmnts_len = com_vec_len_m(&statements, ast_Stmnt);
  bptr->block.stmnts = com_vec_release(&statements);
  bptr->common.span = com_loc_span_m(lbracespan.start, end);
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
  com_loc_LnCol end;

  parse_next(parser, diagnostics);

  // return's scope
  rep->ret.label = parse_alloc_obj_m(parser, ast_Label);
  ast_parseLabel(rep->ret.label, diagnostics, parser);

  // value to return
  rep->ret.expr = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(rep->ret.expr, diagnostics, parser);

  // span
  end = rep->ret.expr->common.span.end;
  rep->common.span = com_loc_span_m(start, end);
  return;
}

static void ast_certain_parseLoopExpr(ast_Expr *lep,
                                      DiagnosticLogger *diagnostics,
                                      ast_Constructor *parser) {
  lep->kind = ast_EK_Loop;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Loop, "expected tk_Loop");
  com_loc_LnCol start = t.span.start;
  com_loc_Span loopspan = t.span;

  t = parse_peek(parser, diagnostics, 1);
  lep->loop.label = parse_alloc_obj_m(parser, ast_Label);
  if (t.kind == tk_Label) {
    ast_parseLabel(lep->loop.label, diagnostics, parser);
  } else {
    *lep->loop.label = (ast_Label){.span = loopspan, .kind = ast_LK_Omitted};
  }

  lep->loop.body = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(lep->loop.body, diagnostics, parser);
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

  // now parse binding
  ast_Identifier *binding = parse_alloc_obj_m(parser, ast_Identifier);
  ast_parseIdentifier(binding, diagnostics, parser);
  vbp->binding.binding = binding;
  vbp->common.span =
      com_loc_span_m(t.span.start, vbp->binding.binding->span.end);
  return;
}

// 'at' pattern 'dollar' binding
static void ast_certain_parseAtLetExpr(ast_Expr *alpp,
                                       DiagnosticLogger *diagnostics,
                                       ast_Constructor *parser) {
  com_mem_zero_obj_m(alpp);
  alpp->kind = ast_EK_AtBind;

  // ensure token is tk at
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_At, "expected tk_At");
  com_loc_LnCol start = t.span.start;

  // now parse pattern
  alpp->atBinding.pat = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(alpp->atBinding.pat, diagnostics, parser);

  // now ensure that we can find a let
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Bind) {
    *dlogger_append(diagnostics) = (Diagnostic){
        .span = t.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("Expected bind token after pattern"),
        .children_len = 0};
  }

  // now parse binding
  ast_Identifier *binding = parse_alloc_obj_m(parser, ast_Identifier);
  ast_parseIdentifier(binding, diagnostics, parser);
  alpp->atBinding.binding = binding;
  alpp->common.span = com_loc_span_m(start, alpp->atBinding.binding->span.end);
  return;
}

// _
static void ast_certain_parseBindIgnoreExpr(ast_Expr *wpp,
                                            DiagnosticLogger *diagnostics,
                                            ast_Constructor *parser) {
  com_mem_zero_obj_m(wpp);
  wpp->kind = ast_EK_BindIgnore;
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Ignore, "expected tk_Ignore");
  wpp->common.span = t.span;
}

// Identifier `:` Type
static void ast_parseCompoundElement(ast_CompoundElement *ptr,
                                     DiagnosticLogger *diagnostics,
                                     ast_Constructor *parser) {
  com_mem_zero_obj_m(ptr);
  ptr->kind = ast_CEK_Element;

  // accept metadata
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  ptr->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  ptr->common.metadata = com_vec_release(&metadata);

  // Get pattern
  ptr->element.name = parse_alloc_obj_m(parser, ast_Identifier);
  ast_parseIdentifier(ptr->element.name, diagnostics, parser);

  com_loc_LnCol start = ptr->element.name->span.start;
  com_loc_LnCol end = ptr->element.name->span.end;

  // Expect colon
  Token t = parse_next(parser, diagnostics);
  if (t.kind != tk_Constrain) {
    *dlogger_append(diagnostics) = (Diagnostic){
        .span = t.span,
        .severity = DSK_Error,
        .message = com_str_lit_m(
            "compound type element expected colon after identifier"),
        .children_len = 0};
    goto CLEANUP;
  }

  // Get Value
  ptr->element.val = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(ptr->element.val, diagnostics, parser);
  end = ptr->element.val->common.span.end;

CLEANUP:
  ptr->common.span = com_loc_span_m(start, end);
  return;
}

static void ast_certain_parseStructExpr(ast_Expr *ptr,
                                        DiagnosticLogger *diagnostics,
                                        ast_Constructor *parser) {
  com_mem_zero_obj_m(ptr);
  ptr->kind = ast_EK_Struct;

  // guarantee token exists
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_BracketLeft, "expected tk_BracketLeft");
  com_loc_LnCol start = t.span.start;

  // now we must parse the block containing the
  // cases
  com_vec elements = parse_alloc_vec(parser);

  com_loc_LnCol end;

  PARSE_LIST(
      &elements,                // members_vec_ptr
      diagnostics,              // dlogger_ptr
      ast_parseCompoundElement, // member_parse_function
      ast_CompoundElement,      // member_kind
      tk_BracketRight,          // delimiting_token_kind
      "expected a closing right bracket for struct type def ", // missing_delimiter_error
      end,                                                     // end_lncol
      parser                                                   // parser
  )

  // Get interior elements
  ptr->structLiteral.elements_len =
      com_vec_len_m(&elements, ast_CompoundElement);
  ptr->structLiteral.elements = com_vec_release(&elements);
  ptr->common.span = com_loc_span_m(start, end);
  return;
}

// braces, literals
// (function application) match >> @ & . ... [] (postfixes)
// neg pos not - + ...= ... (prefixes)
// .. ..= (range)
// : (constraints)
// * / % (multiplication and division)
// + - (addition and subtraction)
// < <= > >= == != (comparators)
// and or xor (logical operators)
// ++ -- ^ !^ (set operators)
// , | (type operators)
// -> (Function definiton ) RIGHT ASSOCIATIVE
// = (Assignment)

static void parseTermExpr(ast_Expr *l, DiagnosticLogger *diagnostics,
                          ast_Constructor *parser) {

  // value metadata;
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  Token t = parse_peek(parser, diagnostics, 1);
  // Decide which expression it is
  switch (t.kind) {
  // Literals
  case tk_Int: {
    ast_certain_parseIntExpr(l, diagnostics, parser);
    break;
  }
  case tk_Real: {
    ast_certain_parseRealExpr(l, diagnostics, parser);
    break;
  }
  case tk_BracketLeft: {
    ast_certain_parseStructExpr(l, diagnostics, parser);
    break;
  }
  case tk_String: {
    ast_certain_parseStringExpr(l, diagnostics, parser);
    break;
  }
  case tk_BraceLeft: {
    ast_certain_parseBlockExpr(l, diagnostics, parser);
    break;
  }
  case tk_Ret: {
    ast_certain_parseRetExpr(l, diagnostics, parser);
    break;
  }
  case tk_Loop: {
    ast_certain_parseLoopExpr(l, diagnostics, parser);
    break;
  }
  case tk_At: {
    ast_certain_parseAtLetExpr(l, diagnostics, parser);
    break;
  }
  case tk_Bind: {
    ast_certain_parseBindExpr(l, diagnostics, parser);
    break;
  }
  case tk_Mut: {
    ast_certain_parseMutExpr(l, diagnostics, parser);
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
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_UnexpectedToken"),
                     .children_len = 0};
  }
  }
  l->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  l->common.metadata = com_vec_release(&metadata);
}

static void ast_certain_postfix_parseFieldAccessExpr(
    ast_Expr *fave, DiagnosticLogger *diagnostics, ast_Constructor *parser,
    ast_Expr *root) {
  com_mem_zero_obj_m(fave);
  fave->kind = ast_EK_FieldAccess;
  fave->fieldAccess.root = root;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_FieldAccess, "expected tk_FieldAccess");

  fave->fieldAccess.field = parse_alloc_obj_m(parser, ast_Identifier);
  ast_parseIdentifier(fave->fieldAccess.field, diagnostics, parser);
  fave->common.span = com_loc_span_m(root->common.span.start,
                                     fave->fieldAccess.field->span.end);
}


static void ast_certain_postfix_parseCallExpr(ast_Expr *cptr,
                                              DiagnosticLogger *diagnostics,
                                              ast_Constructor *parser,
                                              ast_Expr *root) {

  com_assert_m(parse_peek(parser, diagnostics, 1).kind == tk_ParenLeft,
               "expected tk_ParenLeft");
  parse_drop(parser, diagnostics);

  com_mem_zero_obj_m(cptr);

  cptr->kind = ast_EK_BinaryOp;
  cptr->binaryOp.op = ast_EBOK_Call;

  ast_Expr *rhs  = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(rhs, diagnostics, parser);

  cptr->binaryOp.left_operand = root;
  cptr->binaryOp.right_operand = rhs;

  Token t = parse_next(parser, diagnostics);
  if (t.kind != tk_ParenRight) {
    *dlogger_append(diagnostics) = (Diagnostic){
        .span = t.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("call expr expected closing paren after expr"),
        .children_len = 0};
  }

  cptr->common.span = com_loc_span_m(root->common.span.start, t.span.end);
}

static void ast_certain_postfix_parsePipeExpr(ast_Expr *pptr,
                                              DiagnosticLogger *diagnostics,
                                              ast_Constructor *parser,
                                              ast_Expr *root) {

  com_assert_m(parse_peek(parser, diagnostics, 1).kind == tk_Pipe,
               "expected tk_Pipe");
  parse_drop(parser, diagnostics);

  com_mem_zero_obj_m(pptr);
  pptr->kind = ast_EK_BinaryOp;
  pptr->binaryOp.op = ast_EBOK_Pipe;

  ast_Expr *rhs  = parse_alloc_obj_m(parser, ast_Expr);
  parseTermExpr(rhs, diagnostics, parser);

  pptr->binaryOp.left_operand = root;
  pptr->binaryOp.right_operand = rhs;
  pptr->common.span =
      com_loc_span_m(root->common.span.start, rhs->common.span.end);
}

static void ast_certain_postfix_parseMatchExpr(ast_Expr *mptr,
                                               DiagnosticLogger *diagnostics,
                                               ast_Constructor *parser,
                                               ast_Expr *root) {
  com_mem_zero_obj_m(mptr);
  mptr->kind = ast_EK_Match;
  mptr->match.root = root;

  // guarantee token exists
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Match, "expected tk_Match");

  // now we must parse the block containing the
  // cases
  com_vec cases = parse_alloc_vec(parser);

  com_loc_LnCol end;

  // Expect beginning brace
  t = parse_next(parser, diagnostics);

  if (t.kind != tk_BraceLeft) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_MatchNoLeftBrace"),
                     .children_len = 0};
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&cases,                 // members_vec_ptr
             diagnostics,            // dlogger_ptr
             ast_parseExpr,          // member_parse_function
             ast_Expr,               // member_kind
             tk_BraceRight,          // delimiting_token_kind
             "DK_MatchNoRightBrace", // missing_delimiter_error
             end,                    // end_lncol
             parser                  // parser
  )

CLEANUP:
  // Get interior cases
  mptr->match.cases_len = com_vec_len_m(&cases, ast_Expr);
  mptr->match.cases = com_vec_release(&cases);

  mptr->common.span = com_loc_span_m(root->common.span.start, end);
  return;
}

static bool ast (tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Range: {
    *val = ast_EBOK_Range;
    return true;
  }
  case tk_RangeInclusive: {
    *val = ast_EBOK_RangeInclusive;
    return true;
  }
  default: {
    return false;
  }
  }
}

static void parsePostfixExpr(ast_Expr *l, DiagnosticLogger *diagnostics,
                             ast_Constructor *parser) {
  // Because it's postfix, we must take a somewhat
  // unorthodox approach here We Parse the level
  // one expr and then use a while loop to process
  // the rest of the stuff

  ast_Expr *root = l;
  parseTermExpr(root, diagnostics, parser);

  while (true) {
    // represents the old operation
    ast_Expr *v;

    Token t = parse_peekPastMetadata(parser, diagnostics, 1);
    switch (t.kind) {
    case tk_Ref: {
      // get metadata
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      // allocate space for operation
      v = parse_alloc_obj_m(parser, ast_Expr);
      *v = *root;
      root->kind = ast_EK_UnaryOp;
      root->unaryOp.op = ast_EUOK_Ref;
      root->unaryOp.operand = v;
      root->common.span = com_loc_span_m(v->common.span.start, t.span.end);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      parse_next(parser, diagnostics);
      break;
    }
    case tk_Deref: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Expr);
      *v = *root;
      root->kind = ast_EK_UnaryOp;
      root->unaryOp.op = ast_EUOK_Deref;
      root->unaryOp.operand = v;
      root->common.span = com_loc_span_m(v->common.span.start, t.span.end);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      parse_next(parser, diagnostics);
      break;
    }
    case tk_Copy: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Expr);
      *v = *root;
      root->kind = ast_EK_UnaryOp;
      root->unaryOp.op = ast_EUOK_Copy;
      root->unaryOp.operand = v;
      root->common.span = com_loc_span_m(v->common.span.start, t.span.end);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      parse_next(parser, diagnostics);
      break;
    }
    case tk_FieldAccess: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Expr);
      *v = *root;
      ast_certain_postfix_parseFieldAccessExpr(root, diagnostics, parser, v);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      break;
    }
    case tk_In: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Expr);
      *v = *root;
      ast_certain_postfix_parseInExpr(root, diagnostics, parser, v);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      break;
    }
    case tk_ParenLeft: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Expr);
      *v = *root;
      ast_certain_postfix_parseCallExpr(root, diagnostics, parser, v);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      break;
    }
    case tk_Pipe: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Expr);
      *v = *root;
      ast_certain_postfix_parsePipeExpr(root, diagnostics, parser, v);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      break;
    }
    case tk_Match: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Expr);
      *v = *root;
      ast_certain_postfix_parseMatchExpr(root, diagnostics, parser, v);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      break;
    }
    default: {
      // there are no more level 2 expressions
      return;
    }
    }
  }
}

static void ast_parsePrefixExpr(ast_Expr *l, DiagnosticLogger *diagnostics,
                                ast_Constructor *parser) {
  Token t = parse_peekPastMetadata(parser, diagnostics, 1);
  switch (t.kind) {
  case tk_Plus: {
    l->unaryOp.op = ast_EUOK_Pos;
    break;
  }
  case tk_Minus: {
    l->unaryOp.op = ast_EUOK_Neg;
    break;
  }
  case tk_Not: {
    l->unaryOp.op = ast_EUOK_Not;
    break;
  }
  default: {
    // there is no level 3 expression
    parsePostfixExpr(l, diagnostics, parser);
    return;
  }
  }

  // this will only execute if an L3 operator
  // exists
  l->kind = ast_EK_UnaryOp;

  // first get metadata
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  l->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  l->common.metadata = com_vec_release(&metadata);
  // consume operator
  parse_next(parser, diagnostics);

  // Now parse the rest of the expression
  l->unaryOp.operand = parse_alloc_obj_m(parser, ast_Expr);
  ast_parsePrefixExpr(l->unaryOp.operand, diagnostics, parser);

  // finally calculate the misc stuff
  l->common.span =
      com_loc_span_m(t.span.start, l->unaryOp.operand->common.span.end);

  // metadata
}

// type is the type of object that the generated function will parse
// x is the index level of the function
// lower_fn is the name of the function that will be called to evaluate the left
// and right op_det_fn is the name of the function that determines the binary
// operator this function should take a pointer to the type and return a bool if
// successful
#define FN_BINOP_PARSE_EXPR(lower_fn, switch_fn, fn_name)                      \
  static void fn_name(ast_Expr *expr, DiagnosticLogger *diagnostics,           \
                      ast_Constructor *parser) {                               \
    ast_Expr v;                                                                \
    lower_fn(&v, diagnostics, parser);                                         \
                                                                               \
    Token t = parse_peekPastMetadata(parser, diagnostics, 1);                  \
    bool success = switch_fn(t.kind, &expr->binaryOp.op);                      \
    if (!success) {                                                            \
      /* there is no level x expression */                                     \
      *expr = v;                                                               \
      return;                                                                  \
    }                                                                          \
    /* this will only execute if the operator exists */                        \
    expr->kind = ast_EK_BinaryOp;                                              \
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

static bool ast_opDetRangeExpr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Range: {
    *val = ast_EBOK_Range;
    return true;
  }
  case tk_RangeInclusive: {
    *val = ast_EBOK_RangeInclusive;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_EXPR(ast_parsePrefixExpr, ast_opDetRangeExpr, ast_parseRangeExpr)

static bool ast_opDetConstrainExpr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Constrain: {
    *val = ast_EBOK_Constrain;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_EXPR(ast_parseRangeExpr, ast_opDetConstrainExpr,
                    ast_parseConstrainExpr)

static bool ast_opDetProductExpr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Mul: {
    *val = ast_EBOK_Mul;
    return true;
  }
  case tk_Div: {
    *val = ast_EBOK_Div;
    return true;
  }
  case tk_Rem: {
    *val = ast_EBOK_Rem;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_EXPR(ast_parseConstrainExpr, ast_opDetProductExpr,
                    ast_parseProductExpr)

static bool ast_opDetSumExpr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Plus: {
    *val = ast_EBOK_Add;
    return true;
  }
  case tk_Minus: {
    *val = ast_EBOK_Sub;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_EXPR(ast_parseProductExpr, ast_opDetSumExpr, ast_parseSumExpr)

static bool ast_opDetCompareExpr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_CompLess: {
    *val = ast_EBOK_CompLess;
    return true;
  }
  case tk_CompGreater: {
    *val = ast_EBOK_CompGreater;
    return true;
  }
  case tk_CompLessEqual: {
    *val = ast_EBOK_CompLessEqual;
    return true;
  }
  case tk_CompGreaterEqual: {
    *val = ast_EBOK_CompGreaterEqual;
    return true;
  }
  case tk_CompEqual: {
    *val = ast_EBOK_CompEqual;
    return true;
  }
  case tk_CompNotEqual: {
    *val = ast_EBOK_CompNotEqual;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_EXPR(ast_parseSumExpr, ast_opDetCompareExpr,
                    ast_parseCompareExpr)

static bool ast_opDetBooleanExpr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_And: {
    *val = ast_EBOK_And;
    return true;
  }
  case tk_Or: {
    *val = ast_EBOK_Or;
    return true;
  }
  case tk_Xor: {
    *val = ast_EBOK_Xor;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_EXPR(ast_parseCompareExpr, ast_opDetBooleanExpr,
                    ast_parseBooleanExpr)

static bool ast_opDetSetExpr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Union: {
    *val = ast_EBOK_Union;
    return true;
  }
  case tk_Difference: {
    *val = ast_EBOK_Difference;
    return true;
  }
  case tk_Intersection: {
    *val = ast_EBOK_Intersection;
    return true;
  }
  case tk_SymDifference: {
    *val = ast_EBOK_SymDifference;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_EXPR(ast_parseBooleanExpr, ast_opDetSetExpr, ast_parseSetExpr)

static bool ast_opDetTypeExpr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Sum: {
    *val = ast_EBOK_Sum;
    return true;
  }
  case tk_Product: {
    *val = ast_EBOK_Product;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_EXPR(ast_parseSetExpr, ast_opDetTypeExpr, ast_parseTypeExpr)

static bool ast_opDetFnExpr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Assign: {
    *val = ast_EBOK_Fn;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_EXPR(ast_parseTypeExpr, ast_opDetFnExpr, ast_parseFnExpr)

// shim method
static void ast_parseExpr(ast_Expr *ptr, DiagnosticLogger *diagnostics,
                          ast_Constructor *parser) {
  ast_parseFnExpr(ptr, diagnostics, parser);
}

static void ast_certain_parseDeferStmnt(ast_Stmnt *dsp,
                                        DiagnosticLogger *diagnostics,
                                        ast_Constructor *parser) {
  dsp->kind = ast_SK_Defer;
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

void ast_parseStmnt(ast_Stmnt *sp, DiagnosticLogger *diagnostics,
                    ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  // peek next token
  Token t = parse_peek(parser, diagnostics, 1);
  switch (t.kind) {
  case tk_Defer: {
    ast_certain_parseDeferStmnt(sp, diagnostics, parser);
    break;
  }
  default: {
    ast_Expr *expr = parse_alloc_obj_m(parser, ast_Expr);
    ast_parseExpr(expr, diagnostics, parser);

    if (parse_peek(parser, diagnostics, 1).kind != tk_Assign) {
      sp->kind = ast_SK_Expr;
      sp->expr.expr = expr;
      sp->common.span = sp->expr.expr->common.span;
      break;
    }

    parse_drop(parser, diagnostics);

    ast_Expr *val = parse_alloc_obj_m(parser, ast_Expr);
    ast_parseExpr(val, diagnostics, parser);

    sp->kind = ast_SK_Assign;
    sp->assign.pat = expr;
    sp->assign.val = val;
    sp->common.span = com_loc_span_m(sp->assign.pat->common.span.start,
                                     sp->assign.val->common.span.end);
    break;
  }
  }
  sp->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  sp->common.metadata = com_vec_release(&metadata);
}

bool ast_eof(ast_Constructor *parser, DiagnosticLogger *d) {
  return parse_peek(parser, d, 1).kind == tk_Eof;
}
