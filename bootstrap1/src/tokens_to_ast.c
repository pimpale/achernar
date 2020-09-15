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

static void ast_parsePat(ast_Pat *pp, DiagnosticLogger *diagnostics,
                         ast_Constructor *parser);

static void ast_parseReference(ast_Reference *ptr,
                               DiagnosticLogger *diagnostics,
                               ast_Constructor *parser) {
  com_mem_zero_obj_m(ptr);

  Token t = parse_peek(parser, diagnostics, 1);

  // start and finish
  com_loc_LnCol start = t.span.start;

  com_vec pathSegments = parse_alloc_vec(parser);

  while (true) {
    t = parse_next(parser, diagnostics);
    if (t.kind == tk_Identifier) {
      *com_vec_push_m(&pathSegments, com_str) = t.identifierToken.data;
      if (parse_next(parser, diagnostics).kind != tk_ModResolution) {
        usize segments_len = com_vec_len_m(&pathSegments, com_str);
        *ptr = (ast_Reference){
            .kind = ast_RK_Reference,
            .span = com_loc_span_m(start, t.span.end),
            .reference = {.segments_len = segments_len,
                          .segments = com_vec_release(&pathSegments)}};
        return;
      }
    } else {
      *dlogger_append(diagnostics) = (Diagnostic){
          .span = t.span,
          .severity = DSK_Error,
          .message = com_str_lit_m("mod reference expected an identifier"),
          .children_len = 0};
      usize segments_len = com_vec_len_m(&pathSegments, com_str);
      *ptr = (ast_Reference){
          .kind = ast_RK_Reference,
          .span = com_loc_span_m(start, t.span.end),
          .reference = {.segments_len = segments_len,
                        .segments = com_vec_release(&pathSegments)}};
      return;
    }
  }
}

static void ast_parseBinding(ast_Binding *ptr, DiagnosticLogger *diagnostics,
                             ast_Constructor *parser) {
  com_mem_zero_obj_m(ptr);
  Token t = parse_next(parser, diagnostics);
  ptr->span = t.span;
  switch (t.kind) {
  case tk_Underscore: {
    ptr->kind = ast_BK_Ignore;
    break;
  }
  case tk_Identifier: {
    ptr->kind = ast_BK_Bind;
    ptr->bind.name = t.identifierToken.data;
    break;
  }
  default: {
    *dlogger_append(diagnostics) = (Diagnostic){
        .span = t.span,
        .severity = DSK_Error,
        .message = com_str_lit_m(
            "binding expected either `_` for drop or an identifier to bind to"),
        .children_len = 0};
    ptr->kind = ast_BK_None;
    break;
  }
  }
}

static void ast_parseField(ast_Field *ptr, DiagnosticLogger *diagnostics,
                           ast_Constructor *parser) {
  com_mem_zero_obj_m(ptr);
  Token t = parse_next(parser, diagnostics);
  ptr->span = t.span;
  switch (t.kind) {
  case tk_Identifier: {
    ptr->kind = ast_FK_Field;
    ptr->field.val = t.identifierToken.data;
    break;
  }
  default: {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("field expected an identifier"),
                     .children_len = 0};
    ptr->kind = ast_FK_None;
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

static void ast_certain_parseNilTypeExpr(ast_Expr *ptr,
                                     DiagnosticLogger *diagnostics,
                                     ast_Constructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_NilType, "expected a tk_NilType");
  ptr->kind = ast_EK_NilType;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseNeverTypeExpr(ast_Expr *ptr,
                                     DiagnosticLogger *diagnostics,
                                     ast_Constructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_NeverType, "expected a tk_NeverType");
  ptr->kind = ast_EK_NeverType;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseIntExpr(ast_Expr *ptr,
                                     DiagnosticLogger *diagnostics,
                                     ast_Constructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Int, "expected a tk_Int");
  ptr->kind = ast_EK_IntLiteral;
  ptr->intLiteral.value = t.intToken.data;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseBoolExpr(ast_Expr *ptr,
                                      DiagnosticLogger *diagnostics,
                                      ast_Constructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Bool, "expected a tk_Bool");
  ptr->kind = ast_EK_BoolLiteral;
  ptr->boolLiteral.value = t.boolToken.data;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseRealExpr(ast_Expr *ptr,
                                      DiagnosticLogger *diagnostics,
                                      ast_Constructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Real, "expected tk_Real");
  ptr->kind = ast_EK_RealLiteral;
  ptr->realLiteral.value = t.realToken.data;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseStringExpr(ast_Expr *sptr,
                                        DiagnosticLogger *diagnostics,
                                        ast_Constructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_String, "expected a tk_String");
  sptr->kind = ast_EK_StringLiteral;
  sptr->stringLiteral.value = t.stringToken.data;
  sptr->common.span = t.span;
  return;
}

static void ast_certain_parseFnExpr(ast_Expr *fptr,
                                       DiagnosticLogger *diagnostics,
                                       ast_Constructor *parser) {
  com_mem_zero_obj_m(fptr);

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Fn, "expected tk_Fn");
  com_loc_LnCol start = t.span.start;

  ast_Binding *name = parse_alloc_obj_m(parser, ast_Binding);

  // check for identifier (if found then it is a recursive fn definition)

  t = parse_peek(parser, diagnostics, 1);
  if (t.kind == tk_Identifier) {
    ast_parseBinding(name, diagnostics, parser);
  } else {
    name->kind = ast_BK_Ignore;
    name->span = t.span;
  }

  // check for leftparen
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_ParenLeft) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("fn expected left paren"),
                     .children_len = 0};
  }

  com_vec parametersv = parse_alloc_vec(parser);

  com_loc_LnCol end;
  PARSE_LIST(&parametersv,                 // members_vec_ptr
             diagnostics,                  // dlogger_ptr
             ast_parsePat,                 // member_parse_function
             ast_Pat,                      // member_kind
             tk_ParenRight,                // delimiting_token_kind
             "DK_FnValExpectedRightParen", // missing_delimiter_error
             end,                          // end_lncol
             parser                        // parser
  )

  usize parameters_len = com_vec_len_m(&parametersv, ast_Pat);
  ast_Pat *parameters = com_vec_release(&parametersv);

  ast_Expr *retType = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(retType, diagnostics, parser);

  if (t.kind != tk_Arrow) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_FnValExpectedArrow"),
                     .children_len = 0};
    end = t.span.end;
  }

  ast_Expr *body = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(fptr->fn.body, diagnostics, parser);

  end = body->common.span.end;

  com_loc_Span span = com_loc_span_m(start, end);
  *fptr = (ast_Expr){.kind = ast_EK_Fn,
                     .common = {.span = span},
                     .fn = {.name = name,
                            .parameters = parameters,
                            .parameters_len = parameters_len,
                            .type = retType,
                            .body = body}};
}

static void ast_certain_parseFnTypeExpr(ast_Expr *fte,
                                    DiagnosticLogger *diagnostics,
                                    ast_Constructor *parser) {
  com_mem_zero_obj_m(fte);

  Token t = parse_next(parser, diagnostics);

  com_assert_m(t.kind == tk_Fn, "expected tk_Fn");

  com_loc_LnCol start = t.span.start;
  com_loc_LnCol end;

  // check for leftparen
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_ParenLeft) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_FnTypeExpectedLeftParen"),
                     .children_len = 0};
    end = t.span.end;
    goto CLEANUP;
  }

  com_vec parameters = parse_alloc_vec(parser);

  PARSE_LIST(&parameters,                   // members_vec_ptr
             diagnostics,                   // dlogger_ptr
             ast_parseExpr,                 // member_parse_function
             ast_Expr,                      // member_kind
             tk_ParenRight,                 // delimiting_token_kind
             "DK_FnTypeExpectedRightParen", // missing_delimiter_error
             end,                           // end_lncol
             parser                         // parser
  )

  fte->fnType.parameters_len = com_vec_len_m(&parameters, ast_Expr);
  fte->fnType.parameters = com_vec_release(&parameters);

  fte->fnType.type = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(fte->fnType.type, diagnostics, parser);

CLEANUP:
  fte->common.span = com_loc_span_m(start, end);
}



static void ast_certain_parseLabelLabelBinding(ast_LabelBinding *r,
                                               DiagnosticLogger *diagnostics,
                                               ast_Constructor *parser) {
  com_mem_zero_obj_m(r);
  r->kind = ast_LBK_Label;
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Label, "expected tk_Label");
  r->span = t.span;
  r->label.label = t.labelToken.data;
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
  bptr->block.label = parse_alloc_obj_m(parser, ast_LabelBinding);
  if (t.kind == tk_Label) {
    ast_certain_parseLabelLabelBinding(bptr->block.label, diagnostics, parser);
  } else {
    *bptr->block.label =
        (ast_LabelBinding){.span = lbracespan, .kind = ast_LBK_Omitted};
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

static void ast_parseLabelReference(ast_LabelReference *ptr,
                                    DiagnosticLogger *diagnostics,
                                    ast_Constructor *parser) {
  com_mem_zero_obj_m(ptr);
  Token t = parse_next(parser, diagnostics);
  ptr->span = t.span;
  if (t.kind == tk_Label) {
    ptr->kind = ast_LRK_Label;
    ptr->label.label = t.labelToken.data;
  } else {
    ptr->kind = ast_LRK_None;
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("Expected label"),
                     .children_len = 0};
  }
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
  rep->ret.label = parse_alloc_obj_m(parser, ast_LabelReference);
  ast_parseLabelReference(rep->ret.label, diagnostics, parser);

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
  lep->loop.label = parse_alloc_obj_m(parser, ast_LabelBinding);
  if (t.kind == tk_Label) {
    ast_certain_parseLabelLabelBinding(lep->loop.label, diagnostics, parser);
  } else {
    *lep->loop.label =
        (ast_LabelBinding){.span = loopspan, .kind = ast_LBK_Omitted};
  }

  lep->loop.body = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(lep->loop.body, diagnostics, parser);
  lep->common.span = com_loc_span_m(start, lep->loop.body->common.span.end);
  return;
}

static void ast_certain_parseReferenceExpr(ast_Expr *rptr,
                                           DiagnosticLogger *diagnostics,
                                           ast_Constructor *parser) {
  com_mem_zero_obj_m(rptr);
  rptr->kind = ast_EK_Reference;
  rptr->reference.path = parse_alloc_obj_m(parser, ast_Reference);
  ast_parseReference(rptr->reference.path, diagnostics, parser);
  rptr->common.span = rptr->reference.path->span;
  return;
}

// field := Value
static void ast_certain_parseRecordExpr(ast_Expr *rvp,
                                        DiagnosticLogger *diagnostics,
                                        ast_Constructor *parser) {
  // zero-initialize bp
  com_mem_zero_obj_m(rvp);
  rvp->kind = ast_EK_Record;

  com_loc_LnCol end;

  rvp->record.field = parse_alloc_obj_m(parser, ast_Field);
  ast_parseField(rvp->record.field, diagnostics, parser);

  // check if define
  Token t = parse_next(parser, diagnostics);
  if (t.kind == tk_Define) {
    // Get value of variable
    rvp->record.val = parse_alloc_obj_m(parser, ast_Expr);
    ast_parseExpr(rvp->record.val, diagnostics, parser);
    end = rvp->record.val->common.span.end;
  } else {
    end = t.span.end;
    rvp->record.val = parse_alloc_obj_m(parser, ast_Expr);
    rvp->record.val->kind = ast_EK_Omitted;
    rvp->record.val->common.span = rvp->record.field->span;
    rvp->record.val->common.metadata_len = 0;
    *dlogger_append(diagnostics) = (Diagnostic){
        .span = t.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("DK_StructMemberLiteralExpectedDefine"),
        .children_len = 0};
  }

  rvp->common.span = com_loc_span_m(rvp->record.field->span.start, end);
  return;
}

// Level1Val braces, literals
// Level2Val as match () & @ . (postfixes)
// Level3Val -- ++ ! (prefixes)
// Level4Val -> (pipeline)
// Level5Val * / % (multiplication and division)
// Level6Val + - (addition and subtraction)
// Level7Val < <= > >= == != (comparators)
// Level8Val && (logical and)
// Level9Val || (logical or)
// Level10Val , (create tuple)
// Level11Val = += -= *= /= %= (Assignment)

static void parseL1Expr(ast_Expr *l1, DiagnosticLogger *diagnostics,
                        ast_Constructor *parser) {

  // value metadata;
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  Token t = parse_peek(parser, diagnostics, 1);
  // Decide which expression it is
  switch (t.kind) {
  // Literals
  case tk_Int: {
    ast_certain_parseIntExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Bool: {
    ast_certain_parseBoolExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Real: {
    ast_certain_parseRealExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Nil: {
    ast_certain_parseNilExpr(l1, diagnostics, parser);
    break;
  }
  case tk_NilType: {
    ast_certain_parseNilExpr(l1, diagnostics, parser);
    break;
  }
  case tk_NeverType: {
    ast_certain_parseNilExpr(l1, diagnostics, parser);
    break;
  }
  case tk_String: {
    ast_certain_parseStringExpr(l1, diagnostics, parser);
    break;
  }
  case tk_BraceLeft: {
    ast_certain_parseBlockExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Fn: {
    ast_certain_parseFnExpr(l1, diagnostics, parser);
    break;
  }
  case tk_FnType: {
    ast_certain_parseFnTypeExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Ret: {
    ast_certain_parseRetExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Loop: {
    ast_certain_parseLoopExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Identifier: {
    Token t2 = parse_peek(parser, diagnostics, 2);
    if (t2.kind == tk_Define) {
      ast_certain_parseRecordExpr(l1, diagnostics, parser);
    } else {
      ast_certain_parseReferenceExpr(l1, diagnostics, parser);
    }
    break;
  }
  default: {
    l1->kind = ast_EK_None;
    l1->common.span = t.span;
    parse_next(parser, diagnostics);
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_UnexpectedToken"),
                     .children_len = 0};
  }
  }
  l1->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  l1->common.metadata = com_vec_release(&metadata);
}

static void ast_certain_postfix_parseFieldAcessExpr(
    ast_Expr *fave, DiagnosticLogger *diagnostics, ast_Constructor *parser,
    ast_Expr *root) {
  com_mem_zero_obj_m(fave);
  fave->kind = ast_EK_FieldAccess;
  fave->fieldAccess.root = root;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_FieldAccess, "expected tk_FieldAccess");

  fave->fieldAccess.field = parse_alloc_obj_m(parser, ast_Field);
  ast_parseField(fave->fieldAccess.field, diagnostics, parser);
  fave->common.span = com_loc_span_m(root->common.span.start,
                                     fave->fieldAccess.field->span.end);
}

static void ast_certain_postfix_parseCallExpr(ast_Expr *cptr,
                                              DiagnosticLogger *diagnostics,
                                              ast_Constructor *parser,
                                              ast_Expr *root) {
  com_mem_zero_obj_m(cptr);

  cptr->kind = ast_EK_Call;
  cptr->call.function = root;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_ParenLeft, "expected tk_ParenLeft");

  com_loc_LnCol end;

  com_vec parameters = parse_alloc_vec(parser);

  PARSE_LIST(&parameters,            // members_vec_ptr
             diagnostics,            // dlogger_ptr
             ast_parseExpr,           // member_parse_function
             ast_Expr,               // member_kind
             tk_ParenRight,          // delimiting_token_kind
             "DK_CallExpectedParen", // missing_delimiter_error
             end,                    // end_lncol
             parser                  // parser
  )

  cptr->call.parameters_len = com_vec_len_m(&parameters, ast_Expr);
  cptr->call.parameters = com_vec_release(&parameters);

  cptr->common.span = com_loc_span_m(root->common.span.start, end);
}

static void ast_certain_postfix_parsePipeExpr(ast_Expr *pptr,
                                              DiagnosticLogger *diagnostics,
                                              ast_Constructor *parser,
                                              ast_Expr *root) {
  com_mem_zero_obj_m(pptr);
  pptr->kind = ast_EK_Pipe;
  pptr->pipe.root = root;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Pipe, "expected tk_Pipe");

  pptr->pipe.fn = parse_alloc_obj_m(parser, ast_Expr);
  parseL1Expr(pptr->pipe.fn, diagnostics, parser);

  com_loc_LnCol end;

  com_vec parameters = parse_alloc_vec(parser);

  if(parse_peek(parser,  diagnostics, 1).kind == tk_ParenLeft) {
    parse_drop(parser, diagnostics) ;
    PARSE_LIST(&parameters,            // members_vec_ptr
               diagnostics,            // dlogger_ptr
               ast_parseExpr,           // member_parse_function
               ast_Expr,               // member_kind
               tk_ParenRight,          // delimiting_token_kind
               "DK_PipeExpectedParen", // missing_delimiter_error
               end,                    // end_lncol
               parser                  // parser
    )
  }

  pptr->pipe.parameters_len = com_vec_len_m(&parameters, ast_Expr);
  pptr->pipe.parameters = com_vec_release(&parameters);

  pptr->common.span =
      com_loc_span_m(root->common.span.start, pptr->pipe.fn->common.span.end);
}

// Pattern `=>` Val
static void ast_parseMatchCase(ast_MatchCase *mcep,
                               DiagnosticLogger *diagnostics,
                               ast_Constructor *parser) {
  com_mem_zero_obj_m(mcep);
  mcep->kind = ast_MCK_Case;

  com_loc_LnCol start = parse_peek(parser, diagnostics, 1).span.start;

  // Get pattern
  mcep->matchCase.pat = parse_alloc_obj_m(parser, ast_Pat);
  ast_parsePat(mcep->matchCase.pat, diagnostics, parser);

  com_loc_LnCol end = mcep->matchCase.pat->common.span.end;

  // Expect colon
  Token t = parse_next(parser, diagnostics);
  if (t.kind != tk_Arrow) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_MatchCaseNoArrow"),
                     .children_len = 0};
    goto CLEANUP;
  }

  // Get Value
  mcep->matchCase.val = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(mcep->matchCase.val, diagnostics, parser);
  end = mcep->matchCase.val->common.span.end;

CLEANUP:
  mcep->common.span = com_loc_span_m(start, end);
  return;
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
             ast_parseMatchCase,     // member_parse_function
             ast_MatchCase,          // member_kind
             tk_BraceRight,          // delimiting_token_kind
             "DK_MatchNoRightBrace", // missing_delimiter_error
             end,                    // end_lncol
             parser                  // parser
  )

CLEANUP:
  // Get interior cases
  mptr->match.cases_len = com_vec_len_m(&cases, ast_MatchCase);
  mptr->match.cases = com_vec_release(&cases);

  mptr->common.span = com_loc_span_m(root->common.span.start, end);
  return;
}

static void parseL2Expr(ast_Expr *l2, DiagnosticLogger *diagnostics,
                        ast_Constructor *parser) {
  // Because it's postfix, we must take a somewhat
  // unorthodox approach here We Parse the level
  // one expr and then use a while loop to process
  // the rest of the stuff

  ast_Expr *root = l2;
  parseL1Expr(root, diagnostics, parser);

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
    case tk_FieldAccess: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Expr);
      *v = *root;
      ast_certain_postfix_parseFieldAcessExpr(root, diagnostics, parser, v);
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

static void ast_parseL3Expr(ast_Expr *l3, DiagnosticLogger *diagnostics,
                            ast_Constructor *parser) {
  Token t = parse_peekPastMetadata(parser, diagnostics, 1);
  switch (t.kind) {
  case tk_Not: {
    l3->unaryOp.op = ast_EUOK_Not;
    break;
  }
  default: {
    // there is no level 3 expression
    parseL2Expr(l3, diagnostics, parser);
    return;
  }
  }

  // this will only execute if an L3 operator
  // exists
  l3->kind = ast_EK_UnaryOp;

  // first get metadata
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  l3->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  l3->common.metadata = com_vec_release(&metadata);
  // consume operator
  parse_next(parser, diagnostics);

  // Now parse the rest of the expression
  l3->unaryOp.operand = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseL3Expr(l3->unaryOp.operand, diagnostics, parser);

  // finally calculate the misc stuff
  l3->common.span =
      com_loc_span_m(t.span.start, l3->unaryOp.operand->common.span.end);

  // metadata
}

// type is the type of object that the generated function will parse
// x is the index level of the function
// lower_fn is the name of the function that will be called to evaluate the left
// and right op_det_fn is the name of the function that determines the binary
// operator this function should take a pointer to the type and return a bool if
// successful
#define FN_BINOP_PARSE_LX_EXPR(type, type_shorthand, x, lower_fn)              \
  static void ast_parseL##x##type(ast_##type *expr,                            \
                                  DiagnosticLogger *diagnostics,               \
                                  ast_Constructor *parser) {                   \
    ast_##type v;                                                              \
    lower_fn(&v, diagnostics, parser);                                         \
                                                                               \
    Token t = parse_peekPastMetadata(parser, diagnostics, 1);                  \
    bool success = ast_opDetL##x##type(t.kind, &expr->binaryOp.op);            \
    if (!success) {                                                            \
      /* there is no level x expression */                                     \
      *expr = v;                                                               \
      return;                                                                  \
    }                                                                          \
    /* this will only execute if the operator exists */                        \
    expr->kind = type_shorthand##_BinaryOp;                                    \
                                                                               \
    /* set the left side */                                                    \
    expr->binaryOp.left_operand = parse_alloc_obj_m(parser, ast_##type);       \
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
    expr->binaryOp.right_operand = parse_alloc_obj_m(parser, ast_##type);      \
    ast_parseL##x##type(expr->binaryOp.right_operand, diagnostics, parser);    \
                                                                               \
    /* calculate misc stuff */                                                 \
    expr->common.span =                                                        \
        com_loc_span_m(expr->binaryOp.left_operand->common.span.start,         \
                       expr->binaryOp.right_operand->common.span.end);         \
                                                                               \
    return;                                                                    \
  }

static bool ast_opDetL4Expr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Mul: {
    *val = ast_EBOK_Mul;
    return true;
  }
  case tk_IDiv: {
    *val = ast_EBOK_IDiv;
    return true;
  }
  case tk_FDiv: {
    *val = ast_EBOK_FDiv;
    return true;
  }
  case tk_IRem: {
    *val = ast_EBOK_IRem;
    return true;
  }
  case tk_FRem: {
    *val = ast_EBOK_FRem;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 4, ast_parseL3Expr)

static bool ast_opDetL5Expr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Add: {
    *val = ast_EBOK_Add;
    return true;
  }
  case tk_Sub: {
    *val = ast_EBOK_Sub;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 5, ast_parseL4Expr)

// Parses a single term that will not collide with
// patterns
static void ast_parseExprTerm(ast_Expr *term, DiagnosticLogger *diagnostics,
                             ast_Constructor *parser) {
  ast_parseL5Expr(term, diagnostics, parser);
}

static bool ast_opDetL6Expr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
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

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 6, ast_parseL5Expr)

static bool ast_opDetL7Expr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
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

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 7, ast_parseL6Expr)

static bool ast_opDetL8Expr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Product: {
    *val = ast_EBOK_Product;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 8, ast_parseL7Expr)

static bool ast_opDetL9Expr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Assign: {
    *val = ast_EBOK_Assign;
    return true;
  }
  case tk_AssignAdd: {
    *val = ast_EBOK_AssignAdd;
    return true;
  }
  case tk_AssignSub: {
    *val = ast_EBOK_AssignSub;
    return true;
  }
  case tk_AssignMul: {
    *val = ast_EBOK_AssignMul;
    return true;
  }
  case tk_AssignIDiv: {
    *val = ast_EBOK_AssignIDiv;
    return true;
  }
  case tk_AssignFDiv: {
    *val = ast_EBOK_AssignFDiv;
    return true;
  }
  case tk_AssignIRem: {
    *val = ast_EBOK_AssignIRem;
    return true;
  }
  case tk_AssignFRem: {
    *val = ast_EBOK_AssignFRem;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 9, ast_parseL8Expr)

// shim method
static void ast_parseExpr(ast_Expr *ptr, DiagnosticLogger *diagnostics,
                          ast_Constructor *parser) {
  ast_parseL9Expr(ptr, diagnostics, parser);
}

// field '->' pat
static void ast_certain_parseRecordPat(ast_Pat *rpp,
                                       DiagnosticLogger *diagnostics,
                                       ast_Constructor *parser) {
  com_mem_zero_obj_m(rpp);
  rpp->kind = ast_PK_Record;

  // parse field
  rpp->record.field = parse_alloc_obj_m(parser, ast_Field);
  ast_parseField(rpp->record.field, diagnostics, parser);

  // expect arrow
  Token t = parse_next(parser, diagnostics);
  if (t.kind != tk_Arrow) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m(
                         "pattern struct field expected arrow after field"),
                     .children_len = 0};
  }

  // Parse pattern
  rpp->record.pat = parse_alloc_obj_m(parser, ast_Pat);
  ast_parsePat(rpp->record.pat, diagnostics, parser);

  rpp->common.span = com_loc_span_m(rpp->record.field->span.start,
                                    rpp->record.pat->common.span.end);
  return;
}

// '{' pat '}'
static void ast_certain_parseGroupPat(ast_Pat *gpep,
                                      DiagnosticLogger *diagnostics,
                                      ast_Constructor *parser) {
  com_mem_zero_obj_m(gpep);
  gpep->kind = ast_PK_Group;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_BraceLeft, "expected tk_BraceLeft");
  com_loc_LnCol start = t.span.start;
  com_loc_LnCol end;

  gpep->group.inner = parse_alloc_obj_m(parser, ast_Pat);
  ast_parsePat(gpep->group.inner, diagnostics, parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceRight) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_PatGroupExpectedRightBrace"),
                     .children_len = 0};
    end = t.span.end;
  } else {
    end = gpep->group.inner->common.span.end;
  }

  gpep->common.span = com_loc_span_m(start, end);
}

// (('val' identifier) | '_' ) [: type]
static void ast_certain_parseBindingPat(ast_Pat *vbp,
                                        DiagnosticLogger *diagnostics,
                                        ast_Constructor *parser) {
  com_mem_zero_obj_m(vbp);
  vbp->kind = ast_PK_Binding;
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Identifier || t.kind == tk_Underscore,
               "expected tk_Identifier or tk_Underscore");
  ast_Binding *binding = parse_alloc_obj_m(parser, ast_Binding);
  ast_parseBinding(binding, diagnostics, parser);
  vbp->binding.binding = binding;
  vbp->common.span = vbp->binding.binding->span;
  return;
}

static void ast_parseL1Pat(ast_Pat *l1, DiagnosticLogger *diagnostics,
                           ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  Token t = parse_peek(parser, diagnostics, 1);
  switch (t.kind) {
  case tk_BraceLeft: {
    ast_certain_parseGroupPat(l1, diagnostics, parser);
    break;
  }
  case tk_Underscore: {
    ast_certain_parseBindingPat(l1, diagnostics, parser);
    break;
  }
  case tk_Identifier: {
    Token t2 = parse_peek(parser, diagnostics, 2);
    switch (t2.kind) {
    case tk_Record: {
      ast_certain_parseRecordPat(l1, diagnostics, parser);
      break;
    }
    default: {
      ast_certain_parseBindingPat(l1, diagnostics, parser);
    }
    }
    break;
  }
  default: {
    l1->kind = ast_PK_None;
    l1->common.span = t.span;
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_PatUnexpectedToken"),
                     .children_len = 0};
    parse_next(parser, diagnostics);
    break;
  }
  }
  l1->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  l1->common.metadata = com_vec_release(&metadata);
}

static void ast_parseL2Pat(ast_Pat *l2, DiagnosticLogger *diagnostics,
                           ast_Constructor *parser) {
  Token t = parse_peekPastMetadata(parser, diagnostics, 1);
  switch (t.kind) {
  case tk_Not: {
    l2->unaryOp.op = ast_PEUOK_Not;
    break;
  }
  default: {
    // there is no level
    // expression
    ast_parseL1Pat(l2, diagnostics, parser);
    return;
  }
  }

  // this will only execute
  // if an L3 operator
  // exists
  l2->kind = ast_PK_UnaryOp;

  // metadata
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  l2->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  l2->common.metadata = com_vec_release(&metadata);

  // accept operator
  t = parse_next(parser, diagnostics);

  // Now parse the rest of
  // the expression
  l2->unaryOp.operand = parse_alloc_obj_m(parser, ast_Pat);
  ast_parseL2Pat(l2->unaryOp.operand, diagnostics, parser);

  // finally calculate the
  // misc stuff
  l2->common.span =
      com_loc_span_m(t.span.start, l2->unaryOp.operand->common.span.end);
}

static bool ast_opDetL3Pat(tk_Kind tk, ast_PatBinaryOpKind *val) {
  switch (tk) {
  case tk_Product: {
    *val = ast_PEBOK_Product;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Pat, ast_PK, 3, ast_parseL2Pat)

static void ast_parsePat(ast_Pat *ppe, DiagnosticLogger *diagnostics,
                         ast_Constructor *parser) {
  ast_parseL3Pat(ppe, diagnostics, parser);
}

static void ast_certain_parseLetStmnt(ast_Stmnt *vdsp,
                                     DiagnosticLogger *diagnostics,

                                     ast_Constructor *parser) {

    vdsp->kind = ast_SK_Let;
  // zero-initialize vdsp
  com_mem_zero_obj_m(vdsp);

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Let, "expected tk_Let");
  com_loc_LnCol start = t.span.start;

  // Get Binding
    vdsp->let.pat = parse_alloc_obj_m(parser, ast_Pat);;
  ast_parsePat(vdsp->let.pat , diagnostics, parser);

  com_loc_LnCol end;

  // Expect define
  t = parse_peek(parser, diagnostics, 1);
  if (t.kind == tk_Define) {
    // accept the define token
    parse_next(parser, diagnostics);

    vdsp->let.val = parse_alloc_obj_m(parser, ast_Expr);
    ast_parseExpr(vdsp->let.val, diagnostics, parser);
    end = vdsp->let.val->common.span.end;
  } else {
    vdsp->let.val = parse_alloc_obj_m(parser, ast_Expr);
    vdsp->let.val->kind = ast_EK_Omitted;
    vdsp->let.val->common.span = vdsp->let.pat->common.span;
    vdsp->let.val->common.metadata_len = 0;
    end = vdsp->let.pat->common.span.end;
  }
  vdsp->common.span = com_loc_span_m(start, end);
}

static void ast_certain_parseDeferStmnt(ast_Stmnt *dsp,
                                        DiagnosticLogger *diagnostics,
                                        ast_Constructor *parser) {
  dsp->kind = ast_SK_DeferStmnt;
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Defer, "expected tk_Defer");

  // label
  dsp->deferStmnt.label = parse_alloc_obj_m(parser, ast_LabelReference);
  ast_parseLabelReference(dsp->deferStmnt.label, diagnostics, parser);

  // value
  dsp->deferStmnt.val = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(dsp->deferStmnt.val, diagnostics, parser);

  // span
  dsp->common.span =
      com_loc_span_m(t.span.start, dsp->deferStmnt.val->common.span.end);
  return;
}

static void ast_certain_parseModStmnt(ast_Stmnt *nsp,
                                      DiagnosticLogger *diagnostics,
                                      ast_Constructor *parser) {
  com_mem_zero_obj_m(nsp);
  nsp->kind = ast_SK_Mod;
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Mod, "expected tk_Mod");
  com_loc_LnCol start = t.span.start;
  com_loc_LnCol end;

  // Create list of
  // statements
  com_vec statements = parse_alloc_vec(parser);

  nsp->mod.name = parse_alloc_obj_m(parser, ast_ModBinding);
  ast_parseModBinding(nsp->mod.name, diagnostics, parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceLeft) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_ModExpectedLeftBrace"),
                     .children_len = 0};
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&statements,                // members_vec_ptr
             diagnostics,                // dlogger_ptr
             ast_parseStmnt,             // member_parse_function
             ast_Stmnt,                  // member_kind
             tk_BraceRight,              // delimiting_token_kind
             "DK_ModExpectedRightBrace", // missing_delimiter_error
             end,                        // end_lncol
             parser                      // parser
  )

CLEANUP:
  nsp->mod.stmnts_len = com_vec_len_m(&statements, ast_Stmnt);
  nsp->mod.stmnts = com_vec_release(&statements);
  nsp->common.span = com_loc_span_m(start, end);
}

static void ast_certain_parseUseStmnt(ast_Stmnt *usp,
                                      DiagnosticLogger *diagnostics,
                                      ast_Constructor *parser) {
  com_mem_zero_obj_m(usp);
  usp->kind = ast_SK_Use;
  Token t = parse_next(parser,
                       diagnostics); // drop use token
  com_assert_m(t.kind == tk_Use, "expected tk_Use");
  com_loc_LnCol start = t.span.start;

  // parse path
  usp->useStmnt.path = parse_alloc_obj_m(parser, ast_Reference);
  ast_parseReference(usp->useStmnt.path, diagnostics, parser);

  usp->common.span = com_loc_span_m(start, usp->useStmnt.path->span.end);
}

void ast_parseStmnt(ast_Stmnt *sp, DiagnosticLogger *diagnostics,
                    ast_Constructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  // peek next token
  Token t = parse_peek(parser, diagnostics, 1);
  switch (t.kind) {
    // Macros
  case tk_Use: {
    ast_certain_parseUseStmnt(sp, diagnostics, parser);
    break;
  }
  case tk_Mod: {
    ast_certain_parseModStmnt(sp, diagnostics, parser);
    break;
  }
  case tk_Let: {
    ast_certain_parseLetStmnt(sp, diagnostics, parser);
    break;
  }
  case tk_Defer: {
    ast_certain_parseDeferStmnt(sp, diagnostics, parser);
    break;
  }
  // essions
  default: {
    sp->kind = ast_SK_Expr;
    sp->expr.expr = parse_alloc_obj_m(parser, ast_Expr);
    ast_parseExpr(sp->expr.expr, diagnostics, parser);
    sp->common.span = sp->expr.expr->common.span;
    break;
  }
  }
  sp->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  sp->common.metadata = com_vec_release(&metadata);
}

bool ast_eof(ast_Constructor *parser, DiagnosticLogger *d) {
  return parse_peek(parser, d, 1).kind == tk_Eof;
}
