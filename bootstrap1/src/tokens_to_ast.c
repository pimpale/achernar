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

static void ast_certain_parseFnExpr(ast_Expr *fptr,
                                    DiagnosticLogger *diagnostics,
                                    ast_Constructor *parser) {
  com_mem_zero_obj_m(fptr);

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Fn, "expected tk_Fn");
  com_loc_LnCol start = t.span.start;

  ast_Expr *parameters = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(parameters, diagnostics, parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Arrow) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_FnValExpectedArrow"),
                     .children_len = 0};
  }

  ast_Expr *body = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(body, diagnostics, parser);

  com_loc_LnCol end = body->common.span.end;

  com_loc_Span span = com_loc_span_m(start, end);

  *fptr = (ast_Expr){.kind = ast_EK_Fn,
                     .common = {.span = span},
                     .fn = {.parameters = parameters, .body = body}};
}

static void ast_certain_parseFnTypeExpr(ast_Expr *fte,
                                        DiagnosticLogger *diagnostics,
                                        ast_Constructor *parser) {
  com_mem_zero_obj_m(fte);

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_FnType, "expected tk_FnType");
  com_loc_LnCol start = t.span.start;

  ast_Expr *parameters = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(parameters, diagnostics, parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Arrow) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_FnTypeValExpectedArrow"),
                     .children_len = 0};
  }

  ast_Expr *body = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(body, diagnostics, parser);

  com_loc_LnCol end = body->common.span.end;

  com_loc_Span span = com_loc_span_m(start, end);

  *fte = (ast_Expr){.kind = ast_EK_FnType,
                    .common = {.span = span},
                    .fn = {.parameters = parameters, .body = body}};
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

  // ensure token is tk let
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

// 'at' pattern 'let' binding
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
        .message = com_str_lit_m("Expected let token after pattern"),
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
  com_assert_m(t.kind == tk_BindIgnore, "expected tk_BindIgnore");
  wpp->common.span = t.span;
}

// Identifier `:` Type
static void ast_parseCompoundTypeElement(ast_CompoundTypeElement *ptr,
                                         DiagnosticLogger *diagnostics,
                                         ast_Constructor *parser) {
  com_mem_zero_obj_m(ptr);
  ptr->kind = ast_CTEK_Element;

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
  ptr->element.type = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(ptr->element.type, diagnostics, parser);
  end = ptr->element.type->common.span.end;

CLEANUP:
  ptr->common.span = com_loc_span_m(start, end);
  return;
}

static void ast_certain_parseStructTypeExpr(ast_Expr *ptr,
                                            DiagnosticLogger *diagnostics,
                                            ast_Constructor *parser) {
  com_mem_zero_obj_m(ptr);
  ptr->kind = ast_EK_StructType;

  // guarantee token exists
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_StructType, "expected tk_StructType");
  com_loc_LnCol start = t.span.start;

  // now we must parse the block containing the
  // cases
  com_vec elements = parse_alloc_vec(parser);

  com_loc_LnCol end;

  // Expect beginning brace
  t = parse_next(parser, diagnostics);

  if (t.kind != tk_BraceLeft) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("expected left brace"),
                     .children_len = 0};
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(
      &elements,                    // members_vec_ptr
      diagnostics,                  // dlogger_ptr
      ast_parseCompoundTypeElement, // member_parse_function
      ast_CompoundTypeElement,      // member_kind
      tk_BraceRight,                // delimiting_token_kind
      "expected a closing right brace for struct type def ", // missing_delimiter_error
      end,                                                   // end_lncol
      parser                                                 // parser
  )

CLEANUP:
  // Get interior elements
  ptr->structType.elements_len =
      com_vec_len_m(&elements, ast_CompoundTypeElement);
  ptr->structType.elements = com_vec_release(&elements);
  ptr->common.span = com_loc_span_m(start, end);
  return;
}

static void ast_certain_parseEnumTypeExpr(ast_Expr *ptr,
                                          DiagnosticLogger *diagnostics,
                                          ast_Constructor *parser) {
  com_mem_zero_obj_m(ptr);
  ptr->kind = ast_EK_EnumType;

  // guarantee token exists
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_EnumType, "expected tk_EnumType");
  com_loc_LnCol start = t.span.start;

  // now we must parse the block containing the
  // cases
  com_vec elements = parse_alloc_vec(parser);

  com_loc_LnCol end;

  // Expect beginning brace
  t = parse_next(parser, diagnostics);

  if (t.kind != tk_BraceLeft) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("expected left brace"),
                     .children_len = 0};
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(
      &elements,                    // members_vec_ptr
      diagnostics,                  // dlogger_ptr
      ast_parseCompoundTypeElement, // member_parse_function
      ast_CompoundTypeElement,      // member_kind
      tk_BraceRight,                // delimiting_token_kind
      "expected a closing right brace for enum type def ", // missing_delimiter_error
      end,                                                 // end_lncol
      parser                                               // parser
  )

CLEANUP:
  // Get interior elements
  ptr->enumType.elements_len =
      com_vec_len_m(&elements, ast_CompoundTypeElement);
  ptr->enumType.elements = com_vec_release(&elements);
  ptr->common.span = com_loc_span_m(start, end);
  return;
}

// Identifier `:=` Value
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
  com_loc_LnCol end;

  // Expect define
  Token t = parse_next(parser, diagnostics);
  if (t.kind != tk_Define) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m(
                         "compound element expected define after identifier"),
                     .children_len = 0};
  }

  // Get Value
  ptr->element.val = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(ptr->element.val, diagnostics, parser);
  end = ptr->element.val->common.span.end;

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
  com_assert_m(t.kind == tk_Struct, "expected tk_Struct");
  com_loc_LnCol start = t.span.start;

  // now we must parse the block containing the
  // cases
  com_vec elements = parse_alloc_vec(parser);

  com_loc_LnCol end;

  // Expect beginning brace
  t = parse_next(parser, diagnostics);

  if (t.kind != tk_BraceLeft) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("expected left brace"),
                     .children_len = 0};
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(
      &elements,                // members_vec_ptr
      diagnostics,              // dlogger_ptr
      ast_parseCompoundElement, // member_parse_function
      ast_CompoundElement,      // member_kind
      tk_BraceRight,            // delimiting_token_kind
      "expected a closing right brace for struct type def ", // missing_delimiter_error
      end,                                                   // end_lncol
      parser                                                 // parser
  )

CLEANUP:
  // Get interior elements
  ptr->structLiteral.elements_len =
      com_vec_len_m(&elements, ast_CompoundElement);
  ptr->structLiteral.elements = com_vec_release(&elements);
  ptr->common.span = com_loc_span_m(start, end);
  return;
}

// Level1Val braces, literals
// Level2Val match new () -> $ @ . ... (postfixes)
// Level3Val not ... ...= (prefixes)
// Level4Val .. ..= : (range + constraints)
// Level5Val * / % (multiplication and division)
// Level6Val + - (addition and subtraction)
// Level7Val < <= > >= == != (comparators)
// Level8Val and or xor (logical operators)
// Level9Val ++ -- ^ !^ (set operators)
// Level10Val , | (type operators)
// Level11Val = (Assignment)

static void parseL1Expr(ast_Expr *l1, DiagnosticLogger *diagnostics,
                        ast_Constructor *parser) {

  // value metadata;
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  Token t = parse_peek(parser, diagnostics, 1);
  // Decide which expression it is
  switch (t.kind) {
  // Literals
  case tk_Nil: {
    ast_certain_parseNilExpr(l1, diagnostics, parser);
    break;
  }
  case tk_NilType: {
    ast_certain_parseNilTypeExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Int: {
    ast_certain_parseIntExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Real: {
    ast_certain_parseRealExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Struct: {
    ast_certain_parseStructExpr(l1, diagnostics, parser);
    break;
  }
  case tk_NeverType: {
    ast_certain_parseNeverTypeExpr(l1, diagnostics, parser);
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
  case tk_StructType: {
    ast_certain_parseStructTypeExpr(l1, diagnostics, parser);
    break;
  }
  case tk_EnumType: {
    ast_certain_parseEnumTypeExpr(l1, diagnostics, parser);
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
  case tk_At: {
    ast_certain_parseAtLetExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Bind: {
    ast_certain_parseBindExpr(l1, diagnostics, parser);
    break;
  }
  case tk_BindIgnore: {
    ast_certain_parseBindIgnoreExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Identifier: {
    ast_certain_parseIdentifierExpr(l1, diagnostics, parser);
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

  cptr->kind = ast_EK_Call;
  cptr->call.root = root;

  cptr->call.parameters = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(cptr->call.parameters, diagnostics, parser);

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
  pptr->kind = ast_EK_Pipe;
  pptr->pipe.root = root;

  pptr->pipe.fn = parse_alloc_obj_m(parser, ast_Expr);
  parseL1Expr(pptr->pipe.fn, diagnostics, parser);

  Token t = parse_next(parser, diagnostics);
  if (t.kind != tk_ParenLeft) {
    *dlogger_append(diagnostics) = (Diagnostic){
        .span = t.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("pipe expr expected opening paren after function"),
        .children_len = 0};
  }

  pptr->pipe.parameters = parse_alloc_obj_m(parser, ast_Expr);
  parseL1Expr(pptr->pipe.parameters, diagnostics, parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_ParenRight) {
    *dlogger_append(diagnostics) = (Diagnostic){
        .span = t.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("pipe expr expected closing paren after parameters"),
        .children_len = 0};
  }

  pptr->common.span =
      com_loc_span_m(root->common.span.start, t.span.end);
}

// Expr `=>` Expr
static void ast_parseMatchCase(ast_MatchCase *mcep,
                               DiagnosticLogger *diagnostics,
                               ast_Constructor *parser) {

  // accept metadata
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  mcep->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  mcep->common.metadata = com_vec_release(&metadata);

  com_mem_zero_obj_m(mcep);
  mcep->kind = ast_MCK_Case;

  com_loc_LnCol start = parse_peek(parser, diagnostics, 1).span.start;

  // Get pattern
  mcep->matchCase.pat = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(mcep->matchCase.pat, diagnostics, parser);

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

static void ast_certain_postfix_parseNewExpr(ast_Expr *nptr,
                                             DiagnosticLogger *diagnostics,
                                             ast_Constructor *parser,
                                             ast_Expr *root) {
  com_mem_zero_obj_m(nptr);
  nptr->kind = ast_EK_New;
  nptr->new.root = root;

  // guarantee token exists
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_New, "expected tk_New");
  com_loc_LnCol end = t.span.end;

  // now we must parse the block containing the
  // cases
  com_vec elements = parse_alloc_vec(parser);

  // Expect beginning brace
  t = parse_peek(parser, diagnostics, 1);

  if (t.kind == tk_BraceLeft) {
    parse_drop(parser, diagnostics);
    PARSE_LIST(&elements,                // members_vec_ptr
               diagnostics,              // dlogger_ptr
               ast_parseCompoundElement, // member_parse_function
               ast_CompoundElement,      // member_kind
               tk_BraceRight,            // delimiting_token_kind
               "DK_NewNoRightBrace",   // missing_delimiter_error
               end,                      // end_lncol
               parser                    // parser
    )
  }

  // Get interior elements
  nptr->new.elements_len = com_vec_len_m(&elements, ast_CompoundElement);
  nptr->new.elements = com_vec_release(&elements);

  nptr->common.span = com_loc_span_m(root->common.span.start, end);
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
    case tk_Ineq: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Expr);
      *v = *root;
      root->kind = ast_EK_UnaryOp;
      root->unaryOp.op = ast_EUOK_IneqGreater;
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
    case tk_New: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Expr);
      *v = *root;
      ast_certain_postfix_parseNewExpr(root, diagnostics, parser, v);
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
  case tk_Ineq: {
    l3->unaryOp.op = ast_EUOK_IneqLesser;
    break;
  }
  case tk_IneqInclusive: {
    l3->unaryOp.op = ast_EUOK_IneqLesserInclusive;
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
  case tk_Constrain: {
    *val = ast_EBOK_Constrain;
    return true;
  }
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

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 4, ast_parseL3Expr)

static bool ast_opDetL5Expr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
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

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 5, ast_parseL4Expr)

static bool ast_opDetL6Expr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
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

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 6, ast_parseL5Expr)

static bool ast_opDetL7Expr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
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

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 7, ast_parseL6Expr)

static bool ast_opDetL8Expr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
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

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 8, ast_parseL7Expr)

static bool ast_opDetL9Expr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
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

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 9, ast_parseL8Expr)

static bool ast_opDetL10Expr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
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

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 10, ast_parseL9Expr)

static bool ast_opDetL11Expr(tk_Kind tk, ast_ExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Assign: {
    *val = ast_EBOK_Assign;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Expr, ast_EK, 11, ast_parseL10Expr)

// shim method
static void ast_parseExpr(ast_Expr *ptr, DiagnosticLogger *diagnostics,
                          ast_Constructor *parser) {
  ast_parseL11Expr(ptr, diagnostics, parser);
}

static void ast_certain_parseDefStmnt(ast_Stmnt *vdsp,
                                      DiagnosticLogger *diagnostics,
                                      ast_Constructor *parser) {
  vdsp->kind = ast_SK_Def;
  // zero-initialize vdsp
  com_mem_zero_obj_m(vdsp);

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Def, "expected tk_Def");
  com_loc_LnCol start = t.span.start;

  // Get Identifier
  vdsp->def.pat = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(vdsp->def.pat, diagnostics, parser);

  vdsp->common.span = com_loc_span_m(start, vdsp->def.pat->common.span.end);
}

static void ast_certain_parseLetStmnt(ast_Stmnt *vlsp,
                                      DiagnosticLogger *diagnostics,
                                      ast_Constructor *parser) {
  vlsp->kind = ast_SK_Let;
  // zero-initialize vlsp
  com_mem_zero_obj_m(vlsp);

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Let, "expected tk_Let");
  com_loc_LnCol start = t.span.start;

  // Get Identifier
  vlsp->let.pat = parse_alloc_obj_m(parser, ast_Expr);
  ast_parseExpr(vlsp->let.pat, diagnostics, parser);

  com_loc_LnCol end;

  // Expect define
  t = parse_peek(parser, diagnostics, 1);
  if (t.kind == tk_Define) {
    // accept the define token
    parse_next(parser, diagnostics);

    // create val
    vlsp->let.val = parse_alloc_obj_m(parser, ast_Expr);
    ast_parseExpr(vlsp->let.val, diagnostics, parser);
    end = vlsp->let.val->common.span.end;
  } else {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m(
                         "Expected := after pattern and a value to assign"),
                     .children_len = 0};

    vlsp->let.val = parse_alloc_obj_m(parser, ast_Expr);
    vlsp->let.val->kind = ast_EK_None;
    vlsp->let.val->common.span = vlsp->let.pat->common.span;
    vlsp->let.val->common.metadata_len = 0;
    end = vlsp->let.pat->common.span.end;
  }
  vlsp->common.span = com_loc_span_m(start, end);
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
    // Macros
  case tk_Let: {
    ast_certain_parseLetStmnt(sp, diagnostics, parser);
    break;
  }
  case tk_Def: {
    ast_certain_parseDefStmnt(sp, diagnostics, parser);
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
