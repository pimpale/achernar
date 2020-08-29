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
static void *parse_alloc(AstConstructor *parser, usize len) {
  return com_allocator_handle_get((com_allocator_alloc(
      parser->_a,
      (com_allocator_HandleData){.len = len,
                                 .flags = com_allocator_defaults(parser->_a) |
                                          com_allocator_NOLEAK})));
}

#define parse_alloc_obj_m(parser, type)                                        \
  (type *)parse_alloc((parser), sizeof(type))

// utility method to allocate some noleak memory from the parser
static com_vec parse_alloc_vec(AstConstructor *parser) {
  return com_vec_create(com_allocator_alloc(
      parser->_a,
      (com_allocator_HandleData){.len = 20,
                                 .flags = com_allocator_defaults(parser->_a) |
                                          com_allocator_NOLEAK |
                                          com_allocator_REALLOCABLE}));
}

// AstConstructor
AstConstructor ast_create(com_reader *r, com_allocator *a) {
  return (AstConstructor){
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
static Token parse_rawNext(AstConstructor *parser,
                           DiagnosticLogger *diagnostics) {
  return tk_next(parser->_reader, diagnostics, parser->_a);
}

// If the peeked token stack is not empty:
//    Ret the first element of the top of the token
//    Pop the first element of the next_metadata_stack
//    For each element in the next metadata stack, push it to the top of the
//    current scope
// Else fetch next raw token
static Token parse_next(AstConstructor *pp, DiagnosticLogger *diagnostics) {

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
static void parse_drop(AstConstructor *ac, DiagnosticLogger *diagnostics) {
  parse_next(ac, diagnostics);
}

// gets the k'th token
// K must be greater than 0
static Token parse_peek(AstConstructor *pp, DiagnosticLogger *diagnostics,
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

void ast_destroy(AstConstructor *pp) {
  com_queue_destroy(&pp->_next_tokens_queue);
}

// returns a vector containing all the metadata encountered here
static com_vec parse_getMetadata(AstConstructor *parser,
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
static Token parse_peekPastMetadata(AstConstructor *parser,
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
static void ast_parseVal(ast_Val *ptr, DiagnosticLogger *diagnostics,
                         AstConstructor *parser);
static void ast_parseType(ast_Type *tep, DiagnosticLogger *diagnostics,
                          AstConstructor *parser);
static void ast_parsePat(ast_Pat *pp, DiagnosticLogger *diagnostics,
                         AstConstructor *parser);

static void ast_certain_parseMacro(attr_UNUSED ast_Macro *mpe,
                                   attr_UNUSED DiagnosticLogger *diagnostics,
                                   attr_UNUSED AstConstructor *parser) {
  // TODO
}

static void ast_parseModReference(ast_ModReference *root,
                                  DiagnosticLogger *diagnostics,
                                  AstConstructor *parser) {
  // the root element is the current sope
  *root = (ast_ModReference){.kind = ast_MRK_Omitted,
                             .span = parse_peek(parser, diagnostics, 1).span};

  while (true) {
    Token t = parse_peek(parser, diagnostics, 1);
    if (t.kind != tk_Identifier) {
      // drop faulty token
      parse_drop(parser, diagnostics);

      // log error
      *dlogger_append(diagnostics) = (Diagnostic){
          .span = t.span,
          .severity = DSK_Error,
          .message = com_str_lit_m("mod reference expected an identifier"),
          .children_len = 0};
      // return
      return;
    }

    // move root to a newly allocated mod reference
    ast_ModReference *mr = parse_alloc_obj_m(parser, ast_ModReference);
    *mr = *root;

    // now create a new root, with the newly allocated mod reference as a child
    *root = (ast_ModReference){
        .kind = ast_MRK_Reference,
        .reference = {.name = t.identifierToken.data, .mod = mr},
        .span = com_loc_span_m(mr->span.start, t.span.end)};

    // now we can drop the identifier
    parse_drop(parser, diagnostics);

    // if the next item is a / then we accept it, otherwise we break
    if (parse_peek(parser, diagnostics, 1).kind == tk_ModResolution) {
      parse_drop(parser, diagnostics);
    } else {
      break;
    }
  }
}

static void ast_parseModBinding(ast_ModBinding *ptr,
                                DiagnosticLogger *diagnostics,
                                AstConstructor *parser) {
  com_mem_zero_obj_m(ptr);
  Token t = parse_next(parser, diagnostics);
  ptr->span = t.span;
  switch (t.kind) {
  case tk_Identifier: {
    ptr->kind = ast_MBK_Binding;
    ptr->binding.value = t.identifierToken.data;
    break;
  }
  default: {
    // throw error
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m(
                         "mod binding expected an identifier to bind to"),
                     .children_len = 0};
    ptr->kind = ast_MBK_None;
    break;
  }
  }
}

static void ast_parseReference(ast_Reference *ptr,
                               DiagnosticLogger *diagnostics,
                               AstConstructor *parser) {
  com_mem_zero_obj_m(ptr);

  Token t1 = parse_peek(parser, diagnostics, 1);
  if (t1.kind != tk_Identifier) {
    // drop faulty reference token
    parse_drop(parser, diagnostics);

    // log an error
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t1.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("reference expected identifier"),
                     .children_len = 0};

    // return reference
    *ptr = (ast_Reference){.kind = ast_RK_None, .span = t1.span};
    return;
  }

  ptr->kind = ast_RK_Reference;
  ptr->reference.mod = parse_alloc_obj_m(parser, ast_ModReference);

  Token t2 = parse_peek(parser, diagnostics, 2);

  if (t2.kind == tk_ModResolution || t2.kind == tk_MemberResolution) {
    // this means that the reference has a defined mod
    ast_parseModReference(ptr->reference.mod, diagnostics, parser);
    // now expect a ::
    Token mr = parse_next(parser, diagnostics);
    if (mr.kind != tk_MemberResolution) {
      // log an error
      *dlogger_append(diagnostics) = (Diagnostic){
          .span = mr.span,
          .severity = DSK_Error,
          .message = com_str_lit_m("reference expected :: after mod"),
          .children_len = 0};

      // return reference
      ptr->span = com_loc_span_m(t1.span.start, mr.span.end);
      ptr->reference.name = com_str_lit_m("");
      return;
    }
  } else {
    // otherwise we use the default mod
    *ptr->reference.mod =
        (ast_ModReference){.kind = ast_MRK_Omitted, .span = t1.span};
  }

  // now expect identifier

  Token identifier = parse_next(parser, diagnostics);

  if (identifier.kind != tk_Identifier) {
    // log an error
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = identifier.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("reference expected identifier"),
                     .children_len = 0};

    // return reference
    ptr->span = com_loc_span_m(t1.span.start, identifier.span.end);
    ptr->reference.name = com_str_lit_m("");
    return;
  }

  ptr->reference.name = identifier.identifierToken.data;
  ptr->span = com_loc_span_m(t1.span.start, identifier.span.end);
}

static void ast_parseBinding(ast_Binding *ptr, DiagnosticLogger *diagnostics,
                             AstConstructor *parser) {
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
    ptr->bind.val = t.identifierToken.data;
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
                           AstConstructor *parser) {
  com_mem_zero_obj_m(ptr);
  Token t = parse_next(parser, diagnostics);
  ptr->span = t.span;
  switch (t.kind) {
  case tk_Identifier: {
    ptr->kind = ast_FK_FieldStr;
    ptr->strField.val = t.identifierToken.data;
    break;
  }
  case tk_Int: {
    ptr->kind = ast_FK_FieldInt;
    ptr->intField.val = t.intToken.data;
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

static void ast_certain_parseNilVal(ast_Val *ptr, DiagnosticLogger *diagnostics,
                                    AstConstructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Nil, "expected a tk_Nil");
  ptr->kind = ast_VK_NilLiteral;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseIntVal(ast_Val *ptr, DiagnosticLogger *diagnostics,
                                    AstConstructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Int, "expected a tk_Int");
  ptr->kind = ast_VK_IntLiteral;
  ptr->intLiteral.value = t.intToken.data;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseBoolVal(ast_Val *ptr,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Bool, "expected a tk_Bool");
  ptr->kind = ast_VK_BoolLiteral;
  ptr->boolLiteral.value = t.boolToken.data;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseFloatVal(ast_Val *ptr,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Float, "expected tk_Float");
  ptr->kind = ast_VK_FloatLiteral;
  ptr->floatLiteral.value = t.floatToken.data;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseCharVal(ast_Val *ptr,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Char, "expected tk_Char");
  ptr->kind = ast_VK_CharLiteral;
  ptr->charLiteral.value = t.charToken.data;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseStringVal(ast_Val *sptr,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_String, "expected a tk_String");
  sptr->kind = ast_VK_StringLiteral;
  sptr->stringLiteral.value = t.stringToken.data;
  sptr->common.span = t.span;
  return;
}

static void ast_certain_parseFnVal(ast_Val *fptr, DiagnosticLogger *diagnostics,
                                   AstConstructor *parser) {
  com_mem_zero_obj_m(fptr);

  fptr->kind = ast_VK_Fn;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Fn, "expected tk_Fn");
  com_loc_LnCol start = t.span.start;
  com_loc_LnCol end = t.span.end;

  // check for leftparen
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_ParenLeft) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("fn expected left paren"),
                     .children_len = 0};
    goto CLEANUP;
  }

  com_loc_Span rarenspan = t.span;

  com_vec parameters = parse_alloc_vec(parser);

  PARSE_LIST(&parameters,                  // members_vec_ptr
             diagnostics,                  // dlogger_ptr
             ast_parsePat,                 // member_parse_function
             ast_Pat,                      // member_kind
             tk_ParenRight,                // delimiting_token_kind
             "DK_FnValExpectedRightParen", // missing_delimiter_error
             end,                          // end_lncol
             parser                        // parser
  )

  fptr->fn.parameters_len = com_vec_len_m(&parameters, ast_Pat);
  fptr->fn.parameters = com_vec_release(&parameters);

  t = parse_peek(parser, diagnostics, 1);
  if (t.kind == tk_Colon) {
    fptr->fn.type = parse_alloc_obj_m(parser, ast_Type);
    // advance
    parse_next(parser, diagnostics);

    ast_parseType(fptr->fn.type, diagnostics, parser);
  } else {
    fptr->fn.type = parse_alloc_obj_m(parser, ast_Type);
    *fptr->fn.type =
        (ast_Type){.common = {.span = rarenspan, .metadata_len = 0},
                   .kind = ast_TK_Omitted};
  }

  t = parse_next(parser, diagnostics);

  if (t.kind != tk_Arrow) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_FnValExpectedArrow"),
                     .children_len = 0};
    end = t.span.end;
    goto CLEANUP;
  }

  fptr->fn.body = parse_alloc_obj_m(parser, ast_Val);
  ast_parseVal(fptr->fn.body, diagnostics, parser);
  end = fptr->fn.body->common.span.end;

CLEANUP:
  fptr->common.span = com_loc_span_m(start, end);
}

static void ast_certain_parseLabelLabelBinding(ast_LabelBinding *r,
                                               DiagnosticLogger *diagnostics,
                                               AstConstructor *parser) {
  com_mem_zero_obj_m(r);
  r->kind = ast_LBK_Label;
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Label, "expected tk_Label");
  r->span = t.span;
  r->label.label = t.labelToken.data;
}

static void ast_certain_parseBlockVal(ast_Val *bptr,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  com_mem_zero_obj_m(bptr);
  bptr->kind = ast_VK_Block;

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
                                    AstConstructor *parser) {
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

static void ast_certain_parseRetVal(ast_Val *rep, DiagnosticLogger *diagnostics,
                                    AstConstructor *parser) {
  com_mem_zero_obj_m(rep);
  rep->kind = ast_VK_Ret;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Ret, "expected tk_Ret");

  com_loc_LnCol start = t.span.start;
  com_loc_LnCol end;

  parse_next(parser, diagnostics);

  // return's scope
  rep->returnExpr.label = parse_alloc_obj_m(parser, ast_LabelReference);
  ast_parseLabelReference(rep->returnExpr.label, diagnostics, parser);

  // value to return
  rep->returnExpr.value = parse_alloc_obj_m(parser, ast_Val);
  ast_parseVal(rep->returnExpr.value, diagnostics, parser);

  // span
  end = rep->returnExpr.value->common.span.end;
  rep->common.span = com_loc_span_m(start, end);
  return;
}

static void ast_certain_parseLoopVal(ast_Val *lep,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {
  lep->kind = ast_VK_Loop;

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

  lep->loop.body = parse_alloc_obj_m(parser, ast_Val);
  ast_parseVal(lep->loop.body, diagnostics, parser);
  lep->common.span = com_loc_span_m(start, lep->loop.body->common.span.end);
  return;
}

static void ast_parseReferenceVal(ast_Val *rptr, DiagnosticLogger *diagnostics,
                                  AstConstructor *parser) {
  com_mem_zero_obj_m(rptr);
  rptr->kind = ast_VK_Reference;
  rptr->reference.path = parse_alloc_obj_m(parser, ast_Reference);
  ast_parseReference(rptr->reference.path, diagnostics, parser);
  rptr->common.span = rptr->reference.path->span;
  return;
}

static void ast_certain_parseMacroValStructMember(ast_ValStructMember *vsemp,
                                                  DiagnosticLogger *diagnostics,
                                                  AstConstructor *parser) {
  com_mem_zero_obj_m(vsemp);
  vsemp->kind = ast_VSMK_Macro;
  vsemp->macro.macro = parse_alloc_obj_m(parser, ast_Macro);
  ast_certain_parseMacro(vsemp->macro.macro, diagnostics, parser);
  vsemp->common.span = vsemp->macro.macro->span;
}

// field := Value
static void
ast_certain_parseMemberValStructMember(ast_ValStructMember *vsmep,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  // zero-initialize bp
  com_mem_zero_obj_m(vsmep);
  vsmep->kind = ast_VSMK_Member;

  com_loc_LnCol end;

  vsmep->member.field = parse_alloc_obj_m(parser, ast_Field);
  ast_parseField(vsmep->member.field, diagnostics, parser);

  // check if define
  Token t = parse_next(parser, diagnostics);
  if (t.kind == tk_Define) {
    // Get value of variable
    vsmep->member.val = parse_alloc_obj_m(parser, ast_Val);
    ast_parseVal(vsmep->member.val, diagnostics, parser);
    end = vsmep->member.val->common.span.end;
  } else {
    *dlogger_append(diagnostics) = (Diagnostic){
        .span = t.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("DK_StructMemberLiteralExpectedDefine"),
        .children_len = 0};
    vsmep->member.val = NULL;
    end = t.span.end;
    goto CLEANUP;
  }

CLEANUP:
  vsmep->common.span = com_loc_span_m(vsmep->member.field->span.start, end);
  return;
}

static void ast_parseValStructMember(ast_ValStructMember *vsmep,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_peek(parser, diagnostics, 1);
  switch (t.kind) {
  case tk_Macro: {
    ast_certain_parseMacroValStructMember(vsmep, diagnostics, parser);
    break;
  }
  case tk_Identifier: {
    ast_certain_parseMemberValStructMember(vsmep, diagnostics, parser);
    break;
  }
  default: {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_StructLiteralExpectedEntry"),
                     .children_len = 0};

    vsmep->kind = ast_VSMK_None;
    vsmep->common.span = t.span;
    // discard token
    parse_next(parser, diagnostics);
    break;
  }
  }
  vsmep->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  vsmep->common.metadata = com_vec_release(&metadata);
}

static void ast_certain_parseValStruct(ast_Val *sve,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  com_mem_zero_obj_m(sve);
  sve->kind = ast_VK_StructLiteral;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Struct, "expected tk_Struct");

  com_loc_LnCol start = t.span.start;
  com_loc_LnCol end;

  com_vec members = parse_alloc_vec(parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceLeft) {
    end = t.span.end;
    *dlogger_append(diagnostics) = (Diagnostic){
        .span = t.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("DK_StructLiteralExpectedLeftBrace"),
        .children_len = 0};
    goto CLEANUP;
  }

  PARSE_LIST(&members,                             // members_vec_ptr
             diagnostics,                          // dlogger_ptr
             ast_parseValStructMember,             // member_parse_function
             ast_ValStructMember,                  // member_kind
             tk_BraceRight,                        // delimiting_token_kind
             "DK_StructLiteralExpectedRightBrace", // missing_delimiter_error
             end,                                  // end_lncol
             parser                                // parser
  )

CLEANUP:
  sve->structExpr.members_len = com_vec_len_m(&members, ast_ValStructMember);
  sve->structExpr.members = com_vec_release(&members);
  sve->common.span = com_loc_span_m(start, end);
  return;
}

static void ast_certain_parseMacroVal(ast_Val *ptr,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  com_mem_zero_obj_m(ptr);
  ptr->kind = ast_VK_Macro;
  ptr->macro.macro = parse_alloc_obj_m(parser, ast_Macro);
  ast_certain_parseMacro(ptr->macro.macro, diagnostics, parser);
  ptr->common.span = ptr->macro.macro->span;
}

// Level1Val parentheses, braces, literals
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

static void parseL1Val(ast_Val *l1, DiagnosticLogger *diagnostics,
                       AstConstructor *parser) {

  // value metadata;
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  Token t = parse_peek(parser, diagnostics, 1);
  // Decide which expression it is
  switch (t.kind) {
  // Macro
  case tk_Macro: {
    ast_certain_parseMacroVal(l1, diagnostics, parser);
    break;
  }
  // Literals
  case tk_Int: {
    ast_certain_parseIntVal(l1, diagnostics, parser);
    break;
  }
  case tk_Bool: {
    ast_certain_parseBoolVal(l1, diagnostics, parser);
    break;
  }
  case tk_Float: {
    ast_certain_parseFloatVal(l1, diagnostics, parser);
    break;
  }
  case tk_Char: {
    ast_certain_parseCharVal(l1, diagnostics, parser);
    break;
  }
  case tk_Nil: {
    ast_certain_parseNilVal(l1, diagnostics, parser);
    break;
  }
  case tk_String: {
    ast_certain_parseStringVal(l1, diagnostics, parser);
    break;
  }
  case tk_BraceLeft: {
    ast_certain_parseBlockVal(l1, diagnostics, parser);
    break;
  }
  case tk_Fn: {
    ast_certain_parseFnVal(l1, diagnostics, parser);
    break;
  }
  case tk_Struct: {
    ast_certain_parseValStruct(l1, diagnostics, parser);
    break;
  }
  case tk_Ret: {
    ast_certain_parseRetVal(l1, diagnostics, parser);
    break;
  }
  case tk_Loop: {
    ast_certain_parseLoopVal(l1, diagnostics, parser);
    break;
  }
  case tk_Identifier: {
    ast_parseReferenceVal(l1, diagnostics, parser);
    break;
  }
  default: {
    l1->kind = ast_VK_None;
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

static void
ast_certain_postfix_parseFieldAcessVal(ast_Val *fave,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser, ast_Val *root) {
  com_mem_zero_obj_m(fave);
  fave->kind = ast_VK_FieldAccess;
  fave->fieldAccess.root = root;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_FieldAccess, "expected tk_FieldAccess");

  fave->fieldAccess.field = parse_alloc_obj_m(parser, ast_Field);
  ast_parseField(fave->fieldAccess.field, diagnostics, parser);
  fave->common.span = com_loc_span_m(root->common.span.start,
                                     fave->fieldAccess.field->span.end);
}

static void ast_certain_postfix_parseCallVal(ast_Val *cptr,
                                             DiagnosticLogger *diagnostics,
                                             AstConstructor *parser,
                                             ast_Val *root) {
  com_mem_zero_obj_m(cptr);

  cptr->kind = ast_VK_Call;
  cptr->call.function = root;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_ParenLeft, "expected tk_ParenLeft");

  com_loc_LnCol end;

  com_vec parameters = parse_alloc_vec(parser);

  PARSE_LIST(&parameters,            // members_vec_ptr
             diagnostics,            // dlogger_ptr
             ast_parseVal,           // member_parse_function
             ast_Val,                // member_kind
             tk_ParenRight,          // delimiting_token_kind
             "DK_CallExpectedParen", // missing_delimiter_error
             end,                    // end_lncol
             parser                  // parser
  )

  cptr->call.parameters_len = com_vec_len_m(&parameters, ast_Val);
  cptr->call.parameters = com_vec_release(&parameters);

  cptr->common.span = com_loc_span_m(root->common.span.start, end);
}

static void ast_certain_postfix_parseAsVal(ast_Val *aptr,
                                           DiagnosticLogger *diagnostics,
                                           AstConstructor *parser,
                                           ast_Val *root) {
  com_mem_zero_obj_m(aptr);
  aptr->kind = ast_VK_As;
  aptr->as.root = root;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_As, "expected tk_As");

  aptr->as.type = parse_alloc_obj_m(parser, ast_Type);
  ast_parseType(aptr->as.type, diagnostics, parser);
  aptr->common.span =
      com_loc_span_m(root->common.span.start, aptr->as.type->common.span.end);
}

static void ast_certain_postfix_parsePipeVal(ast_Val *pptr,
                                             DiagnosticLogger *diagnostics,
                                             AstConstructor *parser,
                                             ast_Val *root) {
  com_mem_zero_obj_m(pptr);
  pptr->kind = ast_VK_Pipe;
  pptr->pipe.root = root;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Pipe, "expected tk_Pipe");

  pptr->pipe.fn = parse_alloc_obj_m(parser, ast_Val);
  parseL1Val(pptr->pipe.fn, diagnostics, parser);
  pptr->common.span =
      com_loc_span_m(root->common.span.start, pptr->pipe.fn->common.span.end);
}

// pat Pattern => ,
static void ast_certain_ParsePatMatchCase(ast_MatchCase *mcep,
                                          DiagnosticLogger *diagnostics,
                                          AstConstructor *parser) {
  com_mem_zero_obj_m(mcep);
  mcep->kind = ast_MCK_Case;

  // Get Pat
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Pat, "expected tk_Pat");
  com_loc_LnCol start = t.span.start;

  // Get pattern
  mcep->matchCase.pattern = parse_alloc_obj_m(parser, ast_Pat);
  ast_parsePat(mcep->matchCase.pattern, diagnostics, parser);

  com_loc_LnCol end = mcep->matchCase.pattern->common.span.end;

  // Expect colon
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Arrow) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_MatchCaseNoArrow"),
                     .children_len = 0};
    goto CLEANUP;
  }

  // Get Value
  mcep->matchCase.val = parse_alloc_obj_m(parser, ast_Val);
  ast_parseVal(mcep->matchCase.val, diagnostics, parser);
  end = mcep->matchCase.val->common.span.end;

CLEANUP:
  mcep->common.span = com_loc_span_m(start, end);
  return;
}

static void ast_certain_parseMacroMatchCase(ast_MatchCase *mcep,
                                            DiagnosticLogger *diagnostics,
                                            AstConstructor *parser) {
  com_mem_zero_obj_m(mcep);
  mcep->kind = ast_MCK_Macro;
  mcep->macro.macro = parse_alloc_obj_m(parser, ast_Macro);
  ast_certain_parseMacro(mcep->macro.macro, diagnostics, parser);
  mcep->common.span = mcep->macro.macro->span;
}

static void ast_parseMatchCase(ast_MatchCase *mcep,
                               DiagnosticLogger *diagnostics,
                               AstConstructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_peek(parser, diagnostics, 1);
  switch (t.kind) {
  case tk_Pat: {
    ast_certain_ParsePatMatchCase(mcep, diagnostics, parser);
    break;
  }
  case tk_Macro: {
    ast_certain_parseMacroMatchCase(mcep, diagnostics, parser);
    break;
  }
  default: {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_MatchCaseNoPat"),
                     .children_len = 0};
    mcep->kind = ast_MCK_None;
    mcep->common.span = t.span;
    // discard token
    parse_next(parser, diagnostics);
    break;
  }
  }
  mcep->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  mcep->common.metadata = com_vec_release(&metadata);
}

static void ast_certain_postfix_parseMatchVal(ast_Val *mptr,
                                              DiagnosticLogger *diagnostics,
                                              AstConstructor *parser,
                                              ast_Val *root) {
  com_mem_zero_obj_m(mptr);
  mptr->kind = ast_VK_Match;
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

static void parseL2Val(ast_Val *l2, DiagnosticLogger *diagnostics,
                       AstConstructor *parser) {
  // Because it's postfix, we must take a somewhat
  // unorthodox approach here We Parse the level
  // one expr and then use a while loop to process
  // the rest of the stuff

  ast_Val *root = l2;
  parseL1Val(root, diagnostics, parser);

  while (true) {
    // represents the old operation
    ast_Val *v;

    Token t = parse_peekPastMetadata(parser, diagnostics, 1);
    switch (t.kind) {
    case tk_Ref: {
      // get metadata
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      // allocate space for operation
      v = parse_alloc_obj_m(parser, ast_Val);
      *v = *root;
      root->kind = ast_VK_UnaryOp;
      root->unaryOp.op = ast_VEUOK_Ref;
      root->unaryOp.operand = v;
      root->common.span = com_loc_span_m(v->common.span.start, t.span.end);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      parse_next(parser, diagnostics);
      break;
    }
    case tk_Deref: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Val);
      *v = *root;
      root->kind = ast_VK_UnaryOp;
      root->unaryOp.op = ast_VEUOK_Deref;
      root->unaryOp.operand = v;
      root->common.span = com_loc_span_m(v->common.span.start, t.span.end);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      parse_next(parser, diagnostics);
      break;
    }
    case tk_FieldAccess: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Val);
      *v = *root;
      ast_certain_postfix_parseFieldAcessVal(root, diagnostics, parser, v);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      break;
    }
    case tk_ParenLeft: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Val);
      *v = *root;
      ast_certain_postfix_parseCallVal(root, diagnostics, parser, v);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      break;
    }
    case tk_As: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Val);
      *v = *root;
      ast_certain_postfix_parseAsVal(root, diagnostics, parser, v);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      break;
    }
    case tk_Pipe: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Val);
      *v = *root;
      ast_certain_postfix_parsePipeVal(root, diagnostics, parser, v);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      break;
    }
    case tk_Match: {
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      v = parse_alloc_obj_m(parser, ast_Val);
      *v = *root;
      ast_certain_postfix_parseMatchVal(root, diagnostics, parser, v);
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

static void ast_parseL3Val(ast_Val *l3, DiagnosticLogger *diagnostics,
                           AstConstructor *parser) {
  Token t = parse_peekPastMetadata(parser, diagnostics, 1);
  switch (t.kind) {
  case tk_Not: {
    l3->unaryOp.op = ast_VEUOK_Not;
    break;
  }
  default: {
    // there is no level 3 expression
    parseL2Val(l3, diagnostics, parser);
    return;
  }
  }

  // this will only execute if an L3 operator
  // exists
  l3->kind = ast_VK_UnaryOp;

  // first get metadata
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  l3->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  l3->common.metadata = com_vec_release(&metadata);
  // consume operator
  parse_next(parser, diagnostics);

  // Now parse the rest of the expression
  l3->unaryOp.operand = parse_alloc_obj_m(parser, ast_Val);
  ast_parseL3Val(l3->unaryOp.operand, diagnostics, parser);

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
                                  AstConstructor *parser) {                    \
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

static bool ast_opDetL4Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
  switch (tk) {
  case tk_Mul: {
    *val = ast_VEBOK_Mul;
    return true;
  }
  case tk_IDiv: {
    *val = ast_VEBOK_IDiv;
    return true;
  }
  case tk_FDiv: {
    *val = ast_VEBOK_FDiv;
    return true;
  }
  case tk_IRem: {
    *val = ast_VEBOK_IRem;
    return true;
  }
  case tk_FRem: {
    *val = ast_VEBOK_FRem;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 4, ast_parseL3Val)

static bool ast_opDetL5Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
  switch (tk) {
  case tk_Add: {
    *val = ast_VEBOK_Add;
    return true;
  }
  case tk_Sub: {
    *val = ast_VEBOK_Sub;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 5, ast_parseL4Val)

// Parses a single term that will not collide with
// patterns
static void ast_parseValTerm(ast_Val *term, DiagnosticLogger *diagnostics,
                             AstConstructor *parser) {
  ast_parseL5Val(term, diagnostics, parser);
}

static bool ast_opDetL6Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
  switch (tk) {
  case tk_CompLess: {
    *val = ast_VEBOK_CompLess;
    return true;
  }
  case tk_CompGreater: {
    *val = ast_VEBOK_CompGreater;
    return true;
  }
  case tk_CompLessEqual: {
    *val = ast_VEBOK_CompLessEqual;
    return true;
  }
  case tk_CompGreaterEqual: {
    *val = ast_VEBOK_CompGreaterEqual;
    return true;
  }
  case tk_CompEqual: {
    *val = ast_VEBOK_CompEqual;
    return true;
  }
  case tk_CompNotEqual: {
    *val = ast_VEBOK_CompNotEqual;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 6, ast_parseL5Val)

static bool ast_opDetL7Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
  switch (tk) {
  case tk_Intersection: {
    *val = ast_VEBOK_Intersection;
    return true;
  }
  case tk_Union: {
    *val = ast_VEBOK_Union;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 7, ast_parseL6Val)

static bool ast_opDetL8Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
  switch (tk) {
  case tk_And: {
    *val = ast_VEBOK_And;
    return true;
  }
  case tk_Or: {
    *val = ast_VEBOK_Or;
    return true;
  }
  case tk_Xor: {
    *val = ast_VEBOK_Xor;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 8, ast_parseL7Val)

static bool ast_opDetL9Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
  switch (tk) {
  case tk_Product: {
    *val = ast_VEBOK_Product;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 9, ast_parseL8Val)

static bool ast_opDetL10Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
  switch (tk) {
  case tk_Assign: {
    *val = ast_VEBOK_Assign;
    return true;
  }
  case tk_AssignAdd: {
    *val = ast_VEBOK_AssignAdd;
    return true;
  }
  case tk_AssignSub: {
    *val = ast_VEBOK_AssignSub;
    return true;
  }
  case tk_AssignMul: {
    *val = ast_VEBOK_AssignMul;
    return true;
  }
  case tk_AssignIDiv: {
    *val = ast_VEBOK_AssignIDiv;
    return true;
  }
  case tk_AssignFDiv: {
    *val = ast_VEBOK_AssignFDiv;
    return true;
  }
  case tk_AssignIRem: {
    *val = ast_VEBOK_AssignIRem;
    return true;
  }
  case tk_AssignFRem: {
    *val = ast_VEBOK_AssignFRem;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 10, ast_parseL9Val)

// shim method
static void ast_parseVal(ast_Val *ptr, DiagnosticLogger *diagnostics,
                         AstConstructor *parser) {
  ast_parseL10Val(ptr, diagnostics, parser);
}

// field : Type,
static void
ast_certain_parseMemberTypeStructMember(ast_TypeStructMember *tsmep,
                                        DiagnosticLogger *diagnostics,
                                        AstConstructor *parser) {
  // zero-initialize bp
  com_mem_zero_obj_m(tsmep);
  tsmep->kind = ast_TSMK_StructMember;

  com_loc_LnCol start;
  com_loc_LnCol end;

  // get.identifierToken.data
  tsmep->structMember.field = parse_alloc_obj_m(parser, ast_Field);
  ast_parseField(tsmep->structMember.field, diagnostics, parser);
  start = tsmep->structMember.field->span.start;

  // check if colon
  Token t = parse_peek(parser, diagnostics, 1);
  if (t.kind == tk_Colon) {
    // advance through colon
    parse_next(parser, diagnostics);
    // Get structMember.type of variable
    tsmep->structMember.type = parse_alloc_obj_m(parser, ast_Type);
    ast_parseType(tsmep->structMember.type, diagnostics, parser);
    end = tsmep->structMember.type->common.span.end;
  } else {
    end = tsmep->structMember.field->span.end;
    tsmep->structMember.type = parse_alloc_obj_m(parser, ast_Type);
    tsmep->structMember.type->kind = ast_TK_Omitted;
    tsmep->structMember.type->common.span = tsmep->structMember.field->span;
    tsmep->structMember.type->common.metadata_len = 0;
  }

  tsmep->common.span = com_loc_span_m(start, end);
  return;
}

static void
ast_certain_parseMacroTypeStructMember(ast_TypeStructMember *tsmep,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  com_mem_zero_obj_m(tsmep);
  tsmep->kind = ast_TSMK_Macro;
  tsmep->macro.macro = parse_alloc_obj_m(parser, ast_Macro);
  ast_certain_parseMacro(tsmep->macro.macro, diagnostics, parser);
  tsmep->common.span = tsmep->macro.macro->span;
}

static void ast_parseTypeStructMember(ast_TypeStructMember *tsmep,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_peek(parser, diagnostics, 1);
  switch (t.kind) {
  case tk_Macro: {
    ast_certain_parseMacroTypeStructMember(tsmep, diagnostics, parser);
    break;
  }
  case tk_Identifier: {
    ast_certain_parseMemberTypeStructMember(tsmep, diagnostics, parser);
    break;
  }
  default: {
    *dlogger_append(diagnostics) = (Diagnostic){
        .span = t.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("DK_StructMemberExpectedIdentifier"),
        .children_len = 0};
    tsmep->kind = ast_TSMK_None;
    tsmep->common.span = t.span;
    // discard token
    parse_next(parser, diagnostics);
  }
  }
  tsmep->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  tsmep->common.metadata = com_vec_release(&metadata);
}

static void ast_certain_parseStructType(ast_Type *ste,
                                        DiagnosticLogger *diagnostics,
                                        AstConstructor *parser) {
  com_mem_zero_obj_m(ste);
  ste->kind = ast_TK_Struct;

  Token t = parse_next(parser, diagnostics);
  switch (t.kind) {
  case tk_Struct: {
    ste->structExpr.kind = ast_TSK_Struct;
    break;
  }
  case tk_Enum: {
    ste->structExpr.kind = ast_TSK_Enum;
    break;
  }
  default: {
    com_assert_m(t.kind == tk_Struct || t.kind == tk_Enum, "expected tk_Enum");
  }
  }

  com_loc_LnCol start = t.span.start;
  com_loc_LnCol end;

  com_vec members = parse_alloc_vec(parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceLeft) {
    end = t.span.end;
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_StructExpectedLeftBrace"),
                     .children_len = 0};
    goto CLEANUP;
  }

  PARSE_LIST(&members,                      // members_vec_ptr
             diagnostics,                   // dlogger_ptr
             ast_parseTypeStructMember,     // member_parse_function
             ast_TypeStructMember,          // member_kind
             tk_BraceRight,                 // delimiting_token_kind
             "DK_StructExpectedRightBrace", // missing_delimiter_error
             end,                           // end_lncol
             parser                         // parser
  )

CLEANUP:
  ste->structExpr.members_len = com_vec_len_m(&members, ast_TypeStructMember);
  ste->structExpr.members = com_vec_release(&members);
  ste->common.span = com_loc_span_m(start, end);
  return;
}

static void ast_certain_parseReferenceType(ast_Type *rtep,
                                           DiagnosticLogger *diagnostics,
                                           AstConstructor *parser) {
  com_mem_zero_obj_m(rtep);
  rtep->kind = ast_TK_Reference;
  rtep->reference.path = parse_alloc_obj_m(parser, ast_Reference);
  ast_parseReference(rtep->reference.path, diagnostics, parser);
  rtep->common.span = rtep->reference.path->span;
}

static void ast_certain_parseNilType(ast_Type *vte,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Nil, "expected tk_Nil");
  vte->kind = ast_TK_Nil;
  vte->common.span = t.span;
  return;
}

static void ast_certain_parseNeverType(ast_Type *vte,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Never, "expected tk_Never");
  vte->kind = ast_TK_Never;
  vte->common.span = t.span;
  return;
}

static void ast_certain_parseFnType(ast_Type *fte,
                                    DiagnosticLogger *diagnostics,
                                    AstConstructor *parser) {
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
             ast_parseType,                 // member_parse_function
             ast_Type,                      // member_kind
             tk_ParenRight,                 // delimiting_token_kind
             "DK_FnTypeExpectedRightParen", // missing_delimiter_error
             end,                           // end_lncol
             parser                         // parser
  )

  fte->fn.parameters_len = com_vec_len_m(&parameters, ast_Type);
  fte->fn.parameters = com_vec_release(&parameters);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Colon) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_FnTypeExpectedColon"),
                     .children_len = 0};
    end = t.span.end;
    goto CLEANUP;
  }

  fte->fn.type = parse_alloc_obj_m(parser, ast_Type);
  ast_parseType(fte->fn.type, diagnostics, parser);

CLEANUP:
  fte->common.span = com_loc_span_m(start, end);
}

static void ast_certain_parseGroupType(ast_Type *gtep,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  com_mem_zero_obj_m(gtep);
  gtep->kind = ast_TK_Group;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_BraceLeft, "expected tk_BraceLeft");
  com_loc_LnCol start = t.span.start;
  com_loc_LnCol end;

  gtep->group.inner = parse_alloc_obj_m(parser, ast_Type);
  ast_parseType(gtep->group.inner, diagnostics, parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceRight) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_"
                                              "TypeGroupExpectedRightBrace"),
                     .children_len = 0};
    end = t.span.end;
  } else {
    end = gtep->group.inner->common.span.end;
  }

  gtep->common.span = com_loc_span_m(start, end);
}

static void ast_certain_parseMacroType(ast_Type *tep,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  com_mem_zero_obj_m(tep);
  tep->kind = ast_TK_Macro;
  tep->macro.macro = parse_alloc_obj_m(parser, ast_Macro);
  ast_certain_parseMacro(tep->macro.macro, diagnostics, parser);
  tep->common.span = tep->macro.macro->span;
}

static void ast_parseL1Type(ast_Type *l1, DiagnosticLogger *diagnostics,
                            AstConstructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_peek(parser, diagnostics, 1);
  switch (t.kind) {
  case tk_Macro: {
    ast_certain_parseMacroType(l1, diagnostics, parser);
    break;
  }
  case tk_Identifier: {
    ast_certain_parseReferenceType(l1, diagnostics, parser);
    break;
  }
  case tk_Enum:
  case tk_Struct: {
    ast_certain_parseStructType(l1, diagnostics, parser);
    break;
  }
  case tk_Nil: {
    ast_certain_parseNilType(l1, diagnostics, parser);
    break;
  }
  case tk_Never: {
    ast_certain_parseNeverType(l1, diagnostics, parser);
    break;
  }
  case tk_Fn: {
    ast_certain_parseFnType(l1, diagnostics, parser);
    break;
  }
  case tk_BraceLeft: {
    ast_certain_parseGroupType(l1, diagnostics, parser);
    break;
  }
  default: {
    l1->kind = ast_TK_None;
    l1->common.span = t.span;
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_"
                                              "TypeUnexpected"
                                              "Token"),
                     .children_len = 0}; // drop
                                         // faulty
                                         // token
    parse_next(parser, diagnostics);
    break;
  }
  }

  l1->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  l1->common.metadata = com_vec_release(&metadata);
}

static void ast_parseFieldAccessType(ast_Type *srte,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser, ast_Type *root) {
  com_mem_zero_obj_m(srte);
  srte->kind = ast_TK_FieldAccess;
  srte->fieldAccess.root = root;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_FieldAccess, "expected tk_FieldAccess");

  srte->fieldAccess.field = parse_alloc_obj_m(parser, ast_Field);
  ast_parseField(srte->fieldAccess.field, diagnostics, parser);

  srte->common.span = com_loc_span_m(root->common.span.start,
                                     srte->fieldAccess.field->span.end);
}

static void ast_parseL2Type(ast_Type *l2, DiagnosticLogger *diagnostics,
                            AstConstructor *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff

  ast_Type *root = l2;
  ast_parseL1Type(root, diagnostics, parser);

  while (true) {
    // represents the new operation
    ast_Type *ty;

    Token t = parse_peekPastMetadata(parser, diagnostics, 1);
    switch (t.kind) {
    case tk_Ref: {
      // get metadata
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      ty = parse_alloc_obj_m(parser, ast_Type);
      *ty = *root;
      root->kind = ast_TK_UnaryOp;
      root->unaryOp.op = ast_TEUOK_Ref;
      root->unaryOp.operand = ty;
      root->common.span = com_loc_span_m(ty->common.span.start, t.span.end);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      parse_next(parser, diagnostics);
      break;
    }
    case tk_Deref: {
      // get metadata
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      ty = parse_alloc_obj_m(parser, ast_Type);
      *ty = *root;
      root->kind = ast_TK_UnaryOp;
      root->unaryOp.op = ast_TEUOK_Deref;
      root->unaryOp.operand = ty;
      root->common.span = com_loc_span_m(ty->common.span.start, t.span.end);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      parse_next(parser, diagnostics);
      break;
    }
    case tk_FieldAccess: {
      // get metadata
      com_vec metadata = parse_getMetadata(parser, diagnostics);
      ty = parse_alloc_obj_m(parser, ast_Type);
      *ty = *root;
      ast_parseFieldAccessType(root, diagnostics, parser, ty);
      root->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
      root->common.metadata = com_vec_release(&metadata);
      break;
    }
    default: {
      // there are no more level2
      // expressions
      return;
    }
    }
  }
}

static bool ast_opDetL3Type(tk_Kind tk, ast_TypeBinaryOpKind *val) {
  switch (tk) {
  case tk_Intersection: {
    *val = ast_TEBOK_Intersection;
    return true;
  }
  case tk_Union: {
    *val = ast_TEBOK_Union;
    return true;
  }
  default: {
    // there is no level 3 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Type, ast_TK, 3, ast_parseL2Type)

static bool ast_opDetL4Type(tk_Kind tk, ast_TypeBinaryOpKind *val) {
  switch (tk) {
  case tk_Product: {
    *val = ast_TEBOK_Product;
    return true;
  }
  case tk_Sum: {
    *val = ast_TEBOK_Sum;
    return true;
  }
  default: {
    // there is no level 4 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Type, ast_TK, 4, ast_parseL3Type)

static void ast_parseType(ast_Type *tep, DiagnosticLogger *diagnostics,
                          AstConstructor *parser) {
  ast_parseL4Type(tep, diagnostics, parser);
}

static void ast_certain_parseValRestrictionPat(ast_Pat *vrpe,
                                               DiagnosticLogger *diagnostics,
                                               AstConstructor *parser) {
  com_mem_zero_obj_m(vrpe);

  Token t = parse_next(parser, diagnostics);
  com_loc_LnCol start = t.span.start;

  vrpe->kind = ast_PK_ValRestriction;

  switch (t.kind) {
  case tk_CompEqual: {
    vrpe->valRestriction.restriction = ast_PEVRK_CompEqual;
    break;
  }
  case tk_CompNotEqual: {
    vrpe->valRestriction.restriction = ast_PEVRK_CompNotEqual;
    break;
  }
  case tk_CompGreaterEqual: {
    vrpe->valRestriction.restriction = ast_PEVRK_CompGreaterEqual;
    break;
  }
  case tk_CompGreater: {
    vrpe->valRestriction.restriction = ast_PEVRK_CompGreater;
    break;
  }
  case tk_CompLess: {
    vrpe->valRestriction.restriction = ast_PEVRK_CompLess;
    break;
  }
  case tk_CompLessEqual: {
    vrpe->valRestriction.restriction = ast_PEVRK_CompLessEqual;
    break;
  }
  default: {
    com_assert_unreachable_m("expected a comparator");
  }
  }
  vrpe->valRestriction.val = parse_alloc_obj_m(parser, ast_Val);
  ast_parseValTerm(vrpe->valRestriction.val, diagnostics, parser);
  com_loc_LnCol end = vrpe->valRestriction.val->common.span.end;

  vrpe->common.span = com_loc_span_m(start, end);
  return;
}

static void ast_certain_parseTypeRestrictionPat(ast_Pat *trpe,
                                                DiagnosticLogger *diagnostics,
                                                AstConstructor *parser) {
  com_mem_zero_obj_m(trpe);
  trpe->kind = ast_PK_TypeRestriction;

  // parse field
  trpe->typeRestriction.name = parse_alloc_obj_m(parser, ast_Binding);
  ast_parseBinding(trpe->typeRestriction.name, diagnostics, parser);

  Token t = parse_peek(parser, diagnostics, 1);

  // parse type
  trpe->typeRestriction.type = parse_alloc_obj_m(parser, ast_Type);
  if (t.kind != tk_Colon) {
    trpe->typeRestriction.type->kind = ast_TK_Omitted;
    trpe->typeRestriction.type->common.span = trpe->typeRestriction.name->span;
    trpe->typeRestriction.type->common.metadata_len = 0;
  } else {
    ast_parseType(trpe->typeRestriction.type, diagnostics, parser);
  }

  trpe->common.span =
      com_loc_span_m(trpe->typeRestriction.name->span.start,
                     trpe->typeRestriction.type->common.span.end);
}

// field '=>' pat
static void ast_certain_parseFieldPatStructMember(ast_PatStructMember *psmep,
                                                  DiagnosticLogger *diagnostics,
                                                  AstConstructor *parser) {
  com_mem_zero_obj_m(psmep);
  psmep->kind = ast_PSMK_Field;

  // parse field
  psmep->field.field = parse_alloc_obj_m(parser, ast_Field);
  ast_parseField(psmep->field.field, diagnostics, parser);

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
  psmep->field.pat = parse_alloc_obj_m(parser, ast_Pat);
  ast_parsePat(psmep->field.pat, diagnostics, parser);

  psmep->common.span = com_loc_span_m(psmep->field.field->span.start,
                                      psmep->field.pat->common.span.end);
  return;
}

static void ast_certain_parseMacroPatStructMember(ast_PatStructMember *psmep,
                                                  DiagnosticLogger *diagnostics,
                                                  AstConstructor *parser) {
  com_mem_zero_obj_m(psmep);
  psmep->kind = ast_PSMK_Macro;
  psmep->macro.macro = parse_alloc_obj_m(parser, ast_Macro);
  ast_certain_parseMacro(psmep->macro.macro, diagnostics, parser);
  psmep->common.span = psmep->macro.macro->span;
}

static void ast_parsePatStructMember(ast_PatStructMember *psmep,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);
  Token t = parse_peek(parser, diagnostics, 1);
  switch (t.kind) {
  case tk_Pat: {
    ast_certain_parseFieldPatStructMember(psmep, diagnostics, parser);
    break;
  }
  case tk_Macro: {
    ast_certain_parseMacroPatStructMember(psmep, diagnostics, parser);
    break;
  }
  default: {
    psmep->kind = ast_PSMK_None;
    psmep->common.span = t.span;
    parse_next(parser, diagnostics);
    *dlogger_append(diagnostics) = (Diagnostic){
        .span = t.span,
        .severity = DSK_Error,
        .message = com_str_lit_m("within a pattern struct, expected a "
                                 "pattern struct member or macro"),
        .children_len = 0};
  }
  }
  psmep->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  psmep->common.metadata = com_vec_release(&metadata);
}

static void ast_certain_parseStructPat(ast_Pat *spe,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  com_mem_zero_obj_m(spe);
  spe->kind = ast_PK_Struct;

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Struct, "expected tk_Struct");

  com_loc_LnCol start = t.span.start;
  com_loc_LnCol end;

  com_vec members = parse_alloc_vec(parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceLeft) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_"
                                              "PatStructExpectedLeftB"
                                              "race"),
                     .children_len = 0};
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&members,                         // members_vec_ptr
             diagnostics,                      // dlogger_ptr
             ast_parsePatStructMember,         // member_parse_function
             ast_PatStructMember,              // member_kind
             tk_BraceRight,                    // delimiting_token_kind
             "DK_PatStructExpectedRightBrace", // missing_delimiter_error
             end,                              // end_lncol
             parser                            // parser
  )
CLEANUP:
  spe->structExpr.members_len = com_vec_len_m(&members, ast_PatStructMember);
  spe->structExpr.members = com_vec_release(&members);
  spe->common.span = com_loc_span_m(start, end);
  return;
}

static void ast_certain_parseGroupPat(ast_Pat *gpep,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
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

static void ast_parseL1Pat(ast_Pat *l1, DiagnosticLogger *diagnostics,
                           AstConstructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  Token t = parse_peek(parser, diagnostics, 1);
  switch (t.kind) {
  case tk_BraceLeft: {
    ast_certain_parseGroupPat(l1, diagnostics, parser);
    break;
  }
  case tk_Struct: {
    ast_certain_parseStructPat(l1, diagnostics, parser);
    break;
  }
  case tk_Identifier:
  case tk_Colon: {
    ast_certain_parseTypeRestrictionPat(l1, diagnostics, parser);
    break;
  }
  case tk_CompEqual:
  case tk_CompNotEqual:
  case tk_CompGreaterEqual:
  case tk_CompGreater:
  case tk_CompLess:
  case tk_CompLessEqual: {
    ast_certain_parseValRestrictionPat(l1, diagnostics, parser);
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
                           AstConstructor *parser) {
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
  case tk_Intersection: {
    *val = ast_PEBOK_Intersection;
    return true;
  }
  case tk_Union: {
    *val = ast_PEBOK_Union;
    return true;
  }
  default: {
    // there is no level 3
    // expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Pat, ast_PK, 3, ast_parseL2Pat)

static bool ast_opDetL4Pat(tk_Kind tk, ast_PatBinaryOpKind *val) {
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

FN_BINOP_PARSE_LX_EXPR(Pat, ast_PK, 4, ast_parseL3Pat)

static bool ast_opDetL5Pat(tk_Kind tk, ast_PatBinaryOpKind *val) {
  switch (tk) {
  case tk_And: {
    *val = ast_PEBOK_And;
    return true;
  }
  case tk_Or: {
    *val = ast_PEBOK_Or;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Pat, ast_PK, 5, ast_parseL4Pat)

static void ast_parsePat(ast_Pat *ppe, DiagnosticLogger *diagnostics,
                         AstConstructor *parser) {
  ast_parseL5Pat(ppe, diagnostics, parser);
}

static void ast_certain_parseValDecl(ast_Stmnt *vdsp,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {
  // zero-initialize vdsp
  com_mem_zero_obj_m(vdsp);

  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Val, "expected tk_Val");
  com_loc_LnCol start = t.span.start;

  // Get Binding
  ast_Pat *pat = parse_alloc_obj_m(parser, ast_Pat);
  ast_parsePat(pat, diagnostics, parser);

  com_loc_LnCol end;

  // Expect define
  t = parse_peek(parser, diagnostics, 1);
  if (t.kind == tk_Define) {
    // set components
    vdsp->kind = ast_SK_ValDeclDefine;
    vdsp->valDeclDefine.pat = pat;
    // accept the define
    // token
    parse_next(parser, diagnostics);

    vdsp->valDeclDefine.val = parse_alloc_obj_m(parser, ast_Val);
    ast_parseVal(vdsp->valDeclDefine.val, diagnostics, parser);
    end = vdsp->valDeclDefine.val->common.span.end;
  } else {
    // set components
    vdsp->kind = ast_SK_ValDecl;
    vdsp->valDecl.pat = pat;
    end = vdsp->valDecl.pat->common.span.end;
  }

  vdsp->common.span = com_loc_span_m(start, end);
}

static void ast_certain_parseTypeDecl(ast_Stmnt *tdp,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  com_mem_zero_obj_m(tdp);
  tdp->kind = ast_SK_TypeDecl;
  Token t = parse_next(parser, diagnostics);
  // enforce that next token
  // is type
  com_assert_m(t.kind == tk_Type, "expected tk_Type");
  com_loc_LnCol start = t.span.start;

  com_loc_LnCol end;

  // parse binding
  tdp->typeDecl.name = parse_alloc_obj_m(parser, ast_Binding);
  ast_parseBinding(tdp->typeDecl.name, diagnostics, parser);

  // Now get define
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Define) {
    *dlogger_append(diagnostics) =
        (Diagnostic){.span = t.span,
                     .severity = DSK_Error,
                     .message = com_str_lit_m("DK_TypeDeclExpectedDefine"),
                     .children_len = 0};
    end = t.span.end;
    goto CLEANUP;
  }

  // get type
  tdp->typeDecl.type = parse_alloc_obj_m(parser, ast_Type);
  ast_parseType(tdp->typeDecl.type, diagnostics, parser);
  end = tdp->typeDecl.type->common.span.end;

CLEANUP:
  tdp->common.span = com_loc_span_m(start, end);
  return;
}

static void ast_certain_parseDeferStmnt(ast_Stmnt *dsp,
                                        DiagnosticLogger *diagnostics,
                                        AstConstructor *parser) {
  dsp->kind = ast_SK_DeferStmnt;
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Defer, "expected tk_Defer");

  // label
  dsp->deferStmnt.label = parse_alloc_obj_m(parser, ast_LabelReference);
  ast_parseLabelReference(dsp->deferStmnt.label, diagnostics, parser);

  // value
  dsp->deferStmnt.val = parse_alloc_obj_m(parser, ast_Val);
  ast_parseVal(dsp->deferStmnt.val, diagnostics, parser);

  // span
  dsp->common.span =
      com_loc_span_m(t.span.start, dsp->deferStmnt.val->common.span.end);
  return;
}

static void ast_certain_parseMacroStmnt(ast_Stmnt *msp,
                                        DiagnosticLogger *diagnostics,
                                        AstConstructor *parser) {
  com_mem_zero_obj_m(msp);
  msp->kind = ast_SK_Macro;
  msp->macro.macro = parse_alloc_obj_m(parser, ast_Macro);
  ast_certain_parseMacro(msp->macro.macro, diagnostics, parser);
  msp->common.span = msp->macro.macro->span;
}

static void ast_certain_parseModStmnt(ast_Stmnt *nsp,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  com_mem_zero_obj_m(nsp);
  nsp->kind = ast_SK_Mod;
  Token t = parse_next(parser, diagnostics);
  com_assert_m(t.kind == tk_Mod, "expected tk_Mod");
  com_loc_LnCol start = t.span.start;
  com_loc_LnCol end;

  // Create list of
  // statements
  com_vec statements = parse_alloc_vec(parser);

  nsp->modStmnt.name = parse_alloc_obj_m(parser, ast_ModBinding) ;
  ast_parseModBinding(nsp->modStmnt.name , diagnostics, parser) ;

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
  nsp->modStmnt.stmnts_len = com_vec_len_m(&statements, ast_Stmnt);
  nsp->modStmnt.stmnts = com_vec_release(&statements);
  nsp->common.span = com_loc_span_m(start, end);
}

static void ast_certain_parseUseStmnt(ast_Stmnt *usp,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  com_mem_zero_obj_m(usp);
  usp->kind = ast_SK_Use;
  Token t = parse_next(parser,
                       diagnostics); // drop use token
  com_assert_m(t.kind == tk_Use, "expected tk_Use");
  com_loc_LnCol start = t.span.start;

  // parse path
  usp->useStmnt.path = parse_alloc_obj_m(parser, ast_ModReference);
  ast_parseModReference(usp->useStmnt.path, diagnostics, parser);

  usp->common.span = com_loc_span_m(start, usp->useStmnt.path->span.end);
}

void ast_parseStmnt(ast_Stmnt *sp, DiagnosticLogger *diagnostics,
                           AstConstructor *parser) {
  com_vec metadata = parse_getMetadata(parser, diagnostics);

  // peek next token
  Token t = parse_peek(parser, diagnostics, 1);
  switch (t.kind) {
    // Macros
  case tk_Macro: {
    ast_certain_parseMacroStmnt(sp, diagnostics, parser);
    break;
  }
  case tk_Use: {
    ast_certain_parseUseStmnt(sp, diagnostics, parser);
    break;
  }
  case tk_Mod: {
    ast_certain_parseModStmnt(sp, diagnostics, parser);
    break;
  }
  case tk_Val: {
    ast_certain_parseValDecl(sp, diagnostics, parser);
    break;
  }
  case tk_Type: {
    ast_certain_parseTypeDecl(sp, diagnostics, parser);
    break;
  }
  case tk_Defer: {
    ast_certain_parseDeferStmnt(sp, diagnostics, parser);
    break;
  }
  // essions
  default: {
    sp->kind = ast_SK_Val;
    sp->val.val = parse_alloc_obj_m(parser, ast_Val);
    ast_parseVal(sp->val.val, diagnostics, parser);
    sp->common.span = sp->val.val->common.span;
    break;
  }
  }
  sp->common.metadata_len = com_vec_len_m(&metadata, ast_Metadata);
  sp->common.metadata = com_vec_release(&metadata);
}

bool ast_eof(AstConstructor *parser, DiagnosticLogger*d)
{
  return parse_peek(parser,  d, 1).kind == tk_Eof;
}
