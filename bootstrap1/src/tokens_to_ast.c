#include "tokens_to_ast.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "code_to_tokens.h"
#include "constants.h"
#include "queue.h"
#include "std_allocator.h"
#include "token.h"
#include "vector.h"

// convoluted function to save repetitive tasks
#define PARSE_LIST(members_vec_ptr, dlogger_ptr, member_parse_function,        \
                   member_kind, delimiting_token_kind,                         \
                   missing_delimiter_error_msg, end_lncol, parser)             \
                                                                               \
  while (true) {                                                               \
    Token pl_ntk = parse_peek(parser, diagnostics); /* next token kind */      \
    if (pl_ntk.kind == delimiting_token_kind) {                                \
      end_lncol = pl_ntk.span.end;                                             \
      parse_next(parser, dlogger_ptr); /* accept delimiting tk */              \
      break;                                                                   \
    } else if (pl_ntk.kind == tk_Eof) {                                        \
      *dlogger_append(dlogger_ptr) = diagnostic_standalone(                    \
          pl_ntk.span, DSK_Error, (missing_delimiter_error_msg));              \
      end_lncol = pl_ntk.span.end;                                             \
      break;                                                                   \
    }                                                                          \
    /* if there wasn't an end delimiter, push the last token back */           \
    member_parse_function(VEC_PUSH(members_vec_ptr, member_kind), diagnostics, \
                          parser);                                             \
  }

// AstConstructor
AstConstructor ast_create(Lexer *lp, Allocator *a) {
  return (AstConstructor){
      .a = a,      // Allocator
      .lexer = lp, // Lexer Pointer
      .next_tokens_queue =
          queue_create(vec_create(a)), // Queue of peeked tokens
  };
}

/// gets the next token, ignoring buffering
static Token parse_rawNext(AstConstructor *parser,
                           DiagnosticLogger *diagnostics) {
  return tk_next(parser->lexer, diagnostics, parser->a);
}

// If the peeked token stack is not empty:
//    Return the first element of the top of the token
//    Pop the first element of the next_comments_stack
//    For each element in the next comments stack, push it to the top of the
//    current scope
// Else fetch next raw token
static Token parse_next(AstConstructor *pp, DiagnosticLogger *diagnostics) {

  // the current scope we aim to push the comments to
  if (QUEUE_LEN(&pp->next_tokens_queue, Token) > 0) {
    // set the token
    Token ret;
    QUEUE_POP(&pp->next_tokens_queue, &ret, Token);
    return ret;
  } else {
    return parse_rawNext(pp, diagnostics);
  }
}

// gets the k'th token
// K must be greater than 0
static Token parse_peekNth(AstConstructor *pp, size_t k,
                           DiagnosticLogger *diagnostics) {
  assert(k > 0);

  for (size_t i = QUEUE_LEN(&pp->next_tokens_queue, Token); i < k; i++) {
    // parse the token and add it to the top of the stack
    *QUEUE_PUSH(&pp->next_tokens_queue, Token) = parse_rawNext(pp, diagnostics);
  }

  // return the most recent token added
  return *QUEUE_GET(&pp->next_tokens_queue,
                    QUEUE_LEN(&pp->next_tokens_queue, Token) - k, Token);
}

static Token parse_peek(AstConstructor *parser, DiagnosticLogger *diagnostics) {
  return parse_peekNth(parser, 1, diagnostics);
}

void ast_destroy(AstConstructor *pp) { queue_destroy(&pp->next_tokens_queue); }

// returns a vector containing all the comments encountered here
static Vector parse_getComments(AstConstructor *parser,
                                DiagnosticLogger *diagnostics) {
  Vector comments = vec_create(parser->a);
  while (parse_peek(parser, diagnostics).kind == tk_Comment) {
    Token c = parse_next(parser, diagnostics);
    *VEC_PUSH(&comments, ast_Comment) =
        (ast_Comment){.span = c.span,
                      .scope = c.commentToken.scope,
                      .data = c.commentToken.comment};
  }
  return comments;
}

// returns the first noncomment token
static Token parse_peekPastComments(AstConstructor *parser,
                                    DiagnosticLogger *diagnostics) {
  uint64_t n = 1;
  while (parse_peekNth(parser, n, diagnostics).kind == tk_Comment) {
    n++;
  }
  return parse_peekNth(parser, n, diagnostics);
}

// Note that all errors resync at the statement level
static void ast_parseStmnt(ast_Stmnt *stmnt, DiagnosticLogger *diagnostics,
                           AstConstructor *parser);
static void ast_parseVal(ast_Val *ptr, DiagnosticLogger *diagnostics,
                         AstConstructor *parser);
static void ast_parseType(ast_Type *tep, DiagnosticLogger *diagnostics,
                          AstConstructor *parser);
static void ast_parsePat(ast_Pat *pp, DiagnosticLogger *diagnostics,
                         AstConstructor *parser);

static void ast_certain_parseMacro(ast_Macro *mpe,
                                   DiagnosticLogger *diagnostics,
                                   AstConstructor *parser) {
  ZERO(mpe);

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Macro);

  LnCol start = t.span.start;
  LnCol end;

  mpe->name = t.macroToken.data;

  Vector tokens = vec_create(parser->a);

  uint64_t depth = 1;
  while (true) {
    t = parse_next(parser, diagnostics);
    if (t.kind == tk_Eof) {
      *dlogger_append(diagnostics) = diagnostic_standalone(
          t.span, DSK_Error, "macro expected a closing backtick");
      break;
    } else {
      if (t.kind == tk_Macro) {
        depth++;
      } else if (t.kind == tk_Backtick) {
        depth--;
      }
      *VEC_PUSH(&tokens, Token) = t;
      if (depth == 0) {
        break;
      }
    }
  }
  end = t.span.end;

  mpe->tokens_len = VEC_LEN(&tokens, ast_Stmnt);
  mpe->tokens = vec_release(&tokens);

  mpe->span = SPAN(start, end);
}

static void ast_parseReference(ast_Reference *ptr,
                               DiagnosticLogger *diagnostics,
                               AstConstructor *parser) {
  ZERO(ptr);

  Token t = parse_next(parser, diagnostics);

  // start and finish
  LnCol start = t.span.start;
  LnCol end;

  Vector pathSegments = vec_create(parser->a);

  if (t.kind == tk_Identifier) {
    *VEC_PUSH(&pathSegments, char *) = t.identifierToken.data;
    ptr->kind = ast_RK_Path;
  } else {
    ptr->kind = ast_RK_None;
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error, "reference expected identifier");
    end = t.span.end;
    goto CLEANUP;
  }

  while (true) {
    t = parse_peek(parser, diagnostics);
    if (t.kind == tk_ScopeResolution) {
      // discard the scope resolution
      parse_next(parser, diagnostics);
      // now check if we have an issue
      t = parse_next(parser, diagnostics);
      if (t.kind != tk_Identifier) {
        *dlogger_append(diagnostics) = diagnostic_standalone(
            t.span, DSK_Error, "reference expected identifier");
        end = t.span.end;
        goto CLEANUP;
      }
      *VEC_PUSH(&pathSegments, char *) = t.identifierToken.data;
    } else {
      // we've reached the end of the path
      end = t.span.end;
      break;
    }
  }

CLEANUP:
  ptr->path.segments_len = VEC_LEN(&pathSegments, char *);
  ptr->path.segments = vec_release(&pathSegments);

  ptr->span = SPAN(start, end);
}

static void ast_parseBinding(ast_Binding *ptr, DiagnosticLogger *diagnostics,
                             AstConstructor *parser) {
  ZERO(ptr);
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
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error,
        "binding expected either `_` for drop or an identifier to bind to");
    ptr->kind = ast_BK_None;
    break;
  }
  }
}

static void ast_parseField(ast_Field *ptr, DiagnosticLogger *diagnostics,
                           AstConstructor *parser) {
  ZERO(ptr);
  Token t = parse_next(parser, diagnostics);
  ptr->span = t.span;
  switch (t.kind) {
  case tk_Identifier: {
    ptr->kind = ast_FK_Field;
    ptr->field.name = t.identifierToken.data;
    break;
  }
  default: {
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error, "field expected an identifier");
    ptr->kind = ast_FK_None;
    break;
  }
  }
}

static void ast_certain_parseNilVal(ast_Val *ptr, DiagnosticLogger *diagnostics,
                                    AstConstructor *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Nil);
  ptr->kind = ast_VK_NilLiteral;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseIntVal(ast_Val *ptr, DiagnosticLogger *diagnostics,
                                    AstConstructor *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Int);
  ptr->kind = ast_VK_IntLiteral;
  ptr->intLiteral.value = t.intToken.data;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseBoolVal(ast_Val *ptr,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Bool);
  ptr->kind = ast_VK_BoolLiteral;
  ptr->boolLiteral.value = t.boolToken.data;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseFloatVal(ast_Val *ptr,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Float);
  ptr->kind = ast_VK_FloatLiteral;
  ptr->floatLiteral.value = t.floatToken.data;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseCharVal(ast_Val *ptr,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Char);
  ptr->kind = ast_VK_CharLiteral;
  ptr->charLiteral.value = t.charToken.data;
  ptr->common.span = t.span;
  return;
}

static void ast_certain_parseStringVal(ast_Val *sptr,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_String);
  sptr->kind = ast_VK_StringLiteral;
  sptr->stringLiteral.value = t.stringToken.data;
  sptr->stringLiteral.value_len = t.stringToken.data_len;
  sptr->common.span = t.span;
  return;
}

static void ast_certain_parseFnVal(ast_Val *fptr, DiagnosticLogger *diagnostics,
                                   AstConstructor *parser) {
  ZERO(fptr);

  fptr->kind = ast_VK_Fn;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Fn);
  LnCol start = t.span.start;
  LnCol end = t.span.end;

  // check for leftparen
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_ParenLeft) {
    *dlogger_append(diagnostics) =
        diagnostic_standalone(t.span, DSK_Error, "fn expected left paren");
    goto CLEANUP;
  }

  Span lparenspan = t.span;

  Vector parameters = vec_create(parser->a);

  PARSE_LIST(&parameters,                  // members_vec_ptr
             diagnostics,                  // dlogger_ptr
             ast_parsePat,                 // member_parse_function
             ast_Pat,                      // member_kind
             tk_ParenRight,                // delimiting_token_kind
             "DK_FnValExpectedRightParen", // missing_delimiter_error
             end,                          // end_lncol
             parser                        // parser
  )

  fptr->fn.parameters_len = VEC_LEN(&parameters, ast_Pat);
  fptr->fn.parameters = vec_release(&parameters);

  t = parse_peek(parser, diagnostics);
  if (t.kind == tk_Colon) {
    fptr->fn.type = ALLOC(parser->a, ast_Type);
    // advance
    parse_next(parser, diagnostics);

    ast_parseType(fptr->fn.type, diagnostics, parser);
  } else {
    fptr->fn.type = ALLOC(parser->a, ast_Type);
    *fptr->fn.type =
        (ast_Type){.common = {.span = lparenspan, .comments_len = 0},
                   .kind = ast_TK_Omitted};
  }

  t = parse_next(parser, diagnostics);

  if (t.kind != tk_Arrow) {
    *dlogger_append(diagnostics) =
        diagnostic_standalone(t.span, DSK_Error, "DK_FnValExpectedArrow");
    end = t.span.end;
    goto CLEANUP;
  }

  fptr->fn.body = ALLOC(parser->a, ast_Val);
  ast_parseVal(fptr->fn.body, diagnostics, parser);
  end = fptr->fn.body->common.span.end;

CLEANUP:
  fptr->common.span = SPAN(start, end);
}

static void ast_certain_parseLabelLabelBinding(ast_LabelBinding *lp,
                                               DiagnosticLogger *diagnostics,
                                               AstConstructor *parser) {
  ZERO(lp);
  lp->kind = ast_LBK_Label;
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Label);
  lp->span = t.span;
  lp->label.label = t.labelToken.data;
}

static void ast_certain_parseBlockVal(ast_Val *bptr,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  ZERO(bptr);
  bptr->kind = ast_VK_Block;

  // Parse leftbrace
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_BraceLeft);
  Span lbracespan = t.span;

  t = parse_peek(parser, diagnostics);
  // blocks may be labeled
  bptr->block.label = ALLOC(parser->a, ast_LabelBinding);
  if (t.kind == tk_Label) {
    ast_certain_parseLabelLabelBinding(bptr->block.label, diagnostics, parser);
  } else {
    *bptr->block.label =
        (ast_LabelBinding){.span = lbracespan, .kind = ast_LBK_Omitted};
  }

  // Create list of statements
  Vector statements = vec_create(parser->a);

  LnCol end;

  PARSE_LIST(&statements,                  // members_vec_ptr
             diagnostics,                  // dlogger_ptr
             ast_parseStmnt,               // member_parse_function
             ast_Stmnt,                    // member_kind
             tk_BraceRight,                // delimiting_token_kind
             "DK_BlockExpectedRightBrace", // missing_delimiter_error
             end,                          // end_lncol
             parser                        // parser
  )

  bptr->block.stmnts_len = VEC_LEN(&statements, ast_Stmnt);
  bptr->block.stmnts = vec_release(&statements);
  bptr->common.span = SPAN(lbracespan.start, end);
  return;
}

static void ast_parseLabelReference(ast_LabelReference *ptr,
                                    DiagnosticLogger *diagnostics,
                                    AstConstructor *parser) {
  ZERO(ptr);
  Token t = parse_next(parser, diagnostics);
  ptr->span = t.span;
  if (t.kind == tk_Label) {
    ptr->kind = ast_LRK_Label;
    ptr->label.label = t.labelToken.data;
  } else {
    ptr->kind = ast_LRK_None;
    *dlogger_append(diagnostics) =
        diagnostic_standalone(t.span, DSK_Error, "Expected label");
  }
}

static void ast_certain_parseReturnVal(ast_Val *rep,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  ZERO(rep);
  rep->kind = ast_VK_Return;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Return);
  Span retspan = t.span;

  LnCol start = t.span.start;
  LnCol end;

  parse_next(parser, diagnostics);

  // return's scope
  rep->returnExpr.label = ALLOC(parser->a, ast_LabelReference);
  ast_parseLabelReference(rep->returnExpr.label, diagnostics, parser);

  // value to return
  rep->returnExpr.value = ALLOC(parser->a, ast_Val);
  ast_parseVal(rep->returnExpr.value, diagnostics, parser);

  // span
  end = rep->returnExpr.value->common.span.end;
  rep->common.span = SPAN(start, end);
  return;
}

static void ast_certain_parseLoopVal(ast_Val *lep,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {
  lep->kind = ast_VK_Loop;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Loop);
  LnCol start = t.span.start;
  Span loopspan = t.span;

  t = parse_peek(parser, diagnostics);
  lep->loop.label = ALLOC(parser->a, ast_LabelBinding);
  if (t.kind == tk_Label) {
    ast_certain_parseLabelLabelBinding(lep->loop.label, diagnostics, parser);
  } else {
    *lep->loop.label =
        (ast_LabelBinding){.span = loopspan, .kind = ast_LBK_Omitted};
  }

  lep->loop.body = ALLOC(parser->a, ast_Val);
  ast_parseVal(lep->loop.body, diagnostics, parser);
  lep->common.span = SPAN(start, lep->loop.body->common.span.end);
  return;
}

static void parseReferenceVal(ast_Val *rptr, DiagnosticLogger *diagnostics,
                              AstConstructor *parser) {
  ZERO(rptr);
  rptr->kind = ast_VK_Reference;
  rptr->reference.path = ALLOC(parser->a, ast_Reference);
  ast_parseReference(rptr->reference.path, diagnostics, parser);
  rptr->common.span = rptr->reference.path->span;
  return;
}

static void ast_certain_parseMacroValStructMember(ast_ValStructMember *vsemp,
                                                  DiagnosticLogger *diagnostics,
                                                  AstConstructor *parser) {
  ZERO(vsemp);
  vsemp->kind = ast_VSMK_Macro;
  vsemp->macro.macro = ALLOC(parser->a, ast_Macro);
  ast_certain_parseMacro(vsemp->macro.macro, diagnostics, parser);
  vsemp->common.span = vsemp->macro.macro->span;
}

// field := Value
static void
ast_certain_parseMemberValStructMember(ast_ValStructMember *vsmep,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  // zero-initialize bp
  ZERO(vsmep);
  vsmep->kind = ast_VSMK_Member;

  LnCol end;

  vsmep->member.field = ALLOC(parser->a, ast_Field);
  ast_parseField(vsmep->member.field, diagnostics, parser);

  // check if define
  Token t = parse_next(parser, diagnostics);
  if (t.kind == tk_Define) {
    // Get value of variable
    vsmep->member.val = ALLOC(parser->a, ast_Val);
    ast_parseVal(vsmep->member.val, diagnostics, parser);
    end = vsmep->member.val->common.span.end;
  } else {
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error, "DK_StructMemberLiteralExpectedDefine");
    vsmep->member.val = NULL;
    end = t.span.end;
    goto CLEANUP;
  }

CLEANUP:
  vsmep->common.span = SPAN(vsmep->member.field->span.start, end);
  return;
}

static void ast_parseValStructMember(ast_ValStructMember *vsmep,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {
  Vector comments = parse_getComments(parser, diagnostics);
  Token t = parse_peek(parser, diagnostics);
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
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error, "DK_StructLiteralExpectedEntry");

    vsmep->kind = ast_VSMK_None;
    vsmep->common.span = t.span;
    // discard token
    parse_next(parser, diagnostics);
    break;
  }
  }
  vsmep->common.comments_len = VEC_LEN(&comments, ast_Comment);
  vsmep->common.comments = vec_release(&comments);
}

static void ast_certain_parseValStruct(ast_Val *sve,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  ZERO(sve);
  sve->kind = ast_VK_StructLiteral;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Struct);

  LnCol start = t.span.start;
  LnCol end;

  Vector members = vec_create(parser->a);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceLeft) {
    end = t.span.end;
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error, "DK_StructLiteralExpectedLeftBrace");
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
  sve->structExpr.members_len = VEC_LEN(&members, ast_ValStructMember);
  sve->structExpr.members = vec_release(&members);
  sve->common.span = SPAN(start, end);
  return;
}

static void ast_certain_parseMacroVal(ast_Val *ptr,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  ZERO(ptr);
  ptr->kind = ast_VK_Macro;
  ptr->macro.macro = ALLOC(parser->a, ast_Macro);
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

  // value comments;
  Vector comments = parse_getComments(parser, diagnostics);

  Token t = parse_peek(parser, diagnostics);
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
  case tk_Return: {
    ast_certain_parseReturnVal(l1, diagnostics, parser);
    break;
  }
  case tk_Loop: {
    ast_certain_parseLoopVal(l1, diagnostics, parser);
    break;
  }
  case tk_Identifier: {
    parseReferenceVal(l1, diagnostics, parser);
    break;
  }
  default: {
    l1->kind = ast_VK_None;
    l1->common.span = t.span;
    parse_next(parser, diagnostics);
    *dlogger_append(diagnostics) =
        diagnostic_standalone(t.span, DSK_Error, "DK_UnexpectedToken");
  }
  }
  l1->common.comments_len = VEC_LEN(&comments, ast_Comment);
  l1->common.comments = vec_release(&comments);
}

static voidast_certain_postfix_parseFieldAcessVal(ast_Val *fave,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser, ast_Val *root) {
  ZERO(fave);
  fave->kind = ast_VK_FieldAccess;
  fave->fieldAccess.root = root;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_FieldAccess);

  fave->fieldAccess.field = ALLOC(parser->a, ast_Field);
  ast_parseField(fave->fieldAccess.field, diagnostics, parser);
  fave->common.span =
      SPAN(root->common.span.start, fave->fieldAccess.field->span.end);
}

static void ast_certain_postfix_parseCallVal(ast_Val *cptr,
                                             DiagnosticLogger *diagnostics,
                                             AstConstructor *parser,
                                             ast_Val *root) {
  ZERO(cptr);

  cptr->kind = ast_VK_Call;
  cptr->call.function = root;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_ParenLeft);

  LnCol end;

  Vector parameters = vec_create(parser->a);

  PARSE_LIST(&parameters,            // members_vec_ptr
             diagnostics,            // dlogger_ptr
             ast_parseVal,           // member_parse_function
             ast_Val,                // member_kind
             tk_ParenRight,          // delimiting_token_kind
             "DK_CallExpectedParen", // missing_delimiter_error
             end,                    // end_lncol
             parser                  // parser
  )

  cptr->call.parameters_len = VEC_LEN(&parameters, ast_Val);
  cptr->call.parameters = vec_release(&parameters);

  cptr->common.span = SPAN(root->common.span.start, end);
}

static void ast_certain_postfix_parseAsVal(ast_Val *aptr,
                                           DiagnosticLogger *diagnostics,
                                           AstConstructor *parser,
                                           ast_Val *root) {
  ZERO(aptr);
  aptr->kind = ast_VK_As;
  aptr->as.root = root;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_As);

  aptr->as.type = ALLOC(parser->a, ast_Type);
  ast_parseType(aptr->as.type, diagnostics, parser);
  aptr->common.span =
      SPAN(root->common.span.start, aptr->as.type->common.span.end);
}

// pat Pattern => ,
static void ast_certain_ParsePatMatchCase(ast_MatchCase *mcep,
                                          DiagnosticLogger *diagnostics,
                                          AstConstructor *parser) {
  ZERO(mcep);
  mcep->kind = ast_MCK_Case;

  // Get Pat
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Pat);
  LnCol start = t.span.start;

  // Get pattern
  mcep->matchCase.pattern = ALLOC(parser->a, ast_Pat);
  ast_parsePat(mcep->matchCase.pattern, diagnostics, parser);

  LnCol end = mcep->matchCase.pattern->common.span.end;

  // Expect colon
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Arrow) {
    *dlogger_append(diagnostics) =
        diagnostic_standalone(t.span, DSK_Error, "DK_MatchCaseNoArrow");
    goto CLEANUP;
  }

  // Get Value
  mcep->matchCase.val = ALLOC(parser->a, ast_Val);
  ast_parseVal(mcep->matchCase.val, diagnostics, parser);
  end = mcep->matchCase.val->common.span.end;

CLEANUP:
  mcep->common.span = SPAN(start, end);
  return;
}

static void ast_certain_parseMacroMatchCase(ast_MatchCase *mcep,
                                            DiagnosticLogger *diagnostics,
                                            AstConstructor *parser) {
  ZERO(mcep);
  mcep->kind = ast_MCK_Macro;
  mcep->macro.macro = ALLOC(parser->a, ast_Macro);
  ast_certain_parseMacro(mcep->macro.macro, diagnostics, parser);
  mcep->common.span = mcep->macro.macro->span;
}

static void ast_parseMatchCase(ast_MatchCase *mcep,
                               DiagnosticLogger *diagnostics,
                               AstConstructor *parser) {
  Vector comments = parse_getComments(parser, diagnostics);
  Token t = parse_peek(parser, diagnostics);
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
        diagnostic_standalone(t.span, DSK_Error, "DK_MatchCaseNoPat");

    mcep->kind = ast_MCK_None;
    mcep->common.span = t.span;
    // discard token
    parse_next(parser, diagnostics);
    break;
  }
  }
  mcep->common.comments_len = VEC_LEN(&comments, ast_Comment);
  mcep->common.comments = vec_release(&comments);
}

static void ast_certain_postfix_parseMatchVal(ast_Val *mptr,
                                              DiagnosticLogger *diagnostics,
                                              AstConstructor *parser,
                                              ast_Val *root) {
  ZERO(mptr);
  mptr->kind = ast_VK_Match;
  mptr->match.root = root;

  // guarantee token exists
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Match);

  // now we must parse the block containing the cases
  Vector cases = vec_create(parser->a);

  LnCol end;

  // Expect beginning brace
  t = parse_next(parser, diagnostics);

  if (t.kind != tk_BraceLeft) {
    *dlogger_append(diagnostics) =
        diagnostic_standalone(t.span, DSK_Error, "DK_MatchNoLeftBrace");
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
  mptr->match.cases_len = VEC_LEN(&cases, ast_MatchCase);
  mptr->match.cases = vec_release(&cases);

  mptr->common.span = SPAN(root->common.span.start, end);
  return;
}

static void parseL2Val(ast_Val *l2, DiagnosticLogger *diagnostics,
                       AstConstructor *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff

  ast_Val *root = l2;
  parseL1Val(root, diagnostics, parser);

  while (true) {
    // represents the old operation
    ast_Val *v;

    Token t = parse_peekPastComments(parser, diagnostics);
    switch (t.kind) {
    case tk_Ref: {
      // get comments
      Vector comments = parse_getComments(parser, diagnostics);
      // allocate space for operation
      v = ALLOC(parser->a, ast_Val);
      *v = *root;
      root->kind = ast_VK_UnaryOp;
      root->unaryOp.op = ast_VEUOK_Ref;
      root->unaryOp.operand = v;
      root->common.span = SPAN(v->common.span.start, t.span.end);
      root->common.comments_len = VEC_LEN(&comments, ast_Comment);
      root->common.comments = vec_release(&comments);
      parse_next(parser, diagnostics);
      break;
    }
    case tk_Deref: {
      Vector comments = parse_getComments(parser, diagnostics);
      v = ALLOC(parser->a, ast_Val);
      *v = *root;
      root->kind = ast_VK_UnaryOp;
      root->unaryOp.op = ast_VEUOK_Deref;
      root->unaryOp.operand = v;
      root->common.span = SPAN(v->common.span.start, t.span.end);
      root->common.comments_len = VEC_LEN(&comments, ast_Comment);
      root->common.comments = vec_release(&comments);
      parse_next(parser, diagnostics);
      break;
    }
    case tk_FieldAccess: {
      Vector comments = parse_getComments(parser, diagnostics);
      v = ALLOC(parser->a, ast_Val);
      *v = *root;
      ast_certain_postfix_parseFieldAcessVal(root, diagnostics, parser, v);
      root->common.comments_len = VEC_LEN(&comments, ast_Comment);
      root->common.comments = vec_release(&comments);
      break;
    }
    case tk_ParenLeft: {
      Vector comments = parse_getComments(parser, diagnostics);
      v = ALLOC(parser->a, ast_Val);
      *v = *root;
      ast_certain_postfix_parseCallVal(root, diagnostics, parser, v);
      root->common.comments_len = VEC_LEN(&comments, ast_Comment);
      root->common.comments = vec_release(&comments);
      break;
    }
    case tk_As: {
      Vector comments = parse_getComments(parser, diagnostics);
      v = ALLOC(parser->a, ast_Val);
      *v = *root;
      ast_certain_postfix_parseAsVal(root, diagnostics, parser, v);
      root->common.comments_len = VEC_LEN(&comments, ast_Comment);
      root->common.comments = vec_release(&comments);
      break;
    }
    case tk_Match: {
      Vector comments = parse_getComments(parser, diagnostics);
      v = ALLOC(parser->a, ast_Val);
      *v = *root;
      ast_certain_postfix_parseMatchVal(root, diagnostics, parser, v);
      root->common.comments_len = VEC_LEN(&comments, ast_Comment);
      root->common.comments = vec_release(&comments);
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
  Token t = parse_peekPastComments(parser, diagnostics);
  switch (t.kind) {
  case tk_Negate: {
    l3->unaryOp.op = ast_VEUOK_Negate;
    break;
  }
  case tk_Posit: {
    l3->unaryOp.op = ast_VEUOK_Posit;
    break;
  }
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

  // this will only execute if an L3 operator exists
  l3->kind = ast_VK_UnaryOp;

  // first get comments
  Vector comments = parse_getComments(parser, diagnostics);
  l3->common.comments_len = VEC_LEN(&comments, ast_Comment);
  l3->common.comments = vec_release(&comments);
  // consume operator
  parse_next(parser, diagnostics);

  // Now parse the rest of the expression
  l3->unaryOp.operand = ALLOC(parser->a, ast_Val);
  ast_parseL3Val(l3->unaryOp.operand, diagnostics, parser);

  // finally calculate the misc stuff
  l3->common.span = SPAN(t.span.start, l3->unaryOp.operand->common.span.end);

  // comments
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
    Token t = parse_peekPastComments(parser, diagnostics);                     \
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
    expr->binaryOp.left_operand = ALLOC(parser->a, ast_##type);                \
    *expr->binaryOp.left_operand = v;                                          \
                                                                               \
    /* first get comments */                                                   \
    Vector comments = parse_getComments(parser, diagnostics);                  \
    expr->common.comments_len = VEC_LEN(&comments, ast_Comment);               \
    expr->common.comments = vec_release(&comments);                            \
    /* consume operator */                                                     \
    parse_next(parser, diagnostics);                                           \
                                                                               \
    /* now parse the rest of the expression */                                 \
    expr->binaryOp.right_operand = ALLOC(parser->a, ast_##type);               \
    ast_parseL##x##type(expr->binaryOp.right_operand, diagnostics, parser);    \
                                                                               \
    /* calculate misc stuff */                                                 \
    expr->common.span = SPAN(expr->binaryOp.left_operand->common.span.start,   \
                             expr->binaryOp.right_operand->common.span.end);   \
                                                                               \
    return;                                                                    \
  }

static inline bool ast_opDetL4Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
  switch (tk) {
  case tk_Pipe: {
    *val = ast_VEBOK_Pipeline;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 4, ast_parseL3Val)

// Parses a single term that will not collide with patterns
static inline void ast_parseValTerm(ast_Val *term,
                                    DiagnosticLogger *diagnostics,
                                    AstConstructor *parser) {
  ast_parseL4Val(term, diagnostics, parser);
}

static inline bool ast_opDetL5Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
  switch (tk) {
  case tk_Mul: {
    *val = ast_VEBOK_Mul;
    return true;
  }
  case tk_Div: {
    *val = ast_VEBOK_Div;
    return true;
  }
  case tk_Mod: {
    *val = ast_VEBOK_Mod;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 5, ast_parseL4Val)

static inline bool ast_opDetL6Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
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

FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 6, ast_parseL5Val)

static inline bool ast_opDetL7Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
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

FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 7, ast_parseL6Val)

static inline bool ast_opDetL8Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
  switch (tk) {
  case tk_And: {
    *val = ast_VEBOK_And;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 8, ast_parseL7Val)

static inline bool ast_opDetL9Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
  switch (tk) {
  case tk_Or: {
    *val = ast_VEBOK_Or;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 9, ast_parseL8Val)

static inline bool ast_opDetL10Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
  switch (tk) {
  case tk_Tuple: {
    *val = ast_VEBOK_Tuple;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 10, ast_parseL9Val)

static bool ast_opDetL11Val(tk_Kind tk, ast_ValBinaryOpKind *val) {
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
  case tk_AssignDiv: {
    *val = ast_VEBOK_AssignDiv;
    return true;
  }
  case tk_AssignMod: {
    *val = ast_VEBOK_AssignMod;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Val, ast_VK, 11, ast_parseL10Val)

// shim method
static void ast_parseVal(ast_Val *ptr, DiagnosticLogger *diagnostics,
                         AstConstructor *parser) {
  ast_parseL11Val(ptr, diagnostics, parser);
}

// field : Type,
static void
ast_certain_parseMemberTypeStructMember(ast_TypeStructMember *tsmep,
                                        DiagnosticLogger *diagnostics,
                                        AstConstructor *parser) {
  // zero-initialize bp
  ZERO(tsmep);
  tsmep->kind = ast_TSMK_StructMember;

  LnCol start;
  LnCol end;

  // get.identifierToken.data
  tsmep->structMember.field = ALLOC(parser->a, ast_Field);
  ast_parseField(tsmep->structMember.field, diagnostics, parser);
  start = tsmep->structMember.field->span.start;

  // check if colon
  Token t = parse_peek(parser, diagnostics);
  if (t.kind == tk_Colon) {
    // advance through colon
    parse_next(parser, diagnostics);
    // Get structMember.type of variable
    tsmep->structMember.type = ALLOC(parser->a, ast_Type);
    ast_parseType(tsmep->structMember.type, diagnostics, parser);
    end = tsmep->structMember.type->common.span.end;
  } else {
    end = tsmep->structMember.field->span.end;
    tsmep->structMember.type = ALLOC(parser->a, ast_Type);
    tsmep->structMember.type->kind = ast_TK_Omitted;
    tsmep->structMember.type->common.span = tsmep->structMember.field->span;
    tsmep->structMember.type->common.comments_len = 0;
  }

  tsmep->common.span = SPAN(start, end);
  return;
}

static void
ast_certain_parseMacroTypeStructMember(ast_TypeStructMember *tsmep,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  ZERO(tsmep);
  tsmep->kind = ast_TSMK_Macro;
  tsmep->macro.macro = ALLOC(parser->a, ast_Macro);
  ast_certain_parseMacro(tsmep->macro.macro, diagnostics, parser);
  tsmep->common.span = tsmep->macro.macro->span;
}

static void ast_parseTypeStructMember(ast_TypeStructMember *tsmep,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  Vector comments = parse_getComments(parser, diagnostics);
  Token t = parse_peek(parser, diagnostics);
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
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error, "DK_StructMemberExpectedIdentifier");
    tsmep->kind = ast_TSMK_None;
    tsmep->common.span = t.span;
    // discard token
    parse_next(parser, diagnostics);
  }
  }
  tsmep->common.comments_len = VEC_LEN(&comments, ast_Comment);
  tsmep->common.comments = vec_release(&comments);
}

static void ast_certain_parseStructType(ast_Type *ste,
                                        DiagnosticLogger *diagnostics,
                                        AstConstructor *parser) {
  ZERO(ste);
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
    assert(t.kind == tk_Struct || t.kind == tk_Enum);
  }
  }

  LnCol start = t.span.start;
  LnCol end;

  Vector members = vec_create(parser->a);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceLeft) {
    end = t.span.end;
    *dlogger_append(diagnostics) =
        diagnostic_standalone(t.span, DSK_Error, "DK_StructExpectedLeftBrace");
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
  ste->structExpr.members_len = VEC_LEN(&members, ast_TypeStructMember);
  ste->structExpr.members = vec_release(&members);
  ste->common.span = SPAN(start, end);
  return;
}

static void ast_certain_parseReferenceType(ast_Type *rtep,
                                           DiagnosticLogger *diagnostics,
                                           AstConstructor *parser) {
  ZERO(rtep);
  rtep->kind = ast_TK_Reference;
  rtep->reference.path = ALLOC(parser->a, ast_Reference);
  ast_parseReference(rtep->reference.path, diagnostics, parser);
  rtep->common.span = rtep->reference.path->span;
}

static void ast_certain_parseNilType(ast_Type *vte,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Nil);
  vte->kind = ast_TK_Nil;
  vte->common.span = t.span;
  return;
}

static void ast_certain_parseNeverType(ast_Type *vte,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Never);
  vte->kind = ast_TK_Never;
  vte->common.span = t.span;
  return;
}

static void ast_certain_parseFnType(ast_Type *fte,
                                    DiagnosticLogger *diagnostics,
                                    AstConstructor *parser) {
  ZERO(fte);

  Token t = parse_next(parser, diagnostics);

  assert(t.kind == tk_Fn);

  LnCol start = t.span.start;
  LnCol end;

  // check for leftparen
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_ParenLeft) {
    *dlogger_append(diagnostics) =
        diagnostic_standalone(t.span, DSK_Error, "DK_FnTypeExpectedLeftParen");
    end = t.span.end;
    goto CLEANUP;
  }

  Vector parameters = vec_create(parser->a);

  PARSE_LIST(&parameters,                   // members_vec_ptr
             diagnostics,                   // dlogger_ptr
             ast_parseType,                 // member_parse_function
             ast_Type,                      // member_kind
             tk_ParenRight,                 // delimiting_token_kind
             "DK_FnTypeExpectedRightParen", // missing_delimiter_error
             end,                           // end_lncol
             parser                         // parser
  )

  fte->fn.parameters_len = VEC_LEN(&parameters, ast_Type);
  fte->fn.parameters = vec_release(&parameters);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Colon) {
    *dlogger_append(diagnostics) =
        diagnostic_standalone(t.span, DSK_Error, "DK_FnTypeExpectedColon");
    end = t.span.end;
    goto CLEANUP;
  }

  fte->fn.type = ALLOC(parser->a, ast_Type);
  ast_parseType(fte->fn.type, diagnostics, parser);

CLEANUP:
  fte->common.span = SPAN(start, end);
}

static void ast_certain_parseGroupType(ast_Type *gtep,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  ZERO(gtep);
  gtep->kind = ast_TK_Group;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_BraceLeft);
  LnCol start = t.span.start;
  LnCol end;

  gtep->group.inner = ALLOC(parser->a, ast_Type);
  ast_parseType(gtep->group.inner, diagnostics, parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceRight) {
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error, "DK_TypeGroupExpectedRightBrace");
    end = t.span.end;
  } else {
    end = gtep->group.inner->common.span.end;
  }

  gtep->common.span = SPAN(start, end);
}

static void ast_certain_parseMacroType(ast_Type *tep,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  ZERO(tep);
  tep->kind = ast_TK_Macro;
  tep->macro.macro = ALLOC(parser->a, ast_Macro);
  ast_certain_parseMacro(tep->macro.macro, diagnostics, parser);
  tep->common.span = tep->macro.macro->span;
}

static void ast_parseL1Type(ast_Type *l1, DiagnosticLogger *diagnostics,
                            AstConstructor *parser) {
  Vector comments = parse_getComments(parser, diagnostics);
  Token t = parse_peek(parser, diagnostics);
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
        diagnostic_standalone(t.span, DSK_Error, "DK_TypeUnexpectedToken");
    // drop faulty token
    parse_next(parser, diagnostics);
    break;
  }
  }

  l1->common.comments_len = VEC_LEN(&comments, ast_Comment);
  l1->common.comments = vec_release(&comments);
}

static void ast_parseFieldAccessType(ast_Type *srte,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser, ast_Type *root) {
  ZERO(srte);
  srte->kind = ast_TK_FieldAccess;
  srte->fieldAccess.root = root;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_FieldAccess);

  srte->fieldAccess.field = ALLOC(parser->a, ast_Field);
  ast_parseField(srte->fieldAccess.field, diagnostics, parser);

  srte->common.span =
      SPAN(root->common.span.start, srte->fieldAccess.field->span.end);
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

    Token t = parse_peekPastComments(parser, diagnostics);
    switch (t.kind) {
    case tk_Ref: {
      // get comments
      Vector comments = parse_getComments(parser, diagnostics);
      ty = ALLOC(parser->a, ast_Type);
      *ty = *root;
      root->kind = ast_TK_UnaryOp;
      root->unaryOp.op = ast_TEUOK_Ref;
      root->unaryOp.operand = ty;
      root->common.span = SPAN(ty->common.span.start, t.span.end);
      root->common.comments_len = VEC_LEN(&comments, ast_Comment);
      root->common.comments = vec_release(&comments);
      parse_next(parser, diagnostics);
      break;
    }
    case tk_Deref: {
      // get comments
      Vector comments = parse_getComments(parser, diagnostics);
      ty = ALLOC(parser->a, ast_Type);
      *ty = *root;
      root->kind = ast_TK_UnaryOp;
      root->unaryOp.op = ast_TEUOK_Deref;
      root->unaryOp.operand = ty;
      root->common.span = SPAN(ty->common.span.start, t.span.end);
      root->common.comments_len = VEC_LEN(&comments, ast_Comment);
      root->common.comments = vec_release(&comments);
      parse_next(parser, diagnostics);
      break;
    }
    case tk_ScopeResolution: {
      // get comments
      Vector comments = parse_getComments(parser, diagnostics);
      ty = ALLOC(parser->a, ast_Type);
      *ty = *root;
      ast_parseFieldAccessType(root, diagnostics, parser, ty);
      root->common.comments_len = VEC_LEN(&comments, ast_Comment);
      root->common.comments = vec_release(&comments);
      break;
    }
    default: {
      // there are no more level2 expressions
      return;
    }
    }
  }
}

static inline bool ast_opDetL3Type(tk_Kind tk, ast_TypeBinaryOpKind *val) {
  switch (tk) {
  case tk_Tuple: {
    *val = ast_TEBOK_Tuple;
    return true;
  }
  default: {
    // there is no level 3 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Type, ast_TK, 3, ast_parseL2Type)

static inline bool ast_opDetL4Type(tk_Kind tk, ast_TypeBinaryOpKind *val) {
  switch (tk) {
  case tk_Union: {
    *val = ast_TEBOK_Union;
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
  ZERO(vrpe);

  Token t = parse_next(parser, diagnostics);
  LnCol start = t.span.start;

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
    assert(t.kind == tk_CompEqual || t.kind == tk_CompNotEqual ||
           t.kind == tk_CompGreaterEqual || t.kind == tk_CompGreater ||
           t.kind == tk_CompLess || t.kind == tk_CompLessEqual);
    abort();
  }
  }
  vrpe->valRestriction.val = ALLOC(parser->a, ast_Val);
  ast_parseValTerm(vrpe->valRestriction.val, diagnostics, parser);
  LnCol end = vrpe->valRestriction.val->common.span.end;

  vrpe->common.span = SPAN(start, end);
  return;
}

static void ast_certain_parseTypeRestrictionPat(ast_Pat *trpe,
                                                DiagnosticLogger *diagnostics,
                                                AstConstructor *parser) {
  ZERO(trpe);
  trpe->kind = ast_PK_TypeRestriction;

  // parse field
  trpe->typeRestriction.name = ALLOC(parser->a, ast_Binding);
  ast_parseBinding(trpe->typeRestriction.name, diagnostics, parser);

  Token t = parse_peek(parser, diagnostics);

  // parse type
  trpe->typeRestriction.type = ALLOC(parser->a, ast_Type);
  if (t.kind != tk_Colon) {
    trpe->typeRestriction.type->kind = ast_TK_Omitted;
    trpe->typeRestriction.type->common.span = trpe->typeRestriction.name->span;
    trpe->typeRestriction.type->common.comments_len = 0;
  } else {
    ast_parseType(trpe->typeRestriction.type, diagnostics, parser);
  }

  trpe->common.span = SPAN(trpe->typeRestriction.name->span.start,
                           trpe->typeRestriction.type->common.span.end);
}

// field '=>' pat
static void ast_certain_parseFieldPatStructMember(ast_PatStructMember *psmep,
                                                  DiagnosticLogger *diagnostics,
                                                  AstConstructor *parser) {
  ZERO(psmep);
  psmep->kind = ast_PSMK_Field;

  // parse field
  psmep->field.field = ALLOC(parser->a, ast_Field);
  ast_parseField(psmep->field.field, diagnostics, parser);

  // expect arrow
  Token t = parse_next(parser, diagnostics);
  if (t.kind != tk_Arrow) {
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error, "pattern struct field expected arrow after field");
  }

  // Parse pattern
  psmep->field.pat = ALLOC(parser->a, ast_Pat);
  ast_parsePat(psmep->field.pat, diagnostics, parser);

  psmep->common.span =
      SPAN(psmep->field.field->span.start, psmep->field.pat->common.span.end);
  return;
}

static void ast_certain_parseMacroPatStructMember(ast_PatStructMember *psmep,
                                                  DiagnosticLogger *diagnostics,
                                                  AstConstructor *parser) {
  ZERO(psmep);
  psmep->kind = ast_PSMK_Macro;
  psmep->macro.macro = ALLOC(parser->a, ast_Macro);
  ast_certain_parseMacro(psmep->macro.macro, diagnostics, parser);
  psmep->common.span = psmep->macro.macro->span;
}

static void ast_parsePatStructMember(ast_PatStructMember *psmep,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {
  Vector comments = parse_getComments(parser, diagnostics);
  Token t = parse_peek(parser, diagnostics);
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
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error,
        "within a pattern struct, expected a pattern struct member or macro");
  }
  }
  psmep->common.comments_len = VEC_LEN(&comments, ast_Comment);
  psmep->common.comments = vec_release(&comments);
}

static void ast_certain_parseStructPat(ast_Pat *spe,
                                       DiagnosticLogger *diagnostics,
                                       AstConstructor *parser) {
  ZERO(spe);
  spe->kind = ast_PK_Struct;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Struct);

  LnCol start = t.span.start;
  LnCol end;

  Vector members = vec_create(parser->a);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceLeft) {
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error, "DK_PatStructExpectedLeftBrace");
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
  spe->structExpr.members_len = VEC_LEN(&members, ast_PatStructMember);
  spe->structExpr.members = vec_release(&members);
  spe->common.span = SPAN(start, end);
  return;
}

static void ast_certain_parseGroupPat(ast_Pat *gpep,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  ZERO(gpep);
  gpep->kind = ast_PK_Group;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_BraceLeft);
  LnCol start = t.span.start;
  LnCol end;

  gpep->group.inner = ALLOC(parser->a, ast_Pat);
  ast_parsePat(gpep->group.inner, diagnostics, parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceRight) {
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error, "DK_PatGroupExpectedRightBrace");
    end = t.span.end;
  } else {
    end = gpep->group.inner->common.span.end;
  }

  gpep->common.span = SPAN(start, end);
}

static void ast_parseL1Pat(ast_Pat *l1, DiagnosticLogger *diagnostics,
                           AstConstructor *parser) {
  Vector comments = parse_getComments(parser, diagnostics);

  Token t = parse_peek(parser, diagnostics);
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
        diagnostic_standalone(t.span, DSK_Error, "DK_PatUnexpectedToken");
    parse_next(parser, diagnostics);
    break;
  }
  }
  l1->common.comments_len = VEC_LEN(&comments, ast_Comment);
  l1->common.comments = vec_release(&comments);
}

static void ast_parseL2Pat(ast_Pat *l2, DiagnosticLogger *diagnostics,
                           AstConstructor *parser) {
  Token t = parse_peekPastComments(parser, diagnostics);
  switch (t.kind) {
  case tk_Not: {
    l2->unaryOp.op = ast_PEUOK_Not;
    break;
  }
  default: {
    // there is no level expression
    ast_parseL1Pat(l2, diagnostics, parser);
    return;
  }
  }

  // this will only execute if an L3 operator exists
  l2->kind = ast_PK_UnaryOp;

  // comments
  Vector comments = parse_getComments(parser, diagnostics);
  l2->common.comments_len = VEC_LEN(&comments, ast_Comment);
  l2->common.comments = vec_release(&comments);

  // accept operator
  t = parse_next(parser, diagnostics);

  // Now parse the rest of the expression
  l2->unaryOp.operand = ALLOC(parser->a, ast_Pat);
  ast_parseL2Pat(l2->unaryOp.operand, diagnostics, parser);

  // finally calculate the misc stuff
  l2->common.span = SPAN(t.span.start, l2->unaryOp.operand->common.span.end);
}

static inline bool ast_opDetL3Pat(tk_Kind tk, ast_PatBinaryOpKind *val) {
  switch (tk) {
  case tk_Tuple: {
    *val = ast_PEBOK_Tuple;
    return true;
  }
  default: {
    // there is no level 4 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Pat, ast_PK, 3, ast_parseL2Pat)

static inline bool ast_opDetL4Pat(tk_Kind tk, ast_PatBinaryOpKind *val) {
  switch (tk) {
  case tk_Union: {
    *val = ast_PEBOK_Union;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Pat, ast_PK, 4, ast_parseL3Pat)

static inline bool ast_opDetL5Pat(tk_Kind tk, ast_PatBinaryOpKind *val) {
  switch (tk) {
  case tk_And: {
    *val = ast_PEBOK_And;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Pat, ast_PK, 5, ast_parseL4Pat)

static inline bool ast_opDetL6Pat(tk_Kind tk, ast_PatBinaryOpKind *val) {
  switch (tk) {
  case tk_Or: {
    *val = ast_PEBOK_Or;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(Pat, ast_PK, 6, ast_parseL5Pat)

static void ast_parsePat(ast_Pat *ppe, DiagnosticLogger *diagnostics,
                         AstConstructor *parser) {
  ast_parseL6Pat(ppe, diagnostics, parser);
}

static void ast_certain_parseValDecl(ast_Stmnt *vdsp,
                                     DiagnosticLogger *diagnostics,
                                     AstConstructor *parser) {
  // zero-initialize vdsp
  ZERO(vdsp);

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Val);
  LnCol start = t.span.start;

  // Get Binding
  ast_Pat *pat = ALLOC(parser->a, ast_Pat);
  ast_parsePat(pat, diagnostics, parser);

  LnCol end;

  // Expect define
  t = parse_peek(parser, diagnostics);
  if (t.kind == tk_Define) {
    // set components
    vdsp->kind = ast_SK_ValDeclDefine;
    vdsp->valDeclDefine.pat = pat;
    // accept the define token
    parse_next(parser, diagnostics);

    vdsp->valDeclDefine.val = ALLOC(parser->a, ast_Val);
    ast_parseVal(vdsp->valDeclDefine.val, diagnostics, parser);
    end = vdsp->valDeclDefine.val->common.span.end;
  } else {
    // set components
    vdsp->kind = ast_SK_ValDecl;
    vdsp->valDecl.pat = pat;
    end = vdsp->valDecl.pat->common.span.end;
  }

  vdsp->common.span = SPAN(start, end);
}

static void ast_certain_parseTypeDecl(ast_Stmnt *tdp,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  ZERO(tdp);
  tdp->kind = ast_SK_TypeDecl;
  Token t = parse_next(parser, diagnostics);
  // enforce that next token is type
  assert(t.kind == tk_Type);
  LnCol start = t.span.start;

  LnCol end;

  // parse binding
  tdp->typeDecl.name = ALLOC(parser->a, ast_Binding);
  ast_parseBinding(tdp->typeDecl.name, diagnostics, parser);

  // Now get define
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Define) {
    *dlogger_append(diagnostics) =
        diagnostic_standalone(t.span, DSK_Error, "DK_TypeDeclExpectedDefine");
    end = t.span.end;
    goto CLEANUP;
  }

  // get type
  tdp->typeDecl.type = ALLOC(parser->a, ast_Type);
  ast_parseType(tdp->typeDecl.type, diagnostics, parser);
  end = tdp->typeDecl.type->common.span.end;

CLEANUP:
  tdp->common.span = SPAN(start, end);
  return;
}

static void ast_certain_parseDeferStmnt(ast_Stmnt *dsp,
                                        DiagnosticLogger *diagnostics,
                                        AstConstructor *parser) {
  dsp->kind = ast_SK_DeferStmnt;
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Defer);
  LnCol start = t.span.start;

  // label
  dsp->deferStmnt.label = ALLOC(parser->a, ast_LabelReference);
  ast_parseLabelReference(dsp->deferStmnt.label, diagnostics, parser);

  // value
  dsp->deferStmnt.val = ALLOC(parser->a, ast_Val);
  ast_parseVal(dsp->deferStmnt.val, diagnostics, parser);

  // span
  dsp->common.span = SPAN(t.span.start, dsp->deferStmnt.val->common.span.end);
  return;
}

static void ast_certain_parseMacroStmnt(ast_Stmnt *msp,
                                        DiagnosticLogger *diagnostics,
                                        AstConstructor *parser) {
  ZERO(msp);
  msp->kind = ast_SK_Macro;
  msp->macro.macro = ALLOC(parser->a, ast_Macro);
  ast_certain_parseMacro(msp->macro.macro, diagnostics, parser);
  msp->common.span = msp->macro.macro->span;
}

static void ast_certain_parseNamespaceStmnt(ast_Stmnt *nsp,
                                            DiagnosticLogger *diagnostics,
                                            AstConstructor *parser) {
  ZERO(nsp);
  nsp->kind = ast_SK_Namespace;
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Namespace);
  LnCol start = t.span.start;
  LnCol end;

  // Create list of statements
  Vector statements = vec_create(parser->a);

  // namespace name
  t = parse_next(parser, diagnostics);
  if(t.kind != tk_Identifier) {
    nsp->namespaceStmnt.name = NULL;
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error, "expected identifier here");
    end = t.span.end;
    goto CLEANUP;
  }
  nsp->namespaceStmnt.name = t.identifierToken.data;

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceLeft) {
    *dlogger_append(diagnostics) = diagnostic_standalone(
        t.span, DSK_Error, "DK_NamespaceExpectedLeftBrace");
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&statements,                      // members_vec_ptr
             diagnostics,                      // dlogger_ptr
             ast_parseStmnt,                   // member_parse_function
             ast_Stmnt,                        // member_kind
             tk_BraceRight,                    // delimiting_token_kind
             "DK_NamespaceExpectedRightBrace", // missing_delimiter_error
             end,                              // end_lncol
             parser                            // parser
  )

CLEANUP:
  nsp->namespaceStmnt.stmnts_len = VEC_LEN(&statements, ast_Stmnt);
  nsp->namespaceStmnt.stmnts = vec_release(&statements);
  nsp->common.span = SPAN(start, end);
}

static void ast_certain_parseUseStmnt(ast_Stmnt *usp,
                                      DiagnosticLogger *diagnostics,
                                      AstConstructor *parser) {
  ZERO(usp);
  usp->kind = ast_SK_Use;
  Token t = parse_next(parser, diagnostics); // drop use token
  assert(t.kind == tk_Use);
  LnCol start = t.span.start;

  // parse path
  usp->useStmnt.path = ALLOC(parser->a, ast_Reference);
  ast_parseReference(usp->useStmnt.path, diagnostics, parser);

  usp->common.span = SPAN(start, usp->useStmnt.path->span.end);
}

static void ast_parseStmnt(ast_Stmnt *sp, DiagnosticLogger *diagnostics,
                           AstConstructor *parser) {
  Vector comments = parse_getComments(parser, diagnostics);

  // peek next token
  Token t = parse_peek(parser, diagnostics);
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
  case tk_Namespace: {
    ast_certain_parseNamespaceStmnt(sp, diagnostics, parser);
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
    sp->val.val = ALLOC(parser->a, ast_Val);
    ast_parseVal(sp->val.val, diagnostics, parser);
    sp->common.span = sp->val.val->common.span;
    break;
  }
  }
  sp->common.comments_len = VEC_LEN(&comments, ast_Comment);
  sp->common.comments = vec_release(&comments);
}

bool ast_nextStmntAndCheckNext(ast_Stmnt *s, DiagnosticLogger *diagnostics,
                               AstConstructor *parser) {
  Token t = parse_peek(parser, diagnostics);
  if (t.kind == tk_Eof) {
    return false;
  }
  ast_parseStmnt(s, diagnostics, parser);
  return true;
}
