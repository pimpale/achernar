#include "ast_parse.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "constants.h"
#include "json.h"
#include "lex.h"
#include "queue.h"
#include "std_allocator.h"
#include "token.h"
#include "utils.h"
#include "vector.h"

// convoluted function to save repetitive tasks
#define PARSE_LIST(members_vec_ptr, diagnostics_vec_ptr,                       \
                   member_parse_function, member_kind, delimiting_token_kind,  \
                   missing_delimiter_error, end_lncol, parser)                 \
                                                                               \
  while (true) {                                                               \
    Token pl_ntk = parse_peek(parser); /* next token kind */                   \
    if (pl_ntk.kind == delimiting_token_kind) {                                \
      end_lncol = pl_ntk.span.end;                                             \
      parse_next(parser, diagnostics_vec_ptr); /* accept delimiting tk */      \
      break;                                                                   \
    } else if (pl_ntk.kind == tk_Eof) {                                        \
      *VEC_PUSH(diagnostics_vec_ptr, Diagnostic) =                             \
          DIAGNOSTIC(missing_delimiter_error, pl_ntk.span);                    \
      end_lncol = pl_ntk.span.end;                                             \
      break;                                                                   \
    }                                                                          \
    /* if there wasn't an end delimiter, push the last token back */           \
    member_parse_function(VEC_PUSH(members_vec_ptr, member_kind), diagnostics, \
                          parser);                                             \
  }

// Parser
Parser parse_create(Lexer *lp, Allocator *a) {
  return (Parser){
      .a = a,      // Allocator
      .lexer = lp, // Lexer Pointer
      .next_tokens_queue =
          queue_create(vec_create(a)), // Queue of peeked tokens
      .next_diagnostics_queue =
          queue_create(vec_create(a)), // Queue<Vector<Diagnostic>>
      .paren_depth = 0,
      .brace_depth = 0,
      .bracket_depth = 0,
  };
}

/// gets the next token, ignoring buffering
static Token parse_rawNext(Parser *parser, Vector *diagnostics) {
  while (true) {
    Token c = tk_next(parser->lexer, diagnostics, parser->a);
    switch (c.kind) {
    case tk_ParenLeft: {
      parser->paren_depth++;
      return c;
    }
    case tk_ParenRight: {
      parser->paren_depth--;
      return c;
    }
    case tk_BraceLeft: {
      parser->brace_depth++;
      return c;
    }
    case tk_BraceRight: {
      parser->brace_depth--;
      return c;
    }
    case tk_BracketLeft: {
      parser->bracket_depth++;
      return c;
    }
    case tk_BracketRight: {
      parser->bracket_depth--;
      return c;
    }
    default: {
      return c;
    }
    }
  }
}

// If the peeked token stack is not empty:
//    Return the first element of the top of the token
//    Pop the first element of the next_comments_stack
//    For each element in the next comments stack, push it to the top of the
//    current scope
// Else fetch next raw token
static Token parse_next(Parser *pp, Vector *diagnostics) {
  // the current scope we aim to push the comments to
  if (QUEUE_LEN(&pp->next_tokens_queue, Token) > 0) {
    // we want to merge together the next token's diagnostics and comments into
    // the provided ones

    // Vector containing all diagnostics for the next
    Vector next_token_diagnostics;
    QUEUE_POP(&pp->next_diagnostics_queue, &next_token_diagnostics, Vector);

    // append next_token_diagnostics to the diagnostics vector
    vec_append(diagnostics, &next_token_diagnostics);

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
static Token parse_peekNth(Parser *pp, size_t k) {
  assert(k > 0);

  for (size_t i = QUEUE_LEN(&pp->next_tokens_queue, Token); i < k; i++) {
    // Create vector to store any diagnostics
    Vector *next_token_diagnostics =
        QUEUE_PUSH(&pp->next_diagnostics_queue, Vector);
    *next_token_diagnostics = vec_create(pp->a);

    // parse the token and add it to the top of the stack
    *QUEUE_PUSH(&pp->next_tokens_queue, Token) =
        parse_rawNext(pp, next_token_diagnostics);
  }
  // return the most recent token added
  return *QUEUE_GET(&pp->next_tokens_queue, 0, Token);
}

static Token parse_peek(Parser *parser) { return parse_peekNth(parser, 1); }

Allocator *parse_release(Parser *pp) {
  while (QUEUE_LEN(&pp->next_diagnostics_queue, Vector) != 0) {
    Vector diagnostics;
    QUEUE_POP(&pp->next_diagnostics_queue, &diagnostics, Vector);
    vec_destroy(&diagnostics);
  }
  queue_destroy(&pp->next_diagnostics_queue);
  queue_destroy(&pp->next_tokens_queue);
  return pp->a;
}

// Heuristic to resync the parser after reaching a syntax error
// Continues reading tokens until we jump out of a paren, brace, or bracket
// group discards any comments and tokens found
// Will store any lexer errors into the diagnostics
static void parse_resync(Parser *parser, Vector *diagnostics) {
  Vector comments = vec_create(parser->a);
  int64_t initial_paren_depth = parser->paren_depth;
  int64_t initial_brace_depth = parser->brace_depth;
  int64_t initial_bracket_depth = parser->bracket_depth;
  while (true) {
    if (initial_brace_depth > parser->brace_depth &&
        initial_paren_depth > parser->paren_depth &&
        initial_bracket_depth > parser->bracket_depth) {
      break;
    }
    Token t = parse_rawNext(parser, diagnostics);
    if (t.kind == tk_Eof) {
      break;
    }
  }
}

// returns a vector containing all the comments encountered here
static Vector parse_getComments(Parser *parser, Vector *diagnostics) {
  Vector comments = vec_create(parser->a);
  while(parse_peek(parser).kind == tk_Comment) { 
      Token c = parse_next(parser, diagnostics);
      *VEC_PUSH(&comments, Comment) = (Comment) { 
          .span = c.span,
          .scope = c.commentToken.scope,
          .data = c.commentToken.comment
      };
  }
  return comments;
}

// returns the first noncomment token
static Token parse_peekPastComments(Parser *parser) {
  uint64_t n = 1;
  while(parse_peekNth(parser, n).kind == tk_Comment) {
    n++;
  }
  return parse_peekNth(parser, n);
}

// Note that all errors resync at the statement level
static void parseStmnt(Stmnt *stmnt, Vector *diagnostics, Parser *parser);
static void parseValExpr(ValExpr *vep, Vector *diagnostics, Parser *parser);
static void parseTypeExpr(TypeExpr *tep, Vector *diagnostics, Parser *parser);
static void parsePatExpr(PatExpr *pp, Vector *diagnostics,
                             Parser *parser);

static void certain_parseMacroExpr(MacroExpr *mpe, Vector *diagnostics,
                                 Parser *parser) {
  ZERO(mpe);

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Macro);

  LnCol start = t.span.start;
  LnCol end;

  mpe->name = t.macroToken.data;

  Vector tokens = vec_create(parser->a);

  uint64_t depth = 1;
  while(depth > 0) {
      t = parse_peek(parser);
      if(t.kind == tk_Eof) {
        end = t.span.end;
        *VEC_PUSH(diagnostics, Diagnostic) = 
          DIAGNOSTIC(DK_MacroExprExpectedClosingBacktick, t.span);
          break;
      } else {
        if(t.kind == tk_Macro) {
            depth++;
        } else if(t.kind == tk_Backtick) {
            depth--;
        }
        *VEC_PUSH(&tokens, Token) = t;
      }
  }

  mpe->tokens_len = VEC_LEN(&tokens, Stmnt);
  mpe->tokens = vec_release(&tokens);

  mpe->node.span = SPAN(start, end);
}

static void parsePath(Path *pp, Vector *diagnostics, Parser *parser) {
  Token t = parse_next(parser, diagnostics);

  // start and finish
  LnCol start = t.span.start;
  LnCol end;

  Vector pathSegments = vec_create(parser->a);
  *VEC_PUSH(&pathSegments, char *) = t.identifierToken.data;

  if(t.kind != tk_Identifier) {
    *VEC_PUSH(diagnostics, Diagnostic) = DIAGNOSTIC(DK_PathExpectedIdentifier, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  while (true) {
    t = parse_peek(parser);
    if (t.kind == tk_ScopeResolution) {
      // discard the scope resolution
      parse_next(parser, diagnostics);
      // now check if we have an issue
      t = parse_next(parser, diagnostics);
      if (t.kind != tk_Identifier) {
        *VEC_PUSH(diagnostics, Diagnostic) =
            DIAGNOSTIC(DK_PathExpectedIdentifier, t.span);
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
  pp->pathSegments_len = VEC_LEN(&pathSegments, char *);
  pp->pathSegments = vec_release(&pathSegments);

  pp->node.span = SPAN(start, end);
  pp->node.comments_len = 0;
}


static void certain_parseNilValExpr(ValExpr *vep, Vector *diagnostics,
                                      Parser *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Nil);
  vep->kind = VEK_NilLiteral;
  vep->node.span = t.span;
  return;
}

static void certain_parseIntValExpr(ValExpr *vep, Vector *diagnostics,
                                      Parser *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Int);
  vep->kind = VEK_IntLiteral;
  vep->intLiteral.value = t.intToken.data;
  vep->node.span = t.span;
  return;
}

static void certain_parseBoolValExpr(ValExpr *vep, Vector *diagnostics,
                                       Parser *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Bool);
  vep->kind = VEK_BoolLiteral;
  vep->boolLiteral.value = t.boolToken.data;
  vep->node.span = t.span;
  return;
}

static void certain_parseFloatValExpr(ValExpr *vep, Vector *diagnostics,
                                        Parser *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Float);
  vep->kind = VEK_FloatLiteral;
  vep->floatLiteral.value = t.floatToken.data;
  vep->node.span = t.span;
  return;
}

static void certain_parseCharValExpr(ValExpr *vep, Vector *diagnostics,
                                       Parser *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Char);
  vep->kind = VEK_CharLiteral;
  vep->charLiteral.value = t.floatToken.data;
  vep->node.span = t.span;
  return;
}

static void certain_parseStringValExpr(ValExpr *svep, Vector *diagnostics,
                                         Parser *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_String);
  svep->kind = VEK_StringLiteral;
  svep->stringLiteral.value = t.stringToken.data;
  svep->node.span = t.span;
  return;
}

static void certain_parseFnValExpr(ValExpr *fvep, Vector *diagnostics,
                                     Parser *parser) {
  ZERO(fvep);

  fvep->kind = VEK_Fn;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Fn);
  LnCol start = t.span.start;
  LnCol end = t.span.end;

  // check for leftparen
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_ParenLeft) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnValExprExpectedLeftParen, t.span);
    goto CLEANUP;
  }

  Span lparenspan = t.span;

  Vector parameters = vec_create(parser->a);

  PARSE_LIST(&parameters,                      // members_vec_ptr
             diagnostics,                      // diagnostics_vec_ptr
             parsePatExpr,                 // member_parse_function
             PatExpr,                      // member_kind
             tk_ParenRight,                    // delimiting_token_kind
             DK_FnValExprExpectedRightParen, // missing_delimiter_error
             end,                              // end_lncol
             parser                            // parser
  )

  fvep->fnExpr.parameters_len = VEC_LEN(&parameters, PatExpr);
  fvep->fnExpr.parameters = vec_release(&parameters);

  t = parse_peek(parser);
  if (t.kind == tk_Colon) {
    fvep->fnExpr.type = ALLOC(parser->a, TypeExpr);
    // advance
    parse_next(parser, diagnostics);

    parseTypeExpr(fvep->fnExpr.type, diagnostics, parser);
  } else {
    fvep->fnExpr.type = ALLOC(parser->a, TypeExpr);
    *fvep->fnExpr.type= (TypeExpr) {
        .node = {
            .span = lparenspan,
            .comments_len = 0
        },
        .kind = TEK_Omitted
    };
  }

  t = parse_next(parser, diagnostics);

  if (t.kind != tk_Arrow) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnValExprExpectedArrow, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  fvep->fnExpr.body = ALLOC(parser->a, ValExpr);
  parseValExpr(fvep->fnExpr.body, diagnostics, parser);
  end = fvep->fnExpr.body->node.span.end;

CLEANUP:
  fvep->node.span = SPAN(start, end);
}

static void certain_parseLabelLabel(Label *lp, Vector *diagnostics, Parser *parser) {
  ZERO(lp);
  lp->kind = LEK_Label;
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Label);
  lp->node = (AstNode) {
      .span = t.span,
      .comments_len = 0
  };
  lp->label.label = t.labelToken.data;
}

static void certain_parseBlockValExpr(ValExpr *bvep, Vector *diagnostics,
                                        Parser *parser) {
  ZERO(bvep);
  bvep->kind = VEK_Block;

  // Parse leftbrace
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_BraceLeft);
  Span lbracespan = t.span;

  t = parse_peek(parser);
  // blocks may be labeled
  bvep->blockExpr.label = ALLOC(parser->a, Label);
  if (t.kind == tk_Label) {
      certain_parseLabelLabel(bvep->blockExpr.label, diagnostics, parser);
  } else {
      *bvep->blockExpr.label = (Label) {
        .node = {
            .span = lbracespan,
            .comments_len = 0
        },
        .kind = LEK_Omitted
      };
  }

  // Create list of statements
  Vector statements = vec_create(parser->a);

  LnCol end;

  PARSE_LIST(&statements,                // members_vec_ptr
             diagnostics,                // diagnostics_vec_ptr
             parseStmnt,                 // member_parse_function
             Stmnt,                      // member_kind
             tk_BraceRight,              // delimiting_token_kind
             DK_BlockExpectedRightBrace, // missing_delimiter_error
             end,                        // end_lncol
             parser                      // parser
  )

  bvep->blockExpr.statements_len = VEC_LEN(&statements, Stmnt);
  bvep->blockExpr.statements = vec_release(&statements);
  bvep->node.span = SPAN(lbracespan.start, end);
  return;
}
static void certain_parseReturnValExpr(ValExpr *rep, Vector *diagnostics,
                                         Parser *parser) {
  ZERO(rep);
  rep->kind = VEK_Return;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Return);
  Span retspan = t.span;

  LnCol start = t.span.start;
  LnCol end;

  parse_next(parser, diagnostics);

  t = parse_peek(parser);
  // blocks may be labeled
  rep->returnExpr.label = ALLOC(parser->a, Label);
  if (t.kind == tk_Label) {
      certain_parseLabelLabel(rep->returnExpr.label, diagnostics, parser);
  } else {
      *rep->returnExpr.label = (Label) {
        .node = {
            .span = retspan,
            .comments_len = 0
        },
        .kind = LEK_Omitted
      };
  }

  rep->returnExpr.value = ALLOC(parser->a, ValExpr);
  parseValExpr(rep->returnExpr.value, diagnostics, parser);
  end = rep->returnExpr.value->node.span.end;
  rep->node.span = SPAN(start, end);
  return;
}

static void certain_parseLoopValExpr(ValExpr *lep, Vector *diagnostics,
                                       Parser *parser) {
  lep->kind = VEK_Loop;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Loop);
  LnCol start = t.span.start;
  Span loopspan = t.span;

  t = parse_peek(parser);
  lep->loopExpr.label = ALLOC(parser->a, Label);
  if (t.kind == tk_Label) {
      certain_parseLabelLabel(lep->loopExpr.label, diagnostics, parser);
  } else {
      *lep->loopExpr.label = (Label) {
        .node = {
            .span = loopspan,
            .comments_len = 0
        },
        .kind = LEK_Omitted
      };
  }

  lep->loopExpr.value = ALLOC(parser->a, ValExpr);
  parseValExpr(lep->loopExpr.value, diagnostics, parser);
  lep->node.span = SPAN(start, lep->loopExpr.value->node.span.end);
  return;
}

// pat Pattern => Expr,
static void certain_parsePatMatchCaseExpr(MatchCaseExpr *mcep,
                               Vector *diagnostics, Parser *parser) {
  ZERO(mcep);

  // Get Pat
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Pat);
  LnCol start = t.span.start;


  // Get pattern
  mcep->matchCase.pattern = ALLOC(parser->a, PatExpr);
  parsePatExpr(mcep->matchCase.pattern, diagnostics, parser);

  LnCol end = mcep->matchCase.pattern->node.span.end;

  // Expect colon
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Arrow) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_MatchCaseNoArrow, t.span);
    parse_resync(parser, diagnostics);
    goto CLEANUP;
  }

  // Get Value
  mcep->matchCase.val= ALLOC(parser->a, ValExpr);
  parseValExpr(mcep->matchCase.val, diagnostics, parser);
  end = mcep->matchCase.val->node.span.end;

CLEANUP:
  mcep->node.span = SPAN(start, end);
  return;
}

static void certain_parseMacroMatchCaseExpr(MatchCaseExpr *mcep,
                                                  Vector *diagnostics, Parser *parser) {
  ZERO(mcep);
  mcep->kind = MCK_Macro;
  mcep->macro.macro= ALLOC(parser->a, MacroExpr);
  certain_parseMacroExpr(mcep->macro.macro, diagnostics, parser);
  mcep->node.span = mcep->macro.macro->node.span;
}

static void parseMatchCaseExpr(MatchCaseExpr *mcep,
                               Vector *diagnostics, Parser *parser) {
  Vector comments = parse_getComments(parser, diagnostics);
  Token t = parse_peek(parser);
  switch(t.kind) {
    case tk_Pat: {
      certain_parsePatMatchCaseExpr(mcep, diagnostics, parser);
      break;
  }
  case tk_Macro: {
      certain_parseMacroMatchCaseExpr(mcep, diagnostics, parser);
      break;
  }
  default: {
     *VEC_PUSH(diagnostics, Diagnostic) = DIAGNOSTIC(DK_MatchCaseNoPat, t.span);

      mcep->kind = MCK_None;
      mcep->node.span = t.span;
      // discard token
      parse_next(parser, diagnostics);
      break;
  }
  }
  mcep->node.comments_len = VEC_LEN(&comments, Comment);
  mcep->node.comments = vec_release(&comments);
}
static void certain_parseMatchValExpr(ValExpr *mvep, Vector *diagnostics,
                                        Parser *parser) {
  ZERO(mvep);
  mvep->kind = VEK_Match;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Match);
  LnCol start = t.span.start;

  // Get expression to match against
  mvep->matchExpr.val = ALLOC(parser->a, ValExpr);
  parseValExpr(mvep->matchExpr.val, diagnostics, parser);
  // now we must parse the block containing the cases
  Vector cases = vec_create(parser->a);

  LnCol end;

  // Expect beginning brace
  t = parse_next(parser, diagnostics);

  if (t.kind != tk_BraceLeft) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_MatchNoLeftBrace, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&cases,                 // members_vec_ptr
             diagnostics,            // diagnostics_vec_ptr
             parseMatchCaseExpr,     // member_parse_function
             MatchCaseExpr, // member_kind
             tk_BraceRight,          // delimiting_token_kind
             DK_MatchNoRightBrace,   // missing_delimiter_error
             end,                    // end_lncol
             parser                  // parser
  )

CLEANUP:
  // Get interior cases
  mvep->matchExpr.cases_len = VEC_LEN(&cases, MatchCaseExpr);
  mvep->matchExpr.cases = vec_release(&cases);

  mvep->node.span = SPAN(start, end);
  return;
}

static void parseReferenceValExpr(ValExpr *rvep, Vector *diagnostics,
                                    Parser *parser) {
  ZERO(rvep);
  rvep->kind = VEK_Reference;
  rvep->reference.path = ALLOC(parser->a, Path);
  parsePath(rvep->reference.path, diagnostics, parser);
  rvep->node.span = rvep->reference.path->node.span;
  return;
}

static void certain_parseMacroValStructMemberExpr(ValStructMemberExpr *vsemp,
                                                  Vector *diagnostics, Parser *parser) {
  ZERO(vsemp);
  vsemp->kind = VSMEK_Macro;
  vsemp->macro.macro= ALLOC(parser->a, MacroExpr);
  certain_parseMacroExpr(vsemp->macro.macro, diagnostics, parser);
  vsemp->node.span = vsemp->macro.macro->node.span;
}

// field := Value
static void certain_parseMemberValStructMemberExpr(ValStructMemberExpr *vsmep,
                                       Vector *diagnostics, Parser *parser) {
  // zero-initialize bp
  ZERO(vsmep);
  vsmep->kind = VSMEK_ValStructMemberExpr;

  LnCol start;
  LnCol end;

  // identifier data
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Identifier);
  Span identitySpan = t.span;
  start = identitySpan.start;
  vsmep->valStructMemberExpr.name = t.identifierToken.data;

  // check if define
  t = parse_next(parser, diagnostics);
  if (t.kind == tk_Define) {
    // Get value of variable
    vsmep->valStructMemberExpr.val = ALLOC(parser->a, ValExpr);
    parseValExpr(vsmep->valStructMemberExpr.val, diagnostics, parser);
    end = vsmep->valStructMemberExpr.val->node.span.end;
  } else {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_StructMemberLiteralExpectedDefine, t.span);
    vsmep->valStructMemberExpr.val = NULL;
    end = t.span.end;
    goto CLEANUP;
  }

CLEANUP:
  vsmep->node.span = SPAN(start, end);
  return;
}

static void parseValStructMemberExpr(ValStructMemberExpr *vsmep,
                                       Vector *diagnostics, Parser *parser) {
  Vector comments = parse_getComments(parser, diagnostics); 
  Token t = parse_peek(parser);
  switch(t.kind) {
    case tk_Macro: {
      certain_parseMacroValStructMemberExpr(vsmep, diagnostics, parser);
      break;
    }
    case tk_Identifier: {
        certain_parseMemberValStructMemberExpr(vsmep, diagnostics, parser);
        break;
    }
    default: {
     *VEC_PUSH(diagnostics, Diagnostic) = DIAGNOSTIC(DK_StructLiteralExpectedEntry, t.span);

      vsmep->kind = VSMEK_None;
      vsmep->node.span = t.span;
      // discard token
      parse_next(parser, diagnostics);
      break;
    }
  }
  vsmep->node.comments_len = VEC_LEN(&comments, Comment);
  vsmep->node.comments = vec_release(&comments);
}

static void certain_parseValStructExpr(ValExpr *sve, Vector *diagnostics,
                                         Parser *parser) {
  ZERO(sve);
  sve->kind = VEK_StructLiteral;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Struct);

  LnCol start = t.span.start;
  LnCol end;

  Vector members = vec_create(parser->a);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceLeft) {
    end = t.span.end;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_StructLiteralExpectedLeftBrace, t.span);
    goto CLEANUP;
  }

  PARSE_LIST(&members,                           // members_vec_ptr
             diagnostics,                        // diagnostics_vec_ptr
             parseValStructMemberExpr,           // member_parse_function
             ValStructMemberExpr,                // member_kind
             tk_BraceRight,                      // delimiting_token_kind
             DK_StructLiteralExpectedRightBrace, // missing_delimiter_error
             end,                                // end_lncol
             parser                              // parser
  )

CLEANUP:
  sve->structExpr.members_len =
      VEC_LEN(&members, ValStructMemberExpr);
  sve->structExpr.members = vec_release(&members);
  sve->node.span = SPAN(start, end);
  return;
}

// Level1ValExpr parentheses, braces, literals
// Level2ValExpr as () [] & @ . -> (postfixes)
// Level3ValExpr -- ++ ! (prefixes)
// Level4ValExpr -> (pipeline)
// Level5ValExpr * / % (multiplication and division)
// Level6ValExpr + - (addition and subtraction)
// Level7ValExpr < <= > >= == != (comparators)
// Level8ValExpr && (logical and)
// Level9ValExpr || (logical or)
// Level10ValExpr , (create tuple)
// Level11ValExpr = += -= *= /= %= (Assignment)

static void parseL1ValExpr(ValExpr *l1, Vector *diagnostics,
                             Parser *parser) {

  // value comments;
  Vector comments = parse_getComments(parser, diagnostics);
  l1->node.comments_len = VEC_LEN(&comments, Comment);
  l1->node.comments = vec_release(&comments);

  Token t = parse_peek(parser);
  // Decide which expression it is
  switch (t.kind) {
  // Literals
  case tk_Int: {
    certain_parseIntValExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Bool: {
    certain_parseBoolValExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Float: {
    certain_parseFloatValExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Char: {
    certain_parseCharValExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Nil: {
    certain_parseNilValExpr(l1, diagnostics, parser);
    break;
  }
  case tk_String: {
    certain_parseStringValExpr(l1, diagnostics, parser);
    break;
  }
  case tk_BraceLeft: {
    certain_parseBlockValExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Fn: {
    certain_parseFnValExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Struct: {
    certain_parseValStructExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Return: {
    certain_parseReturnValExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Loop: {
    certain_parseLoopValExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Match: {
    certain_parseMatchValExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Identifier: {
    parseReferenceValExpr(l1, diagnostics, parser);
    break;
  }
  default: {
    ZERO(l1);
    l1->kind = VEK_None;
    l1->node.span = t.span;
    parse_next(parser, diagnostics);
    *VEC_PUSH(diagnostics, Diagnostic) = DIAGNOSTIC(DK_UnexpectedToken, t.span);
  }
  }
}

static void parseFieldAccessValExpr(ValExpr *fave, Vector *diagnostics,
                                      Parser *parser, ValExpr *root) {
  ZERO(fave);
  fave->kind = VEK_FieldAccess;
  fave->fieldAccess.value = root;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_FieldAccess);

  // Now we get the field
  t = parse_peek(parser);
  if (t.kind != tk_Identifier) {
    // it is possible we encounter an error
    fave->fieldAccess.field = NULL;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FieldAccessExpectedIdentifier, t.span);
  } else {
    fave->fieldAccess.field = t.identifierToken.data;
  }
  fave->node.span = SPAN(root->node.span.start, t.span.end);
}

static void parseCallValExpr(ValExpr *cvep, Vector *diagnostics,
                               Parser *parser, ValExpr *root) {
  ZERO(cvep);

  cvep->kind = VEK_Call;
  cvep->callExpr.function = root;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_ParenLeft);

  LnCol end;

  Vector parameters = vec_create(parser->a);

  PARSE_LIST(&parameters,          // members_vec_ptr
             diagnostics,          // diagnostics_vec_ptr
             parseValExpr,       // member_parse_function
             ValExpr,            // member_kind
             tk_ParenRight,        // delimiting_token_kind
             DK_CallExpectedParen, // missing_delimiter_error
             end,                  // end_lncol
             parser                // parser
  )

  cvep->callExpr.parameters_len = VEC_LEN(&parameters, ValExpr);
  cvep->callExpr.parameters = vec_release(&parameters);

  cvep->node.span = SPAN(root->node.span.start, end);
}

static void parseAsValExpr(ValExpr *avep, Vector *diagnostics,
                             Parser *parser, ValExpr *root) {
  ZERO(avep);
  avep->kind = VEK_As;
  avep->asExpr.value = root;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_As);

  avep->asExpr.type = ALLOC(parser->a, TypeExpr);
  parseTypeExpr(avep->asExpr.type, diagnostics, parser);
}

static void parseL2ValExpr(ValExpr *l2, Vector *diagnostics,
                             Parser *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff

  ValExpr *root = l2;
  parseL1ValExpr(root, diagnostics, parser);

  while (true) {
    // represents the old operation
    ValExpr *v;

    Token t = parse_peekPastComments(parser);
    switch (t.kind) {
    case tk_Ref: {
      // get comments
      Vector comments = parse_getComments(parser, diagnostics);
      // allocate space for operation
      v = ALLOC(parser->a, ValExpr);
      *v = *root;
      root->kind = VEK_UnaryOp;
      root->unaryOp.op= VEUOK_Ref;
      root->unaryOp.operand = v;
      root->node.span = SPAN(v->node.span.start, t.span.end);
      root->node.comments_len = VEC_LEN(&comments, Comment);
      root->node.comments= vec_release(&comments);
      parse_next(parser, diagnostics);
      break;
    }
    case tk_Deref: {
      Vector comments = parse_getComments(parser, diagnostics);
      v = ALLOC(parser->a, ValExpr);
      *v = *root;
      root->kind = VEK_UnaryOp;
      root->unaryOp.op = VEUOK_Deref;
      root->unaryOp.operand = v;
      root->node.span = SPAN(v->node.span.start, t.span.end);
      root->node.comments_len = VEC_LEN(&comments, Comment);
      root->node.comments= vec_release(&comments);
      parse_next(parser, diagnostics);
      break;
    }
    case tk_FieldAccess: {
      Vector comments = parse_getComments(parser, diagnostics);
      v = ALLOC(parser->a, ValExpr);
      *v = *root;
      parseFieldAccessValExpr(root, diagnostics, parser, v);
      break;
    }
    case tk_ParenLeft: {
      Vector comments = parse_getComments(parser, diagnostics);
      v = ALLOC(parser->a, ValExpr);
      *v = *root;
      parseCallValExpr(root, diagnostics, parser, v);
      root->node.comments_len = VEC_LEN(&comments, Comment);
      root->node.comments= vec_release(&comments);
      break;
    }
    case tk_As: {
      Vector comments = parse_getComments(parser, diagnostics);
      v = ALLOC(parser->a, ValExpr);
      *v = *root;
      parseAsValExpr(root, diagnostics, parser, v);
      root->node.comments_len = VEC_LEN(&comments, Comment);
      root->node.comments= vec_release(&comments);
      break;
    }
    default: {
      // there are no more level 2 expressions
      return;
    }
    }
  }
}

static void parseL3ValExpr(ValExpr *l3, Vector *diagnostics,
                             Parser *parser) {
  Token t = parse_peekPastComments(parser);
  switch (t.kind) {
  case tk_Negate: {
    l3->unaryOp.op= VEUOK_Negate;
    break;
  }
  case tk_Posit: {
    l3->unaryOp.op= VEUOK_Posit;
    break;
  }
  case tk_Not: {
    l3->unaryOp.op= VEUOK_Not;
    break;
  }
  default: {
    // there is no level 3 expression
    parseL2ValExpr(l3, diagnostics, parser);
    return;
  }
  }

  // this will only execute if an L3 operator exists
  l3->kind = VEK_UnaryOp;

  // first get comments
  Vector comments = parse_getComments(parser, diagnostics);
  l3->node.comments_len = VEC_LEN(&comments, Comment);
  l3->node.comments = vec_release(&comments);
  // consume operator
  parse_next(parser, diagnostics);

  // Now parse the rest of the expression
  l3->unaryOp.operand = ALLOC(parser->a, ValExpr);
  parseL3ValExpr(l3->unaryOp.operand, diagnostics, parser);

  // finally calculate the misc stuff
  l3->node.span = SPAN(t.span.start, l3->unaryOp.operand->node.span.end);

  // comments
}

// type is the type of object that the generated function will parse
// x is the index level of the function
// lower_fn is the name of the function that will be called to evaluate the left
// and right op_det_fn is the name of the function that determines the binary
// operator this function should take a pointer to the type and return a bool if
// successful
#define FN_BINOP_PARSE_LX_EXPR(type, type_shorthand, x, lower_fn)              \
  static void parseL##x##type(type * expr, Vector *diagnostics,                 \
                              Parser *parser) {                                \
    type v;                                                                    \
    lower_fn(&v, diagnostics, parser);                                         \
                                                                               \
    Token t = parse_peekPastComments(parser);                                              \
    bool success = opDetL##x##type(t.kind, &expr->binaryOp.op);          \
    if (!success) {                                                            \
      /* there is no level x expression */                                     \
      *expr = v;                                                               \
      return;                                                                  \
    }                                                                          \
    /* this will only execute if the operator exists */                        \
    expr->kind = type_shorthand##_BinaryOp;                                    \
                                                                               \
    /* set the left side */                                                    \
    expr->binaryOp.left_operand = ALLOC(parser->a, type);                      \
    *expr->binaryOp.left_operand = v;                                          \
                                                                               \
    /* first get comments */                   \
  Vector comments = parse_getComments(parser, diagnostics); \
  expr->node.comments_len = VEC_LEN(&comments, Comment); \
  expr->node.comments = vec_release(&comments); \
    /* consume operator */                                        \
    parse_next(parser, diagnostics);                                           \
                                                                               \
    /* now parse the rest of the expression */                                 \
    expr->binaryOp.right_operand = ALLOC(parser->a, type);                     \
    parseL##x##type(expr->binaryOp.right_operand, diagnostics, parser);        \
                                                                               \
    /* calculate misc stuff */                                                 \
    expr->node.span = SPAN(expr->binaryOp.left_operand->node.span.start,                 \
                      expr->binaryOp.right_operand->node.span.end);                 \
                                                                               \
    return;                                                                    \
  }

static inline bool opDetL4ValExpr(tk_Kind tk,
                                    ValExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Pipe: {
    *val = VEBOK_Pipeline;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValExpr, VEK, 4, parseL3ValExpr)

// Parses a single term that will not collide with patterns
static inline void parseValExprTerm(ValExpr *term, Vector *diagnostics,
                                      Parser *parser) {
  parseL4ValExpr(term, diagnostics, parser);
}

static inline bool opDetL5ValExpr(tk_Kind tk,
                                    ValExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Mul: {
    *val = VEBOK_Mul;
    return true;
  }
  case tk_Div: {
    *val = VEBOK_Div;
    return true;
  }
  case tk_Mod: {
    *val = VEBOK_Mod;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValExpr, VEK, 5, parseL4ValExpr)

static inline bool opDetL6ValExpr(tk_Kind tk,
                                    ValExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Add: {
    *val = VEBOK_Add;
    return true;
  }
  case tk_Sub: {
    *val = VEBOK_Sub;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValExpr, VEK, 6, parseL5ValExpr)

static inline bool opDetL7ValExpr(tk_Kind tk,
                                    ValExprBinaryOpKind *val) {
  switch (tk) {
  case tk_CompLess: {
    *val = VEBOK_CompLess;
    return true;
  }
  case tk_CompGreater: {
    *val = VEBOK_CompGreater;
    return true;
  }
  case tk_CompLessEqual: {
    *val = VEBOK_CompLessEqual;
    return true;
  }
  case tk_CompGreaterEqual: {
    *val = VEBOK_CompGreaterEqual;
    return true;
  }
  case tk_CompEqual: {
    *val = VEBOK_CompEqual;
    return true;
  }
  case tk_CompNotEqual: {
    *val = VEBOK_CompNotEqual;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValExpr, VEK, 7, parseL6ValExpr)

static inline bool opDetL8ValExpr(tk_Kind tk,
                                    ValExprBinaryOpKind *val) {
  switch (tk) {
  case tk_And: {
    *val = VEBOK_And;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValExpr, VEK, 8, parseL7ValExpr)

static inline bool opDetL9ValExpr(tk_Kind tk,
                                    ValExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Or: {
    *val = VEBOK_Or;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValExpr, VEK, 9, parseL8ValExpr)

static inline bool opDetL10ValExpr(tk_Kind tk,
                                     ValExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Tuple: {
    *val = VEBOK_Tuple;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValExpr, VEK, 10, parseL9ValExpr)

static bool opDetL11ValExpr(tk_Kind tk, ValExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Assign: {
    *val = VEBOK_Assign;
    return true;
  }
  case tk_AssignAdd: {
    *val = VEBOK_AssignAdd;
    return true;
  }
  case tk_AssignSub: {
    *val = VEBOK_AssignSub;
    return true;
  }
  case tk_AssignMul: {
    *val = VEBOK_AssignMul;
    return true;
  }
  case tk_AssignDiv: {
    *val = VEBOK_AssignDiv;
    return true;
  }
  case tk_AssignMod: {
    *val = VEBOK_AssignMod;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValExpr, VEK, 11, parseL10ValExpr)

// shim method
static void parseValExpr(ValExpr *vep, Vector *diagnostics,
                           Parser *parser) {
  parseL11ValExpr(vep, diagnostics, parser);
}

// field : Type,
static void certain_parseTypeStructMemberExpr(struct TypeStructMemberExpr_s *tsmep,
                                      Vector *diagnostics, Parser *parser) {
  // zero-initialize bp
  ZERO(tsmep);

  LnCol start;
  LnCol end;

  // get.identifierToken.data
  Token t = parse_next(parser, diagnostics);
  Span identitySpan = t.span;
  start = identitySpan.start;

  if (t.kind != tk_Identifier) {
    tsmep->name = NULL;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_StructMemberExpectedIdentifier, t.span);
    end = identitySpan.end;
    goto CLEANUP;
  }

  tsmep->name = t.identifierToken.data;

  // check if colon
  t = parse_peek(parser);
  if (t.kind == tk_Colon) {
    // advance through colon
    parse_next(parser, diagnostics);
    // Get type of variable
    tsmep->type = ALLOC(parser->a, TypeExpr);
    parseTypeExpr(tsmep->type, diagnostics, parser);
    end = tsmep->type->node.span.end;
  } else {
    end = identitySpan.end;
    tsmep->type = ALLOC(parser->a, TypeExpr);
    tsmep->type->kind = TEK_Omitted;
    tsmep->type->node.span = identitySpan;
    tsmep->type->comments_len = 0;
  }

CLEANUP:
  tsmep->node.span = SPAN(start, end);
  Vector comments = popCommentScopeParser(parser);
  tsmep->comments_len = VEC_LEN(&comments, Comment);
  tsmep->comments = vec_release(&comments);
  return;
}

static void parseTypeStructMemberExpr(struct TypeStructMemberExpr_s *tsmep,
                                      Vector *diagnostics, Parser *parser) {


static void certain_parseStructTypeExpr(TypeExpr *ste, Vector *diagnostics,
                                        Parser *parser) {
  ZERO(ste);
  ste->kind = TEK_Struct;

  Token t = parse_next(parser, diagnostics);
  switch (t.kind) {
  case tk_Struct: {
    ste->structExpr.kind = TSEK_Struct;
    break;
  }
  case tk_Enum: {
    ste->structExpr.kind = TSEK_Enum;
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
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_StructExpectedLeftBrace, t.span);
    goto CLEANUP;
  }

  PARSE_LIST(&members,                      // members_vec_ptr
             diagnostics,                   // diagnostics_vec_ptr
             parseTypeStructMemberExpr,     // member_parse_function
             struct TypeStructMemberExpr_s, // member_kind
             tk_BraceRight,                 // delimiting_token_kind
             DK_StructExpectedRightBrace,   // missing_delimiter_error
             end,                           // end_lncol
             parser                         // parser
  )

CLEANUP:
  ste->structExpr.members_len =
      VEC_LEN(&members, struct TypeStructMemberExpr_s);
  ste->structExpr.members = vec_release(&members);
  ste->node.span = SPAN(start, end);
  return;
}

static void parseReferenceTypeExpr(TypeExpr *rtep, Vector *diagnostics,
                                   Parser *parser) {
  ZERO(rtep);
  rtep->kind = TEK_Reference;
  rtep->referenceExpr.path = ALLOC(parser->a, Path);
  parsePath(rtep->referenceExpr.path, diagnostics, parser);
  rtep->node.span = rtep->referenceExpr.path->node.span;
}

static void certain_parseNilTypeExpr(TypeExpr *vte, Vector *diagnostics,
                                     Parser *parser) {

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Nil);
  vte->kind = TEK_Nil;
  vte->node.span = t.span;
  return;
}

static void parseFnTypeExpr(TypeExpr *fte, Vector *diagnostics,
                            Parser *parser) {
  ZERO(fte);

  Token t = parse_next(parser, diagnostics);

  assert(t.kind == tk_Fn);

  LnCol start = t.span.start;
  LnCol end;

  // check for leftparen
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_ParenLeft) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnTypeExprExpectedLeftParen, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  Vector parameters = vec_create(parser->a);

  PARSE_LIST(&parameters,                     // members_vec_ptr
             diagnostics,                     // diagnostics_vec_ptr
             parseTypeExpr,                   // member_parse_function
             TypeExpr,                        // member_kind
             tk_ParenRight,                   // delimiting_token_kind
             DK_FnTypeExprExpectedRightParen, // missing_delimiter_error
             end,                             // end_lncol
             parser                           // parser
  )

  fte->fnExpr.parameters_len = VEC_LEN(&parameters, TypeExpr);
  fte->fnExpr.parameters = vec_release(&parameters);

  return;
  fte->fnExpr.parameters = ALLOC(parser->a, TypeExpr);
  parseTypeExpr(fte->fnExpr.parameters, diagnostics, parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Colon) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnTypeExprExpectedColon, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

CLEANUP:
  fte->node.span = SPAN(start, end);
}

static void parseL1TypeExpr(TypeExpr *l1, Vector *diagnostics, Parser *parser) {
  Token t = parse_peekPastComments(parser);
  switch (t.kind) {
  case tk_Identifier: {
    parseReferenceTypeExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Enum:
  case tk_Struct: {
    certain_parseStructTypeExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Nil: {
    certain_parseNilTypeExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Fn: {
    parseFnTypeExpr(l1, diagnostics, parser);
    break;
  }
  default: {
    l1->kind = TEK_None;
    l1->node.span = t.span;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_TypeExprUnexpectedToken, t.span);
    // Resync
    parse_resync(parser, diagnostics);
    break;
  }
  }

  Vector comments = popCommentScopeParser(parser);
  l1->comments_len = VEC_LEN(&comments, Comment);
  l1->comments = vec_release(&comments);
}

static void parseScopeResolutionTypeExpr(TypeExpr *srte, Vector *diagnostics,
                                         Parser *parser, TypeExpr *root) {
  ZERO(srte);
  srte->kind = TEK_FieldAccess;
  srte->fieldAccess.value = root;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_FieldAccess);

  // Now we get the field
  t = parse_peek(parser);
  if (t.kind != tk_Identifier) {
    // it is possible we encounter an error
    srte->fieldAccess.field = NULL;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_TypeExprFieldAccessExpectedIdentifier, t.span);
  } else {
    srte->fieldAccess.field = t.identifierToken.data;
  }

  srte->node.span = SPAN(root->node.span.start, t.span.end);
}

static void parseL2TypeExpr(TypeExpr *l2, Vector *diagnostics, Parser *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff
  TypeExpr *root = l2;
  parseL1TypeExpr(root, diagnostics, parser);

  while (true) {
    // represents the new operation
    TypeExpr *ty;

    Token t = parse_peek(parser);
    switch (t.kind) {
    case tk_Ref: {
      pushCommentScopeParser(parser);
      ty = ALLOC(parser->a, TypeExpr);
      *ty = *root;
      root->kind = TEK_UnaryOp;
      root->unaryOp.operator= TEUOK_Ref;
      root->unaryOp.operand = ty;
      root->node.span = SPAN(ty->node.span.start, t.span.end);
      // discard token
      parse_next(parser, diagnostics);
      break;
    }
    case tk_Deref: {
      pushCommentScopeParser(parser);
      ty = ALLOC(parser->a, TypeExpr);
      *ty = *root;
      root->kind = TEK_UnaryOp;
      root->unaryOp.operator= TEUOK_Deref;
      root->unaryOp.operand = ty;
      root->node.span = SPAN(ty->node.span.start, t.span.end);
      // discard token
      parse_next(parser, diagnostics);
      break;
    }
    case tk_ScopeResolution: {
      pushCommentScopeParser(parser);
      ty = ALLOC(parser->a, TypeExpr);
      *ty = *root;
      parseScopeResolutionTypeExpr(root, diagnostics, parser, ty);
      break;
    }
    default: {
      // there are no more level2 expressions
      return;
    }
    }

    Vector comments = popCommentScopeParser(parser);
    root->comments_len = VEC_LEN(&comments, Comment);
    root->comments = vec_release(&comments);
  }
}

static inline bool opDetL3TypeExpr(tk_Kind tk,
                                   enum TypeExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Tuple: {
    *val = TEBOK_Tuple;
    return true;
  }
  default: {
    // there is no level 3 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(TypeExpr, TEK, 3, parseL2TypeExpr)

static inline bool opDetL4TypeExpr(tk_Kind tk,
                                   enum TypeExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Union: {
    *val = TEBOK_Union;
    return true;
  }
  default: {
    // there is no level 4 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(TypeExpr, TEK, 4, parseL3TypeExpr)

static void parseTypeExpr(TypeExpr *tep, Vector *diagnostics, Parser *parser) {
  parseL4TypeExpr(tep, diagnostics, parser);
}

static void certain_parseValueRestrictionPatExpr(PatExpr *vrpe,
                                                     Vector *diagnostics,
                                                     Parser *parser) {
  ZERO(vrpe);

  Token t = parse_next(parser, diagnostics);
  LnCol start = t.span.start;

  vrpe->kind = PEK_ValueRestriction;

  switch (t.kind) {
  case tk_CompEqual: {
    vrpe->valueRestriction.restriction = PEVRK_CompEqual;
    break;
  }
  case tk_CompNotEqual: {
    vrpe->valueRestriction.restriction = PEVRK_CompNotEqual;
    break;
  }
  case tk_CompGreaterEqual: {
    vrpe->valueRestriction.restriction = PEVRK_CompGreaterEqual;
    break;
  }
  case tk_CompGreater: {
    vrpe->valueRestriction.restriction = PEVRK_CompGreater;
    break;
  }
  case tk_CompLess: {
    vrpe->valueRestriction.restriction = PEVRK_CompLess;
    break;
  }
  case tk_CompLessEqual: {
    vrpe->valueRestriction.restriction = PEVRK_CompLessEqual;
    break;
  }
  default: {
    assert(t.kind == tk_CompEqual || t.kind == tk_CompNotEqual ||
           t.kind == tk_CompGreaterEqual || t.kind == tk_CompGreater ||
           t.kind == tk_CompLess || t.kind == tk_CompLessEqual);
    PANIC();
  }
  }
  vrpe->valueRestriction.valueExpr = ALLOC(parser->a, ValExpr);
  parseValExprTerm(vrpe->valueRestriction.valueExpr, diagnostics, parser);
  LnCol end = vrpe->valueRestriction.valueExpr->node.span.end;

  vrpe->node.span = SPAN(start, end);
  return;
}

static void certain_parseTypeRestrictionPatExpr(PatExpr *trpe,
                                                    Vector *diagnostics,
                                                    Parser *parser) {
  ZERO(trpe);

  trpe->kind = PEK_TypeRestriction;

  bool parseType = false;

  Token t = parse_next(parser, diagnostics);

  LnCol start = t.span.start;
  LnCol end;

  switch (t.kind) {
  // No binding created
  case tk_Colon: {
    trpe->typeRestriction.has_binding = false;
    parseType = true;
    end = t.span.end;
    break;
  }
  // Create a binding
  case tk_Identifier: {
    trpe->typeRestriction.has_binding = true;
    trpe->typeRestriction.binding = t.identifierToken.data;
    end = t.span.end;
    t = parse_peek(parser);
    if (t.kind == tk_Colon) {
      parseType = true;
      // advance through it
      t = parse_next(parser, diagnostics);
    } else {
      parseType = false;
    }
    break;
  }
  default: {
    assert(t.kind == tk_Colon || t.kind == tk_Identifier);
    PANIC();
  }
  }

  if (parseType) {
    trpe->typeRestriction.type = ALLOC(parser->a, TypeExpr);
    parseTypeExpr(trpe->typeRestriction.type, diagnostics, parser);
    end = t.span.end;
  } else {
    trpe->typeRestriction.type = ALLOC(parser->a, TypeExpr);
    trpe->typeRestriction.type->kind = TEK_Omitted;
    trpe->typeRestriction.type->node.span = SPAN(start, end);
    trpe->typeRestriction.type->comments_len = 0;
  }

  trpe->node.span = SPAN(start, end);
}

// pattern ':=' ('..' | identifier )
static void
parsePatternStructMemberExpr(struct PatternStructMemberExpr_s *psmep,
                             Vector *diagnostics, Parser *parser) {
  pushCommentScopeParser(parser);
  ZERO(psmep);

  // pattern
  psmep->pattern = ALLOC(parser->a, PatExpr);
  parsePatExpr(psmep->pattern, diagnostics, parser);

  LnCol start = psmep->pattern->node.span.end;
  LnCol end;

  // get define token
  Token t = parse_next(parser, diagnostics);
  if (t.kind != tk_Define) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_PatternStructExpectedDefine, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  // field value
  t = parse_next(parser, diagnostics);
  switch (t.kind) {
  case tk_Rest: {
    psmep->kind = PSMEK_Rest;
    break;
  }
  case tk_Identifier: {
    // copy identifier
    psmep->kind = PSMEK_Field;
    psmep->field.field = t.identifierToken.data;
    break;
  }
  default: {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_PatternStructExpectedIdentifier, t.span);
    end = t.span.end;
    goto CLEANUP;
  }
  }

  end = t.span.end;

CLEANUP:
  psmep->node.span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  psmep->comments_len = VEC_LEN(&comments, Comment);
  psmep->comments = vec_release(&comments);
  return;
}

static void certain_parseStructPatExpr(PatExpr *spe,
                                           Vector *diagnostics,
                                           Parser *parser) {
  ZERO(spe);
  spe->kind = PEK_Struct;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Struct);

  LnCol start = t.span.start;
  LnCol end;

  Vector members = vec_create(parser->a);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceLeft) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_PatternStructExpectedLeftBrace, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&members,                           // members_vec_ptr
             diagnostics,                        // diagnostics_vec_ptr
             parsePatternStructMemberExpr,       // member_parse_function
             struct PatternStructMemberExpr_s,   // member_kind
             tk_BraceRight,                      // delimiting_token_kind
             DK_PatternStructExpectedRightBrace, // missing_delimiter_error
             end,                                // end_lncol
             parser                              // parser
  )
CLEANUP:
  spe->structExpr.members_len =
      VEC_LEN(&members, struct PatternStructMemberExpr_s);
  spe->structExpr.members = vec_release(&members);
  spe->node.span = SPAN(start, end);
  return;
}

static void certain_parseGroupPatExpr(PatExpr *gpep,
                                          Vector *diagnostics, Parser *parser) {
  ZERO(gpep);
  gpep->kind = PEK_Group;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_BraceLeft);
  LnCol start = t.span.start;
  LnCol end;

  gpep->groupExpr.value = ALLOC(parser->a, PatExpr);
  parsePatExpr(gpep->groupExpr.value, diagnostics, parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceRight) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_PatternGroupExpectedRightBrace, t.span);
    end = t.span.end;
  } else {
    end = gpep->groupExpr.value->node.span.end;
  }

  gpep->node.span = SPAN(start, end);
}

static void parseL1PatExpr(PatExpr *l1, Vector *diagnostics,
                               Parser *parser) {
  pushCommentScopeParser(parser);

  Token t = parse_peek(parser);
  switch (t.kind) {
  case tk_BraceLeft: {
    certain_parseGroupPatExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Struct: {
    certain_parseStructPatExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Identifier:
  case tk_Colon: {
    certain_parseTypeRestrictionPatExpr(l1, diagnostics, parser);
    break;
  }
  case tk_CompEqual:
  case tk_CompNotEqual:
  case tk_CompGreaterEqual:
  case tk_CompGreater:
  case tk_CompLess:
  case tk_CompLessEqual: {
    certain_parseValueRestrictionPatExpr(l1, diagnostics, parser);
    break;
  }
  default: {
    l1->kind = PEK_None;
    l1->node.span = t.span;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_TypeExprUnexpectedToken, t.span);
    // Resync
   parse_resync(parser, diagnostics);
    break;
  }
  }

  Vector comments = popCommentScopeParser(parser);
  l1->comments_len = VEC_LEN(&comments, Comment);
  l1->comments = vec_release(&comments);
}

static void parseL2PatExpr(PatExpr *l2, Vector *diagnostics,
                               Parser *parser) {
  Token t = parse_peek(parser);
  switch (t.kind) {
  case tk_Not: {
    l2->unaryOp.operator= PEUOK_Not;
    break;
  }
  default: {
    // there is no level expression
    parseL1PatExpr(l2, diagnostics, parser);
    return;
  }
  }

  // this will only execute if an L3 operator exists
  l2->kind = PEK_UnaryOp;

  // first create comment scope and go through op
  pushCommentScopeParser(parser);
  t = parse_next(parser, diagnostics);

  // Now parse the rest of the expression
  l2->unaryOp.operand = ALLOC(parser->a, PatExpr);
  parseL2PatExpr(l2->unaryOp.operand, diagnostics, parser);

  // finally calculate the misc stuff
  l2->node.span = SPAN(t.span.start, l2->unaryOp.operand->node.span.end);

  // comments
  Vector comments = popCommentScopeParser(parser);
  l2->comments_len = VEC_LEN(&comments, Comment);
  l2->comments = vec_release(&comments);
}

static inline bool opDetL3PatExpr(tk_Kind tk,
                                      PatExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Tuple: {
    *val = PEBOK_Tuple;
    return true;
  }
  default: {
    // there is no level 4 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(PatExpr, PEK, 3, parseL2PatExpr)

static inline bool opDetL4PatExpr(tk_Kind tk,
                                      PatExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Union: {
    *val = PEBOK_Union;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(PatExpr, PEK, 4, parseL3PatExpr)

static inline bool opDetL5PatExpr(tk_Kind tk,
                                      PatExprBinaryOpKind *val) {
  switch (tk) {
  case tk_And: {
    *val = PEBOK_And;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(PatExpr, PEK, 5, parseL4PatExpr)

static inline bool opDetL6PatExpr(tk_Kind tk,
                                      PatExprBinaryOpKind *val) {
  switch (tk) {
  case tk_Or: {
    *val = PEBOK_Or;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(PatExpr, PEK, 6, parseL5PatExpr)

static void parsePatExpr(PatExpr *ppe, Vector *diagnostics,
                             Parser *parser) {
  parseL6PatExpr(ppe, diagnostics, parser);
}

static void parseValDecl(Stmnt *vdsp, Vector *diagnostics, Parser *parser) {
  // zero-initialize vdsp
  ZERO(vdsp);
  vdsp->kind = SK_ValDecl;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Val);
  LnCol start = t.span.start;

  // Get Binding
  vdsp->valDecl.pattern = ALLOC(parser->a, PatExpr);
  parsePatExpr(vdsp->valDecl.pattern, diagnostics, parser);

  LnCol end;

  // Expect define
  t = parse_peek(parser);
  if (t.kind == tk_Define) {
    // accept the define token
    parse_next(parser, diagnostics);

    vdsp->valDecl.has_value = true;
    vdsp->valDecl.value = ALLOC(parser->a, ValExpr);
    parseValExpr(vdsp->valDecl.value, diagnostics, parser);
    end = vdsp->valDecl.value->node.span.end;
  } else {
    vdsp->valDecl.has_value = false;
    end = vdsp->valDecl.pattern->node.span.end;
  }

  vdsp->node.span = SPAN(start, end);
}

static void parseTypeDecl(Stmnt *tdp, Vector *diagnostics, Parser *parser) {
  ZERO(tdp);
  tdp->kind = SK_TypeDecl;
  Token t = parse_next(parser, diagnostics);
  // enforce that next token is type
  assert(t.kind == tk_Type);
  LnCol start = t.span.start;

  LnCol end;

  // get.identifierToken.data of type decl
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Identifier) {
    tdp->typeDecl.name = NULL;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_TypeDeclExpectedIdentifier, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  tdp->typeDecl.name = t.identifierToken.data;

  // Now get define
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Define) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_TypeDeclExpectedDefine, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  tdp->typeDecl.type = ALLOC(parser->a, TypeExpr);
  parseTypeExpr(tdp->typeDecl.type, diagnostics, parser);
  end = tdp->typeDecl.type->node.span.end;

CLEANUP:
  tdp->node.span = SPAN(start, end);
  return;
}

static void certain_parseDeferStmnt(Stmnt *dsp, Vector *diagnostics,
                                    Parser *parser) {
  dsp->kind = SK_DeferStmnt;
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Defer);
  dsp->deferStmnt.value = ALLOC(parser->a, ValExpr);
  parseValExpr(dsp->deferStmnt.value, diagnostics, parser);
  dsp->node.span = SPAN(t.span.start, dsp->deferStmnt.value->node.span.end);
  return;
}

static void parseStmnt(Stmnt *sp, Vector *diagnostics, Parser *parser) {
  pushCommentScopeParser(parser);

  // peek next token
  Token t = parse_peek(parser);
  switch (t.kind) {
    // Macros
  case tk_Macro: {
    sp->kind = SK_Macro;
    sp->macroStmnt.name = t.macro_call;
    sp->node.span = t.span;
    break;
  }
  case tk_Use: {
    LnCol start = t.span.start;
    parse_next(parser, diagnostics); // drop use token
    sp->kind = SK_Use;
    sp->useStmnt.path = ALLOC(parser->a, Path);
    parsePath(sp->useStmnt.path, diagnostics, parser);
    sp->node.span = SPAN(start, sp->useStmnt.path->node.span.end);
    break;
  }
  case tk_Namespace: {
    LnCol start = t.span.start;
    parse_next(parser, diagnostics); // drop namespace token
    sp->kind = SK_Namespace;
    sp->namespaceStmnt.path = ALLOC(parser->a, Path);
    parsePath(sp->namespaceStmnt.path, diagnostics, parser);
    sp->namespaceStmnt.stmnt = ALLOC(parser->a, Stmnt);
    parseStmnt(sp->namespaceStmnt.stmnt, diagnostics, parser);
    sp->node.span = SPAN(start, sp->namespaceStmnt.stmnt->node.span.end);
    break;
  }
  // Val Stmnt (Decl)
  case tk_Val: {
    parseValDecl(sp, diagnostics, parser);
    break;
  }
  case tk_Type: {
    parseTypeDecl(sp, diagnostics, parser);
    break;
  }
  // Expressions
  default: {
    sp->kind = SK_ValExpr;
    sp->valExpr.value = ALLOC(parser->a, ValExpr);
    parseValExpr(sp->valExpr.value, diagnostics, parser);
    sp->node.span = sp->valExpr.value->node.span;
    break;
  }
  }
  Vector comments = popCommentScopeParser(parser);
  sp->comments_len = VEC_LEN(&comments, Comment);
  sp->comments = vec_release(&comments);
}

bool parse_nextStmntIfExists(Stmnt *s, Vector *diagnostics, Parser *parser) {
  Token t = parse_peek(parser);
  if (t.kind == tk_Eof) {
    return true;
  }
  parseStmnt(s, diagnostics, parser);
  return false;
}
