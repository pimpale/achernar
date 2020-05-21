#include "parseAst.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "error.h"
#include "json.h"
#include "lexer.h"
#include "token.h"
#include "vector.h"

// convoluted function to save repetitive tasks
#define PARSE_LIST(members_vec_ptr, diagnostics_vec_ptr,                       \
                   member_parse_function, member_kind, delimiting_token_kind,  \
                   missing_delimiter_error, end_lncol, parser)                 \
  while (true) {                                                               \
    Token PARSE_LIST_token;                                                    \
    peekTokenParser(parser, &PARSE_LIST_token);                                \
    if (PARSE_LIST_token.kind == delimiting_token_kind) {                      \
      end_lncol = PARSE_LIST_token.span.end;                                   \
      nextTokenParser(parser, &PARSE_LIST_token); /* accept delimiting tk */   \
      break;                                                                   \
    } else if (PARSE_LIST_token.kind == TK_None &&                             \
               PARSE_LIST_token.error == DK_EOF) {                             \
      *VEC_PUSH(diagnostics_vec_ptr, Diagnostic) =                             \
          DIAGNOSTIC(missing_delimiter_error, PARSE_LIST_token.span);          \
      end_lncol = PARSE_LIST_token.span.end;                                   \
      break;                                                                   \
    }                                                                          \
    /* if there wasn't an end delimiter, push the last token back */           \
    member_parse_function(VEC_PUSH(members_vec_ptr, member_kind), parser);     \
  }

// Parser
void createParser(Parser *pp, Lexer *lp, Arena *ar) {
  pp->lexer = lp;
  pp->ar = ar;
  createVector(&pp->comments);
  createVector(&pp->next_comments_stack);
  createVector(&pp->next_tokens_stack);
  pp->paren_depth = 0;
  pp->brace_depth = 0;
  pp->bracket_depth = 0;
}

/// gets the next token, ignoring buffering
static void rawNextTokenParser(Parser *parser, Token *t, Vector *comments) {
  Token c;
  while (true) {
    lexNextToken(parser->lexer, &c);
    switch (c.kind) {
    case TK_Comment: {
      *VEC_PUSH(comments, Comment) = (Comment){
          .span = c.span, .scope = c.comment.scope, .data = c.comment.comment};
      // keep reading
      break;
    }
    case TK_ParenLeft: {
      parser->paren_depth++;
      goto RETURN;
    }
    case TK_ParenRight: {
      parser->paren_depth--;
      goto RETURN;
    }
    case TK_BraceLeft: {
      parser->brace_depth++;
      goto RETURN;
    }
    case TK_BraceRight: {
      parser->brace_depth--;
      goto RETURN;
    }
    case TK_BracketLeft: {
      parser->bracket_depth++;
      goto RETURN;
    }
    case TK_BracketRight: {
      parser->bracket_depth--;
      goto RETURN;
    }
    default: {
      goto RETURN;
    }
    }
  }
RETURN:
  *t = c;
}

// If the peeked token stack is not empty:
//    Return the top element of the top of the token
//    Pop the top of the next_comments_stack
//    For each element in the next comments stack, push it to the top of the
//    current scope
// If has an ungotten token, return that. Else return next in line, and cache it
static void nextTokenParser(Parser *pp, Token *t) {
  // the current scope we aim to push the comments to
  size_t top_index = VEC_LEN(&pp->comments, Vector);
  assert(top_index > 0);
  Vector *current_scope = VEC_GET(&pp->comments, top_index - 1, Vector);
  if (VEC_LEN(&pp->next_tokens_stack, Token) != 0) {
    // set the token
    VEC_POP(&pp->next_tokens_stack, t, Token);

    // now merge the next comments
    // Vector containing all tokens for the next one
    Vector next_token_comments;
    VEC_POP(&pp->next_comments_stack, &next_token_comments, Vector);

    // pop everythin from next_token_comments into current_scope
    size_t next_comments_len = lengthVector(&next_token_comments);
    popVector(&next_token_comments,
              pushVector(current_scope, next_comments_len), next_comments_len);
    // free next_token_comments
    destroyVector(&next_token_comments);
  } else {
    rawNextTokenParser(pp, t, current_scope);
  }
}

// gets the k'th token
// K must be greater than 0
static void peekNthTokenParser(Parser *pp, Token *t, size_t k) {
  assert(k > 0);
  for (size_t i = VEC_LEN(&pp->next_tokens_stack, Token); i < k; i++) {
    // allocate mem for next token
    Token *next_token = VEC_PUSH(&pp->next_tokens_stack, Token);
    // Create vector to store any comments
    Vector *next_token_comments = VEC_PUSH(&pp->next_comments_stack, Vector);
    createWithCapacityVector(next_token_comments, 0);
    // parse the token
    rawNextTokenParser(pp, next_token, next_token_comments);
  }
  // return token
  *t = *VEC_GET(&pp->next_tokens_stack, k - 1, Token);
}

static void peekTokenParser(Parser *parser, Token *t) {
  peekNthTokenParser(parser, t, 1);
}

// pops the top comment scope off the comment stack
/// REQUIRES: `parser` is pointer to valid Parser
/// REQUIRES: the stack has at least one member
/// GUARANTEES: return value is the topmost element of the comment stack
/// GUARANTEES: the topmost element fo the comment stack has been removed
static Vector popCommentScopeParser(Parser *parser) {
  assert(VEC_LEN(&parser->comments, Vector) >= 1);
  Vector v;
  VEC_POP(&parser->comments, &v, Vector);
  return v;
}

// pushes a new empty comment scope to the top of the comment stack
/// REQUIRES: `parser` is pointer to valid Parser
/// GUARANTEES: `parser`'s comment stack has new empty scope on top of stack
static void pushCommentScopeParser(Parser *parser) {
  // We create the vector with zero capacity initially so that
  // allocation is deferred until we actually encounter a comment
  // Most scopes will not have a comment
  Vector *v = VEC_PUSH(&parser->comments, Vector);
  createWithCapacityVector(v, 0);
}

Arena *releaseParser(Parser *pp) {
  for (size_t i = 0; i < VEC_LEN(&pp->next_comments_stack, Vector); i++) {
    destroyVector(VEC_GET(&pp->next_comments_stack, i, Vector));
  }
  destroyVector(&pp->next_comments_stack);
  destroyVector(&pp->next_tokens_stack);

  // if vec len is not 0, then we internal error
  assert(VEC_LEN(&pp->comments, Vector) == 0);
  destroyVector(&pp->comments);
  return pp->ar;
}

// Heuristic to resync the parser after reaching a syntax error
// Continues reading tokens until we jump out of a paren, brace, or bracket
// group discards any comments and tokens found
static void resyncParser(Parser *parser) {
  Token t;
  Vector comments;
  createVector(&comments);
  int64_t initial_paren_depth = parser->paren_depth;
  int64_t initial_brace_depth = parser->brace_depth;
  int64_t initial_bracket_depth = parser->bracket_depth;
  while (true) {
    if (initial_brace_depth <= parser->brace_depth &&
        initial_paren_depth <= parser->paren_depth &&
        initial_bracket_depth <= parser->bracket_depth) {
      break;
    }
    rawNextTokenParser(parser, &t, &comments);
  }
  destroyVector(&comments);
}

// Note that all errors resync at the statement level
static void parseStmnt(Stmnt *sp, Parser *parser);
static void parseValueExpr(ValueExpr *vep, Parser *parser);
static void parseTypeExpr(TypeExpr *tep, Parser *parser);
static void parsePatternExpr(PatternExpr *pp, Parser *parser);

static void certain_parseBuiltin(Builtin *bp, Parser *parser) {
  ZERO(bp);
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_Builtin);

  LnCol start = t.span.start;
  LnCol end;

  bp->name = t.builtin;

  Vector diagnostics;
  createVector(&diagnostics);

  Vector parameters;
  createVector(&parameters);

  nextTokenParser(parser, &t);
  if (t.kind != TK_ParenLeft) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_BuiltinExpectedLeftParen, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&parameters,                  // members_vec_ptr
             &diagnostics,                 // diagnostics_vec_ptr
             parseStmnt,                   // member_parse_function
             Stmnt,                        // member_kind
             TK_ParenRight,                // delimiting_token_kind
             DK_BuiltinExpectedRightParen, // missing_delimiter_error
             end,                          // end_lncol
             parser                        // parser
  )

CLEANUP:
  bp->parameters_len = VEC_LEN(&parameters, Stmnt);
  bp->parameters = manageMemArena(parser->ar, releaseVector(&parameters));

  bp->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  bp->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  bp->span = SPAN(start, end);
}

static void parsePath(Path *pp, Parser *parser) {
  // start comment scope
  pushCommentScopeParser(parser);

  Token t;
  nextTokenParser(parser, &t);
  // start and finish
  LnCol start = t.span.start;
  LnCol end;

  // diagnostics (temp ok value)
  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  Vector pathSegments;
  createVector(&pathSegments);

  // ensure that it is a valid path
  if (t.kind != TK_Identifier) {
    diagnostic = DIAGNOSTIC(DK_PathExpectedIdentifier, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  *VEC_PUSH(&pathSegments, char *) = t.identifier;

  while (true) {
    peekTokenParser(parser, &t);
    if (t.kind == TK_ScopeResolution) {
      // discard the scope resolution
      nextTokenParser(parser, &t);
      // now check if we have an issue
      nextTokenParser(parser, &t);
      if (t.kind != TK_Identifier) {
        diagnostic = DIAGNOSTIC(DK_PathExpectedIdentifier, t.span);
        end = t.span.end;
        goto CLEANUP;
      }

      *VEC_PUSH(&pathSegments, char *) = t.identifier;
    } else {
      // we've reached the end of the path
      end = t.span.end;
      break;
    }
  }

CLEANUP:
  pp->pathSegments_len = VEC_LEN(&pathSegments, char *);
  pp->pathSegments = manageMemArena(parser->ar, releaseVector(&pathSegments));

  if (diagnostic.kind != DK_Ok) {
    pp->diagnostics_len = 1;
    pp->diagnostics = RALLOC(parser->ar, Diagnostic);
    pp->diagnostics[0] = diagnostic;
  } else {
    pp->diagnostics_len = 0;
    pp->diagnostics = NULL;
  }
  pp->span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  pp->comments_len = VEC_LEN(&comments, Comment);
  pp->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void certain_parseVoidValueExpr(ValueExpr *vep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_Void);
  vep->kind = VEK_VoidLiteral;
  vep->span = t.span;
  vep->diagnostics_len = 0;
  return;
}

static void certain_parseIntValueExpr(ValueExpr *vep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_Int);
  vep->kind = VEK_IntLiteral;
  vep->intLiteral.value = t.int_literal;
  vep->span = t.span;
  vep->diagnostics_len = 0;
  return;
}

static void certain_parseBoolValueExpr(ValueExpr *vep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_Bool);
  vep->kind = VEK_BoolLiteral;
  vep->boolLiteral.value = t.bool_literal;
  vep->span = t.span;
  vep->diagnostics_len = 0;
  return;
}

static void certain_parseFloatValueExpr(ValueExpr *vep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_Float);
  vep->kind = VEK_FloatLiteral;
  vep->floatLiteral.value = t.float_literal;
  vep->span = t.span;
  vep->diagnostics_len = 0;
  return;
}

static void certain_parseCharValueExpr(ValueExpr *vep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_Char);
  vep->kind = VEK_CharLiteral;
  vep->charLiteral.value = t.char_literal;
  vep->span = t.span;
  vep->diagnostics_len = 0;
  return;
}

static void certain_parseStringValueExpr(ValueExpr *svep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_String);
  svep->kind = VEK_StringLiteral;
  svep->stringLiteral.value = t.string_literal;
  svep->span = t.span;
  svep->diagnostics_len = 0;
  return;
}

static void certain_parseFnValueExpr(ValueExpr *fvep, Parser *parser) {
  ZERO(fvep);

  fvep->kind = VEK_Fn;

  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_Fn);
  LnCol start = t.span.start;
  LnCol end = t.span.end;

  Vector diagnostics;
  createVector(&diagnostics);

  // check for leftparen
  nextTokenParser(parser, &t);
  Span lparenspan = t.span;
  if (t.kind != TK_ParenLeft) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnValueExprExpectedLeftParen, t.span);
    goto CLEANUP;
  }

  Vector parameters;
  createVector(&parameters);

  PARSE_LIST(&parameters,                      // members_vec_ptr
             &diagnostics,                     // diagnostics_vec_ptr
             parsePatternExpr,                 // member_parse_function
             PatternExpr,                      // member_kind
             TK_ParenRight,                    // delimiting_token_kind
             DK_FnValueExprExpectedRightParen, // missing_delimiter_error
             end,                              // end_lncol
             parser                            // parser
  )

  fvep->fnExpr.parameters_len = VEC_LEN(&parameters, PatternExpr);
  fvep->fnExpr.parameters =
      manageMemArena(parser->ar, releaseVector(&parameters));

  peekTokenParser(parser, &t);
  if (t.kind == TK_Colon) {
    fvep->fnExpr.type = RALLOC(parser->ar, TypeExpr);
    // advance
    nextTokenParser(parser, &t);

    parseTypeExpr(fvep->fnExpr.type, parser);
  } else {
    fvep->fnExpr.type = RALLOC(parser->ar, TypeExpr);
    fvep->fnExpr.type->kind = TEK_Omitted;
    fvep->fnExpr.type->span = lparenspan;
    fvep->fnExpr.type->diagnostics_len = 0;
    fvep->fnExpr.type->comments_len = 0;
  }

  nextTokenParser(parser, &t);

  if (t.kind != TK_Arrow) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnValueExprExpectedArrow, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  fvep->fnExpr.body = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(fvep->fnExpr.body, parser);
  end = fvep->fnExpr.body->span.end;

CLEANUP:
  fvep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  fvep->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  fvep->span = SPAN(start, end);
}

static void certain_parseBlockValueExpr(ValueExpr *bvep, Parser *parser) {
  ZERO(bvep);
  bvep->kind = VEK_Block;

  Token t;
  nextTokenParser(parser, &t);

  LnCol start = t.span.start;

  // blocks may be labeled
  if (t.kind == TK_Label) {
    bvep->blockExpr.has_label = true;
    bvep->blockExpr.label = t.label;
    nextTokenParser(parser, &t);
  } else {
    bvep->blockExpr.has_label = false;
  }

  assert(t.kind == TK_BraceLeft);

  // Create list of statements
  Vector statements;
  createVector(&statements);

  // List of diagnostics
  Vector diagnostics;
  createVector(&diagnostics);

  LnCol end;

  PARSE_LIST(&statements,                // members_vec_ptr
             &diagnostics,               // diagnostics_vec_ptr
             parseStmnt,                 // member_parse_function
             Stmnt,                      // member_kind
             TK_BraceRight,              // delimiting_token_kind
             DK_BlockExpectedRightBrace, // missing_delimiter_error
             end,                        // end_lncol
             parser                      // parser
  )

  bvep->blockExpr.statements_len = VEC_LEN(&statements, Stmnt);
  bvep->blockExpr.statements =
      manageMemArena(parser->ar, releaseVector(&statements));
  bvep->span = SPAN(start, end);
  bvep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  bvep->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  return;
}

static void certain_parseBuiltinValueExpr(ValueExpr *bvep, Parser *parser) {
  bvep->kind = VEK_Builtin;
  bvep->builtinExpr.builtin = RALLOC(parser->ar, Builtin);
  certain_parseBuiltin(bvep->builtinExpr.builtin, parser);
  bvep->diagnostics_len = 0;
  bvep->span = bvep->builtinExpr.builtin->span;
}

static void certain_parseDeferValueExpr(ValueExpr *dep, Parser *parser) {
  dep->kind = VEK_Defer;
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_Defer);
  dep->deferExpr.value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(dep->deferExpr.value, parser);
  dep->span = SPAN(t.span.start, dep->deferExpr.value->span.end);
  dep->diagnostics_len = 0;
  return;
}

static void certain_parseReturnValueExpr(ValueExpr *rep, Parser *parser) {
  ZERO(rep);
  rep->kind = VEK_Return;
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_Return);

  LnCol start = t.span.start;
  LnCol end;

  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  nextTokenParser(parser, &t);
  if (t.kind != TK_Label) {
    diagnostic = DIAGNOSTIC(DK_ReturnExpectedLabel, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  rep->returnExpr.value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(rep->returnExpr.value, parser);
  end = rep->returnExpr.value->span.end;

CLEANUP:
  if (diagnostic.kind == DK_Ok) {
    rep->diagnostics = RALLOC(parser->ar, Diagnostic);
    rep->diagnostics[0] = diagnostic;
    rep->diagnostics_len = 1;
  } else {
    rep->diagnostics_len = 0;
  }
  rep->span = SPAN(start, end);
  return;
}

static void certain_parseContinueValueExpr(ValueExpr *cep, Parser *parser) {
  ZERO(cep);
  cep->kind = VEK_Continue;
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_Continue);

  LnCol start = t.span.start;
  LnCol end;

  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  nextTokenParser(parser, &t);
  end = t.span.end;

  if (t.kind != TK_Label) {
    diagnostic = DIAGNOSTIC(DK_ContinueExpectedLabel, t.span);
    goto CLEANUP;
  }

CLEANUP:
  if (diagnostic.kind == DK_Ok) {
    cep->diagnostics = RALLOC(parser->ar, Diagnostic);
    cep->diagnostics[0] = diagnostic;
    cep->diagnostics_len = 1;
  } else {
    cep->diagnostics_len = 0;
  }
  cep->span = SPAN(start, end);
  return;
}

static void certain_parseLoopValueExpr(ValueExpr *lep, Parser *parser) {
  lep->kind = VEK_Loop;
  Token t;
  nextTokenParser(parser, &t);
  LnCol start = t.span.start;

  if (t.kind == TK_Label) {
    lep->loopExpr.has_label = true;
    lep->loopExpr.label = t.label;
    nextTokenParser(parser, &t);
  } else {
    lep->loopExpr.has_label = false;
  }

  assert(t.kind == TK_Loop);

  lep->loopExpr.value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(lep->loopExpr.value, parser);

  lep->span = SPAN(start, lep->loopExpr.value->span.end);
  lep->diagnostics_len = 0;
  return;
}

// Pattern : Expr,
static void parseMatchCaseExpr(struct MatchCaseExpr_s *mcep, Parser *parser) {
  pushCommentScopeParser(parser);
  ZERO(mcep);
  Token t;

  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  // Get Pat
  nextTokenParser(parser, &t);
  LnCol start = t.span.start;
  LnCol end;
  if (t.kind != TK_Pat) {
    diagnostic = DIAGNOSTIC(DK_MatchCaseNoPat, t.span);
    start = t.span.end;
    end = t.span.end;
    goto CLEANUP;
  }

  // Get pattern
  mcep->pattern = RALLOC(parser->ar, PatternExpr);
  parsePatternExpr(mcep->pattern, parser);

  end = mcep->pattern->span.end;

  // Expect colon
  nextTokenParser(parser, &t);
  if (t.kind != TK_Arrow) {
    diagnostic = DIAGNOSTIC(DK_MatchCaseNoArrow, t.span);
    resyncParser(parser);
    goto CLEANUP;
  }

  // Get Value
  mcep->value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(mcep->value, parser);
  end = mcep->value->span.end;

CLEANUP:
  if (diagnostic.kind != DK_Ok) {
    mcep->diagnostics_len = 1;
    mcep->diagnostics = RALLOC(parser->ar, Diagnostic);
    mcep->diagnostics[0] = diagnostic;
  } else {
    mcep->diagnostics_len = 0;
  }

  mcep->span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  mcep->comments_len = VEC_LEN(&comments, Comment);
  mcep->comments = manageMemArena(parser->ar, releaseVector(&comments));

  return;
}

static void certain_parseMatchValueExpr(ValueExpr *mvep, Parser *parser) {
  ZERO(mvep);
  mvep->kind = VEK_Match;
  Token t;
  nextTokenParser(parser, &t);
  LnCol start = t.span.start;

  if (t.kind == TK_Label) {
    mvep->matchExpr.has_label = true;
    mvep->matchExpr.label = t.label;
    nextTokenParser(parser, &t);
  } else {
    mvep->matchExpr.has_label = false;
  }

  assert(t.kind == TK_Match);

  // Get expression to match against
  mvep->matchExpr.value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(mvep->matchExpr.value, parser);
  // now we must parse the block containing the cases

  Vector cases;
  createVector(&cases);

  Vector diagnostics;
  createVector(&diagnostics);

  LnCol end;

  // Expect beginning brace
  nextTokenParser(parser, &t);

  if (t.kind != TK_BraceLeft) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_MatchNoLeftBrace, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&cases,                 // members_vec_ptr
             &diagnostics,           // diagnostics_vec_ptr
             parseMatchCaseExpr,     // member_parse_function
             struct MatchCaseExpr_s, // member_kind
             TK_BraceRight,          // delimiting_token_kind
             DK_MatchNoRightBrace,   // missing_delimiter_error
             end,                    // end_lncol
             parser                  // parser
  )

CLEANUP:
  // Get interior cases
  mvep->matchExpr.cases_len = VEC_LEN(&cases, struct MatchCaseExpr_s);
  mvep->matchExpr.cases = manageMemArena(parser->ar, releaseVector(&cases));

  mvep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  mvep->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  mvep->span = SPAN(start, end);
  return;
}

static void parseReferenceValueExpr(ValueExpr *rvep, Parser *parser) {
  ZERO(rvep);
  rvep->kind = VEK_Reference;
  rvep->reference.path = RALLOC(parser->ar, Path);
  parsePath(rvep->reference.path, parser);
  rvep->span = rvep->reference.path->span;
  rvep->diagnostics_len = 0;
  return;
}

// field = Value
static void parseValueStructMemberExpr(struct ValueStructMemberExpr_s *vsmep,
                                       Parser *parser) {
  // zero-initialize bp
  ZERO(vsmep);
  pushCommentScopeParser(parser);

  Token t;

  // diagnostics
  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  LnCol start;
  LnCol end;

  // get identifier
  nextTokenParser(parser, &t);

  Span identitySpan = t.span;

  start = identitySpan.start;

  if (t.kind != TK_Identifier) {
    vsmep->name = NULL;
    vsmep->value = NULL;
    diagnostic = DIAGNOSTIC(DK_StructMemberLiteralExpectedIdentifier, t.span);
    end = identitySpan.end;
    goto CLEANUP;
  }

  vsmep->name = t.identifier;

  // check if assign
  nextTokenParser(parser, &t);
  if (t.kind == TK_Colon) {
    // Get value of variable
    vsmep->value = RALLOC(parser->ar, ValueExpr);
    parseValueExpr(vsmep->value, parser);
    end = vsmep->value->span.end;
  } else {
    diagnostic = DIAGNOSTIC(DK_StructMemberLiteralExpectedIdentifier, t.span);
    vsmep->value = NULL;
    end = t.span.end;
    goto CLEANUP;
  }

CLEANUP:
  vsmep->span = SPAN(start, end);

  if (diagnostic.kind != DK_Ok) {
    vsmep->diagnostics_len = 1;
    vsmep->diagnostics = RALLOC(parser->ar, Diagnostic);
    vsmep->diagnostics[0] = diagnostic;
  } else {
    vsmep->diagnostics_len = 0;
    vsmep->diagnostics = NULL;
  }

  Vector comments = popCommentScopeParser(parser);
  vsmep->comments_len = VEC_LEN(&comments, Comment);
  vsmep->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

static void certain_parseValueStructExpr(ValueExpr *sve, Parser *parser) {
  ZERO(sve);
  sve->kind = VEK_StructLiteral;
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_Struct);

  LnCol start = t.span.start;

  Vector members;
  createVector(&members);

  Vector diagnostics;
  createVector(&diagnostics);

  LnCol end;

  nextTokenParser(parser, &t);
  if (t.kind != TK_BraceLeft) {
    end = t.span.end;
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_StructLiteralExpectedLeftBrace, t.span);
    goto CLEANUP;
  }

  PARSE_LIST(&members,                           // members_vec_ptr
             &diagnostics,                       // diagnostics_vec_ptr
             parseValueStructMemberExpr,         // member_parse_function
             struct ValueStructMemberExpr_s,     // member_kind
             TK_BraceRight,                      // delimiting_token_kind
             DK_StructLiteralExpectedRightBrace, // missing_delimiter_error
             end,                                // end_lncol
             parser                              // parser
  )

CLEANUP:
  sve->structExpr.members_len =
      VEC_LEN(&members, struct ValueStructMemberExpr_s);
  sve->structExpr.members = manageMemArena(parser->ar, releaseVector(&members));
  sve->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  sve->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  sve->span = SPAN(start, end);
  return;
}

// Level1ValueExpr parentheses, braces, literals
// Level2ValueExpr as () [] & @ . -> (postfixes)
// Level3ValueExpr - + ! (prefixes)
// Level4ValueExpr -> (pipeline)
// Level5ValueExpr * / % (multiplication and division)
// Level6ValueExpr + - (addition and subtraction)
// Level7ValueExpr < <= > >= == != (comparators)
// Level8ValueExpr && (logical and)
// Level9ValueExpr || (logical or)
// Level10ValueExpr , (create tuple)
// Level11ValueExpr = += -= *= /= %= (Assignment)

static void parseL1ValueExpr(ValueExpr *l1, Parser *parser) {
  pushCommentScopeParser(parser);

  Token t;
  peekTokenParser(parser, &t);
  // Decide which expression it is
  switch (t.kind) {
  // Literals
  case TK_Int: {
    certain_parseIntValueExpr(l1, parser);
    break;
  }
  case TK_Bool: {
    certain_parseBoolValueExpr(l1, parser);
    break;
  }
  case TK_Float: {
    certain_parseFloatValueExpr(l1, parser);
    break;
  }
  case TK_Char: {
    certain_parseCharValueExpr(l1, parser);
    break;
  }
  case TK_Void: {
    certain_parseVoidValueExpr(l1, parser);
    break;
  }
  case TK_Builtin: {
    certain_parseBuiltinValueExpr(l1, parser);
    break;
  }
  case TK_String: {
    certain_parseStringValueExpr(l1, parser);
    break;
  }
  case TK_BraceLeft: {
    certain_parseBlockValueExpr(l1, parser);
    break;
  }
  case TK_Fn: {
    certain_parseFnValueExpr(l1, parser);
    break;
  }
  case TK_Struct: {
    certain_parseValueStructExpr(l1, parser);
    break;
  }
  case TK_Defer: {
    certain_parseDeferValueExpr(l1, parser);
    break;
  }
  case TK_Continue: {
    certain_parseContinueValueExpr(l1, parser);
    break;
  }
  case TK_Return: {
    certain_parseReturnValueExpr(l1, parser);
    break;
  }
  case TK_Loop: {
    certain_parseLoopValueExpr(l1, parser);
    break;
  }
  case TK_Match: {
    certain_parseMatchValueExpr(l1, parser);
    break;
  }
  case TK_Identifier: {
    parseReferenceValueExpr(l1, parser);
    break;
  }
  case TK_None: {
    // put the token error in the value expression.
    ZERO(l1);
    l1->kind = VEK_None;
    l1->span = t.span;
    l1->diagnostics_len = 1;
    l1->diagnostics = RALLOC(parser->ar, Diagnostic);
    l1->diagnostics[0] = DIAGNOSTIC(t.error, t.span);
    // discard this token
    nextTokenParser(parser, &t);
    break;
  }
  default: {
    logInternalError(__LINE__, __func__, "unimplemented: %d at %d, %d", t.kind,
                     t.span.start.ln, t.span.start.col);
    PANIC();
  }
  }
  Vector comments = popCommentScopeParser(parser);
  l1->comments_len = VEC_LEN(&comments, Comment);
  l1->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseFieldAccessValueExpr(ValueExpr *fave, Parser *parser,
                                      ValueExpr *root) {
  ZERO(fave);
  fave->kind = VEK_FieldAccess;
  fave->fieldAccess.value = root;

  Token t;

  nextTokenParser(parser, &t);
  if (t.kind != TK_FieldAccess) {
    INTERNAL_ERROR("expected field access operator");
    PANIC();
  }

  // Now we get the field
  peekTokenParser(parser, &t);
  if (t.kind != TK_Identifier) {
    // it is possible we encounter an error
    fave->fieldAccess.field = NULL;
    fave->diagnostics_len = 1;
    fave->diagnostics = RALLOC(parser->ar, Diagnostic);
    fave->diagnostics[0] = DIAGNOSTIC(DK_FieldAccessExpectedIdentifier, t.span);
  } else {
    fave->fieldAccess.field = t.identifier;
    fave->diagnostics_len = 0;
  }

  fave->span = SPAN(root->span.start, t.span.end);
}

static void parseCallValueExpr(ValueExpr *cvep, Parser *parser,
                               ValueExpr *root) {
  ZERO(cvep);

  cvep->kind = VEK_Call;
  cvep->callExpr.function = root;

  Token t;
  nextTokenParser(parser, &t);
  if (t.kind != TK_ParenLeft) {
    INTERNAL_ERROR("called call value expression parser where there was none");
    PANIC();
  }

  LnCol end;

  Vector parameters;
  createVector(&parameters);
  Vector diagnostics;
  createVector(&diagnostics);

  PARSE_LIST(&parameters,          // members_vec_ptr
             &diagnostics,         // diagnostics_vec_ptr
             parseValueExpr,       // member_parse_function
             ValueExpr,            // member_kind
             TK_ParenRight,        // delimiting_token_kind
             DK_CallExpectedParen, // missing_delimiter_error
             end,                  // end_lncol
             parser                // parser
  )

  cvep->callExpr.parameters_len = VEC_LEN(&parameters, ValueExpr);
  cvep->callExpr.parameters =
      manageMemArena(parser->ar, releaseVector(&parameters));

  cvep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  cvep->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  cvep->span = SPAN(root->span.start, end);
}

static void parseAsValueExpr(ValueExpr *avep, Parser *parser, ValueExpr *root) {
  ZERO(avep);
  avep->kind = VEK_As;
  avep->asExpr.value = root;

  Token t;
  nextTokenParser(parser, &t);
  if (t.kind != TK_As) {
    INTERNAL_ERROR("called as value expression parser where there was none");
    PANIC();
  }

  avep->asExpr.type = RALLOC(parser->ar, TypeExpr);
  parseTypeExpr(avep->asExpr.type, parser);
  avep->diagnostics_len = 0;
}

static void parseL2ValueExpr(ValueExpr *l2, Parser *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff

  ValueExpr *root = l2;
  parseL1ValueExpr(root, parser);

  while (true) {
    Token t;
    // represents the old operation
    ValueExpr *v;

    peekTokenParser(parser, &t);
    switch (t.kind) {
    case TK_Ref: {
      v = RALLOC(parser->ar, ValueExpr);
      *v = *root;
      pushCommentScopeParser(parser);
      root->kind = VEK_UnaryOp;
      root->unaryOp.operator= VEUOK_Ref;
      root->unaryOp.operand = v;
      root->span = SPAN(v->span.start, t.span.end);
      root->diagnostics_len = 0;
      nextTokenParser(parser, &t);
      break;
    }
    case TK_Deref: {
      pushCommentScopeParser(parser);
      v = RALLOC(parser->ar, ValueExpr);
      *v = *root;
      root->kind = VEK_UnaryOp;
      root->unaryOp.operator= VEUOK_Deref;
      root->unaryOp.operand = v;
      root->span = SPAN(v->span.start, t.span.end);
      root->diagnostics_len = 0;
      nextTokenParser(parser, &t);
      break;
    }
    case TK_FieldAccess: {
      pushCommentScopeParser(parser);
      v = RALLOC(parser->ar, ValueExpr);
      *v = *root;
      parseFieldAccessValueExpr(root, parser, v);
      break;
    }
    case TK_ParenLeft: {
      pushCommentScopeParser(parser);
      v = RALLOC(parser->ar, ValueExpr);
      *v = *root;
      parseCallValueExpr(root, parser, v);
      break;
    }
    case TK_As: {
      pushCommentScopeParser(parser);
      v = RALLOC(parser->ar, ValueExpr);
      *v = *root;
      parseAsValueExpr(root, parser, v);
      break;
    }
    default: {
      // there are no more level 2 expressions
      return;
    }
    }

    Vector comments = popCommentScopeParser(parser);
    root->comments_len = VEC_LEN(&comments, Comment);
    root->comments = manageMemArena(parser->ar, releaseVector(&comments));
  }
}

static void parseL3ValueExpr(ValueExpr *l3, Parser *parser) {
  Token t;
  peekTokenParser(parser, &t);
  switch (t.kind) {
  case TK_Sub: {
    l3->unaryOp.operator= VEUOK_Negate;
    break;
  }
  case TK_Add: {
    l3->unaryOp.operator= VEUOK_Posit;
    break;
  }
  case TK_Not: {
    l3->unaryOp.operator= VEUOK_Not;
    break;
  }
  default: {
    // there is no level 3 expression
    parseL2ValueExpr(l3, parser);
    return;
  }
  }

  // this will only execute if an L3 operator exists
  l3->kind = VEK_UnaryOp;

  // first create comment scope and go through op
  pushCommentScopeParser(parser);
  nextTokenParser(parser, &t);

  // Now parse the rest of the expression
  l3->unaryOp.operand = RALLOC(parser->ar, ValueExpr);
  parseL3ValueExpr(l3->unaryOp.operand, parser);

  // finally calculate the misc stuff
  l3->span = SPAN(t.span.start, l3->unaryOp.operand->span.end);
  l3->diagnostics_len = 0;

  // comments
  Vector comments = popCommentScopeParser(parser);
  l3->comments_len = VEC_LEN(&comments, Comment);
  l3->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

// type is the type of object that the generated function will parse
// x is the index level of the function
// lower_fn is the name of the function that will be called to evaluate the left
// and right op_det_fn is the name of the function that determines the binary
// operator this function should take a pointer to the type and return a bool if
// successful
#define FN_BINOP_PARSE_LX_EXPR(type, type_shorthand, x, lower_fn)              \
  static void parseL##x##type(type *l##x, Parser *parser) {                    \
    type v;                                                                    \
    lower_fn(&v, parser);                                                      \
                                                                               \
    Token t;                                                                   \
    peekTokenParser(parser, &t);                                               \
    bool success = opDetL##x##type(t.kind, &l##x->binaryOp.operator);          \
    if (!success) {                                                            \
      /* there is no level x expression */                                     \
      *l##x = v;                                                               \
      return;                                                                  \
    }                                                                          \
    /* this will only execute if the operator exists */                        \
    l##x->kind = type_shorthand##_BinaryOp;                                    \
                                                                               \
    /* set the left side */                                                    \
    l##x->binaryOp.left_operand = RALLOC(parser->ar, type);                    \
    *l##x->binaryOp.left_operand = v;                                          \
                                                                               \
    /* first create comment scope and go through operator */                   \
    pushCommentScopeParser(parser);                                            \
    nextTokenParser(parser, &t);                                               \
                                                                               \
    /* now parse the rest of the expression */                                 \
    l##x->binaryOp.right_operand = RALLOC(parser->ar, type);                   \
    parseL##x##type(l##x->binaryOp.right_operand, parser);                     \
                                                                               \
    /* calculate misc stuff */                                                 \
    l##x->span = SPAN(l##x->binaryOp.left_operand->span.start,                 \
                      l##x->binaryOp.right_operand->span.end);                 \
    l##x->diagnostics_len = 0;                                                 \
                                                                               \
    /* comments */                                                             \
    Vector comments = popCommentScopeParser(parser);                           \
    l##x->comments_len = VEC_LEN(&comments, Comment);                          \
    l##x->comments = manageMemArena(parser->ar, releaseVector(&comments));     \
    return;                                                                    \
  }

static inline bool opDetL4ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Pipe: {
    *val = VEBOK_Pipeline;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 4, parseL3ValueExpr)

// Parses a single term that will not collide with patterns
static inline void parseValueExprTerm(ValueExpr *term, Parser *parser) {
  parseL4ValueExpr(term, parser);
}

static inline bool opDetL5ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Mul: {
    *val = VEBOK_Mul;
    return true;
  }
  case TK_Div: {
    *val = VEBOK_Div;
    return true;
  }
  case TK_Mod: {
    *val = VEBOK_Mod;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 5, parseL4ValueExpr)

static inline bool opDetL6ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Add: {
    *val = VEBOK_Add;
    return true;
  }
  case TK_Sub: {
    *val = VEBOK_Sub;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 6, parseL5ValueExpr)

static inline bool opDetL7ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_CompLess: {
    *val = VEBOK_CompLess;
    return true;
  }
  case TK_CompGreater: {
    *val = VEBOK_CompGreater;
    return true;
  }
  case TK_CompLessEqual: {
    *val = VEBOK_CompLessEqual;
    return true;
  }
  case TK_CompGreaterEqual: {
    *val = VEBOK_CompGreaterEqual;
    return true;
  }
  case TK_CompEqual: {
    *val = VEBOK_CompEqual;
    return true;
  }
  case TK_CompNotEqual: {
    *val = VEBOK_CompNotEqual;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 7, parseL6ValueExpr)

static inline bool opDetL8ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_And: {
    *val = VEBOK_CompLess;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 8, parseL7ValueExpr)

static inline bool opDetL9ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Or: {
    *val = VEBOK_CompGreater;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 9, parseL8ValueExpr)

static inline bool opDetL10ValueExpr(TokenKind tk,
                                     enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Tuple: {
    *val = VEBOK_Tuple;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 10, parseL9ValueExpr)

static bool opDetL11ValueExpr(TokenKind tk, enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Assign: {
    *val = VEBOK_Assign;
    return true;
  }
  case TK_AssignAdd: {
    *val = VEBOK_AssignAdd;
    return true;
  }
  case TK_AssignSub: {
    *val = VEBOK_AssignSub;
    return true;
  }
  case TK_AssignMul: {
    *val = VEBOK_AssignMul;
    return true;
  }
  case TK_AssignDiv: {
    *val = VEBOK_AssignDiv;
    return true;
  }
  case TK_AssignMod: {
    *val = VEBOK_AssignMod;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 11, parseL10ValueExpr)

// shim method
static void parseValueExpr(ValueExpr *vep, Parser *parser) {
  parseL11ValueExpr(vep, parser);
}

// field : Type,
static void parseTypeStructMemberExpr(struct TypeStructMemberExpr_s *tsmep,
                                      Parser *parser) {
  // zero-initialize bp
  ZERO(tsmep);
  pushCommentScopeParser(parser);

  Token t;

  // diagnostics
  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  LnCol start;
  LnCol end;

  // get identifier
  nextTokenParser(parser, &t);

  Span identitySpan = t.span;

  start = identitySpan.start;

  if (t.kind != TK_Identifier) {
    tsmep->name = NULL;
    diagnostic = DIAGNOSTIC(DK_StructMemberExpectedIdentifier, t.span);
    end = identitySpan.end;
    goto CLEANUP;
  }

  tsmep->name = t.identifier;

  // check if colon
  peekTokenParser(parser, &t);
  if (t.kind == TK_Colon) {
    // advance through colon
    nextTokenParser(parser, &t);
    // Get type of variable
    tsmep->type = RALLOC(parser->ar, TypeExpr);
    parseTypeExpr(tsmep->type, parser);
    end = tsmep->type->span.end;
  } else {
    end = identitySpan.end;
    tsmep->type = RALLOC(parser->ar, TypeExpr);
    tsmep->type->kind = TEK_Omitted;
    tsmep->type->span = identitySpan;
    tsmep->type->diagnostics_len = 0;
    tsmep->type->comments_len = 0;
  }

CLEANUP:
  tsmep->span = SPAN(start, end);

  if (diagnostic.kind != DK_Ok) {
    tsmep->diagnostics_len = 1;
    tsmep->diagnostics = RALLOC(parser->ar, Diagnostic);
    tsmep->diagnostics[0] = diagnostic;
  } else {
    tsmep->diagnostics_len = 0;
    tsmep->diagnostics = NULL;
  }

  Vector comments = popCommentScopeParser(parser);
  tsmep->comments_len = VEC_LEN(&comments, Comment);
  tsmep->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

static void certain_parseStructTypeExpr(TypeExpr *ste, Parser *parser) {
  ZERO(ste);
  ste->kind = TEK_Struct;
  Token t;
  nextTokenParser(parser, &t);
  switch (t.kind) {
  case TK_Struct: {
    ste->structExpr.kind = TSEK_Struct;
    break;
  }
  case TK_Enum: {
    ste->structExpr.kind = TSEK_Enum;
    break;
  }
  default: {
    INTERNAL_ERROR("called struct type expression parser where there was no "
                   "struct declaration");
    PANIC();
  }
  }

  LnCol start = t.span.start;
  LnCol end;

  Vector members;
  createVector(&members);

  Vector diagnostics;
  createVector(&diagnostics);

  nextTokenParser(parser, &t);
  if (t.kind != TK_BraceLeft) {
    end = t.span.end;
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_StructExpectedLeftBrace, t.span);
    goto CLEANUP;
  }

  PARSE_LIST(&members,                      // members_vec_ptr
             &diagnostics,                  // diagnostics_vec_ptr
             parseTypeStructMemberExpr,     // member_parse_function
             struct TypeStructMemberExpr_s, // member_kind
             TK_BraceRight,                 // delimiting_token_kind
             DK_StructExpectedRightBrace,   // missing_delimiter_error
             end,                           // end_lncol
             parser                         // parser
  )

CLEANUP:
  ste->structExpr.members_len =
      VEC_LEN(&members, struct TypeStructMemberExpr_s);
  ste->structExpr.members = manageMemArena(parser->ar, releaseVector(&members));
  ste->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  ste->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  ste->span = SPAN(start, end);
  return;
}

static void parseReferenceTypeExpr(TypeExpr *rtep, Parser *parser) {
  ZERO(rtep);
  rtep->kind = TEK_Reference;
  rtep->referenceExpr.path = RALLOC(parser->ar, Path);
  parsePath(rtep->referenceExpr.path, parser);
  rtep->diagnostics_len = 0;
  rtep->span = rtep->referenceExpr.path->span;
}

static void certain_parseVoidTypeExpr(TypeExpr *vte, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_Void);
  vte->kind = TEK_Void;
  vte->span = t.span;
  vte->diagnostics_len = 0;
  return;
}

static void certain_parseBuiltinTypeExpr(TypeExpr *btep, Parser *parser) {
  btep->kind = TEK_Builtin;
  btep->builtinExpr.builtin = RALLOC(parser->ar, Builtin);
  certain_parseBuiltin(btep->builtinExpr.builtin, parser);
  btep->diagnostics_len = 0;
  btep->span = btep->builtinExpr.builtin->span;
}

static void parseFnTypeExpr(TypeExpr *fte, Parser *parser) {
  ZERO(fte);
  Token t;
  nextTokenParser(parser, &t);

  if (t.kind != TK_Fn) {
    INTERNAL_ERROR("called function type expression parser where there was no "
                   "function");
    PANIC();
  }

  LnCol start = t.span.start;
  LnCol end;

  Vector diagnostics;
  createVector(&diagnostics);

  // check for leftparen
  nextTokenParser(parser, &t);
  if (t.kind != TK_ParenLeft) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnTypeExprExpectedLeftParen, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  Vector parameters;
  createVector(&parameters);

  PARSE_LIST(&parameters,                     // members_vec_ptr
             &diagnostics,                    // diagnostics_vec_ptr
             parseTypeExpr,                   // member_parse_function
             TypeExpr,                        // member_kind
             TK_ParenRight,                   // delimiting_token_kind
             DK_FnTypeExprExpectedRightParen, // missing_delimiter_error
             end,                             // end_lncol
             parser                           // parser
  )

  fte->fnExpr.parameters_len = VEC_LEN(&parameters, TypeExpr);
  fte->fnExpr.parameters =
      manageMemArena(parser->ar, releaseVector(&parameters));

  return;
  fte->fnExpr.parameters = RALLOC(parser->ar, TypeExpr);
  parseTypeExpr(fte->fnExpr.parameters, parser);

  nextTokenParser(parser, &t);
  if (t.kind != TK_Colon) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnTypeExprExpectedColon, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

CLEANUP:
  fte->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  fte->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  fte->span = SPAN(start, end);
}

static void parseL1TypeExpr(TypeExpr *l1, Parser *parser) {
  pushCommentScopeParser(parser);
  Token t;
  peekTokenParser(parser, &t);
  switch (t.kind) {
  case TK_Identifier: {
    parseReferenceTypeExpr(l1, parser);
    break;
  }
  case TK_Builtin: {
    certain_parseBuiltinTypeExpr(l1, parser);
    break;
  }
  case TK_Enum:
  case TK_Struct: {
    certain_parseStructTypeExpr(l1, parser);
    break;
  }
  case TK_Void: {
    certain_parseVoidTypeExpr(l1, parser);
    break;
  }
  case TK_Fn: {
    parseFnTypeExpr(l1, parser);
    break;
  }
  default: {
    l1->kind = TEK_None;
    l1->span = t.span;
    l1->diagnostics_len = 1;
    l1->diagnostics = RALLOC(parser->ar, Diagnostic);
    l1->diagnostics[0] = DIAGNOSTIC(DK_TypeExprUnexpectedToken, t.span);
    // Resync
    resyncParser(parser);
    break;
  }
  }

  Vector comments = popCommentScopeParser(parser);
  l1->comments_len = VEC_LEN(&comments, Comment);
  l1->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseScopeResolutionTypeExpr(TypeExpr *srte, Parser *parser,
                                         TypeExpr *root) {
  ZERO(srte);
  srte->kind = TEK_FieldAccess;
  srte->fieldAccess.value = root;

  Token t;

  nextTokenParser(parser, &t);
  if (t.kind != TK_FieldAccess) {
    INTERNAL_ERROR("expected field access operator");
    PANIC();
  }

  // Now we get the field
  peekTokenParser(parser, &t);
  if (t.kind != TK_Identifier) {
    // it is possible we encounter an error
    srte->fieldAccess.field = NULL;
    srte->diagnostics_len = 1;
    srte->diagnostics = RALLOC(parser->ar, Diagnostic);
    srte->diagnostics[0] =
        DIAGNOSTIC(DK_TypeExprFieldAccessExpectedIdentifier, t.span);
  } else {
    srte->fieldAccess.field = t.identifier;
    srte->diagnostics_len = 0;
  }

  srte->span = SPAN(root->span.start, t.span.end);
}

static void parseL2TypeExpr(TypeExpr *l2, Parser *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff
  TypeExpr *root = l2;
  parseL1TypeExpr(root, parser);

  while (true) {
    Token t;
    // represents the new operation
    TypeExpr *ty;

    peekTokenParser(parser, &t);
    switch (t.kind) {
    case TK_Ref: {
      pushCommentScopeParser(parser);
      ty = RALLOC(parser->ar, TypeExpr);
      *ty = *root;
      root->kind = TEK_UnaryOp;
      root->unaryOp.operator= TEUOK_Ref;
      root->unaryOp.operand = ty;
      root->span = SPAN(ty->span.start, t.span.end);
      root->diagnostics_len = 0;
      nextTokenParser(parser, &t);
      break;
    }
    case TK_Deref: {
      pushCommentScopeParser(parser);
      ty = RALLOC(parser->ar, TypeExpr);
      *ty = *root;
      root->kind = TEK_UnaryOp;
      root->unaryOp.operator= TEUOK_Deref;
      root->unaryOp.operand = ty;
      root->span = SPAN(ty->span.start, t.span.end);
      root->diagnostics_len = 0;
      nextTokenParser(parser, &t);
      break;
    }
    case TK_ScopeResolution: {
      pushCommentScopeParser(parser);
      ty = RALLOC(parser->ar, TypeExpr);
      *ty = *root;
      parseScopeResolutionTypeExpr(root, parser, ty);
      break;
    }
    default: {
      // there are no more level2 expressions
      return;
    }
    }

    Vector comments = popCommentScopeParser(parser);
    root->comments_len = VEC_LEN(&comments, Comment);
    root->comments = manageMemArena(parser->ar, releaseVector(&comments));
  }
}

static inline bool opDetL3TypeExpr(TokenKind tk,
                                   enum TypeExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Tuple: {
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

static inline bool opDetL4TypeExpr(TokenKind tk,
                                   enum TypeExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Union: {
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

static void parseTypeExpr(TypeExpr *tep, Parser *parser) {
  parseL4TypeExpr(tep, parser);
}

static void certain_parseValueRestrictionPatternExpr(PatternExpr *vrpe,
                                                     Parser *parser) {
  ZERO(vrpe);

  Token t;
  nextTokenParser(parser, &t);
  LnCol start = t.span.start;

  vrpe->kind = PEK_ValueRestriction;

  switch (t.kind) {
  case TK_CompEqual: {
    vrpe->valueRestriction.restriction = PEVRK_CompEqual;
    break;
  }
  case TK_CompNotEqual: {
    vrpe->valueRestriction.restriction = PEVRK_CompNotEqual;
    break;
  }
  case TK_CompGreaterEqual: {
    vrpe->valueRestriction.restriction = PEVRK_CompGreaterEqual;
    break;
  }
  case TK_CompGreater: {
    vrpe->valueRestriction.restriction = PEVRK_CompGreater;
    break;
  }
  case TK_CompLess: {
    vrpe->valueRestriction.restriction = PEVRK_CompLess;
    break;
  }
  case TK_CompLessEqual: {
    vrpe->valueRestriction.restriction = PEVRK_CompLessEqual;
    break;
  }
  default: {
    INTERNAL_ERROR("called value restrict pattern expr parser where there was "
                   "no value restrict pattern");
    PANIC();
  }
  }
  vrpe->valueRestriction.valueExpr = RALLOC(parser->ar, ValueExpr);
  parseValueExprTerm(vrpe->valueRestriction.valueExpr, parser);
  LnCol end = vrpe->valueRestriction.valueExpr->span.end;

  vrpe->span = SPAN(start, end);
  vrpe->diagnostics_len = 0;
  return;
}

static void certain_parseTypeRestrictionPatternExpr(PatternExpr *trpe,
                                                    Parser *parser) {
  ZERO(trpe);

  trpe->kind = PEK_TypeRestriction;

  bool parseType = false;

  Token t;
  nextTokenParser(parser, &t);

  LnCol start = t.span.start;
  LnCol end;

  switch (t.kind) {
  // No binding created
  case TK_Colon: {
    trpe->typeRestriction.has_binding = false;
    parseType = true;
    end = t.span.end;
    break;
  }
  // Create a binding
  case TK_Identifier: {
    trpe->typeRestriction.has_binding = true;
    trpe->typeRestriction.binding = t.identifier;
    end = t.span.end;
    peekTokenParser(parser, &t);
    if (t.kind == TK_Colon) {
      parseType = true;
      // advance through it
      nextTokenParser(parser, &t);
    } else {
      parseType = false;
    }
    break;
  }
  default: {
    INTERNAL_ERROR("called type restrict pattern expr parser where there was "
                   "no type restrict pattern");
    PANIC();
  }
  }

  if (parseType) {
    trpe->typeRestriction.type = RALLOC(parser->ar, TypeExpr);
    parseTypeExpr(trpe->typeRestriction.type, parser);
    end = t.span.end;
  } else {
    trpe->typeRestriction.type = RALLOC(parser->ar, TypeExpr);
    trpe->typeRestriction.type->kind = TEK_Omitted;
    trpe->typeRestriction.type->span = SPAN(start, end);
    trpe->typeRestriction.type->diagnostics_len = 0;
    trpe->typeRestriction.type->comments_len = 0;
  }

  trpe->span = SPAN(start, end);
  trpe->diagnostics_len = 0;
}

static void
parsePatternStructMemberExpr(struct PatternStructMemberExpr_s *psmep,
                             Parser *parser) {
  pushCommentScopeParser(parser);
  ZERO(psmep);
  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;
  Token t;
  nextTokenParser(parser, &t);
  LnCol start = t.span.end;
  LnCol end;
  switch (t.kind) {
  case TK_Rest: {
    psmep->kind = PSMEK_Rest;
    break;
  }
  case TK_Identifier: {
    // copy identifier
    psmep->kind = PSMEK_Field;
    psmep->field.field = t.identifier;
    break;
  }
  default: {
    diagnostic = DIAGNOSTIC(DK_PatternStructExpectedIdentifier, t.span);
    end = t.span.end;
    goto CLEANUP;
  }
  }

  // test if the statement has an assign
  // The assignment is only omitted if it is a value restriction
  bool has_assign;

  peekTokenParser(parser, &t);
  if (t.kind == TK_Assign) {
    has_assign = true;
    nextTokenParser(parser, &t);
  } else {
    has_assign = false;
  }
  psmep->pattern = RALLOC(parser->ar, PatternExpr);
  parsePatternExpr(psmep->pattern, parser);

  end = psmep->pattern->span.end;

  if (psmep->pattern->kind == PEK_ValueRestriction && has_assign) {
    diagnostic =
        DIAGNOSTIC(DK_PatternStructUnexpectedAssignForValueRestriction, t.span);
    goto CLEANUP;
  } else if (psmep->pattern->kind != PEK_ValueRestriction && !has_assign) {
    diagnostic = DIAGNOSTIC(
        DK_PatternStructExpectedAssignForNonValueRestriction, t.span);
    goto CLEANUP;
  }

CLEANUP:
  if (diagnostic.kind != DK_Ok) {
    psmep->diagnostics_len = 1;
    psmep->diagnostics = RALLOC(parser->ar, Diagnostic);
    psmep->diagnostics[0] = diagnostic;
  } else {
    psmep->diagnostics_len = 0;
  }

  psmep->span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  psmep->comments_len = VEC_LEN(&comments, Comment);
  psmep->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

static void certain_parseStructPatternExpr(PatternExpr *spe, Parser *parser) {
  ZERO(spe);
  spe->kind = PEK_Struct;
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_Struct);

  LnCol start = t.span.start;
  LnCol end;

  Vector members;
  createVector(&members);

  Vector diagnostics;
  createVector(&diagnostics);

  nextTokenParser(parser, &t);
  if (t.kind != TK_BraceLeft) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_PatternStructExpectedLeftBrace, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&members,                           // members_vec_ptr
             &diagnostics,                       // diagnostics_vec_ptr
             parsePatternStructMemberExpr,       // member_parse_function
             struct PatternStructMemberExpr_s,   // member_kind
             TK_BraceRight,                      // delimiting_token_kind
             DK_PatternStructExpectedRightBrace, // missing_delimiter_error
             end,                                // end_lncol
             parser                              // parser
  )
CLEANUP:
  spe->structExpr.members_len =
      VEC_LEN(&members, struct PatternStructMemberExpr_s);
  spe->structExpr.members = manageMemArena(parser->ar, releaseVector(&members));
  spe->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  spe->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  spe->span = SPAN(start, end);
  return;
}

static void certain_parseGroupPatternExpr(PatternExpr *gpep, Parser *parser) {
  ZERO(gpep);
  gpep->kind = PEK_Group;
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == TK_BraceLeft);
  LnCol start = t.span.start;
  LnCol end;

  gpep->groupExpr.value = RALLOC(parser->ar, PatternExpr);
  parsePatternExpr(gpep->groupExpr.value, parser);

  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  nextTokenParser(parser, &t);
  if (t.kind != TK_BraceRight) {
    diagnostic = DIAGNOSTIC(DK_PatternGroupExpectedRightBrace, t.span);
    end = t.span.end;
  } else {
    end = gpep->groupExpr.value->span.end;
  }

  if (diagnostic.kind == DK_Ok) {
    gpep->diagnostics_len = 0;
  } else {
    gpep->diagnostics_len = 1;
    gpep->diagnostics = RALLOC(parser->ar, Diagnostic);
    *gpep->diagnostics = diagnostic;
  }
  gpep->span = SPAN(start, end);
}

static void parseL1PatternExpr(PatternExpr *l1, Parser *parser) {
  pushCommentScopeParser(parser);
  Token t;
  peekTokenParser(parser, &t);
  switch (t.kind) {
  case TK_BraceLeft: {
    certain_parseGroupPatternExpr(l1, parser);
    break;
  }
  case TK_Struct: {
    certain_parseStructPatternExpr(l1, parser);
    break;
  }
  case TK_Identifier:
  case TK_Colon: {
    certain_parseTypeRestrictionPatternExpr(l1, parser);
    break;
  }
  case TK_CompEqual:
  case TK_CompNotEqual:
  case TK_CompGreaterEqual:
  case TK_CompGreater:
  case TK_CompLess:
  case TK_CompLessEqual: {
    certain_parseValueRestrictionPatternExpr(l1, parser);
    break;
  }
  default: {
    l1->kind = PEK_None;
    l1->span = t.span;
    l1->diagnostics_len = 1;
    l1->diagnostics = RALLOC(parser->ar, Diagnostic);
    l1->diagnostics[0] = DIAGNOSTIC(DK_TypeExprUnexpectedToken, t.span);
    // Resync
    resyncParser(parser);
    break;
  }
  }

  Vector comments = popCommentScopeParser(parser);
  l1->comments_len = VEC_LEN(&comments, Comment);
  l1->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseL2PatternExpr(PatternExpr *l2, Parser *parser) {
  Token t;
  peekTokenParser(parser, &t);
  switch (t.kind) {
  case TK_Not: {
    l2->unaryOp.operator= PEUOK_Not;
    break;
  }
  default: {
    // there is no level expression
    parseL1PatternExpr(l2, parser);
    return;
  }
  }

  // this will only execute if an L3 operator exists
  l2->kind = PEK_UnaryOp;

  // first create comment scope and go through op
  pushCommentScopeParser(parser);
  nextTokenParser(parser, &t);

  // Now parse the rest of the expression
  l2->unaryOp.operand = RALLOC(parser->ar, PatternExpr);
  parseL2PatternExpr(l2->unaryOp.operand, parser);

  // finally calculate the misc stuff
  l2->span = SPAN(t.span.start, l2->unaryOp.operand->span.end);
  l2->diagnostics_len = 0;

  // comments
  Vector comments = popCommentScopeParser(parser);
  l2->comments_len = VEC_LEN(&comments, Comment);
  l2->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static inline bool opDetL3PatternExpr(TokenKind tk,
                                      enum PatternExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Tuple: {
    *val = PEBOK_Tuple;
    return true;
  }
  default: {
    // there is no level 4 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(PatternExpr, PEK, 3, parseL2PatternExpr)

static inline bool opDetL4PatternExpr(TokenKind tk,
                                      enum PatternExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Union: {
    *val = PEBOK_Union;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(PatternExpr, PEK, 4, parseL3PatternExpr)

static inline bool opDetL5PatternExpr(TokenKind tk,
                                      enum PatternExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_And: {
    *val = PEBOK_And;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(PatternExpr, PEK, 5, parseL4PatternExpr)

static inline bool opDetL6PatternExpr(TokenKind tk,
                                      enum PatternExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Or: {
    *val = PEBOK_Or;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(PatternExpr, PEK, 6, parseL5PatternExpr)

static void parsePatternExpr(PatternExpr *ppe, Parser *parser) {
  parseL6PatternExpr(ppe, parser);
}

static void parseValDecl(Stmnt *vdsp, Parser *parser) {
  // zero-initialize vdsp
  ZERO(vdsp);
  vdsp->kind = SK_ValDecl;
  // these variables will be reused
  Token t;

  // Get Binding
  vdsp->valDecl.pattern = RALLOC(parser->ar, PatternExpr);
  parsePatternExpr(vdsp->valDecl.pattern, parser);

  LnCol start = vdsp->valDecl.pattern->span.start;
  LnCol end;

  // Expect Equal Sign
  peekTokenParser(parser, &t);
  if (t.kind == TK_Assign) {
    // accept the equal sign
    nextTokenParser(parser, &t);

    vdsp->valDecl.has_value = true;
    vdsp->valDecl.value = RALLOC(parser->ar, ValueExpr);
    parseValueExpr(vdsp->valDecl.value, parser);
    end = vdsp->valDecl.value->span.end;
  } else {
    vdsp->valDecl.has_value = false;
    end = vdsp->valDecl.pattern->span.end;
  }

  vdsp->span = SPAN(start, end);
  vdsp->diagnostics_len = 0;
}

static void parseTypeDecl(Stmnt *tdp, Parser *parser) {
  ZERO(tdp);
  tdp->kind = SK_TypeDecl;
  Token t;

  LnCol end;

  nextTokenParser(parser, &t);

  LnCol start = t.span.start;

  if (t.kind != TK_Identifier) {
    tdp->typeDecl.name = NULL;
    tdp->diagnostics = RALLOC(parser->ar, Diagnostic);
    tdp->diagnostics[0] = DIAGNOSTIC(DK_TypeDeclExpectedIdentifier, t.span);
    tdp->diagnostics_len = 1;
    end = t.span.end;
    goto CLEANUP;
  }

  tdp->typeDecl.name = t.identifier;

  // Now get equals sign
  nextTokenParser(parser, &t);
  if (t.kind != TK_Assign) {
    tdp->diagnostics = RALLOC(parser->ar, Diagnostic);
    tdp->diagnostics[0] = DIAGNOSTIC(DK_TypeDeclExpectedAssign, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  tdp->typeDecl.type = RALLOC(parser->ar, TypeExpr);
  parseTypeExpr(tdp->typeDecl.type, parser);
  end = tdp->typeDecl.type->span.end;
  tdp->diagnostics_len = 0;
  tdp->diagnostics = NULL;

CLEANUP:
  tdp->span = SPAN(start, end);
  return;
}

static void parsePatternExprStmnt(Stmnt *pesp, Parser *parser) {
  ZERO(pesp);
  pesp->kind = SK_PatExpr;

  Token t;
  nextTokenParser(parser, &t);
  if (t.kind != TK_Pat) {
    INTERNAL_ERROR(
        "called pat expr stmnt parser where there was no pat expr stmnt");
    PANIC();
  }
  LnCol start = t.span.start;

  pesp->patExpr.pattern = RALLOC(parser->ar, PatternExpr);
  parsePatternExpr(pesp->patExpr.pattern, parser);
  pesp->span = SPAN(start, pesp->patExpr.pattern->span.end);
  pesp->diagnostics_len = 0;
  return;
}

static void parseTypeExprStmnt(Stmnt *tesp, Parser *parser) {
  ZERO(tesp);
  tesp->kind = SK_TypeExpr;

  Token t;
  nextTokenParser(parser, &t);
  if (t.kind != TK_Type) {
    INTERNAL_ERROR(
        "called type expr stmnt parser where there was no type expr stmnt");
    PANIC();
  }
  LnCol start = t.span.start;

  tesp->typeExpr.type = RALLOC(parser->ar, TypeExpr);
  parseTypeExpr(tesp->typeExpr.type, parser);
  tesp->span = SPAN(start, tesp->typeExpr.type->span.end);
  tesp->diagnostics_len = 0;
  return;
}

static void parseStmnt(Stmnt *sp, Parser *parser) {
  pushCommentScopeParser(parser);

  Token t;
  // peek next token
  peekTokenParser(parser, &t);
  switch (t.kind) {
    // Macros
  case TK_Macro: {
    sp->kind = SK_Macro;
    sp->macroStmnt.name = t.macro_call;
    sp->diagnostics_len = 0;
    sp->span = t.span;
    break;
  }
  case TK_Use: {
    LnCol start = t.span.start;
    nextTokenParser(parser, &t); // drop use token
    sp->kind = SK_Use;
    sp->useStmnt.path = RALLOC(parser->ar, Path);
    parsePath(sp->useStmnt.path, parser);
    sp->span = SPAN(start, sp->useStmnt.path->span.end);
    sp->diagnostics_len = 0;
    break;
  }
  case TK_Namespace: {
    LnCol start = t.span.start;
    nextTokenParser(parser, &t); // drop namespace token
    sp->kind = SK_Namespace;
    sp->namespaceStmnt.path = RALLOC(parser->ar, Path);
    parsePath(sp->namespaceStmnt.path, parser);
    sp->namespaceStmnt.stmnt = RALLOC(parser->ar, Stmnt);
    parseStmnt(sp->namespaceStmnt.stmnt, parser);
    sp->span = SPAN(start, sp->namespaceStmnt.stmnt->span.end);
    sp->diagnostics_len = 0;
    break;
  }
  // Let Stmnt (Decl)
  case TK_Let: {
    LnCol start = t.span.start;
    nextTokenParser(parser, &t);
    peekTokenParser(parser, &t);
    switch (t.kind) {
    case TK_Type: {
      nextTokenParser(parser, &t);
      parseTypeDecl(sp, parser);
      break;
    }
    default: {
      parseValDecl(sp, parser);
      break;
    }
    }
    sp->span = SPAN(start, sp->span.end);
    break;
  }
  // Expressions
  case TK_Type: {
    parseTypeExprStmnt(sp, parser);
    break;
  }
  case TK_Pat: {
    parsePatternExprStmnt(sp, parser);
    break;
  }
  default: {
    // Value Expr Statement
    sp->kind = SK_ValExpr;
    sp->valExpr.value = RALLOC(parser->ar, ValueExpr);
    parseValueExpr(sp->valExpr.value, parser);
    sp->span = sp->valExpr.value->span;
    sp->diagnostics_len = 0;
    break;
  }
  }
  Vector comments = popCommentScopeParser(parser);
  sp->comments_len = VEC_LEN(&comments, Comment);
  sp->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseTranslationUnit(TranslationUnit *tu, Parser *parser) {
  pushCommentScopeParser(parser);

  Token t;
  Vector statements;
  createVector(&statements);

  LnCol end;

  while (true) {
    peekTokenParser(parser, &t);
    if (t.kind == TK_None && t.error == DK_EOF) {
      end = t.span.end;
      break;
    }
    /* if there wasn't an end delimiter, push the last token back */
    parseStmnt(VEC_PUSH(&statements, Stmnt), parser);
  }

  tu->statements_len = VEC_LEN(&statements, Stmnt);
  tu->statements = manageMemArena(parser->ar, releaseVector(&statements));
  tu->span = SPAN(LNCOL(0, 0), end);
  tu->diagnostics_len = 0;

  Vector comments = popCommentScopeParser(parser);
  tu->comments_len = VEC_LEN(&comments, Comment);
  tu->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

void parseTranslationUnitParser(Parser *pp, TranslationUnit *tu) {
  parseTranslationUnit(tu, pp);
}
