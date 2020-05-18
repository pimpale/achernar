#include "parseAst.h"

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

#define EXPECT_TYPE(token, tokenType, onErrLabel)                              \
  do {                                                                         \
    if ((token).kind != (tokenType)) {                                         \
      goto onErrLabel;                                                         \
    }                                                                          \
  } while (false)

// Parser
void createParser(Parser *pp, Lexer *lp, Arena *ar) {
  pp->has_next_token = false;
  pp->lexer = lp;
  pp->ar = ar;
  createVector(&pp->comments);
  createVector(&pp->next_comments);
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
      *VEC_PUSH(comments, Comment) =
          (Comment){.span = c.span,
                    .scope = internArena(parser->ar, c.comment.scope),
                    .data = internArena(parser->ar, c.comment.comment)};
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

// If has an ungotten token, return that. Else return next in line, and cache it
static void nextTokenParser(Parser *pp, Token *t) {
  if (pp->has_next_token) {
    // set the token
    *t = pp->next_token;
    pp->has_next_token = false;
    // merge the next comments
    Vector *top = VEC_PEEK(&pp->comments, Vector);
    size_t next_comments_len = lengthVector(&pp->next_comments);
    popVector(&pp->next_comments, pushVector(top, next_comments_len),
              next_comments_len);

  } else {
    rawNextTokenParser(pp, t, VEC_PEEK(&pp->comments, Vector));
  }
}

// If token exists in line
static void peekTokenParser(Parser *pp, Token *t) {
  if (pp->has_next_token) {
    *t = pp->next_token;
  } else {
    // set the next token
    rawNextTokenParser(pp, &pp->next_token, &pp->next_comments);
    pp->has_next_token = true;
    *t = pp->next_token;
  }
}

// pops the top comment scope off the comment stack
/// REQUIRES: `parser` is pointer to valid Parser
/// REQUIRES: the stack has at least one member
/// GUARANTEES: return value is the topmost element of the comment stack
/// GUARANTEES: the topmost element fo the comment stack has been removed
static Vector popCommentScopeParser(Parser *parser) {
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
  destroyVector(&pp->next_comments);
  // if vec len is not 0, then we internal error
  if (VEC_LEN(&pp->comments, Vector) != 0) {
    INTERNAL_ERROR("not all comment scopes were freed in the parser, memory "
                   "leak has occured");
    PANIC();
  }
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
static void parseBuiltin(Builtin *bp, Parser *parser);
static void parseValueExpr(ValueExpr *vep, Parser *parser);
static void parseTypeExpr(TypeExpr *tep, Parser *parser);
static void parsePatternExpr(PatternExpr *pp, Parser *parser);
static void parseConstExpr(ConstExpr *cep, Parser *parser);

static void parsePath(Path *pp, Parser *parser) {
  // start comment scope
  pushCommentScopeParser(parser);

  Token t;
  nextTokenParser(parser, &t);
  if (t.kind != TK_Identifier) {
    INTERNAL_ERROR("called path parser where there was no path");
    PANIC();
  }

  // start and finish
  LnCol start = t.span.start;
  LnCol end;

  // diagnostics (temp ok value)
  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  Vector pathSegments;
  createVector(&pathSegments);

  *VEC_PUSH(&pathSegments, char *) = internArena(parser->ar, t.identifier);

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

      *VEC_PUSH(&pathSegments, char *) = internArena(parser->ar, t.identifier);
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

static void parseConstValueExpr(ValueExpr *cvep, Parser *parser) {
  ZERO(cvep);
  cvep->kind = VEK_ConstExpr;
  cvep->constExpr.constExpr = RALLOC(parser->ar, ConstExpr);
  parseConstExpr(cvep->constExpr.constExpr, parser);

  cvep->span = cvep->constExpr.constExpr->span;
  cvep->diagnostics_len = 0;
}

static void parseStringValueExpr(ValueExpr *svep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  EXPECT_TYPE(t, TK_StringLiteral, HANDLE_NO_STRING_LITERAL);
  svep->kind = VEK_StringLiteral;
  svep->stringLiteral.value = internArena(parser->ar, t.string_literal);
  svep->span = t.span;
  svep->diagnostics_len = 0;
  return;

HANDLE_NO_STRING_LITERAL:
  INTERNAL_ERROR("called string literal parser where there was no "
                 "string literal");
  PANIC();
}

static void parseFnValueExpr(ValueExpr *fvep, Parser *parser) {
  ZERO(fvep);

  fvep->kind = VEK_Fn;

  Token t;
  nextTokenParser(parser, &t);

  if (t.kind != TK_Fn) {
    INTERNAL_ERROR("called function value expression parser where there was no "
                   "function");
    PANIC();
  }

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

static void parseBlockValueExpr(ValueExpr *bvep, Parser *parser) {
  ZERO(bvep);
  bvep->kind = VEK_Block;

  Token t;
  nextTokenParser(parser, &t);

  if (t.kind != TK_BraceLeft) {
    INTERNAL_ERROR(
        "called a block expresion parser where there was no leftbrace");
    PANIC();
  }

  LnCol start = t.span.start;

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

static void parseIfValueExpr(ValueExpr *ivep, Parser *parser) {
  ZERO(ivep);
  Token t;
  nextTokenParser(parser, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_If, HANDLE_NO_IF);
  ivep->kind = VEK_If;

  // parse condition
  ivep->ifExpr.condition = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(ivep->ifExpr.condition, parser);

  // parse body
  ivep->ifExpr.body = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(ivep->ifExpr.body, parser);

  // if the next value is else
  peekTokenParser(parser, &t);
  if (t.kind == TK_Else) {
    // skip else
    nextTokenParser(parser, &t);
    // parse the else
    ivep->ifExpr.has_else = true;
    ivep->ifExpr.else_body = RALLOC(parser->ar, ValueExpr);
    parseValueExpr(ivep->ifExpr.else_body, parser);
    ivep->span = SPAN(start, ivep->ifExpr.else_body->span.end);
  } else {
    ivep->span = SPAN(start, ivep->ifExpr.body->span.end);
  }
  return;

HANDLE_NO_IF:
  INTERNAL_ERROR("called if expression parser where there was no "
                 "if expression");
  PANIC();
}

static void parseBuiltinValueExpr(ValueExpr *bvep, Parser *parser) {
  bvep->kind = VEK_Builtin;
  bvep->builtinExpr.builtin = RALLOC(parser->ar, Builtin);
  parseBuiltin(bvep->builtinExpr.builtin, parser);
  bvep->diagnostics_len = 0;
  bvep->span = bvep->builtinExpr.builtin->span;
}

static void parseDeferValueExpr(ValueExpr *dep, Parser *parser) {
  dep->kind = VEK_Defer;
  Token t;
  nextTokenParser(parser, &t);
  if (t.kind != TK_Defer) {
    INTERNAL_ERROR("called break parser where there was no continue");
    PANIC();
  }
  dep->deferExpr.value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(dep->deferExpr.value, parser);
  dep->span = SPAN(t.span.start, dep->deferExpr.value->span.end);
  dep->diagnostics_len = 0;
  return;
}

static void parseBreakValueExpr(ValueExpr *bep, Parser *parser) {
  bep->kind = VEK_Break;
  Token t;
  nextTokenParser(parser, &t);
  bep->span = t.span;
  EXPECT_TYPE(t, TK_Break, HANDLE_NO_BREAK);
  bep->breakExpr.value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(bep->breakExpr.value, parser);
  bep->diagnostics_len = 0;
  return;

HANDLE_NO_BREAK:
  INTERNAL_ERROR("called break parser where there was no continue");
  PANIC();
}

static void parseContinueValueExpr(ValueExpr *cep, Parser *parser) {
  cep->kind = VEK_Continue;
  Token t;
  nextTokenParser(parser, &t);
  cep->span = t.span;
  EXPECT_TYPE(t, TK_Continue, HANDLE_NO_CONTINUE);
  cep->diagnostics_len = 0;
  return;

HANDLE_NO_CONTINUE:
  INTERNAL_ERROR("called continue parser where there was no continue");
  PANIC();
}

static void parseReturnValueExpr(ValueExpr *rep, Parser *parser) {
  rep->kind = VEK_Return;
  Token t;
  nextTokenParser(parser, &t);
  rep->span = t.span;
  EXPECT_TYPE(t, TK_Return, HANDLE_NO_RETURN);
  rep->returnExpr.value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(rep->returnExpr.value, parser);
  rep->diagnostics_len = 0;
  return;

HANDLE_NO_RETURN:
  INTERNAL_ERROR("called return parser where there was no continue");
  PANIC();
}

static void parseWhileValueExpr(ValueExpr *wep, Parser *parser) {
  wep->kind = VEK_While;
  Token t;
  nextTokenParser(parser, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_While, HANDLE_NO_WHILE);

  wep->whileExpr.condition = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(wep->whileExpr.condition, parser);

  wep->whileExpr.body = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(wep->whileExpr.body, parser);

  wep->span = SPAN(start, wep->whileExpr.body->span.end);
  wep->diagnostics_len = 0;
  return;

HANDLE_NO_WHILE:
  INTERNAL_ERROR("called continue parser where there was no continue");
  PANIC();
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

static void parseMatchValueExpr(ValueExpr *mvep, Parser *parser) {
  ZERO(mvep);
  mvep->kind = VEK_Match;
  Token t;
  // Ensure match
  nextTokenParser(parser, &t);
  EXPECT_TYPE(t, TK_Match, HANDLE_NO_MATCH);

  LnCol start = t.span.start;

  // Get expression to match against
  mvep->matchExpr.value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(mvep->matchExpr.value, parser);
  // now we must parse the block containing the cases

  // Expect beginning brace
  nextTokenParser(parser, &t);
  EXPECT_TYPE(t, TK_BraceLeft, HANDLE_NO_LEFTBRACE);

  Vector cases;
  createVector(&cases);

  Vector diagnostics;
  createVector(&diagnostics);

  LnCol end;

  PARSE_LIST(&cases,                 // members_vec_ptr
             &diagnostics,           // diagnostics_vec_ptr
             parseMatchCaseExpr,     // member_parse_function
             struct MatchCaseExpr_s, // member_kind
             TK_BraceRight,          // delimiting_token_kind
             DK_MatchNoRightBrace,   // missing_delimiter_error
             end,                    // end_lncol
             parser                  // parser
  )

  // Get interior cases
  mvep->matchExpr.cases_len = VEC_LEN(&cases, struct MatchCaseExpr_s);
  mvep->matchExpr.cases = manageMemArena(parser->ar, releaseVector(&cases));

  mvep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  mvep->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  mvep->span = SPAN(start, end);
  return;

HANDLE_NO_MATCH:
  INTERNAL_ERROR("called match parser where there was no match");
  PANIC();

HANDLE_NO_LEFTBRACE:
  mvep->diagnostics_len = 1;
  mvep->diagnostics = RALLOC(parser->ar, Diagnostic);
  mvep->diagnostics[0] = DIAGNOSTIC(DK_MatchNoLeftBrace, t.span);
  mvep->span = SPAN(start, t.span.end);
  mvep->matchExpr.cases_len = 0;
  mvep->matchExpr.cases = NULL;
  resyncParser(parser);
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

  vsmep->name = internArena(parser->ar, t.identifier);

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

static void parseValueStructExpr(ValueExpr *sve, Parser *parser) {
  ZERO(sve);
  sve->kind = VEK_StructLiteral;
  Token t;
  nextTokenParser(parser, &t);
  if (t.kind != TK_Struct) {
    INTERNAL_ERROR("called struct type expression parser where there was no "
                   "struct declaration");
    PANIC();
  }

  LnCol start = t.span.start;

  nextTokenParser(parser, &t);

  EXPECT_TYPE(t, TK_BraceLeft, HANDLE_NO_LEFTBRACE);

  Vector members;
  createVector(&members);

  Vector diagnostics;
  createVector(&diagnostics);

  LnCol end;

  PARSE_LIST(&members,                       // members_vec_ptr
             &diagnostics,                   // diagnostics_vec_ptr
             parseValueStructMemberExpr,     // member_parse_function
             struct ValueStructMemberExpr_s, // member_kind
             TK_BraceRight,                  // delimiting_token_kind
             DK_StructExpectedRightBrace,    // missing_delimiter_error
             end,                            // end_lncol
             parser                          // parser
  )

  sve->structExpr.members_len =
      VEC_LEN(&members, struct ValueStructMemberExpr_s);
  sve->structExpr.members = manageMemArena(parser->ar, releaseVector(&members));
  sve->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  sve->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  sve->span = SPAN(start, end);
  return;

HANDLE_NO_LEFTBRACE:
  sve->structExpr.members_len = 0;
  sve->structExpr.members = NULL;
  sve->span = SPAN(start, t.span.end);
  sve->diagnostics_len = 1;
  sve->diagnostics = RALLOC(parser->ar, Diagnostic);
  sve->diagnostics[0] = DIAGNOSTIC(DK_StructExpectedLeftBrace, sve->span);
  return;
}

// Level1ValueExpr parentheses, braces, literals
// Level2ValueExpr as () [] & @ . -> (postfixes)
// Level3ValueExpr - + ! (prefixes)
// Level4ValueExpr * / % (multiplication and division)
// Level5ValueExpr + - (addition and subtraction)
// Level6ValueExpr < <= > >= == != (comparators)
// Level7ValueExpr && (logical and)
// Level8ValueExpr || (logical or)
// Level9ValueExpr , (create tuple)
// Level10ValueExpr = += -= *= /= %= (Assignment)

static void parseL1ValueExpr(ValueExpr *l1, Parser *parser) {
  pushCommentScopeParser(parser);

  Token t;
  peekTokenParser(parser, &t);
  // Decide which expression it is
  switch (t.kind) {
  // Literals
  case TK_IntLiteral:
  case TK_BoolLiteral:
  case TK_FloatLiteral:
  case TK_CharLiteral:
  case TK_Dollar: {
    parseConstValueExpr(l1, parser);
    break;
  }
  case TK_Builtin: {
    parseBuiltinValueExpr(l1, parser);
    break;
  }
  case TK_StringLiteral: {
    parseStringValueExpr(l1, parser);
    break;
  }
  case TK_BraceLeft: {
    parseBlockValueExpr(l1, parser);
    break;
  }
  case TK_Fn: {
    parseFnValueExpr(l1, parser);
    break;
  }
  case TK_Struct: {
    parseValueStructExpr(l1, parser);
    break;
  }
  case TK_If: {
    parseIfValueExpr(l1, parser);
    break;
  }
  case TK_Defer: {
    parseDeferValueExpr(l1, parser);
    break;
  }
  case TK_Break: {
    parseBreakValueExpr(l1, parser);
    break;
  }
  case TK_Continue: {
    parseContinueValueExpr(l1, parser);
    break;
  }
  case TK_Return: {
    parseReturnValueExpr(l1, parser);
    break;
  }
  case TK_While: {
    parseWhileValueExpr(l1, parser);
    break;
  }
  case TK_Match: {
    parseMatchValueExpr(l1, parser);
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
    fave->fieldAccess.field = internArena(parser->ar, t.identifier);
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
#define FN_BINOP_PARSE_LX_EXPR(type, type_shorthand, x, lower_fn, op_det_fn)   \
  static void parseL##x##type(type *l##x, Parser *parser) {                    \
    type v;                                                                    \
    lower_fn(&v, parser);                                                      \
                                                                               \
    Token t;                                                                   \
    peekTokenParser(parser, &t);                                               \
    bool success = op_det_fn(t.kind, &l##x->binaryOp.operator);                \
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
    lower_fn(l##x->binaryOp.right_operand, parser);                            \
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
    // there is no level 4 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 4, parseL3ValueExpr, opDetL4ValueExpr)

static inline bool opDetL5ValueExpr(TokenKind tk,
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
    // there is no level 5 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 5, parseL4ValueExpr, opDetL5ValueExpr)

static inline bool opDetL6ValueExpr(TokenKind tk,
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
    // there is no level 7 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 6, parseL5ValueExpr, opDetL6ValueExpr)

static inline bool opDetL7ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_And: {
    *val = VEBOK_CompLess;
    return true;
  }
  default: {
    // there is no level 8 expression
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 7, parseL6ValueExpr, opDetL7ValueExpr)

static inline bool opDetL8ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Or: {
    *val = VEBOK_CompGreater;
    return true;
  }
  default: {
    // there is no level 8 expression
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 8, parseL7ValueExpr, opDetL8ValueExpr)

static inline bool opDetL9ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Tuple: {
    *val = VEBOK_Tuple;
    return true;
  }
  default: {
    // there is no level 9 expression
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 9, parseL8ValueExpr, opDetL9ValueExpr)

static bool opDetL10ValueExpr(TokenKind tk, enum ValueExprBinaryOpKind_e *val) {
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
    // There is no level 10 expr
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 10, parseL9ValueExpr, opDetL10ValueExpr)

// shim method
static void parseValueExpr(ValueExpr *vep, Parser *parser) {
  parseL10ValueExpr(vep, parser);
}

static void parseIntConstExpr(ConstExpr *icep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  EXPECT_TYPE(t, TK_IntLiteral, HANDLE_NO_INT_LITERAL);
  icep->kind = CEK_IntLiteral;
  icep->intLiteral.value = t.int_literal;
  icep->span = t.span;
  icep->diagnostics_len = 0;
  return;

HANDLE_NO_INT_LITERAL:
  INTERNAL_ERROR("called int literal parser where there was no "
                 "int literal");
  PANIC();
}

static void parseBoolConstExpr(ConstExpr *bcep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  EXPECT_TYPE(t, TK_BoolLiteral, HANDLE_NO_BOOL_LITERAL);
  bcep->kind = CEK_BoolLiteral;
  bcep->boolLiteral.value = t.bool_literal;
  bcep->span = t.span;
  bcep->diagnostics_len = 0;
  return;

HANDLE_NO_BOOL_LITERAL:
  INTERNAL_ERROR("called int literal parser where there was no "
                 "int literal");
  PANIC();
}

static void parseFloatConstExpr(ConstExpr *fcep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  EXPECT_TYPE(t, TK_FloatLiteral, HANDLE_NO_FLOAT_LITERAL);
  fcep->kind = CEK_FloatLiteral;
  fcep->floatLiteral.value = t.float_literal;
  fcep->span = t.span;
  fcep->diagnostics_len = 0;
  return;

HANDLE_NO_FLOAT_LITERAL:
  INTERNAL_ERROR("called float literal parser where there was no "
                 "float literal");
  PANIC();
}

static void parseCharConstExpr(ConstExpr *ccep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  EXPECT_TYPE(t, TK_CharLiteral, HANDLE_NO_CHAR_LITERAL);
  ccep->kind = CEK_CharLiteral;
  ccep->charLiteral.value = t.char_literal;
  ccep->span = t.span;
  ccep->diagnostics_len = 0;
  return;

HANDLE_NO_CHAR_LITERAL:
  INTERNAL_ERROR("called char literal parser where there was no "
                 "char literal");
  PANIC();
}

static void parseValueConstExpr(ConstExpr *vcep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  if (t.kind != TK_Dollar) {
    INTERNAL_ERROR("called value const expr parser where there was no "
                   "value const expr");
    PANIC();
  }
  LnCol start = t.span.start;
  vcep->kind = CEK_ValueExpr;
  vcep->valueExpr.expr = RALLOC(parser->ar, ValueExpr);
  parseL3ValueExpr(vcep->valueExpr.expr, parser);
  vcep->span = SPAN(start, vcep->valueExpr.expr->span.end);
  vcep->diagnostics_len = 0;
}

static void parseConstExpr(ConstExpr *cep, Parser *parser) {
  pushCommentScopeParser(parser);

  Token t;
  peekTokenParser(parser, &t);
  switch (t.kind) {
  case TK_IntLiteral: {
    parseIntConstExpr(cep, parser);
    break;
  }
  case TK_BoolLiteral: {
    parseBoolConstExpr(cep, parser);
    break;
  }
  case TK_FloatLiteral: {
    parseFloatConstExpr(cep, parser);
    break;
  }
  case TK_CharLiteral: {
    parseCharConstExpr(cep, parser);
    break;
  }
  case TK_Dollar: {
    parseValueConstExpr(cep, parser);
    break;
  }
  default: {
    // put the token error in the value expression.
    ZERO(cep);
    cep->kind = CEK_None;
    cep->span = t.span;
    cep->diagnostics_len = 1;
    cep->diagnostics = RALLOC(parser->ar, Diagnostic);
    cep->diagnostics[0] = DIAGNOSTIC(DK_ConstExprUnrecognizedLiteral, t.span);
    // discard this token
    nextTokenParser(parser, &t);
    break;
  }
  }
  Vector comments = popCommentScopeParser(parser);
  cep->comments_len = VEC_LEN(&comments, Comment);
  cep->comments = manageMemArena(parser->ar, releaseVector(&comments));
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

  tsmep->name = internArena(parser->ar, t.identifier);

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

static void parseStructTypeExpr(TypeExpr *ste, Parser *parser) {
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

  nextTokenParser(parser, &t);

  EXPECT_TYPE(t, TK_BraceLeft, HANDLE_NO_LEFTBRACE);

  Vector members;
  createVector(&members);

  Vector diagnostics;
  createVector(&diagnostics);

  LnCol end;

  PARSE_LIST(&members,                      // members_vec_ptr
             &diagnostics,                  // diagnostics_vec_ptr
             parseTypeStructMemberExpr,     // member_parse_function
             struct TypeStructMemberExpr_s, // member_kind
             TK_BraceRight,                 // delimiting_token_kind
             DK_StructExpectedRightBrace,   // missing_delimiter_error
             end,                           // end_lncol
             parser                         // parser
  )

  ste->structExpr.members_len =
      VEC_LEN(&members, struct TypeStructMemberExpr_s);
  ste->structExpr.members = manageMemArena(parser->ar, releaseVector(&members));
  ste->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  ste->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  ste->span = SPAN(start, end);
  return;

HANDLE_NO_LEFTBRACE:
  ste->structExpr.members_len = 0;
  ste->structExpr.members = NULL;
  ste->span = SPAN(start, t.span.end);
  ste->diagnostics_len = 1;
  ste->diagnostics = RALLOC(parser->ar, Diagnostic);
  ste->diagnostics[0] = DIAGNOSTIC(DK_StructExpectedLeftBrace, ste->span);
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

static void parseVoidTypeExpr(TypeExpr *vte, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  EXPECT_TYPE(t, TK_Void, HANDLE_NO_VOID);
  vte->kind = TEK_Void;
  vte->span = t.span;
  vte->diagnostics_len = 0;
  return;

HANDLE_NO_VOID:
  INTERNAL_ERROR("called void type expression parser where there was no "
                 "void");
  PANIC();
}

static void parseBuiltinTypeExpr(TypeExpr *btep, Parser *parser) {
  btep->kind = TEK_Builtin;
  btep->builtinExpr.builtin = RALLOC(parser->ar, Builtin);
  parseBuiltin(btep->builtinExpr.builtin, parser);
  btep->diagnostics_len = 0;
  btep->span = btep->builtinExpr.builtin->span;
}

static void parseBuiltin(Builtin *bp, Parser *parser) {
  ZERO(bp);
  Token t;
  nextTokenParser(parser, &t);

  LnCol start;
  LnCol end;

  if (t.kind != TK_Builtin) {
    INTERNAL_ERROR("called builtin parser where there was no "
                   "builtin");
    PANIC();
  }
  bp->name = internArena(parser->ar, t.builtin);
  start = t.span.start;

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
    parseBuiltinTypeExpr(l1, parser);
    break;
  }
  case TK_Enum:
  case TK_Struct: {
    parseStructTypeExpr(l1, parser);
    break;
  }
  case TK_Void: {
    parseVoidTypeExpr(l1, parser);
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
    srte->fieldAccess.field = internArena(parser->ar, t.identifier);
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

FN_BINOP_PARSE_LX_EXPR(TypeExpr, TEK, 3, parseL2TypeExpr, opDetL3TypeExpr)

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

FN_BINOP_PARSE_LX_EXPR(TypeExpr, TEK, 4, parseL3TypeExpr, opDetL4TypeExpr)

static void parseTypeExpr(TypeExpr *tep, Parser *parser) {
  parseL4TypeExpr(tep, parser);
}

static void parseValueRestrictionPatternExpr(PatternExpr *vrpe,
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
  vrpe->valueRestriction.constExpr = RALLOC(parser->ar, ConstExpr);
  parseConstExpr(vrpe->valueRestriction.constExpr, parser);
  LnCol end = vrpe->valueRestriction.constExpr->span.end;

  vrpe->span = SPAN(start, end);
  vrpe->diagnostics_len = 0;
  return;
}

static void parseTypeRestrictionPatternExpr(PatternExpr *trpe, Parser *parser) {
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
    trpe->typeRestriction.binding = internArena(parser->ar, t.identifier);
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
  ZERO(psmep);

  Token t;
  nextTokenParser(parser, &t);

  LnCol start = t.span.end;
  LnCol end;

  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  switch (t.kind) {
  case TK_Rest: {
    psmep->kind = PSMEK_Rest;
    break;
  }
  case TK_Identifier: {
    // copy identifier
    psmep->kind = PSMEK_Field;
    psmep->field.field = internArena(parser->ar, t.identifier);
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

  nextTokenParser(parser, &t);

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

static void parseStructPatternExpr(PatternExpr *spe, Parser *parser) {
  ZERO(spe);
  spe->kind = PEK_Struct;
  Token t;
  nextTokenParser(parser, &t);
  if (t.kind != TK_Struct) {
    INTERNAL_ERROR("called struct pattern expression parser where there was no "
                   "struct pattern declaration");
    PANIC();
  }

  LnCol start = t.span.start;

  nextTokenParser(parser, &t);

  EXPECT_TYPE(t, TK_BraceLeft, HANDLE_NO_LEFTBRACE);

  Vector members;
  createVector(&members);

  Vector diagnostics;
  createVector(&diagnostics);

  LnCol end;

  PARSE_LIST(&members,                           // members_vec_ptr
             &diagnostics,                       // diagnostics_vec_ptr
             parsePatternStructMemberExpr,       // member_parse_function
             struct PatternStructMemberExpr_s,   // member_kind
             TK_BraceRight,                      // delimiting_token_kind
             DK_PatternStructExpectedRightBrace, // missing_delimiter_error
             end,                                // end_lncol
             parser                              // parser
  )

  spe->structExpr.members_len =
      VEC_LEN(&members, struct PatternStructMemberExpr_s);
  spe->structExpr.members = manageMemArena(parser->ar, releaseVector(&members));
  spe->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  spe->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  spe->span = SPAN(start, end);
  return;

HANDLE_NO_LEFTBRACE:
  spe->structExpr.members_len = 0;
  spe->structExpr.members = NULL;
  spe->span = SPAN(start, t.span.end);
  spe->diagnostics_len = 1;
  spe->diagnostics = RALLOC(parser->ar, Diagnostic);
  spe->diagnostics[0] = DIAGNOSTIC(DK_StructExpectedLeftBrace, spe->span);
  return;
}

static void parseL1PatternExpr(PatternExpr *l1, Parser *parser) {
  pushCommentScopeParser(parser);
  Token t;
  peekTokenParser(parser, &t);
  switch (t.kind) {
  case TK_Struct: {
    parseStructPatternExpr(l1, parser);
    return;
  }
  case TK_Identifier:
  case TK_Colon: {
    parseTypeRestrictionPatternExpr(l1, parser);
    break;
  }
  case TK_CompEqual:
  case TK_CompNotEqual:
  case TK_CompGreaterEqual:
  case TK_CompGreater:
  case TK_CompLess:
  case TK_CompLessEqual: {
    parseValueRestrictionPatternExpr(l1, parser);
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
  case TK_Sub: {
    l2->unaryOp.operator= PEUOK_Negate;
    break;
  }
  case TK_Add: {
    l2->unaryOp.operator= PEUOK_Posit;
    break;
  }
  case TK_Not: {
    l2->unaryOp.operator= PEUOK_Not;
    break;
  }
  default: {
    // there is no level 3 expression
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

FN_BINOP_PARSE_LX_EXPR(PatternExpr, PEK, 3, parseL2PatternExpr,
                       opDetL3PatternExpr)

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

FN_BINOP_PARSE_LX_EXPR(PatternExpr, PEK, 4, parseL3PatternExpr,
                       opDetL4PatternExpr)

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

FN_BINOP_PARSE_LX_EXPR(PatternExpr, PEK, 5, parseL4PatternExpr,
                       opDetL5PatternExpr)

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

FN_BINOP_PARSE_LX_EXPR(PatternExpr, PEK, 6, parseL5PatternExpr,
                       opDetL6PatternExpr)

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

  tdp->typeDecl.name = internArena(parser->ar, t.identifier);

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
    sp->macroStmnt.name = internArena(parser->ar, t.macro_call);
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
