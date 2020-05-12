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
    case TK_Semicolon: {
      // Semicolons are a NOP and don't appear in the AST
      // Keep Reading
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

// appends all members of the topmost comment scope on the stack to the second
// topmost member
/// REQUIRES: `parser` is pointer to valid Parser
/// REQUIRES: `parser` has 2 or more elements on the comment stack
/// REQUIRES: the top 2 elements of `parser`'s comment stack are both valid
/// vectors GUARANTEES: the top element of `parser`'s comment stack is removed
/// GUARANTEES: the secondmost element is now the top of the stack
/// GUARANTEES: the new topmost element of the stack contains all comments from
/// the old topmost member
static void mergeCommentScopeParser(Parser *parser) {
  Vector old_top = popCommentScopeParser(parser);
  Vector *new_top = VEC_PEEK(&parser->comments, Vector);

  size_t old_len = lengthVector(&old_top);
  popVector(&old_top, pushVector(new_top, old_len), old_len);
  destroyVector(&old_top);
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
  pp->pathSegments_length = VEC_LEN(&pathSegments, char *);
  pp->pathSegments = manageMemArena(parser->ar, releaseVector(&pathSegments));

  if (diagnostic.kind != DK_Ok) {
    pp->diagnostics_length = 1;
    pp->diagnostics = RALLOC(parser->ar, Diagnostic);
    pp->diagnostics[0] = diagnostic;
  } else {
    pp->diagnostics_length = 0;
    pp->diagnostics = NULL;
  }
  pp->span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  pp->comments_length = VEC_LEN(&comments, Comment);
  pp->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseIntConstExpr(ConstExpr *icep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  EXPECT_TYPE(t, TK_IntLiteral, HANDLE_NO_INT_LITERAL);
  icep->kind = CEK_IntLiteral;
  icep->intLiteral.value = t.int_literal;
  icep->span = t.span;
  icep->diagnostics_length = 0;
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
  bcep->diagnostics_length = 0;
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
  fcep->diagnostics_length = 0;
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
  ccep->diagnostics_length = 0;
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
  parseValueExpr(vcep->valueExpr.expr, parser);
  vcep->span = SPAN(start, vcep->valueExpr.expr->span.end);
  vcep->diagnostics_length = 0;
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
    parseBoolConstExpr(cep, parser);
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
    cep->diagnostics_length = 1;
    cep->diagnostics = RALLOC(parser->ar, Diagnostic);
    cep->diagnostics[0] = DIAGNOSTIC(DK_ConstExprUnrecognizedLiteral, t.span);
    // discard this token
    nextTokenParser(parser, &t);
    break;
  }
  }
  Vector comments = popCommentScopeParser(parser);
  cep->comments_length = VEC_LEN(&comments, Comment);
  cep->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseConstValueExpr(ValueExpr *cvep, Parser *parser) {
  ZERO(cvep);
  cvep->kind = VEK_ConstExpr;
  cvep->constExpr.constExpr = RALLOC(parser->ar, ConstExpr);
  parseConstExpr(cvep->constExpr.constExpr, parser);

  cvep->span = cvep->constExpr.constExpr->span;
  cvep->diagnostics_length = 0;
}

static void parseStringValueExpr(ValueExpr *svep, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  EXPECT_TYPE(t, TK_StringLiteral, HANDLE_NO_STRING_LITERAL);
  svep->kind = VEK_StringLiteral;
  svep->stringLiteral.value = internArena(parser->ar, t.string_literal);
  svep->span = t.span;
  svep->diagnostics_length = 0;
  return;

HANDLE_NO_STRING_LITERAL:
  INTERNAL_ERROR("called string literal parser where there was no "
                 "string literal");
  PANIC();
}

static void parseFnValueExpr(ValueExpr *fvep, Parser *parser) {
  ZERO(fvep);
  Token t;
  nextTokenParser(parser, &t);

  if (t.kind != TK_Fn) {
    INTERNAL_ERROR("called function value expression parser where there was no "
                   "function");
    PANIC();
  }

  LnCol start = t.span.start;
  LnCol end = t.span.end;

  // create diagnostics
  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  // check for leftparen
  nextTokenParser(parser, &t);
  Span lparenspan = t.span;
  if (t.kind != TK_ParenLeft) {
    diagnostic = DIAGNOSTIC(DK_FnValueExprExpectedLeftParen, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  fvep->fnExpr.parameters = RALLOC(parser->ar, PatternExpr);
  parsePatternExpr(fvep->fnExpr.parameters, parser);

  peekTokenParser(parser, &t);
  if (t.kind == TK_Colon) {
    fvep->fnExpr.type = RALLOC(parser->ar, TypeExpr);
    parseTypeExpr(fvep->fnExpr.type, parser);
    end = fvep->fnExpr.type->span.end;
    // advance
    nextTokenParser(parser, &t);
  } else {
    fvep->fnExpr.type = RALLOC(parser->ar, TypeExpr);
    fvep->fnExpr.type->kind = TEK_Omitted;
    fvep->fnExpr.type->span = lparenspan;
    fvep->fnExpr.type->diagnostics_length = 0;
    fvep->fnExpr.type->comments_length = 0;
  }

  nextTokenParser(parser, &t);

  if (t.kind != TK_Arrow) {
    diagnostic = DIAGNOSTIC(DK_FnValueExprExpectedArrow, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  fvep->fnExpr.body = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(fvep->fnExpr.body, parser);
  end = fvep->fnExpr.body->span.end;

CLEANUP:
  if (diagnostic.kind == DK_Ok) {
    fvep->diagnostics_length = 0;
  } else {
    fvep->diagnostics_length = 1;
    fvep->diagnostics = RALLOC(parser->ar, Diagnostic);
    fvep->diagnostics[0] = diagnostic;
  }
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

  bvep->blockExpr.statements_length = VEC_LEN(&statements, Stmnt);
  bvep->blockExpr.statements =
      manageMemArena(parser->ar, releaseVector(&statements));
  bvep->span = SPAN(start, end);
  bvep->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
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

static void parseBreakValueExpr(ValueExpr *bep, Parser *parser) {
  bep->kind = VEK_Break;
  Token t;
  nextTokenParser(parser, &t);
  bep->span = t.span;
  EXPECT_TYPE(t, TK_Break, HANDLE_NO_BREAK);
  bep->breakExpr.value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(bep->breakExpr.value, parser);
  bep->diagnostics_length = 0;
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
  cep->diagnostics_length = 0;
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
  rep->diagnostics_length = 0;
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
  wep->diagnostics_length = 0;
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

  // Get pattern
  mcep->pattern = RALLOC(parser->ar, PatternExpr);
  parsePatternExpr(mcep->pattern, parser);

  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  LnCol start = mcep->pattern->span.start;
  LnCol end = mcep->pattern->span.end;

  // Expect colon
  nextTokenParser(parser, &t);
  if (t.kind != TK_Colon) {
    diagnostic = DIAGNOSTIC(DK_MatchCaseNoColon, t.span);
    resyncParser(parser);
    goto CLEANUP;
  }

  // Get Value
  mcep->value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(mcep->value, parser);
  end = mcep->value->span.end;

CLEANUP:
  if (diagnostic.kind != DK_Ok) {
    mcep->diagnostics_length = 1;
    mcep->diagnostics = RALLOC(parser->ar, Diagnostic);
    mcep->diagnostics[0] = diagnostic;
  } else {
    mcep->diagnostics_length = 0;
  }

  mcep->span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  mcep->comments_length = VEC_LEN(&comments, Comment);
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
  mvep->matchExpr.cases_length = VEC_LEN(&cases, struct MatchCaseExpr_s);
  mvep->matchExpr.cases = manageMemArena(parser->ar, releaseVector(&cases));

  mvep->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  mvep->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  mvep->span = SPAN(start, end);
  return;

HANDLE_NO_MATCH:
  INTERNAL_ERROR("called match parser where there was no match");
  PANIC();

HANDLE_NO_LEFTBRACE:
  mvep->diagnostics_length = 1;
  mvep->diagnostics = RALLOC(parser->ar, Diagnostic);
  mvep->diagnostics[0] = DIAGNOSTIC(DK_MatchNoLeftBrace, t.span);
  mvep->span = SPAN(start, t.span.end);
  mvep->matchExpr.cases_length = 0;
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
  rvep->diagnostics_length = 0;
  return;
}

// Level1ValueExpr parentheses, braces, literals
// Level2ValueExpr as () [] & @ . -> (postfixes)
// Level3ValueExpr - + ! (prefixes)
// Level4ValueExpr * / % (multiplication and division)
// Level5ValueExpr + - (addition and subtraction)
// Level7ValueExpr < <= > >= == != (comparators)
// Level8ValueExpr && (logical and)
// Level8ValueExpr || (logical or)
// Level10ValueExpr , (create tuple)
// Level11ValueExpr = += -= *= /= %= (Assignment)

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
  case TK_If: {
    parseIfValueExpr(l1, parser);
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
    l1->diagnostics_length = 1;
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
  l1->comments_length = VEC_LEN(&comments, Comment);
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
    fave->diagnostics_length = 1;
    fave->diagnostics = RALLOC(parser->ar, Diagnostic);
    fave->diagnostics[0] = DIAGNOSTIC(DK_FieldAccessExpectedIdentifier, t.span);
  } else {
    fave->fieldAccess.field = internArena(parser->ar, t.identifier);
    fave->diagnostics_length = 0;
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

  cvep->callExpr.parameters = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(cvep->callExpr.parameters, parser);

  nextTokenParser(parser, &t);
  if (t.kind != TK_ParenRight) {
    cvep->diagnostics_length = 1;
    cvep->diagnostics = RALLOC(parser->ar, Diagnostic);
    cvep->diagnostics[0] = DIAGNOSTIC(DK_CallExpectedParen, t.span);
  } else {
    cvep->diagnostics_length = 0;
  }
  end = t.span.end;

  cvep->span = SPAN(root->span.start, end);
}

static void parseL2ValueExpr(ValueExpr *l2, Parser *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff
  parseL1ValueExpr(l2, parser);

  while (true) {
    Token t;
    // represents the new operation
    ValueExpr *v;

    peekTokenParser(parser, &t);
    switch (t.kind) {
    case TK_Ref: {
      pushCommentScopeParser(parser);
      v = RALLOC(parser->ar, ValueExpr);
      v->kind = VEK_UnaryOp;
      v->unaryOp.operator= VEUOK_Ref;
      v->unaryOp.operand = l2;
      v->span = SPAN(l2->span.start, t.span.end);
      v->diagnostics_length = 0;
      nextTokenParser(parser, &t);
      break;
    }
    case TK_Deref: {
      pushCommentScopeParser(parser);
      v = RALLOC(parser->ar, ValueExpr);
      v->kind = VEK_UnaryOp;
      v->unaryOp.operator= VEUOK_Deref;
      v->unaryOp.operand = l2;
      v->span = SPAN(l2->span.start, t.span.end);
      v->diagnostics_length = 0;
      nextTokenParser(parser, &t);
      break;
    }
    case TK_FieldAccess: {
      pushCommentScopeParser(parser);
      v = RALLOC(parser->ar, ValueExpr);
      parseFieldAccessValueExpr(v, parser, l2);
      break;
    }
    case TK_ParenLeft: {
      pushCommentScopeParser(parser);
      v = RALLOC(parser->ar, ValueExpr);
      parseCallValueExpr(v, parser, l2);
      break;
    }
    default: {
      // there are no more level 2 expressions
      return;
    }
    }

    Vector comments = popCommentScopeParser(parser);
    v->comments_length = VEC_LEN(&comments, Comment);
    v->comments = manageMemArena(parser->ar, releaseVector(&comments));
    l2 = v;
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
  l3->diagnostics_length = 0;

  // comments
  Vector comments = popCommentScopeParser(parser);
  l3->comments_length = VEC_LEN(&comments, Comment);
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
    l##x->diagnostics_length = 0;                                              \
                                                                               \
    /* comments */                                                             \
    Vector comments = popCommentScopeParser(parser);                           \
    l##x->comments_length = VEC_LEN(&comments, Comment);                       \
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
    *val = VEBOK_Mul;
    return true;
  }
  case TK_Mod: {
    *val = VEBOK_Mul;
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
  case TK_ShiftLeft: {
    *val = VEBOK_BitShl;
    return true;
  }
  case TK_ShiftRight: {
    *val = VEBOK_BitShr;
    return true;
  }
  case TK_BitAnd: {
    *val = VEBOK_BitAnd;
    return true;
  }
  case TK_BitOr: {
    *val = VEBOK_BitOr;
    return true;
  }
  case TK_BitXor: {
    *val = VEBOK_BitXor;
    return true;
  }
  default: {
    // there is no level 6 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 6, parseL5ValueExpr, opDetL6ValueExpr)

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
    // there is no level 7 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 7, parseL6ValueExpr, opDetL7ValueExpr)

static inline bool opDetL8ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_And: {
    *val = VEBOK_CompLess;
    return true;
  }
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

static bool opDetL9ValueExpr(TokenKind tk, enum ValueExprBinaryOpKind_e *val) {
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
  case TK_AssignBitAnd: {
    *val = VEBOK_AssignBitAnd;
    return true;
  }
  case TK_AssignBitOr: {
    *val = VEBOK_AssignBitOr;
    return true;
  }
  default: {
    // There is no level 9 expr
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 9, parseL8ValueExpr, opDetL9ValueExpr)

// shim method
static void parseValueExpr(ValueExpr *vep, Parser *parser) {
  parseL9ValueExpr(vep, parser);
}

// field = Value
static void
parseStructLiteralMemberExpr(struct StructLiteralMemberExpr_s *slmep,
                             Parser *parser) {
  // zero-initialize bp
  ZERO(slmep);
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
    slmep->name = NULL;
    slmep->value = NULL;
    diagnostic = DIAGNOSTIC(DK_StructMemberLiteralExpectedIdentifier, t.span);
    end = identitySpan.end;
    goto CLEANUP;
  }

  slmep->name = internArena(parser->ar, t.identifier);

  // check if assign
  nextTokenParser(parser, &t);
  if (t.kind == TK_Colon) {
    // Get value of variable
    slmep->value = RALLOC(parser->ar, ValueExpr);
    parseValueExpr(slmep->value, parser);
    end = slmep->value->span.end;
  } else {
    diagnostic = DIAGNOSTIC(DK_StructMemberLiteralExpectedIdentifier, t.span);
    slmep->value = NULL;
    end = t.span.end;
    goto CLEANUP;
  }

CLEANUP:
  slmep->span = SPAN(start, end);

  if (diagnostic.kind != DK_Ok) {
    slmep->diagnostics_length = 1;
    slmep->diagnostics = RALLOC(parser->ar, Diagnostic);
    slmep->diagnostics[0] = diagnostic;
  } else {
    slmep->diagnostics_length = 0;
    slmep->diagnostics = NULL;
  }

  Vector comments = popCommentScopeParser(parser);
  slmep->comments_length = VEC_LEN(&comments, Comment);
  slmep->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

// field : Type,
static void parseStructMemberExpr(struct StructMemberExpr_s *smep,
                                  Parser *parser) {
  // zero-initialize bp
  ZERO(smep);
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
    smep->name = NULL;
    diagnostic = DIAGNOSTIC(DK_StructMemberExpectedIdentifier, t.span);
    end = identitySpan.end;
    goto CLEANUP;
  }

  smep->name = internArena(parser->ar, t.identifier);

  // check if colon
  peekTokenParser(parser, &t);
  if (t.kind == TK_Colon) {
    // advance through colon
    nextTokenParser(parser, &t);
    // Get type of variable
    smep->type = RALLOC(parser->ar, TypeExpr);
    parseTypeExpr(smep->type, parser);
    end = smep->type->span.end;
  } else {
    end = identitySpan.end;
    smep->type = RALLOC(parser->ar, TypeExpr);
    smep->type->kind = TEK_Omitted;
    smep->type->span = identitySpan;
    smep->type->diagnostics_length = 0;
    smep->type->comments_length = 0;
  }

CLEANUP:
  smep->span = SPAN(start, end);

  if (diagnostic.kind != DK_Ok) {
    smep->diagnostics_length = 1;
    smep->diagnostics = RALLOC(parser->ar, Diagnostic);
    smep->diagnostics[0] = diagnostic;
  } else {
    smep->diagnostics_length = 0;
    smep->diagnostics = NULL;
  }

  Vector comments = popCommentScopeParser(parser);
  smep->comments_length = VEC_LEN(&comments, Comment);
  smep->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

static void parseStructTypeExpr(TypeExpr *ste, Parser *parser) {
  ZERO(ste);
  ste->kind = TEK_Struct;
  Token t;
  nextTokenParser(parser, &t);
  switch (t.kind) {
  case TK_Struct: {
    ste->structExpr.kind = TESK_Struct;
    break;
  }
  case TK_Pack: {
    ste->structExpr.kind = TESK_Pack;
    break;
  }
  case TK_Union: {
    ste->structExpr.kind = TESK_Union;
    break;
  }
  case TK_Enum: {
    ste->structExpr.kind = TESK_Union;
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

  PARSE_LIST(&members,                    // members_vec_ptr
             &diagnostics,                // diagnostics_vec_ptr
             parseStructMemberExpr,       // member_parse_function
             struct StructMemberExpr_s,   // member_kind
             TK_BraceRight,               // delimiting_token_kind
             DK_StructExpectedRightBrace, // missing_delimiter_error
             end,                         // end_lncol
             parser                       // parser
  )

  ste->structExpr.members_length = VEC_LEN(&members, struct StructMemberExpr_s);
  ste->structExpr.members = manageMemArena(parser->ar, releaseVector(&members));
  ste->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  ste->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  ste->span = SPAN(start, end);
  return;

HANDLE_NO_LEFTBRACE:
  ste->structExpr.members_length = 0;
  ste->structExpr.members = NULL;
  ste->span = SPAN(start, t.span.end);
  ste->diagnostics_length = 1;
  ste->diagnostics = RALLOC(parser->ar, Diagnostic);
  ste->diagnostics[0] = DIAGNOSTIC(DK_StructExpectedLeftBrace, ste->span);
  return;
}

static void parseReferenceTypeExpr(TypeExpr *rtep, Parser *parser) {
  ZERO(rtep);
  rtep->kind = TEK_Reference;
  rtep->referenceExpr.path = RALLOC(parser->ar, Path);
  parsePath(rtep->referenceExpr.path, parser);
  rtep->diagnostics_length = 0;
  rtep->span = rtep->referenceExpr.path->span;
}

static void parseVoidTypeExpr(TypeExpr *vte, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  EXPECT_TYPE(t, TK_Void, HANDLE_NO_VOID);
  vte->kind = TEK_Void;
  vte->span = t.span;
  vte->diagnostics_length = 0;
  return;

HANDLE_NO_VOID:
  INTERNAL_ERROR("called void type expression parser where there was no "
                 "void");
  PANIC();
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
  LnCol end = t.span.end;

  // create diagnostics
  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  // check for leftparen
  nextTokenParser(parser, &t);
  if (t.kind != TK_ParenLeft) {
    diagnostic = DIAGNOSTIC(DK_FnTypeExprExpectedLeftParen, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  fte->fnExpr.parameters = RALLOC(parser->ar, TypeExpr);
  parseTypeExpr(fte->fnExpr.parameters, parser);

  nextTokenParser(parser, &t);
  if (t.kind != TK_Colon) {
    diagnostic = DIAGNOSTIC(DK_FnTypeExprExpectedColon, t.span);
    end = t.span.end;
  }

  fte->fnExpr.type = RALLOC(parser->ar, TypeExpr);
  parseTypeExpr(fte->fnExpr.type, parser);

CLEANUP:
  if (diagnostic.kind == DK_Ok) {
    fte->diagnostics_length = 0;
  } else {
    fte->diagnostics_length = 1;
    fte->diagnostics = RALLOC(parser->ar, Diagnostic);
    fte->diagnostics[0] = diagnostic;
  }
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
  case TK_Enum:
  case TK_Pack:
  case TK_Union:
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
    l1->diagnostics_length = 1;
    l1->diagnostics = RALLOC(parser->ar, Diagnostic);
    l1->diagnostics[0] = DIAGNOSTIC(DK_TypeExprUnexpectedToken, t.span);
    // Resync
    resyncParser(parser);
    break;
  }
  }

  Vector comments = popCommentScopeParser(parser);
  l1->comments_length = VEC_LEN(&comments, Comment);
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
    srte->diagnostics_length = 1;
    srte->diagnostics = RALLOC(parser->ar, Diagnostic);
    srte->diagnostics[0] =
        DIAGNOSTIC(DK_TypeExprFieldAccessExpectedIdentifier, t.span);
  } else {
    srte->fieldAccess.field = internArena(parser->ar, t.identifier);
    srte->diagnostics_length = 0;
  }

  srte->span = SPAN(root->span.start, t.span.end);
}

static void parseL2TypeExpr(TypeExpr *l2, Parser *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff
  parseL1TypeExpr(l2, parser);

  while (true) {
    Token t;
    // represents the new operation
    TypeExpr *ty;

    peekTokenParser(parser, &t);
    switch (t.kind) {
    case TK_Ref: {
      pushCommentScopeParser(parser);
      ty = RALLOC(parser->ar, TypeExpr);
      ty->kind = TEK_UnaryOp;
      ty->unaryOp.operator= TEUOK_Ref;
      ty->unaryOp.operand = l2;
      ty->span = SPAN(l2->span.start, t.span.end);
      ty->diagnostics_length = 0;
      nextTokenParser(parser, &t);
      break;
    }
    case TK_Deref: {
      pushCommentScopeParser(parser);
      ty = RALLOC(parser->ar, TypeExpr);
      ty->kind = TEK_UnaryOp;
      ty->unaryOp.operator= TEUOK_Deref;
      ty->unaryOp.operand = l2;
      ty->span = SPAN(l2->span.start, t.span.end);
      ty->diagnostics_length = 0;
      nextTokenParser(parser, &t);
      break;
    }
    case TK_ScopeResolution: {
      pushCommentScopeParser(parser);
      ty = RALLOC(parser->ar, TypeExpr);
      parseScopeResolutionTypeExpr(ty, parser, l2);
      break;
    }
    default: {
      // there are no more level2 expressions
      return;
    }
    }

    Vector comments = popCommentScopeParser(parser);
    ty->comments_length = VEC_LEN(&comments, Comment);
    ty->comments = manageMemArena(parser->ar, releaseVector(&comments));
    l2 = ty;
  }
}

static inline bool opDetL3TypeExpr(TokenKind tk,
                                   enum TypeExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Comma: {
    *val = TEBOK_Product;
    return true;
  }
  default: {
    // there is no level 4 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(TypeExpr, TEK, 3, parseL2TypeExpr, opDetL3TypeExpr)

static inline bool opDetL4TypeExpr(TokenKind tk,
                                   enum TypeExprBinaryOpKind_e *val) {
  switch (tk) {
  case TK_Comma: {
    *val = TEBOK_Product;
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
  vrpe->valueRestriction.value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(vrpe->valueRestriction.value, parser);
  LnCol end = vrpe->valueRestriction.value->span.end;

  vrpe->span = SPAN(start, end);
  vrpe->diagnostics_length = 0;
  return;
}

static void parseTypeRestrictionPatternExpr(PatternExpr *trpe, Parser *parser) {
  ZERO(trpe);

  trpe->kind = PEK_TypeRestriction;

  bool parseType = false;

  Token t;
  advanceToken(parser, &t);

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
    advanceToken(parser, &t);
    if (t.kind == TK_Colon) {
      parseType = true;
    } else {
      parseType = false;
      setNextToken(parser, &t);
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
  }

  trpe->span = SPAN(start, end);
  trpe->diagnostics_length = 0;
}

static void parseGroupPatternExpr(PatternExpr *gpe, Parser *parser) {
  ZERO(gpe);

  Token t;
  advanceToken(parser, &t);
  if (t.kind != TK_BraceLeft) {
    INTERNAL_ERROR("called group pattern expr parser where there was "
                   "no group pattern expr");
    PANIC();
  }

  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  LnCol start = t.span.start;

  gpe->kind = PEK_UnaryOp;
  gpe->unaryOperator.operator= PEUOK_Group;
  gpe->unaryOperator.operand = RALLOC(parser->ar, PatternExpr);
  parsePatternExpr(gpe->unaryOperator.operand, parser);

  LnCol end = gpe->unaryOperator.operand->span.end;
  advanceToken(parser, &t);

  if (t.kind != TK_BraceRight) {
    diagnostic = DIAGNOSTIC(DK_PatternGroupExpectedRightBrace, t.span);
    resyncParser(parser);
  } else {
    end = t.span.end;
  }

  if (diagnostic.kind != DK_Ok) {
    gpe->diagnostics_length = 1;
    gpe->diagnostics = RALLOC(parser->ar, Diagnostic);
    gpe->diagnostics[0] = diagnostic;
  } else {
    gpe->diagnostics_length = 0;
    gpe->diagnostics = NULL;
  }

  gpe->span = SPAN(start, end);
}

static void
parsePatternExprStructMemberExpr(struct PatternExprStructMemberExpr_s *pesme,
                                 Parser *parser) {
  ZERO(pesme);

  Token t;
  advanceToken(parser, &t);

  LnCol start = t.span.end;
  LnCol end;

  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  switch (t.kind) {
  case TK_Rest: {
    pesme->kind = PESMEK_Rest;
    end = t.span.end;
    break;
  }
  case TK_Identifier: {
    // copy identifier
    pesme->kind = PESMEK_Field;
    pesme->field_name = internArena(parser->ar, t.identifier);
    end = t.span.end;
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
  pesme->pattern = RALLOC(parser->ar, PatternExpr);
  parsePatternExpr(pesme->pattern, parser);

  end = pesme->pattern->span.end;

  advanceToken(parser, &t);

  if (pesme->pattern->kind == PEK_ValueRestriction && has_assign) {
    diagnostic =
        DIAGNOSTIC(DK_PatternStructUnexpectedAssignForValueRestriction, t.span);
    goto CLEANUP;
  } else if (pesme->pattern->kind != PEK_ValueRestriction && !has_assign) {
    diagnostic = DIAGNOSTIC(
        DK_PatternStructExpectedAssignForNonValueRestriction, t.span);
    goto CLEANUP;
  }

CLEANUP:
  if (diagnostic.kind != DK_Ok) {
    pesme->diagnostics_length = 1;
    pesme->diagnostics = RALLOC(parser->ar, Diagnostic);
    pesme->diagnostics[0] = diagnostic;
  } else {
    pesme->diagnostics_length = 0;
  }

  pesme->span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  pesme->comments_length = VEC_LEN(&comments, Comment);
  pesme->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

static void parseStructPatternExpr(PatternExpr *spe, Parser *parser) {
  ZERO(spe);
  spe->kind = PEK_Struct;
  Token t;
  nextTokenParser(parser, &t);
  switch (t.kind) {
  case TK_Struct: {
    spe->structExpr.kind = PESK_Struct;
    break;
  }
  case TK_Pack: {
    spe->structExpr.kind = PESK_Pack;
    break;
  }
  case TK_Union: {
    spe->structExpr.kind = PESK_Union;
    break;
  }
  default: {
    INTERNAL_ERROR("called struct pattern expression parser where there was no "
                   "struct pattern declaration");
    PANIC();
  }
  }

  LnCol start = t.span.start;

  advanceToken(parser, &t);

  EXPECT_TYPE(t, TK_BraceLeft, HANDLE_NO_LEFTBRACE);

  Vector members;
  createVector(&members);

  Vector diagnostics;
  createVector(&diagnostics);

  LnCol end;

  PARSE_LIST(&members,                             // members_vec_ptr
             &diagnostics,                         // diagnostics_vec_ptr
             parsePatternExprStructMemberExpr,     // member_parse_function
             struct PatternExprStructMemberExpr_s, // member_kind
             TK_BraceRight,                        // delimiting_token_kind
             DK_PatternStructExpectedRightBrace,   // missing_delimiter_error
             end,                                  // end_lncol
             parser                                // parser
  )

  spe->structExpr.members_length =
      VEC_LEN(&members, struct PatternExprStructMemberExpr_s);
  spe->structExpr.members = manageMemArena(parser->ar, releaseVector(&members));
  spe->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  spe->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));
  spe->span = SPAN(start, end);
  return;

HANDLE_NO_LEFTBRACE:
  spe->structExpr.members_length = 0;
  spe->structExpr.members = NULL;
  spe->span = SPAN(start, t.span.end);
  spe->diagnostics_length = 1;
  spe->diagnostics = RALLOC(parser->ar, Diagnostic);
  spe->diagnostics[0] = DIAGNOSTIC(DK_StructExpectedLeftBrace, spe->span);
  return;
}

static void parseL1PatternExpr(PatternExpr *l1, Parser *parser) {
  // TODO  convert for patterns
  pushCommentScopeParser(parser);
  Token t;
  advanceToken(parser, &t);
  switch (t.kind) {
  case TK_Pack:
  case TK_Union:
  case TK_Struct: {
    setNextToken(parser, &t);
    parseStructPatternExpr(l1, parser);
    return;
  }
  case TK_CompEqual:
  case TK_CompNotEqual:
  case TK_CompGreaterEqual:
  case TK_CompGreater:
  case TK_CompLess:
  case TK_CompLessEqual: {
    setNextToken(parser, &t);
    parseValueRestrictionPatternExpr(l1, parser);
    break;
  }
  case TK_BraceLeft: {
    setNextToken(parser, &t);
    parseGroupPatternExpr(l1, parser);
    break;
  }
  default: {
    l1->kind = PEK_None;
    l1->span = t.span;
    l1->diagnostics_length = 1;
    l1->diagnostics = RALLOC(parser->ar, Diagnostic);
    l1->diagnostics[0] = DIAGNOSTIC(DK_TypeExprUnexpectedToken, t.span);
    // Resync
    resyncParser(parser);
    break;
  }
  }

  Vector comments = popCommentScopeParser(parser);
  l1->comments_length = VEC_LEN(&comments, Comment);
  l1->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseL2PatternExpr(PatternExpr *l2, Parser *parser) {}

static void parsePatternExpr(PatternExpr *ppe, Parser *parser) {
  // zero-initialize bp
  ZERO(bp);
  pushCommentScopeParser(parser);

  Token t;

  // diagnostics
  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  LnCol start;
  LnCol end;

  // get identifier
  advanceToken(parser, &t);

  Span identitySpan = t.span;

  start = identitySpan.start;

  if (t.kind != TK_Identifier) {
    bp->name = NULL;
    diagnostic = DIAGNOSTIC(DK_BindingExpectedIdentifier, t.span);
    end = identitySpan.end;
    goto CLEANUP;
  }

  bp->name = internArena(parser->ar, t.identifier);

  // check if colon
  advanceToken(parser, &t);
  if (t.kind == TK_Colon) {
    // Get type of variable
    bp->type = RALLOC(parser->ar, TypeExpr);
    parseTypeExpr(bp->type, parser);
    end = bp->type->span.end;
  } else {
    end = identitySpan.end;
    // push back whatever
    setNextToken(parser, &t);
    bp->type = RALLOC(parser->ar, TypeExpr);
    bp->type->kind = TEK_Omitted;
    bp->type->span = identitySpan;
    bp->type->diagnostics_length = 0;
    bp->type->comments_length = 0;
  }

CLEANUP:
  bp->span = SPAN(start, end);

  if (diagnostic.kind != DK_Ok) {
    bp->diagnostics_length = 1;
    bp->diagnostics = RALLOC(parser->ar, Diagnostic);
    bp->diagnostics[0] = diagnostic;
  } else {
    bp->diagnostics_length = 0;
    bp->diagnostics = NULL;
  }

  Vector comments = popCommentScopeParser(parser);
  bp->comments_length = VEC_LEN(&comments, Comment);
  bp->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

static void parseVarDeclStmnt(Stmnt *vdsp, Parser *parser) {
  // zero-initialize vdsp
  ZERO(vdsp);
  vdsp->kind = SK_VarDecl;
  // these variables will be reused
  Token t;

  // Skip let declaration
  advanceToken(parser, &t);
  // The start of the variable declaration
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_Let, HANDLE_NO_LET);

  // Get Binding
  vdsp->varDecl.binding = RALLOC(parser->ar, Binding);
  parseBinding(vdsp->varDecl.binding, parser);

  // Expect Equal Sign
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_Assign, HANDLE_NO_ASSIGN);

  // Get Value;
  vdsp->varDecl.value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(vdsp->varDecl.value, parser);
  vdsp->diagnostics_length = 0;
  return;

HANDLE_NO_LET:
  INTERNAL_ERROR("called variable declaration parser where there was no "
                 "variable declaration");
  PANIC();

HANDLE_NO_ASSIGN:
  vdsp->span = SPAN(start, t.span.end);
  vdsp->diagnostics_length = 1;
  vdsp->diagnostics = RALLOC(parser->ar, Diagnostic);
  vdsp->diagnostics[0] = DIAGNOSTIC(DK_VarDeclStmntExpectedAssign, t.span);
  resyncParser(parser);
}

static void parseTypeDecl(Stmnt *tdp, Parser *parser) {
  ZERO(tdp);
  tdp->kind = SK_TypeAliasStmnt;
  Token t;
  advanceToken(parser, &t);

  if (t.kind != TK_TypeAlias) {
    INTERNAL_ERROR("called type alias declaration parser where there was no "
                   "type alias declaration");
    PANIC();
  }

  LnCol start = t.span.start;

  LnCol end;

  advanceToken(parser, &t);

  if (t.kind != TK_Identifier) {
    tdp->typeAliasStmnt.name = NULL;
    tdp->diagnostics = RALLOC(parser->ar, Diagnostic);
    tdp->diagnostics[0] = DIAGNOSTIC(DK_TypeAliasExpectedIdentifier, t.span);
    tdp->diagnostics_length = 1;
    end = t.span.end;
    goto CLEANUP;
  }

  tdp->typeAliasStmnt.name = internArena(parser->ar, t.identifier);

  // Now get equals sign
  advanceToken(parser, &t);
  if (t.kind != TK_Assign) {
    tdp->diagnostics = RALLOC(parser->ar, Diagnostic);
    tdp->diagnostics[0] = DIAGNOSTIC(DK_TypeAliasExpectedAssign, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  tdp->typeAliasStmnt.type = RALLOC(parser->ar, TypeExpr);
  parseTypeExpr(tdp->typeAliasStmnt.type, parser);
  end = tdp->typeAliasStmnt.type->span.end;
  tdp->diagnostics_length = 0;
  tdp->diagnostics = NULL;

CLEANUP:
  tdp->span = SPAN(start, end);
  return;
}

static void parseStmnt(Stmnt *sp, Parser *parser) {
  pushCommentScopeParser(parser);

  Token t;
  // peek next token
  advanceToken(parser, &t);
  setNextToken(parser, &t);

  switch (t.kind) {
  case TK_Let: {
    parseVarDeclStmnt(sp, parser);
    break;
  }
  case TK_TypeAlias: {
    parseTypeAliasStmnt(sp, parser);
    break;
  }
  default: {
    // Value Expr Statement
    sp->kind = SK_Expr;
    sp->exprStmnt.value = RALLOC(parser->ar, ValueExpr);
    parseValueExpr(sp->exprStmnt.value, parser);
    sp->span = sp->exprStmnt.value->span;
    sp->diagnostics_length = 0;
    break;
  }
  }

  Vector comments = popCommentScopeParser(parser);
  sp->comments_length = VEC_LEN(&comments, Comment);
  sp->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseTranslationUnit(TranslationUnit *tu, Parser *parser) {
  pushCommentScopeParser(parser);

  Token t;
  Vector statements;
  createVector(&statements);

  // List of diagnostics
  Vector diagnostics;
  createVector(&diagnostics);

  while (true) {
    // Check for EOF
    advanceToken(parser, &t);
    if (t.kind == TK_None && t.error == DK_EOF) {
      break;
    }
    // If it wasn't an EOF, we push it back
    setNextToken(parser, &t);

    // Parse and push the statement
    parseStmnt(VEC_PUSH(&statements, Stmnt), parser);

    // semicolon is required
    advanceToken(parser, &t);
    if (t.kind == TK_None && t.error == DK_EOF) {
      // We've hit the end of the file
      break;
    } else if (t.kind == TK_Semicolon) {
      // Do nothing
    } else {
      // give them a missing semicolon error, but keep parsing
      setNextToken(parser, &t);
      *VEC_PUSH(&diagnostics, Diagnostic) =
          DIAGNOSTIC(DK_BlockExpectedSemicolon, t.span);
    }
  }

  LnCol end = t.span.end;

  tu->statements_length = VEC_LEN(&statements, Stmnt);
  tu->statements = manageMemArena(parser->ar, releaseVector(&statements));
  tu->span = SPAN(LNCOL(0, 0), end);
  tu->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  tu->diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));

  Vector comments = popCommentScopeParser(parser);
  tu->comments_length = VEC_LEN(&comments, Comment);
  tu->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

void parseTranslationUnitParser(Parser *pp, TranslationUnit *tu) {
  parseTranslationUnit(tu, pp);
}
