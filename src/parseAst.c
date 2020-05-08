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
    advanceToken(parser, &PARSE_LIST_token);                                   \
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
    setNextToken(parser, &PARSE_LIST_token);                                   \
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
}

// If has an ungotten token, return that. Else return next in line, and cache it
static void advanceToken(Parser *pp, Token *t) {
  if (pp->has_next_token) {
    *t = pp->next_token;
    pp->has_next_token = false;
  } else {
    while (true) {
      Token c;
      lexNextToken(pp->lexer, &c);
      if (c.kind == TK_Comment) {
        // Push a comment object to the vector on the top of the stack
        Vector *current_scope = VEC_PEEK(&pp->comments, Vector);

        *VEC_PUSH(current_scope, Comment) =
            (Comment){.span = c.span,
                      .scope = internArena(pp->ar, c.comment.scope),
                      .data = internArena(pp->ar, c.comment.comment)};
      } else {
        *t = c;
        break;
      }
    }
  }
}

// If token exists in line
static void setNextToken(Parser *pp, Token *t) {
  if (!pp->has_next_token) {
    pp->has_next_token = true;
    pp->next_token = *t;
  } else {
    INTERNAL_ERROR("already set next token");
    PANIC();
  }
}

// pops the top comment scope off the comment stack
/// REQUIRES: `parser` is pointer to valid Parser
/// REQUIRES: the stack has at least one member
/// GUARANTEES: return value is the topmost element of the comment stack
/// GUARANTEES: the topmost element fo the comment stack has been removed
static Vector popCommentScope(Parser *parser) {
  Vector v;
  VEC_POP(&parser->comments, &v, Vector);
  return v;
}

// pushes a new empty comment scope to the top of the comment stack
/// REQUIRES: `parser` is pointer to valid Parser
/// GUARANTEES: `parser`'s comment stack has new empty scope on top of stack
static void pushNewCommentScope(Parser *parser) {
  // We create the vector with zero capacity initially so that
  // allocation is deferred until we actually encounter a comment
  // Most scopes will not have a comment
  Vector *v = VEC_PUSH(&parser->comments, Vector);
  createWithCapacityVector(v, 10);
}

Arena *releaseParser(Parser *pp) {
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
static void mergeCommentScope(Parser *parser) {
  Vector old_top = popCommentScope(parser);
  Vector *new_top = VEC_PEEK(&parser->comments, Vector);

  size_t old_len = lengthVector(&old_top);
  popVector(&old_top, pushVector(new_top, old_len), old_len);
  destroyVector(&old_top);
}

// Jump till after semicolon, taking into account parentheses, brackets,
// attributes, and braces It will ignore subexprs, but can halt at the end
static void resyncStmnt(Parser *parser) {
  int64_t parenDepth = 0;
  int64_t braceDepth = 0;
  int64_t bracketDepth = 0;

  while (true) {
    Token t;
    advanceToken(parser, &t);
    switch (t.kind) {
    case TK_ParenLeft: {
      parenDepth++;
      break;
    }
    case TK_ParenRight: {
      parenDepth--;
      if (parenDepth < 0) {
        return;
      }
      break;
    }
    case TK_BraceLeft: {
      braceDepth++;
      break;
    }
    case TK_BraceRight: {
      braceDepth--;
      if (braceDepth < 0) {
        return;
      }
      break;
    }
    case TK_BracketLeft: {
      bracketDepth++;
      break;
    }
    case TK_BracketRight: {
      bracketDepth--;
      if (bracketDepth < 0) {
        return;
      }
      break;
    }
    case TK_Semicolon: // Fallthrough
    case TK_Comma: {
      if (braceDepth <= 0 && bracketDepth <= 0 && parenDepth <= 0) {
        // put the comma or semicolon back on the stack to parse
        setNextToken(parser, &t);
        return;
      }
      break;
    }
    case TK_None: {
      if (t.error == DK_EOF) {
        return;
      }
      break;
    }
    default: {
      break;
    }
    }
  }
}

// Note that all errors resync at the statement level
static void parseStmnt(Stmnt *sp, Parser *parser);
static void parseValueExpr(ValueExpr *vep, Parser *parser);
static void parseTypeExpr(TypeExpr *tep, Parser *parser);
static void parsePatternExpr(PatternExpr *pp, Parser *parser);

static void parsePath(Path *pp, Parser *parser) {
  // start comment scope
  pushNewCommentScope(parser);

  Token t;
  advanceToken(parser, &t);
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
    advanceToken(parser, &t);
    if (t.kind == TK_ScopeResolution) {
      advanceToken(parser, &t);
      if (t.kind != TK_Identifier) {
        diagnostic = DIAGNOSTIC(DK_PathExpectedIdentifier, t.span);
        end = t.span.end;
        goto CLEANUP;
      }

      *VEC_PUSH(&pathSegments, char *) = internArena(parser->ar, t.identifier);
    } else {
      // we've reached the end of the path
      setNextToken(parser, &t);
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

  Vector comments = popCommentScope(parser);
  pp->comments_length = VEC_LEN(&comments, Comment);
  pp->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseIntValueExpr(ValueExpr *ivep, Parser *parser) {
  Token t;
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_IntLiteral, HANDLE_NO_INT_LITERAL);
  ivep->kind = VEK_IntLiteral;
  ivep->intLiteral.value = t.int_literal;
  ivep->span = t.span;
  ivep->diagnostics_length = 0;
  return;

HANDLE_NO_INT_LITERAL:
  INTERNAL_ERROR("called int literal parser where there was no "
                 "int literal");
  PANIC();
}

static void parseBoolValueExpr(ValueExpr *bvep, Parser *parser) {
  Token t;
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_BoolLiteral, HANDLE_NO_BOOL_LITERAL);
  bvep->kind = VEK_BoolLiteral;
  bvep->boolLiteral.value = t.bool_literal;
  bvep->span = t.span;
  bvep->diagnostics_length = 0;
  return;

HANDLE_NO_BOOL_LITERAL:
  INTERNAL_ERROR("called int literal parser where there was no "
                 "int literal");
  PANIC();
}

static void parseFloatValueExpr(ValueExpr *fvep, Parser *parser) {
  Token t;
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_FloatLiteral, HANDLE_NO_FLOAT_LITERAL);
  fvep->kind = VEK_FloatLiteral;
  fvep->floatLiteral.value = t.float_literal;
  fvep->span = t.span;
  fvep->diagnostics_length = 0;
  return;

HANDLE_NO_FLOAT_LITERAL:
  INTERNAL_ERROR("called float literal parser where there was no "
                 "float literal");
  PANIC();
}

static void parseCharValueExpr(ValueExpr *cvep, Parser *parser) {
  Token t;
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_CharLiteral, HANDLE_NO_CHAR_LITERAL);
  cvep->kind = VEK_CharLiteral;
  cvep->charLiteral.value = t.char_literal;
  cvep->span = t.span;
  cvep->diagnostics_length = 0;
  return;

HANDLE_NO_CHAR_LITERAL:
  INTERNAL_ERROR("called char literal parser where there was no "
                 "char literal");
  PANIC();
}

static void parseStringValueExpr(ValueExpr *svep, Parser *parser) {
  Token t;
  advanceToken(parser, &t);
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
  advanceToken(parser, &t);

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
  advanceToken(parser, &t);
  Span lparenspan = t.span;
  if (t.kind != TK_ParenLeft) {
    diagnostic = DIAGNOSTIC(DK_FnValueExprExpectedLeftParen, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  fvep->fnExpr.parameters = RALLOC(parser->ar, PatternExpr);
  parsePatternExpr(fvep->fnExpr.parameters, parser);

  advanceToken(parser, &t);
  if (t.kind == TK_Colon) {
    fvep->fnExpr.type = RALLOC(parser->ar, TypeExpr);
    parseTypeExpr(fvep->fnExpr.type, parser);
    end = fvep->fnExpr.type->span.end;
  } else {
    setNextToken(parser, &t);
    fvep->fnExpr.type = RALLOC(parser->ar, TypeExpr);
    fvep->fnExpr.type->kind = TEK_Omitted;
    fvep->fnExpr.type->span = lparenspan;
    fvep->fnExpr.type->diagnostics_length = 0;
    fvep->fnExpr.type->comments_length = 0;
  }

  advanceToken(parser, &t);

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
  advanceToken(parser, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_BraceLeft, HANDLE_NO_LEFTBRACE);

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

HANDLE_NO_LEFTBRACE:
  INTERNAL_ERROR(
      "called a block expresion parser where there was no leftbrace");
  PANIC();
}

static void parseIfValueExpr(ValueExpr *ivep, Parser *parser) {
  ZERO(ivep);
  Token t;
  advanceToken(parser, &t);
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
  advanceToken(parser, &t);
  if (t.kind == TK_Else) {
    ivep->ifExpr.has_else = true;
    ivep->ifExpr.else_body = RALLOC(parser->ar, ValueExpr);
    parseValueExpr(ivep->ifExpr.else_body, parser);
    ivep->span = SPAN(start, ivep->ifExpr.else_body->span.end);
  } else {
    // back up
    setNextToken(parser, &t);
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
  advanceToken(parser, &t);
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
  advanceToken(parser, &t);
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
  advanceToken(parser, &t);
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
  advanceToken(parser, &t);
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
  pushNewCommentScope(parser);
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
  advanceToken(parser, &t);
  if (t.kind != TK_Colon) {
    diagnostic = DIAGNOSTIC(DK_MatchCaseNoColon, t.span);
    resyncStmnt(parser);
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

  Vector comments = popCommentScope(parser);
  mcep->comments_length = VEC_LEN(&comments, Comment);
  mcep->comments = manageMemArena(parser->ar, releaseVector(&comments));

  return;
}

static void parseMatchValueExpr(ValueExpr *mvep, Parser *parser) {
  ZERO(mvep);
  mvep->kind = VEK_Match;
  Token t;
  // Ensure match
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_Match, HANDLE_NO_MATCH);

  LnCol start = t.span.start;

  // Get expression to match against
  mvep->matchExpr.value = RALLOC(parser->ar, ValueExpr);
  parseValueExpr(mvep->matchExpr.value, parser);
  // now we must parse the block containing the cases

  // Expect beginning brace
  advanceToken(parser, &t);
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
  resyncStmnt(parser);
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
// Level2ValueExpr () [] $ @ . -> (postfixes)
// Level3ValueExpr - + ~ ! (prefixes)
// Level4ValueExpr * / % (multiplication and division)
// Level5ValueExpr + - (addition and subtraction)
// Level6ValueExpr << >> & | ^ (bitwise operators)
// Level7ValueExpr < <= > >= == != (comparators)
// Level8ValueExpr && || (Boolean Operators)
// Level9ValueExpr = += -= *= /= %= &= |=  (Assignment)

static void parseL1ValueExpr(ValueExpr *l1, Parser *parser) {
  pushNewCommentScope(parser);

  Token t;
  advanceToken(parser, &t);
  // Decide which expression it is
  switch (t.kind) {
  // Literals
  case TK_IntLiteral: {
    setNextToken(parser, &t);
    parseIntValueExpr(l1, parser);
    break;
  }
  case TK_BoolLiteral: {
    setNextToken(parser, &t);
    parseBoolValueExpr(l1, parser);
    break;
  }
  case TK_FloatLiteral: {
    setNextToken(parser, &t);
    parseFloatValueExpr(l1, parser);
    break;
  }
  case TK_CharLiteral: {
    setNextToken(parser, &t);
    parseCharValueExpr(l1, parser);
    break;
  }
  case TK_StringLiteral: {
    setNextToken(parser, &t);
    parseStringValueExpr(l1, parser);
    break;
  }
  case TK_BraceLeft: {
    setNextToken(parser, &t);
    parseBlockValueExpr(l1, parser);
    break;
  }
  case TK_Fn: {
    setNextToken(parser, &t);
    parseFnValueExpr(l1, parser);
    break;
  }
  case TK_If: {
    setNextToken(parser, &t);
    parseIfValueExpr(l1, parser);
    break;
  }
  case TK_Break: {
    setNextToken(parser, &t);
    parseBreakValueExpr(l1, parser);
    break;
  }
  case TK_Continue: {
    setNextToken(parser, &t);
    parseContinueValueExpr(l1, parser);
    break;
  }
  case TK_Return: {
    setNextToken(parser, &t);
    parseReturnValueExpr(l1, parser);
    break;
  }
  case TK_While: {
    setNextToken(parser, &t);
    parseWhileValueExpr(l1, parser);
    break;
  }
  case TK_Match: {
    setNextToken(parser, &t);
    parseMatchValueExpr(l1, parser);
    break;
  }
  case TK_Identifier: {
    setNextToken(parser, &t);
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
    break;
  }
  default: {
    logInternalError(__LINE__, __func__, "unimplemented: %d at %d, %d", t.kind,
                     t.span.start.ln, t.span.start.col);
    PANIC();
  }
  }
  Vector comments = popCommentScope(parser);
  l1->comments_length = VEC_LEN(&comments, Comment);
  l1->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseL2ValueExpr(ValueExpr *l2, Parser *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff
  ValueExpr currentTopLevel;
  parseL1ValueExpr(&currentTopLevel, parser);
  LnCol start = currentTopLevel.span.start;

  while (true) {
    pushNewCommentScope(parser);
    Token t;
    advanceToken(parser, &t);
    switch (t.kind) {
    case TK_Ref: {
      ValueExpr v;
      v.kind = VEK_UnaryOp;
      v.unaryOp.operator= VEUOK_Ref;
      v.unaryOp.operand = RALLOC(parser->ar, ValueExpr);
      *v.unaryOp.operand = currentTopLevel;
      v.span = SPAN(start, t.span.end);
      v.diagnostics_length = 0;
      currentTopLevel = v;
      break;
    }
    case TK_Deref: {
      ValueExpr v;
      v.kind = VEK_UnaryOp;
      v.unaryOp.operator= VEUOK_Deref;
      v.unaryOp.operand = RALLOC(parser->ar, ValueExpr);
      *v.unaryOp.operand = currentTopLevel;
      v.span = SPAN(start, t.span.end);
      v.diagnostics_length = 0;
      currentTopLevel = v;
      break;
    }
    case TK_FieldAccess: {
      ValueExpr v;
      v.kind = VEK_FieldAccess;
      v.fieldAccess.value = RALLOC(parser->ar, ValueExpr);
      *v.fieldAccess.value = currentTopLevel;

      // Now we get the next thing
      advanceToken(parser, &t);
      v.span = SPAN(start, t.span.end);
      if (t.kind != TK_Identifier) {
        // If we encounter an error, we bail out of this L2ValueExpr
        v.fieldAccess.field = NULL;
        setNextToken(parser, &t);
        v.diagnostics_length = 1;
        v.diagnostics = RALLOC(parser->ar, Diagnostic);
        v.diagnostics[0] = DIAGNOSTIC(DK_FieldAccessExpectedIdentifier, v.span);
        *l2 = v;
        break;
      }
      v.fieldAccess.field = internArena(parser->ar, t.identifier);
      v.diagnostics_length = 0;
      currentTopLevel = v;
      break;
    }
    case TK_ParenLeft: {
      ValueExpr v;
      v.kind = VEK_Call;

      Vector args;
      createVector(&args);

      Vector diagnostics;
      createVector(&diagnostics);

      LnCol end;

      PARSE_LIST(&args,                // members_vec_ptr
                 &diagnostics,         // diagnostics_vec_ptr
                 parseValueExpr,       // member_parse_function
                 ValueExpr,            // member_kind
                 TK_ParenRight,        // delimiting_token_kind
                 DK_CallExpectedParen, // missing_delimiter_error
                 end,                  // end_lncol
                 parser                // parser
      )

      v.callExpr.arguments_length = VEC_LEN(&args, ValueExpr);
      v.callExpr.arguments = manageMemArena(parser->ar, releaseVector(&args));

      v.diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
      v.diagnostics = manageMemArena(parser->ar, releaseVector(&diagnostics));

      v.span = SPAN(start, end);

      v.callExpr.function = RALLOC(parser->ar, ValueExpr);
      *v.callExpr.function = currentTopLevel;
      currentTopLevel = v;
      break;
    }
    default: {
      // there are no more level 2 expressions
      mergeCommentScope(parser);
      setNextToken(parser, &t);
      *l2 = currentTopLevel;
      return;
    }
    }

    Vector comments = popCommentScope(parser);
    currentTopLevel.comments_length = VEC_LEN(&comments, Comment);
    currentTopLevel.comments =
        manageMemArena(parser->ar, releaseVector(&comments));
  }
}

static void parseL3ValueExpr(ValueExpr *l3, Parser *parser) {
  Token t;
  pushNewCommentScope(parser);
  advanceToken(parser, &t);
  switch (t.kind) {
  case TK_Sub: {
    l3->unaryOp.operator= VEUOK_Negate;
    break;
  }
  case TK_Add: {
    l3->unaryOp.operator= VEUOK_Posit;
    break;
  }
  case TK_BitNot: {
    l3->unaryOp.operator= VEUOK_BitNot;
    break;
  }
  case TK_Not: {
    l3->unaryOp.operator= VEUOK_LogicalNot;
    break;
  }
  default: {
    // there is no level 3 expression
    mergeCommentScope(parser);
    setNextToken(parser, &t);
    parseL2ValueExpr(l3, parser);
    return;
  }
  }

  // Now parse the rest of the expression
  l3->kind = VEK_UnaryOp;
  l3->unaryOp.operand = RALLOC(parser->ar, ValueExpr);
  parseL3ValueExpr(l3->unaryOp.operand, parser);
  l3->span = SPAN(t.span.start, l3->unaryOp.operand->span.end);
  l3->diagnostics_length = 0;
  Vector comments = popCommentScope(parser);
  l3->comments_length = VEC_LEN(&comments, Comment);
  l3->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseL4ValueExpr(ValueExpr *l4, Parser *parser) {
  ValueExpr v;
  parseL3ValueExpr(&v, parser);

  pushNewCommentScope(parser);

  Token t;
  advanceToken(parser, &t);
  switch (t.kind) {
  case TK_Mul: {
    l4->binaryOp.operator= VEBOK_Mul;
    break;
  }
  case TK_Div: {
    l4->binaryOp.operator= VEBOK_Div;
    break;
  }
  case TK_Mod: {
    l4->binaryOp.operator= VEBOK_Mod;
    break;
  }
  default: {
    // there is no level 4 expression
    mergeCommentScope(parser);
    setNextToken(parser, &t);
    *l4 = v;
    return;
  }
  }

  l4->kind = VEK_BinaryOp;
  l4->binaryOp.left_operand = RALLOC(parser->ar, ValueExpr);
  *l4->binaryOp.left_operand = v;
  l4->binaryOp.right_operand = RALLOC(parser->ar, ValueExpr);
  parseL4ValueExpr(l4->binaryOp.right_operand, parser);
  l4->span = SPAN(l4->binaryOp.left_operand->span.start,
                  l4->binaryOp.right_operand->span.end);
  l4->diagnostics_length = 0;

  Vector comments = popCommentScope(parser);
  l4->comments_length = VEC_LEN(&comments, Comment);
  l4->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

static void parseL5ValueExpr(ValueExpr *l5, Parser *parser) {
  ValueExpr v;
  parseL4ValueExpr(&v, parser);

  pushNewCommentScope(parser);
  Token t;
  advanceToken(parser, &t);
  switch (t.kind) {
  case TK_Add: {
    l5->binaryOp.operator= VEBOK_Add;
    break;
  }
  case TK_Sub: {
    l5->binaryOp.operator= VEBOK_Sub;
    break;
  }
  default: {
    // there is no level 5 expression
    mergeCommentScope(parser);
    setNextToken(parser, &t);
    *l5 = v;
    return;
  }
  }

  l5->kind = VEK_BinaryOp;
  l5->binaryOp.left_operand = RALLOC(parser->ar, ValueExpr);
  *l5->binaryOp.left_operand = v;
  l5->binaryOp.right_operand = RALLOC(parser->ar, ValueExpr);
  parseL5ValueExpr(l5->binaryOp.right_operand, parser);
  l5->span = SPAN(l5->binaryOp.left_operand->span.start,
                  l5->binaryOp.right_operand->span.end);
  l5->diagnostics_length = 0;

  Vector comments = popCommentScope(parser);
  l5->comments_length = VEC_LEN(&comments, Comment);
  l5->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

static void parseL6ValueExpr(ValueExpr *l6, Parser *parser) {
  ValueExpr v;
  parseL5ValueExpr(&v, parser);

  pushNewCommentScope(parser);
  Token t;
  advanceToken(parser, &t);
  switch (t.kind) {
  case TK_ShiftLeft: {
    l6->binaryOp.operator= VEBOK_BitShl;
    break;
  }
  case TK_ShiftRight: {
    l6->binaryOp.operator= VEBOK_BitShr;
    break;
  }
  case TK_BitAnd: {
    l6->binaryOp.operator= VEBOK_BitAnd;
    break;
  }
  case TK_BitOr: {
    l6->binaryOp.operator= VEBOK_BitOr;
    break;
  }
  case TK_BitXor: {
    l6->binaryOp.operator= VEBOK_BitXor;
    break;
  }
  default: {
    // there is no level 6 expression
    mergeCommentScope(parser);
    setNextToken(parser, &t);
    *l6 = v;
    return;
  }
  }

  l6->kind = VEK_BinaryOp;
  l6->binaryOp.left_operand = RALLOC(parser->ar, ValueExpr);
  *l6->binaryOp.left_operand = v;
  l6->binaryOp.right_operand = RALLOC(parser->ar, ValueExpr);
  parseL6ValueExpr(l6->binaryOp.right_operand, parser);
  l6->span = SPAN(l6->binaryOp.left_operand->span.start,
                  l6->binaryOp.right_operand->span.end);
  l6->diagnostics_length = 0;

  Vector comments = popCommentScope(parser);
  l6->comments_length = VEC_LEN(&comments, Comment);
  l6->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

static void parseL7ValueExpr(ValueExpr *l7, Parser *parser) {
  ValueExpr v;
  parseL6ValueExpr(&v, parser);

  pushNewCommentScope(parser);
  Token t;
  advanceToken(parser, &t);
  switch (t.kind) {
  case TK_CompLess: {
    l7->binaryOp.operator= VEBOK_CompLess;
    break;
  }
  case TK_CompGreater: {
    l7->binaryOp.operator= VEBOK_CompGreater;
    break;
  }
  case TK_CompLessEqual: {
    l7->binaryOp.operator= VEBOK_CompLessEqual;
    break;
  }
  case TK_CompGreaterEqual: {
    l7->binaryOp.operator= VEBOK_CompGreaterEqual;
    break;
  }
  case TK_CompEqual: {
    l7->binaryOp.operator= VEBOK_CompEqual;
    break;
  }
  case TK_CompNotEqual: {
    l7->binaryOp.operator= VEBOK_CompNotEqual;
    break;
  }
  default: {
    // there is no level 7 expression
    mergeCommentScope(parser);
    setNextToken(parser, &t);
    *l7 = v;
    return;
  }
  }

  l7->kind = VEK_BinaryOp;
  l7->binaryOp.left_operand = RALLOC(parser->ar, ValueExpr);
  *l7->binaryOp.left_operand = v;
  l7->binaryOp.right_operand = RALLOC(parser->ar, ValueExpr);
  parseL7ValueExpr(l7->binaryOp.right_operand, parser);
  l7->span = SPAN(l7->binaryOp.left_operand->span.start,
                  l7->binaryOp.right_operand->span.end);
  l7->diagnostics_length = 0;

  Vector comments = popCommentScope(parser);
  l7->comments_length = VEC_LEN(&comments, Comment);
  l7->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

static void parseL8ValueExpr(ValueExpr *l8, Parser *parser) {
  ValueExpr v;
  parseL7ValueExpr(&v, parser);

  pushNewCommentScope(parser);
  Token t;
  advanceToken(parser, &t);
  switch (t.kind) {
  case TK_And: {
    l8->binaryOp.operator= VEBOK_LogicalAnd;
    break;
  }
  case TK_Or: {
    l8->binaryOp.operator= VEBOK_LogicalOr;
    break;
  }
  default: {
    // There is no level 8 expr
    mergeCommentScope(parser);
    setNextToken(parser, &t);
    *l8 = v;
    return;
  }
  }

  l8->kind = VEK_BinaryOp;
  l8->binaryOp.left_operand = RALLOC(parser->ar, ValueExpr);
  *l8->binaryOp.left_operand = v;
  l8->binaryOp.right_operand = RALLOC(parser->ar, ValueExpr);
  parseL8ValueExpr(l8->binaryOp.right_operand, parser);
  l8->span = SPAN(l8->binaryOp.left_operand->span.start,
                  l8->binaryOp.right_operand->span.end);
  l8->diagnostics_length = 0;

  Vector comments = popCommentScope(parser);
  l8->comments_length = VEC_LEN(&comments, Comment);
  l8->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

static void parseL9ValueExpr(ValueExpr *l9, Parser *parser) {
  ValueExpr v;
  parseL8ValueExpr(&v, parser);

  pushNewCommentScope(parser);
  Token t;
  advanceToken(parser, &t);
  switch (t.kind) {
  case TK_Assign: {
    l9->binaryOp.operator= VEBOK_Assign;
    break;
  }
  case TK_AssignAdd: {
    l9->binaryOp.operator= VEBOK_AssignAdd;
    break;
  }
  case TK_AssignSub: {
    l9->binaryOp.operator= VEBOK_AssignSub;
    break;
  }
  case TK_AssignMul: {
    l9->binaryOp.operator= VEBOK_AssignMul;
    break;
  }
  case TK_AssignDiv: {
    l9->binaryOp.operator= VEBOK_AssignDiv;
    break;
  }
  case TK_AssignMod: {
    l9->binaryOp.operator= VEBOK_AssignMod;
    break;
  }
  case TK_AssignBitAnd: {
    l9->binaryOp.operator= VEBOK_AssignBitAnd;
    break;
  }
  case TK_AssignBitOr: {
    l9->binaryOp.operator= VEBOK_AssignBitOr;
    break;
  }
  default: {
    // There is no level 8 expr
    mergeCommentScope(parser);
    setNextToken(parser, &t);
    *l9 = v;
    return;
  }
  }

  l9->kind = VEK_BinaryOp;
  l9->binaryOp.left_operand = RALLOC(parser->ar, ValueExpr);
  *l9->binaryOp.left_operand = v;
  l9->binaryOp.right_operand = RALLOC(parser->ar, ValueExpr);
  parseL8ValueExpr(l9->binaryOp.right_operand, parser);
  l9->span = SPAN(l9->binaryOp.left_operand->span.start,
                  l9->binaryOp.right_operand->span.end);
  l9->diagnostics_length = 0;

  Vector comments = popCommentScope(parser);
  l9->comments_length = VEC_LEN(&comments, Comment);
  l9->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

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
  pushNewCommentScope(parser);

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
    slmep->name = NULL;
    slmep->value = NULL;
    diagnostic = DIAGNOSTIC(DK_StructMemberLiteralExpectedIdentifier, t.span);
    end = identitySpan.end;
    goto CLEANUP;
  }

  slmep->name = internArena(parser->ar, t.identifier);

  // check if assign
  advanceToken(parser, &t);
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

  Vector comments = popCommentScope(parser);
  slmep->comments_length = VEC_LEN(&comments, Comment);
  slmep->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

// field : Type,
static void parseStructMemberExpr(struct StructMemberExpr_s *smep,
                                  Parser *parser) {
  // zero-initialize bp
  ZERO(smep);
  pushNewCommentScope(parser);

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
    smep->name = NULL;
    diagnostic = DIAGNOSTIC(DK_StructMemberExpectedIdentifier, t.span);
    end = identitySpan.end;
    goto CLEANUP;
  }

  smep->name = internArena(parser->ar, t.identifier);

  // check if colon
  advanceToken(parser, &t);
  if (t.kind == TK_Colon) {
    // Get type of variable
    smep->type = RALLOC(parser->ar, TypeExpr);
    parseTypeExpr(smep->type, parser);
    end = smep->type->span.end;
  } else {
    end = identitySpan.end;
    // push back whatever
    setNextToken(parser, &t);
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

  Vector comments = popCommentScope(parser);
  smep->comments_length = VEC_LEN(&comments, Comment);
  smep->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

static void parseStructTypeExpr(TypeExpr *ste, Parser *parser) {
  ZERO(ste);
  ste->kind = TEK_Struct;
  Token t;
  advanceToken(parser, &t);
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
    goto HANDLE_NO_STRUCT;
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

HANDLE_NO_STRUCT:
  INTERNAL_ERROR("called struct type expression parser where there was no "
                 "struct declaration");
  PANIC();
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
  advanceToken(parser, &t);
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
  advanceToken(parser, &t);

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
  advanceToken(parser, &t);
  if (t.kind != TK_ParenLeft) {
    diagnostic = DIAGNOSTIC(DK_FnTypeExprExpectedLeftParen, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  fte->fnExpr.parameters = RALLOC(parser->ar, TypeExpr);
  parseTypeExpr(fte->fnExpr.parameters, parser);

  advanceToken(parser, &t);
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
  pushNewCommentScope(parser);
  Token t;
  advanceToken(parser, &t);
  switch (t.kind) {
  case TK_Identifier: {
    setNextToken(parser, &t);
    parseReferenceTypeExpr(l1, parser);
    return;
  }
  case TK_Enum:
  case TK_Pack:
  case TK_Union:
  case TK_Struct: {
    setNextToken(parser, &t);
    parseStructTypeExpr(l1, parser);
    break;
  }
  case TK_ParenLeft: {
    setNextToken(parser, &t);
    parseTupleTypeExpr(l1, parser);
    break;
  }
  case TK_Void: {
    setNextToken(parser, &t);
    parseVoidTypeExpr(l1, parser);
    break;
  }
  case TK_Fn: {
    setNextToken(parser, &t);
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
    resyncStmnt(parser);
    break;
  }
  }

  Vector comments = popCommentScope(parser);
  l1->comments_length = VEC_LEN(&comments, Comment);
  l1->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseL2TypeExpr(TypeExpr *l2, Parser *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff
  TypeExpr currentTopLevel;
  parseL1TypeExpr(&currentTopLevel, parser);
  LnCol start = currentTopLevel.span.start;

  while (true) {
    pushNewCommentScope(parser);

    Token t;
    advanceToken(parser, &t);
    switch (t.kind) {
    case TK_Ref: {
      TypeExpr te;
      te.kind = TEK_UnaryOp;
      te.unaryOp.operator= TEUOK_Ref;
      te.unaryOp.operand = RALLOC(parser->ar, TypeExpr);
      *te.unaryOp.operand = currentTopLevel;
      te.span = SPAN(start, t.span.end);
      te.diagnostics_length = 0;
      currentTopLevel = te;
      break;
    }
    case TK_Deref: {
      TypeExpr te;
      te.kind = TEK_UnaryOp;
      te.unaryOp.operator= TEUOK_Deref;
      te.unaryOp.operand = RALLOC(parser->ar, TypeExpr);
      *te.unaryOp.operand = currentTopLevel;
      te.span = SPAN(start, t.span.end);
      te.diagnostics_length = 0;
      currentTopLevel = te;
      break;
    }
    case TK_ScopeResolution: {
      TypeExpr te;
      te.kind = TEK_FieldAccess;
      te.fieldAccess.value = RALLOC(parser->ar, TypeExpr);
      *te.fieldAccess.value = currentTopLevel;

      // Now we get the next thing
      advanceToken(parser, &t);
      te.span = SPAN(start, t.span.end);
      if (t.kind != TK_Identifier) {
        // If we encounter an error, we bail out of this L2ValueExpr
        te.fieldAccess.field = NULL;
        setNextToken(parser, &t);
        te.diagnostics_length = 1;
        te.diagnostics = RALLOC(parser->ar, Diagnostic);
        te.diagnostics[0] =
            DIAGNOSTIC(DK_TypeExprFieldAccessExpectedIdentifier, t.span);

      } else {
        te.fieldAccess.field = internArena(parser->ar, t.identifier);
        te.diagnostics_length = 0;
      }
      currentTopLevel = te;
      break;
    }
    default: {
      // there are no more level 2 expressions
      mergeCommentScope(parser);
      setNextToken(parser, &t);
      *l2 = currentTopLevel;
      return;
    }
    }
    Vector comments = popCommentScope(parser);
    currentTopLevel.comments_length = VEC_LEN(&comments, Comment);
    currentTopLevel.comments =
        manageMemArena(parser->ar, releaseVector(&comments));
  }
}

static void parseL3TypeExpr(TypeExpr *l3, Parser *parser) {
  TypeExpr ty;
  parseL2TypeExpr(&ty, parser);

  pushNewCommentScope(parser);

  Token t;
  advanceToken(parser, &t);
  switch (t.kind) {
  case TK_Comma: {
    l3->binaryOp.operator= TEBOK_Product;
    break;
  }
  case TK_BitOr: {
    l3->binaryOp.operator= TEBOK_Sum;
    break;
  }
  default: {
    // there is no level 3 expression
    mergeCommentScope(parser);
    setNextToken(parser, &t);
    *l3 = ty;
    return;
  }
  }

  l3->kind = TEK_BinaryOp;
  l3->binaryOp.left_operand = RALLOC(parser->ar, ValueExpr);
  *l3->binaryOp.left_operand = ty;
  l3->binaryOp.right_operand = RALLOC(parser->ar, ValueExpr);
  parseL2TypeExpr(l3->binaryOp.right_operand, parser);
  l3->span = SPAN(l3->binaryOp.left_operand->span.start,
                  l3->binaryOp.right_operand->span.end);
  l3->diagnostics_length = 0;

  Vector comments = popCommentScope(parser);
  l3->comments_length = VEC_LEN(&comments, Comment);
  l3->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

static void parseTypeExpr(TypeExpr *tep, Parser *parser) {
  parseL3TypeExpr(tep, parser);
}

static void parseValueRestrictionPatternExpr(PatternExpr *vrpe,
                                             Parser *parser) {
  ZERO(vrpe);

  Token t;
  advanceToken(parser, &t);
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

static void parseStructPatternExpr(PatternExpr *spe, Parser *parser) {
  ZERO(spe);

  Token t;
  advanceToken(parser, &t);

  if (t.kind != TK_Struct) {
    INTERNAL_ERROR("called struct pattern expr parser where there was "
                   "no struct pattern");
    PANIC();
  }

  LnCol start = t.span.start;
  LnCol end;

  // TODO
}

static void parseL1PatternExpr(PatternExpr *l1, Parser *parser) {
  // TODO  convert for patterns
  pushNewCommentScope(parser);
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
    parseGroup(l1, parser);
    break;
  }
  case TK_Void: {
    setNextToken(parser, &t);
    parseVoidTypeExpr(l1, parser);
    break;
  }
  case TK_Fn: {
    setNextToken(parser, &t);
    parseFnTypeExpr(l1, parser);
    break;
  }
  default: {
    l1->kind = PEK_None;
    l1->span = t.span;
    l1->diagnostics_length = 1;
    l1->diagnostics = RALLOC(parser->ar, Diagnostic);
    l1->diagnostics[0] = DIAGNOSTIC(DK_TypeExprUnexpectedToken, t.span);
    // Resync
    resyncStmnt(parser);
    break;
  }
  }

  Vector comments = popCommentScope(parser);
  l1->comments_length = VEC_LEN(&comments, Comment);
  l1->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseL2PatternExpr(PatternExpr *l2, Parser *parser) {
  parseL1PatternExpr
}

static void parsePatternExpr(PatternExpr *ppe, Parser *parser) {
  // zero-initialize bp
  ZERO(bp);
  pushNewCommentScope(parser);

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

  Vector comments = popCommentScope(parser);
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
  resyncStmnt(parser);
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
  pushNewCommentScope(parser);

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

  Vector comments = popCommentScope(parser);
  sp->comments_length = VEC_LEN(&comments, Comment);
  sp->comments = manageMemArena(parser->ar, releaseVector(&comments));
}

static void parseTranslationUnit(TranslationUnit *tu, Parser *parser) {
  pushNewCommentScope(parser);

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

  Vector comments = popCommentScope(parser);
  tu->comments_length = VEC_LEN(&comments, Comment);
  tu->comments = manageMemArena(parser->ar, releaseVector(&comments));
  return;
}

void parseTranslationUnitParser(Parser *pp, TranslationUnit *tu) {
  parseTranslationUnit(tu, pp);
}
