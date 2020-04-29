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

#define PARSE_DELIMITED_LIST(                                                  \
    members_vec_ptr, diagnostics_vec_ptr, member_parse_function, member_kind,  \
    trailing_spacer_ptr, spacer_token_kind, delimiting_token_kind,             \
    missing_spacer_error, missing_delimiter_error, end_lncol, parser, ar)         \
  while (true) {                                                               \
    Token PARSE_DELIMITED_LIST_token;                                          \
    advanceToken(parser, &PARSE_DELIMITED_LIST_token);                            \
    if (PARSE_DELIMITED_LIST_token.kind == delimiting_token_kind) {            \
      *trailing_spacer_ptr = true;                                             \
      end_lncol = PARSE_DELIMITED_LIST_token.span.end;                         \
      break;                                                                   \
    } else if (PARSE_DELIMITED_LIST_token.kind == TK_None &&                   \
               PARSE_DELIMITED_LIST_token.error == DK_EOF) {                   \
      *VEC_PUSH(diagnostics_vec_ptr, Diagnostic) = DIAGNOSTIC(                 \
          missing_delimiter_error, PARSE_DELIMITED_LIST_token.span);           \
      end_lncol = PARSE_DELIMITED_LIST_token.span.end;                         \
      break;                                                                   \
    }                                                                          \
    /* if there wasn't an end delimiter, push the last token back */           \
    setNextToken(parser, &PARSE_DELIMITED_LIST_token);                            \
    member_parse_function(VEC_PUSH(members_vec_ptr, member_kind), parser, ar);    \
    /* accept spacer, if any. If no comma it must be followed by delimiter.    \
     * Allows trailing spacers */                                              \
    advanceToken(parser, &PARSE_DELIMITED_LIST_token);                            \
    if (PARSE_DELIMITED_LIST_token.kind == delimiting_token_kind) {            \
      *trailing_spacer_ptr = false;                                            \
      end_lncol = PARSE_DELIMITED_LIST_token.span.end;                         \
      break;                                                                   \
    } else if (PARSE_DELIMITED_LIST_token.kind != spacer_token_kind) {         \
      *VEC_PUSH(diagnostics_vec_ptr, Diagnostic) =                             \
          DIAGNOSTIC(missing_spacer_error, PARSE_DELIMITED_LIST_token.span);   \
    }                                                                          \
  }

#define EXPECT_TYPE(token, tokenType, onErrLabel)                              \
  do {                                                                         \
    if ((token).kind != (tokenType)) {                                         \
      goto onErrLabel;                                                         \
    }                                                                          \
  } while (false)


// Parser
void createParser(Parser *pp, Lexer *lp, Arena *ar) {
  pp->has_next_token= false;
  pp->lexer = lp;
  pp->ar = ar;
}

// If has an ungotten token, return that. Else return next in line, and cache it
static void advanceToken(Parser *pp, Token *t) {
  if (pp->has_next_token) {
    *t = pp->next_token;
    pp->has_next_token = false;
  } else {
    lexNextToken(pp->lexer, t);
  }
}

// If token exists in line
static void setNextToken(Parser *pp, Token *t) {
  if (!pp->has_next_token) {
    pp->has_next_token = true;
    pp->next_token= *t;
  } else {
    INTERNAL_ERROR("already set next token");
    PANIC();
  }
}

Arena* releaseParser(Parser *pp) {
  return pp->ar;
}

// Jump till after semicolon, taking into account parentheses, brackets,
// attributes, and braces It will ignore subexprs, but can halt at the end
static void resyncStmnt(Parser *parser, Arena *ar) {
  UNUSED(ar);
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
static void parseStmnt(Stmnt *sp, Parser *parser, Arena *ar);
static void parseValueExpr(ValueExpr *vep, Parser *parser, Arena *ar);
static void parseTypeExpr(TypeExpr *tep, Parser *parser, Arena *ar);
static void parseBinding(Binding *bp, Parser *parser, Arena *ar);

static void parsePath(Path *pp, Parser *parser, Arena *ar) {
  Token t;
  advanceToken(parser, &t);
  if(t.kind != TK_Identifier) {
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

  *VEC_PUSH(&pathSegments, char *) = internArena(t.identifier, ar);

  while (true) {
    advanceToken(parser, &t);
    if (t.kind == TK_ScopeResolution) {
      advanceToken(parser, &t);
      if(t.kind != TK_Identifier) {
        diagnostic =  DIAGNOSTIC(DK_PathExpectedIdentifier, t.span);
        end = t.span.end;
        goto CLEANUP;
      }
      *VEC_PUSH(&pathSegments, char *) = internArena(t.identifier, ar);
    } else {
      // we've reached the end of the path
      setNextToken(parser, &t);
      end = t.span.end;
      break;
    }
  }

CLEANUP:
  pp->pathSegments_length = VEC_LEN(&pathSegments, char *);
  pp->pathSegments = manageMemArena(ar, releaseVector(&pathSegments));

  if(diagnostic.kind != DK_Ok) {
    pp->diagnostics_length = 1;
    pp->diagnostics = allocArena(ar, sizeof(Diagnostic));
    pp->diagnostics[0] = diagnostic;
  } else {
    pp->diagnostics_length = 0;
    pp->diagnostics = NULL;
  }
  pp->span = SPAN(start, end);
}

static void parseIntValueExpr(ValueExpr *ivep, Parser *parser, Arena *ar) {
  UNUSED(ar);
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

static void parseBoolValueExpr(ValueExpr *bvep, Parser *parser, Arena *ar) {
  UNUSED(ar);
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

static void parseFloatValueExpr(ValueExpr *fvep, Parser *parser,
                                Arena *ar) {
  UNUSED(ar);
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

static void parseCharValueExpr(ValueExpr *cvep, Parser *parser, Arena *ar) {
  UNUSED(ar);
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

static void parseStringValueExpr(ValueExpr *svep, Parser *parser,
                                 Arena *ar) {
  UNUSED(ar);
  Token t;
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_StringLiteral, HANDLE_NO_STRING_LITERAL);
  svep->kind = VEK_StringLiteral;
  svep->stringLiteral.value = internArena(t.string_literal, ar);
  svep->span = t.span;
  svep->diagnostics_length = 0;
  return;

HANDLE_NO_STRING_LITERAL:
  INTERNAL_ERROR("called string literal parser where there was no "
                 "string literal");
  PANIC();
}

static void parseGroupValueExpr(ValueExpr *gvep, Parser *parser,
                                Arena *ar) {
  ZERO(gvep);
  gvep->kind = VEK_Group;

  Token t;
  advanceToken(parser, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_ParenLeft, HANDLE_NO_LEFTPAREN);
  gvep->groupExpr.value = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(gvep->groupExpr.value, parser, ar);

  // Expect a rightparen
  advanceToken(parser, &t);
  gvep->span = SPAN(start, t.span.end);
  EXPECT_TYPE(t, TK_ParenRight, HANDLE_NO_RIGHTPAREN);
  gvep->diagnostics_length = 0;
  return;

HANDLE_NO_LEFTPAREN:
  INTERNAL_ERROR("called group parser where there was no "
                 "group");
  PANIC();

HANDLE_NO_RIGHTPAREN:
  resyncStmnt(parser, ar);
  gvep->diagnostics_length = 1;
  gvep->diagnostics = allocArena(ar, sizeof(Diagnostic));
  gvep->diagnostics[0] = DIAGNOSTIC(DK_GroupExpectRightParen, gvep->span);
}

static void parseBlockValueExpr(ValueExpr *bvep, Parser *parser,
                                Arena *ar) {
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

  PARSE_DELIMITED_LIST(
      &statements,                     // members_vec_ptr
      &diagnostics,                    // diagnostics_vec_ptr
      parseStmnt,                      // member_parse_function
      Stmnt,                           // member_kind
      &bvep->blockExpr.suppress_value, // trailing_spacer_ptr (will set to true
                                       // if last element is semicolon
      TK_Semicolon,  // spacer_token_kind (semicolon in block value expr)
      TK_BraceRight, // delimiting_token_kind
      DK_BlockExpectedSemicolon,  // missing_spacer_error
      DK_BlockExpectedRightBrace, // missing_delimiter_error
      end,                        // end_lncol
      parser,                        // parser
      ar                          // ar
  )

  bvep->blockExpr.statements_length = VEC_LEN(&statements, Stmnt);
  bvep->blockExpr.statements = manageMemArena(ar, releaseVector(&statements));
  bvep->span = SPAN(start, end);
  bvep->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  bvep->diagnostics = manageMemArena(ar, releaseVector(&diagnostics));
  return;

HANDLE_NO_LEFTBRACE:
  INTERNAL_ERROR(
      "called a block expresion parser where there was no leftbrace");
  PANIC();
}

static void parseIfValueExpr(ValueExpr *ivep, Parser *parser, Arena *ar) {
  ZERO(ivep);
  Token t;
  advanceToken(parser, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_If, HANDLE_NO_IF);
  ivep->kind = VEK_If;

  // parse condition
  ivep->ifExpr.condition = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(ivep->ifExpr.condition, parser, ar);

  // parse body
  ivep->ifExpr.body = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(ivep->ifExpr.body, parser, ar);

  // if the next value is else
  advanceToken(parser, &t);
  if (t.kind == TK_Else) {
    ivep->ifExpr.has_else = true;
    ivep->ifExpr.else_body = allocArena(ar, sizeof(ValueExpr));
    parseValueExpr(ivep->ifExpr.else_body, parser, ar);
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

static void parseBreakValueExpr(ValueExpr *bep, Parser *parser, Arena *ar) {
  UNUSED(ar);
  bep->kind = VEK_Break;
  Token t;
  advanceToken(parser, &t);
  bep->span = t.span;
  EXPECT_TYPE(t, TK_Break, HANDLE_NO_BREAK);
  bep->diagnostics_length = 0;
  return;

HANDLE_NO_BREAK:
  INTERNAL_ERROR("called break parser where there was no break");
  PANIC();
}

static void parseContinueValueExpr(ValueExpr *cep, Parser *parser,
                                   Arena *ar) {
  UNUSED(ar);
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

static void parseReturnValueExpr(ValueExpr *rep, Parser *parser,
                                 Arena *ar) {
  rep->kind = VEK_Return;
  Token t;
  advanceToken(parser, &t);
  rep->span = t.span;
  EXPECT_TYPE(t, TK_Return, HANDLE_NO_RETURN);
  rep->returnExpr.value = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(rep->returnExpr.value, parser, ar);
  rep->diagnostics_length = 0;
  return;

HANDLE_NO_RETURN:
  INTERNAL_ERROR("called return parser where there was no continue");
  PANIC();
}

static void parseWhileValueExpr(ValueExpr *wep, Parser *parser, Arena *ar) {
  wep->kind = VEK_While;
  Token t;
  advanceToken(parser, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_While, HANDLE_NO_WHILE);

  wep->whileExpr.condition = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(wep->whileExpr.condition, parser, ar);

  wep->whileExpr.body = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(wep->whileExpr.body, parser, ar);

  wep->span = SPAN(start, wep->whileExpr.body->span.end);
  wep->diagnostics_length = 0;
  return;

HANDLE_NO_WHILE:
  INTERNAL_ERROR("called continue parser where there was no continue");
  PANIC();
}

// Pattern : Expr,
static void parseMatchCaseExpr(struct MatchCaseExpr_s *mcep, Parser *parser,
                               Arena *ar) {
  ZERO(mcep);

  Token t;
  // Get pattern
  mcep->pattern = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(mcep->pattern, parser, ar);
  // Expect colon
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_Colon, HANDLE_NO_COLON);
  // Get Value
  mcep->value = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(mcep->value, parser, ar);
  mcep->span = SPAN(mcep->pattern->span.start, mcep->value->span.end);
  mcep->diagnostics_length = 0;
  return;

HANDLE_NO_COLON:
  mcep->span = mcep->pattern->span;
  mcep->diagnostics_length = 1;
  mcep->diagnostics = allocArena(ar, sizeof(Diagnostic));
  mcep->diagnostics[0] = DIAGNOSTIC(DK_MatchCaseNoColon, t.span);
  resyncStmnt(parser, ar);
  return;
}

static void parseMatchValueExpr(ValueExpr *mvep, Parser *parser,
                                Arena *ar) {
  ZERO(mvep);
  mvep->kind = VEK_Match;
  Token t;
  // Ensure match
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_Match, HANDLE_NO_MATCH);

  LnCol start = t.span.start;

  // Get expression to match against
  mvep->matchExpr.value = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(mvep->matchExpr.value, parser, ar);
  // now we must parse the block containing the cases

  // Expect beginning brace
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_BraceLeft, HANDLE_NO_LEFTBRACE);

  Vector cases;
  createVector(&cases);

  Vector diagnostics;
  createVector(&diagnostics);

  LnCol end;

  PARSE_DELIMITED_LIST(
      &cases,                          // members_vec_ptr
      &diagnostics,                    // diagnostics_vec_ptr
      parseMatchCaseExpr,              // member_parse_function
      struct MatchCaseExpr_s,          // member_kind
      &mvep->matchExpr.trailing_comma, // trailing_spacer_ptr (will set to true
                                       // if last element is semicolon
      TK_Comma,             // spacer_token_kind (semicolon in block value expr)
      TK_BraceRight,        // delimiting_token_kind
      DK_MatchNoComma,      // missing_spacer_error
      DK_MatchNoRightBrace, // missing_delimiter_error
      end,                  // end_lncol
      parser,                  // parser
      ar                    // ar
  )

  // Get interior cases
  mvep->matchExpr.cases_length = VEC_LEN(&cases, struct MatchCaseExpr_s);
  mvep->matchExpr.cases = manageMemArena(ar, releaseVector(&cases));

  mvep->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  mvep->diagnostics = manageMemArena(ar, releaseVector(&diagnostics));
  mvep->span = SPAN(start, end);
  return;

HANDLE_NO_MATCH:
  INTERNAL_ERROR("called match parser where there was no match");
  PANIC();

HANDLE_NO_LEFTBRACE:
  mvep->diagnostics_length = 1;
  mvep->diagnostics = allocArena(ar, sizeof(Diagnostic));
  mvep->diagnostics[0] = DIAGNOSTIC(DK_MatchNoLeftBrace, t.span);
  mvep->span = SPAN(start, t.span.end);
  mvep->matchExpr.cases_length = 0;
  mvep->matchExpr.cases = NULL;
  resyncStmnt(parser, ar);
  return;
}

static void parseReferenceValueExpr(ValueExpr *rvep, Parser *parser,
                                    Arena *ar) {
  ZERO(rvep);
  rvep->kind = VEK_Reference;
  rvep->reference.path = allocArena(ar, sizeof(Path));
  parsePath(rvep->reference.path, parser, ar);
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

static void parseL1ValueExpr(ValueExpr *l1, Parser *parser, Arena *ar) {
  Token t;
  advanceToken(parser, &t);
  // Decide which expression it is
  switch (t.kind) {
  // Literals
  case TK_IntLiteral: {
    setNextToken(parser, &t);
    parseIntValueExpr(l1, parser, ar);
    return;
  }
  case TK_BoolLiteral: {
    setNextToken(parser, &t);
    parseBoolValueExpr(l1, parser, ar);
    return;
  }
  case TK_FloatLiteral: {
    setNextToken(parser, &t);
    parseFloatValueExpr(l1, parser, ar);
    return;
  }
  case TK_CharLiteral: {
    setNextToken(parser, &t);
    parseCharValueExpr(l1, parser, ar);
    return;
  }
  case TK_StringLiteral: {
    setNextToken(parser, &t);
    parseStringValueExpr(l1, parser, ar);
    return;
  }
  case TK_ParenLeft: {
    setNextToken(parser, &t);
    parseGroupValueExpr(l1, parser, ar);
    return;
  }
  case TK_BraceLeft: {
    setNextToken(parser, &t);
    parseBlockValueExpr(l1, parser, ar);
    return;
  }
  case TK_If: {
    setNextToken(parser, &t);
    parseIfValueExpr(l1, parser, ar);
    return;
  }
  case TK_Break: {
    setNextToken(parser, &t);
    parseBreakValueExpr(l1, parser, ar);
    return;
  }
  case TK_Continue: {
    setNextToken(parser, &t);
    parseContinueValueExpr(l1, parser, ar);
    return;
  }
  case TK_Return: {
    setNextToken(parser, &t);
    parseReturnValueExpr(l1, parser, ar);
    return;
  }
  case TK_While: {
    setNextToken(parser, &t);
    parseWhileValueExpr(l1, parser, ar);
    return;
  }
  case TK_Match: {
    setNextToken(parser, &t);
    parseMatchValueExpr(l1, parser, ar);
    return;
  }
  case TK_Identifier: {
    setNextToken(parser, &t);
    parseReferenceValueExpr(l1, parser, ar);
    return;
  }
  case TK_None: {
    // put the token error in the value expression.
    ZERO(l1);
    l1->kind = VEK_None;
    l1->span = t.span;
    l1->diagnostics_length = 1;
    l1->diagnostics = allocArena(ar, sizeof(Diagnostic));
    l1->diagnostics[0] = DIAGNOSTIC(t.error, t.span);
    return;
  }
  default: {
    logInternalError(__LINE__, __func__, "unimplemented: %d at %d, %d", t.kind,
                     t.span.start.ln, t.span.start.col);
    PANIC();
  }
  }
}

static void parseL2ValueExpr(ValueExpr *l2, Parser *parser, Arena *ar) {

  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff
  ValueExpr currentTopLevel;
  parseL1ValueExpr(&currentTopLevel, parser, ar);
  LnCol start = currentTopLevel.span.start;

  while (true) {
    Token t;
    advanceToken(parser, &t);
    switch (t.kind) {
    case TK_Ref: {
      ValueExpr v;
      v.kind = VEK_UnaryOp;
      v.unaryOp.operator= VEUOK_Ref;
      v.unaryOp.operand = allocArena(ar, sizeof(ValueExpr));
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
      v.unaryOp.operand = allocArena(ar, sizeof(ValueExpr));
      *v.unaryOp.operand = currentTopLevel;
      v.span = SPAN(start, t.span.end);
      v.diagnostics_length = 0;
      currentTopLevel = v;
      break;
    }
    case TK_FieldAccess: {
      ValueExpr v;
      v.kind = VEK_FieldAccess;
      v.fieldAccess.value = allocArena(ar, sizeof(ValueExpr));
      *v.fieldAccess.value = currentTopLevel;

      // Now we get the next thing
      advanceToken(parser, &t);
      v.span = SPAN(start, t.span.end);
      if (t.kind != TK_Identifier) {
        // If we encounter an error, we bail out of this L2ValueExpr
        v.fieldAccess.field = NULL;
        setNextToken(parser, &t);
        v.diagnostics_length = 1;
        v.diagnostics = allocArena(ar, sizeof(Diagnostic));
        v.diagnostics[0] = DIAGNOSTIC(DK_FieldAccessExpectedIdentifier, v.span);
        *l2 = v;
        return;
      }
      v.fieldAccess.field = internArena(t.identifier, ar);
      v.diagnostics_length = 0;
      currentTopLevel = v;
      break;
    }
    case TK_BracketLeft: {
      ValueExpr v;
      v.kind = VEK_BinaryOp;
      v.binaryOp.operator= VEBOK_ArrayAccess;
      v.binaryOp.left_operand = allocArena(ar, sizeof(ValueExpr));
      v.binaryOp.right_operand = allocArena(ar, sizeof(ValueExpr));
      *v.binaryOp.left_operand = currentTopLevel;
      parseValueExpr(v.binaryOp.right_operand, parser, ar);
      // expect closing bracket
      advanceToken(parser, &t);

      // Calculate span and diagnostics
      v.span = SPAN(start, t.span.end);
      if (t.kind != TK_BracketRight) {
        // If we miss the bracket we bail out of this subexpr, setting the
        // next token
        v.diagnostics_length = 1;
        v.diagnostics = allocArena(ar, sizeof(Diagnostic));
        v.diagnostics[0] = DIAGNOSTIC(DK_ArrayAccessExpectedBracket, v.span);
        setNextToken(parser, &t);
        *l2 = v;
        return;
      }
      v.diagnostics_length = 0;
      currentTopLevel = v;
      break;
    }
    case TK_ParenLeft: {
      ValueExpr v;
      Vector arguments;
      createVector(&arguments);
      // Parse the arguments (Comma Seperated list of valueexprs)
      while (true) {
        // Check for end paren
        advanceToken(parser, &t);
        if (t.kind == TK_ParenRight) {
          break;
        }
        // If it wasn't an end paren, we push it back
        setNextToken(parser, &t);

        // Parse and push the argument
        parseValueExpr(VEC_PUSH(&arguments, ValueExpr), parser, ar);

        // Accept comma, if any
        // If there's no comma then it MUST be followed by an end paren
        // This also allows trailing commas
        advanceToken(parser, &t);
        if (t.kind != TK_Comma) {
          if (t.kind == TK_ParenRight) {
            break;
          } else {
            // If the next value isn't an end paren, then we throw an error
            v.kind = VEK_Call;
            v.callExpr.function = allocArena(ar, sizeof(ValueExpr));
            *v.callExpr.function = currentTopLevel;
            v.callExpr.arguments_length = VEC_LEN(&arguments, ValueExpr);
            v.callExpr.arguments =
                manageMemArena(ar, releaseVector(&arguments));

            // Calculate span and diagnostics
            v.span = SPAN(start, t.span.end);
            v.diagnostics_length = 1;
            v.diagnostics = allocArena(ar, sizeof(Diagnostic));
            v.diagnostics[0] = DIAGNOSTIC(DK_FunctionCallExpectedParen, v.span);
            // Bail out of the subexpr
            setNextToken(parser, &t);
            currentTopLevel = v;
            return;
          }
        }
      }

      v.kind = VEK_Call;
      v.callExpr.function = allocArena(ar, sizeof(ValueExpr));
      *v.callExpr.function = currentTopLevel;
      v.callExpr.arguments_length = VEC_LEN(&arguments, ValueExpr);
      v.callExpr.arguments = manageMemArena(ar, releaseVector(&arguments));

      // Calculate span and diagnostics
      v.span = SPAN(start, t.span.end);
      v.diagnostics_length = 0;
      currentTopLevel = v;
      break;
    }
    default: {
      // there are no more level 2 expressions
      setNextToken(parser, &t);
      *l2 = currentTopLevel;
      return;
    }
    }
  }
}

static void parseL3ValueExpr(ValueExpr *l3, Parser *parser, Arena *ar) {
  Token t;
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
    setNextToken(parser, &t);
    parseL2ValueExpr(l3, parser, ar);
    return;
  }
  }

  // Now parse the rest of the expression
  l3->kind = VEK_UnaryOp;
  l3->unaryOp.operand = allocArena(ar, sizeof(ValueExpr));
  parseL3ValueExpr(l3->unaryOp.operand, parser, ar);
  l3->span = SPAN(t.span.start, l3->unaryOp.operand->span.end);
  l3->diagnostics_length = 0;
  return;
}

static void parseL4ValueExpr(ValueExpr *l4, Parser *parser, Arena *ar) {
  ValueExpr v;
  parseL3ValueExpr(&v, parser, ar);

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
    setNextToken(parser, &t);
    *l4 = v;
    return;
  }
  }

  l4->kind = VEK_BinaryOp;
  l4->binaryOp.left_operand = allocArena(ar, sizeof(ValueExpr));
  *l4->binaryOp.left_operand = v;
  l4->binaryOp.right_operand = allocArena(ar, sizeof(ValueExpr));
  parseL4ValueExpr(l4->binaryOp.right_operand, parser, ar);
  l4->span = SPAN(l4->binaryOp.left_operand->span.start,
                  l4->binaryOp.right_operand->span.end);
  l4->diagnostics_length = 0;
  return;
}

static void parseL5ValueExpr(ValueExpr *l5, Parser *parser, Arena *ar) {
  ValueExpr v;
  parseL4ValueExpr(&v, parser, ar);

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
    setNextToken(parser, &t);
    *l5 = v;
    return;
  }
  }

  l5->kind = VEK_BinaryOp;
  l5->binaryOp.left_operand = allocArena(ar, sizeof(ValueExpr));
  *l5->binaryOp.left_operand = v;
  l5->binaryOp.right_operand = allocArena(ar, sizeof(ValueExpr));
  parseL5ValueExpr(l5->binaryOp.right_operand, parser, ar);
  l5->span = SPAN(l5->binaryOp.left_operand->span.start,
                  l5->binaryOp.right_operand->span.end);
  l5->diagnostics_length = 0;
  return;
}

static void parseL6ValueExpr(ValueExpr *l6, Parser *parser, Arena *ar) {
  ValueExpr v;
  parseL5ValueExpr(&v, parser, ar);

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
    setNextToken(parser, &t);
    *l6 = v;
    return;
  }
  }

  l6->kind = VEK_BinaryOp;
  l6->binaryOp.left_operand = allocArena(ar, sizeof(ValueExpr));
  *l6->binaryOp.left_operand = v;
  l6->binaryOp.right_operand = allocArena(ar, sizeof(ValueExpr));
  parseL6ValueExpr(l6->binaryOp.right_operand, parser, ar);
  l6->span = SPAN(l6->binaryOp.left_operand->span.start,
                  l6->binaryOp.right_operand->span.end);
  l6->diagnostics_length = 0;
  return;
}

static void parseL7ValueExpr(ValueExpr *l7, Parser *parser, Arena *ar) {
  ValueExpr v;
  parseL6ValueExpr(&v, parser, ar);

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
  case TK_Equal: {
    l7->binaryOp.operator= VEBOK_CompEqual;
    break;
  }
  case TK_NotEqual: {
    l7->binaryOp.operator= VEBOK_CompNotEqual;
    break;
  }
  default: {
    // there is no level 7 expression
    setNextToken(parser, &t);
    *l7 = v;
    return;
  }
  }

  l7->kind = VEK_BinaryOp;
  l7->binaryOp.left_operand = allocArena(ar, sizeof(ValueExpr));
  *l7->binaryOp.left_operand = v;
  l7->binaryOp.right_operand = allocArena(ar, sizeof(ValueExpr));
  parseL7ValueExpr(l7->binaryOp.right_operand, parser, ar);
  l7->span = SPAN(l7->binaryOp.left_operand->span.start,
                  l7->binaryOp.right_operand->span.end);
  l7->diagnostics_length = 0;
  return;
}

static void parseL8ValueExpr(ValueExpr *l8, Parser *parser, Arena *ar) {
  ValueExpr v;
  parseL7ValueExpr(&v, parser, ar);

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
    setNextToken(parser, &t);
    *l8 = v;
    return;
  }
  }

  l8->kind = VEK_BinaryOp;
  l8->binaryOp.left_operand = allocArena(ar, sizeof(ValueExpr));
  *l8->binaryOp.left_operand = v;
  l8->binaryOp.right_operand = allocArena(ar, sizeof(ValueExpr));
  parseL8ValueExpr(l8->binaryOp.right_operand, parser, ar);
  l8->span = SPAN(l8->binaryOp.left_operand->span.start,
                  l8->binaryOp.right_operand->span.end);
  l8->diagnostics_length = 0;
  return;
}

static void parseL9ValueExpr(ValueExpr *l9, Parser *parser, Arena *ar) {
  ValueExpr v;
  parseL8ValueExpr(&v, parser, ar);

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
    setNextToken(parser, &t);
    *l9 = v;
    return;
  }
  }

  l9->kind = VEK_BinaryOp;
  l9->binaryOp.left_operand = allocArena(ar, sizeof(ValueExpr));
  *l9->binaryOp.left_operand = v;
  l9->binaryOp.right_operand = allocArena(ar, sizeof(ValueExpr));
  parseL8ValueExpr(l9->binaryOp.right_operand, parser, ar);
  l9->span = SPAN(l9->binaryOp.left_operand->span.start,
                  l9->binaryOp.right_operand->span.end);
  l9->diagnostics_length = 0;
  return;
}

// shim method
static void parseValueExpr(ValueExpr *vep, Parser *parser, Arena *ar) {
  parseL9ValueExpr(vep, parser, ar);
}

static void parseStructTypeExpr(TypeExpr *ste, Parser *parser, Arena *ar) {
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

  PARSE_DELIMITED_LIST(
      &members,                        // members_vec_ptr
      &diagnostics,                    // diagnostics_vec_ptr
      parseBinding,                   // member_parse_function
      Binding,                        // member_kind
      &ste->structExpr.trailing_comma, // trailing_spacer_ptr (will set to true
                                       // if last element is semicolon)
      TK_Comma,      // spacer_token_kind (semicolon in block value expr)
      TK_BraceRight, // delimiting_token_kind
      DK_StructExpectedComma,      // missing_spacer_error
      DK_StructExpectedRightBrace, // missing_delimiter_error
      end,                         // end_lncol
      parser,                         // parser
      ar                           // ar
  )

  ste->structExpr.members_length = VEC_LEN(&members, Binding);
  ste->structExpr.members = manageMemArena(ar, releaseVector(&members));
  ste->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  ste->diagnostics = manageMemArena(ar, releaseVector(&diagnostics));
  ste->span = SPAN(start, end);
  return;

HANDLE_NO_LEFTBRACE:
  ste->structExpr.members_length = 0;
  ste->structExpr.members = NULL;
  ste->span = SPAN(start, t.span.end);
  ste->diagnostics_length = 1;
  ste->diagnostics = allocArena(ar, sizeof(Diagnostic));
  ste->diagnostics[0] = DIAGNOSTIC(DK_StructExpectedLeftBrace, ste->span);
  return;

HANDLE_NO_STRUCT:
  INTERNAL_ERROR("called struct type expression parser where there was no "
                 "struct declaration");
  PANIC();
}

static void parseReferenceTypeExpr(TypeExpr *rtep, Parser *parser,
                                   Arena *ar) {
  ZERO(rtep);
  rtep->kind = TEK_Reference;
  rtep->referenceExpr.path = allocArena(ar, sizeof(Path));
  parsePath(rtep->referenceExpr.path, parser, ar);
  rtep->diagnostics_length = 0;
  rtep->span = rtep->referenceExpr.path->span;
}

static void parseTypeofTypeExpr(TypeExpr *tte, Parser *parser, Arena *ar) {
  // zero-initialize ttep
  ZERO(tte);
  tte->kind = TEK_Typeof;

  Token t;
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_Typeof, HANDLE_NO_TYPEOF);
  tte->typeofExpr.value = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(tte->typeofExpr.value, parser, ar);
  tte->diagnostics_length = 0;
  tte->span = SPAN(t.span.start, tte->typeofExpr.value->span.end);
  return;

HANDLE_NO_TYPEOF:
  INTERNAL_ERROR("called typeof type expression parser where there was no "
                 "typeof");
  PANIC();
}

static void parseTupleTypeExpr(TypeExpr *tte, Parser *parser, Arena *ar) {
  ZERO(tte);
  tte->kind = TEK_Tuple;

  Token t;
  advanceToken(parser, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_ParenLeft, HANDLE_NO_LEFTPAREN);

  Vector members;
  createVector(&members);

  Vector diagnostics;
  createVector(&diagnostics);

  LnCol end;

  PARSE_DELIMITED_LIST(
      &members,                       // members_vec_ptr
      &diagnostics,                   // diagnostics_vec_ptr
      parseTypeExpr,                  // member_parse_function
      TypeExpr,                       // member_kind
      &tte->tupleExpr.trailing_comma, // trailing_spacer_ptr (will set to true
                                      // if last element is semicolon
      TK_Comma,      // spacer_token_kind (semicolon in block value expr)
      TK_ParenRight, // delimiting_token_kind
      DK_TupleExpectedComma,      // missing_spacer_error
      DK_TupleExpectedRightParen, // missing_delimiter_error
      end,                        // end_lncol
      parser,                        // parser
      ar                          // ar
  )
  tte->tupleExpr.members_length = VEC_LEN(&members, Binding);
  tte->tupleExpr.members = manageMemArena(ar, releaseVector(&members));
  tte->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  tte->diagnostics = manageMemArena(ar, releaseVector(&diagnostics));
  tte->span = SPAN(start, end);
  return;

HANDLE_NO_LEFTPAREN:
  INTERNAL_ERROR("called tuple type expression parser where there was no "
                 "tuple");
  PANIC();
}

static void parseVoidTypeExpr(TypeExpr *vte, Parser *parser, Arena *ar) {
  UNUSED(ar);
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

static void parseFnTypeExpr(TypeExpr *fte, Parser *parser, Arena *ar) {
  ZERO(fte);
  Token t;
  advanceToken(parser, &t);

  if (t.kind != TK_Function) {
    INTERNAL_ERROR("called function type expression parser where there was no "
                   "function");
    PANIC();
  }

  LnCol start = t.span.start;
  LnCol end = t.span.end;

  // create diagnostics
  Vector diagnostics;
  createVector(&diagnostics);

  // check for leftparen
  advanceToken(parser, &t);
  if (t.kind != TK_ParenLeft) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnTypeExprExpectedLeftParen, t.span);
    end = t.span.end;
    goto CLEANUP_DIAGNOSTICS;
  }

  // create members list
  Vector parameters;
  createVector(&parameters);

  PARSE_DELIMITED_LIST(
      &parameters,   // members_vec_ptr
      &diagnostics,  // diagnostics_vec_ptr
      parseTypeExpr, // member_parse_function
      TypeExpr,      // member_kind
      &fte->fnExpr
           .parameters_trailing_comma, // trailing_spacer_ptr (will set to true
                                       // if last element is semicolon
      TK_Comma,      // spacer_token_kind (semicolon in block value expr)
      TK_ParenRight, // delimiting_token_kind
      DK_FnTypeExprExpectedComma,      // missing_spacer_error
      DK_FnTypeExprExpectedRightParen, // missing_delimiter_error
      end,                             // end_lncol
      parser,                             // parser
      ar                               // ar
  )

  advanceToken(parser, &t);
  if (t.kind != TK_Colon) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnTypeExprExpectedColon, t.span);
    end = t.span.end;
    goto CLEANUP_PARAMETERS;
  }

  fte->fnExpr.result = allocArena(ar, sizeof(TypeExpr));
  parseTypeExpr(fte->fnExpr.result, parser, ar);

CLEANUP_PARAMETERS:
  fte->fnExpr.parameters_length = VEC_LEN(&parameters, TypeExpr);
  fte->fnExpr.parameters = manageMemArena(ar, releaseVector(&parameters));

CLEANUP_DIAGNOSTICS:
  fte->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  fte->diagnostics = manageMemArena(ar, releaseVector(&diagnostics));
  fte->span = SPAN(start, end);
}

static void parseL1TypeExpr(TypeExpr *l1, Parser *parser, Arena *ar) {
  Token t;
  advanceToken(parser, &t);
  switch (t.kind) {
  case TK_Identifier: {
    setNextToken(parser, &t);
    parseReferenceTypeExpr(l1, parser, ar);
    return;
  }
  case TK_Enum:
  case TK_Pack:
  case TK_Union:
  case TK_Struct: {
    setNextToken(parser, &t);
    parseStructTypeExpr(l1, parser, ar);
    return;
  }
  case TK_Typeof: {
    setNextToken(parser, &t);
    parseTypeofTypeExpr(l1, parser, ar);
    return;
  }
  case TK_ParenLeft: {
    setNextToken(parser, &t);
    parseTupleTypeExpr(l1, parser, ar);
    return;
  }
  case TK_Void: {
    setNextToken(parser, &t);
    parseVoidTypeExpr(l1, parser, ar);
    break;
  }
  case TK_Function: {
    setNextToken(parser, &t);
    parseFnTypeExpr(l1, parser, ar);
    break;
  }
  default: {
    l1->kind = TEK_None;
    l1->span = t.span;
    l1->diagnostics_length = 1;
    l1->diagnostics = allocArena(ar, sizeof(Diagnostic));
    l1->diagnostics[0] = DIAGNOSTIC(DK_TypeExprUnexpectedToken, t.span);
    // Resync
    resyncStmnt(parser, ar);
    return;
  }
  }
}

static void parseL2TypeExpr(TypeExpr *l2, Parser *parser, Arena *ar) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff
  TypeExpr currentTopLevel;
  parseL1TypeExpr(&currentTopLevel, parser, ar);
  LnCol start = currentTopLevel.span.start;

  while (true) {
    Token t;
    advanceToken(parser, &t);
    switch (t.kind) {
    case TK_Ref: {
      TypeExpr te;
      te.kind = TEK_UnaryOp;
      te.unaryOp.operator= TEUOK_Ref;
      te.unaryOp.operand = allocArena(ar, sizeof(TypeExpr));
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
      te.unaryOp.operand = allocArena(ar, sizeof(TypeExpr));
      *te.unaryOp.operand = currentTopLevel;
      te.span = SPAN(start, t.span.end);
      te.diagnostics_length = 0;
      currentTopLevel = te;
      break;
    }
    case TK_FieldAccess: {
      TypeExpr te;
      te.kind = TEK_FieldAccess;
      te.fieldAccess.value = allocArena(ar, sizeof(TypeExpr));
      *te.fieldAccess.value = currentTopLevel;

      // Now we get the next thing
      advanceToken(parser, &t);
      te.span = SPAN(start, t.span.end);
      if (t.kind != TK_Identifier) {
        // If we encounter an error, we bail out of this L2ValueExpr
        te.fieldAccess.field = NULL;
        setNextToken(parser, &t);
        te.diagnostics_length = 1;
        te.diagnostics = allocArena(ar, sizeof(Diagnostic));
        te.diagnostics[0] =
            DIAGNOSTIC(DK_TypeExprFieldAccessExpectedIdentifier, te.span);
        *l2 = te;
        return;
      }
      te.fieldAccess.field = internArena(t.identifier, ar);
      te.diagnostics_length = 0;
      currentTopLevel = te;
      break;
    }
    default: {
      // there are no more level 2 expressions
      setNextToken(parser, &t);
      *l2 = currentTopLevel;
      return;
    }
    }
  }
}

static void parseTypeExpr(TypeExpr *tep, Parser *parser, Arena *ar) {
  parseL2TypeExpr(tep, parser, ar);
}

static void parseBinding(Binding *bp, Parser *parser, Arena *ar) {
  // zero-initialize bp
  ZERO(bp);

  // these variables will be reused
  Token t;

  // get identifier
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_Identifier, HANDLE_NO_IDENTIFIER);
  Span identitySpan = t.span;
  bp->name = internArena(t.identifier, ar);

  // check if colon
  advanceToken(parser, &t);
  if (t.kind == TK_Colon) {
    // Get type of variable
    bp->type = allocArena(ar, sizeof(TypeExpr));
    parseTypeExpr(bp->type, parser, ar);
  } else {
    // push back whatever
    setNextToken(parser, &t);
    bp->type = allocArena(ar, sizeof(TypeExpr));
    bp->type->kind = TEK_Omitted;
    bp->type->span = identitySpan;
    bp->type->diagnostics_length = 0;
  }

  bp->span = SPAN(identitySpan.start, bp->type->span.end);
  bp->diagnostics_length = 0;
  return;

  // Error handling
  // Set error, and give back the error causing thing
  // Generally, we use the t token to find the last parsed token

HANDLE_NO_IDENTIFIER:
  // If it's not an identifier token, we must resync
  bp->name = NULL;
  bp->span = t.span;
  bp->diagnostics_length = 1;
  bp->diagnostics = allocArena(ar, sizeof(Diagnostic));
  bp->diagnostics[0] = DIAGNOSTIC(DK_BindingExpectedIdentifier, t.span);
  resyncStmnt(parser, ar);
  return;
}

static void parseVarDeclStmnt(Stmnt *vdsp, Parser *parser, Arena *ar) {
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
  vdsp->varDecl.binding = allocArena(ar, sizeof(Binding));
  parseBinding(vdsp->varDecl.binding, parser, ar);

  // Expect Equal Sign
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_Assign, HANDLE_NO_ASSIGN);

  // Get Value;
  vdsp->varDecl.value = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(vdsp->varDecl.value, parser, ar);
  vdsp->diagnostics_length = 0;
  return;

HANDLE_NO_LET:
  INTERNAL_ERROR("called variable declaration parser where there was no "
                 "variable declaration");
  PANIC();

HANDLE_NO_ASSIGN:
  vdsp->span = SPAN(start, t.span.end);
  vdsp->diagnostics_length = 1;
  vdsp->diagnostics = allocArena(ar, sizeof(Diagnostic));
  vdsp->diagnostics[0] = DIAGNOSTIC(DK_VarDeclStmntExpectedAssign, t.span);
  resyncStmnt(parser, ar);
}

static void parseFnDeclStmnt(Stmnt *fdsp, Parser *parser, Arena *ar) {
  // zero-initialize fdsp
  ZERO(fdsp);

  fdsp->kind = SK_FnDecl;

  // these variables will be reused
  Token t;
  // Skip fn declaration
  advanceToken(parser, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_Function, HANDLE_NO_FN);

  // get name
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_Identifier, HANDLE_NO_IDENTIFIER);
  fdsp->fnDecl.name = internArena(t.identifier, ar);

  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_ParenLeft, HANDLE_NO_LEFTPAREN);

  Vector parameters;
  createVector(&parameters);

  Vector diagnostics;
  createVector(&diagnostics);

  LnCol end;

  PARSE_DELIMITED_LIST(
      &parameters,                         // members_vec_ptr
      &diagnostics,                        // diagnostics_vec_ptr
      parseBinding,                        // member_parse_function
      Binding,                             // member_kind
      &fdsp->fnDecl.params_trailing_comma, // trailing_spacer_ptr (will set to
                                           // true if last element is semicolon
      TK_Comma,      // spacer_token_kind (semicolon in block value expr)
      TK_ParenRight, // delimiting_token_kind
      DK_FnDeclStmntExpectedComma,      // missing_spacer_error
      DK_FnDeclStmntExpectedRightParen, // missing_delimiter_error
      end,                              // end_lncol
      parser,                              // parser
      ar                                // ar
  )

  // Copy arguments in
  fdsp->fnDecl.params_length = VEC_LEN(&parameters, Binding);
  fdsp->fnDecl.params = manageMemArena(ar, releaseVector(&parameters));

  // Colon return type delimiter
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_Colon, HANDLE_NO_COLON);

  // Return type
  fdsp->fnDecl.type = allocArena(ar, sizeof(TypeExpr));
  parseTypeExpr(fdsp->fnDecl.type, parser, ar);

  // Equal sign expression delimiter
  advanceToken(parser, &t);
  EXPECT_TYPE(t, TK_Assign, HANDLE_NO_ASSIGN);

  fdsp->fnDecl.body = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(fdsp->fnDecl.body, parser, ar);
  fdsp->span = SPAN(start, fdsp->fnDecl.body->span.end);
  fdsp->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  fdsp->diagnostics = manageMemArena(ar, releaseVector(&diagnostics));
  return;

  // Error handlers
HANDLE_NO_FN:
  INTERNAL_ERROR("called function declaration parser where there was no "
                 "function declaration");
  PANIC();

HANDLE_NO_IDENTIFIER:
  fdsp->diagnostics_length = 1;
  fdsp->diagnostics = allocArena(ar, sizeof(Diagnostic));
  fdsp->diagnostics[0] = DIAGNOSTIC(DK_FnDeclStmntExpectedIdentifier, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->fnDecl.params_length = 0;
  fdsp->fnDecl.params = NULL;
  resyncStmnt(parser, ar);
  return;

HANDLE_NO_LEFTPAREN:
  fdsp->diagnostics_length = 1;
  fdsp->diagnostics = allocArena(ar, sizeof(Diagnostic));
  fdsp->diagnostics[0] = DIAGNOSTIC(DK_FnDeclStmntExpectedLeftParen, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->fnDecl.params_length = 0;
  fdsp->fnDecl.params = NULL;
  resyncStmnt(parser, ar);
  return;

HANDLE_NO_COLON:
  *VEC_PUSH(&diagnostics, Diagnostic) =
      DIAGNOSTIC(DK_FnDeclStmntExpectedColon, t.span);
  fdsp->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  fdsp->diagnostics = manageMemArena(ar, releaseVector(&diagnostics));
  fdsp->span = SPAN(start, t.span.end);
  resyncStmnt(parser, ar);
  return;

HANDLE_NO_ASSIGN:
  *VEC_PUSH(&diagnostics, Diagnostic) =
      DIAGNOSTIC(DK_FnDeclStmntExpectedAssign, t.span);
  fdsp->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  fdsp->diagnostics = manageMemArena(ar, releaseVector(&diagnostics));
  fdsp->span = SPAN(start, t.span.end);
  resyncStmnt(parser, ar);
  return;
}

static void parseTypeAliasStmnt(Stmnt *adsp, Parser *parser, Arena *ar) {
  ZERO(adsp);
  adsp->kind = SK_TypeAliasStmnt;
  Token t;
  advanceToken(parser, &t);

  if(t.kind != TK_TypeAlias) {
    INTERNAL_ERROR("called type alias declaration parser where there was no "
        "type alias declaration");
    PANIC();
  }

  LnCol start = t.span.start;

  LnCol end;

  advanceToken(parser, &t);

  if(t.kind != TK_Identifier) {
    adsp->typeAliasStmnt.name = NULL;
    adsp->diagnostics = allocArena(ar, sizeof(Diagnostic));
    adsp->diagnostics[0] =
      DIAGNOSTIC(DK_TypeAliasExpectedIdentifier, t.span);
    adsp->diagnostics_length = 1;
    end = t.span.end;
    goto CLEANUP;
  }

  adsp->typeAliasStmnt.name = internArena(t.identifier, ar);

  // Now get equals sign
  advanceToken(parser, &t);
  if(t.kind != TK_Assign) {
    adsp->diagnostics = allocArena(ar, sizeof(Diagnostic));
    adsp->diagnostics[0] =
      DIAGNOSTIC(DK_TypeAliasExpectedAssign, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  adsp->typeAliasStmnt.type = allocArena(ar, sizeof(TypeExpr));
  parseTypeExpr(adsp->typeAliasStmnt.type, parser, ar);
  end = adsp->typeAliasStmnt.type->span.end;
  adsp->diagnostics_length = 0;
  adsp->diagnostics = NULL;

CLEANUP:
  adsp->span = SPAN(start, end);
  return;
}

static void parseStmnt(Stmnt *sp, Parser *parser, Arena *ar) {
  Token t;
  // peek next token
  advanceToken(parser, &t);
  setNextToken(parser, &t);

  switch (t.kind) {
  case TK_Function: {
    parseFnDeclStmnt(sp, parser, ar);
    return;
  }
  case TK_Let: {
    parseVarDeclStmnt(sp, parser, ar);
    return;
  }
  case TK_TypeAlias: {
    parseTypeAliasStmnt(sp, parser, ar);
    return;
  }
  default: {
    // Value Expr Statement
    sp->kind = SK_Expr;
    sp->exprStmnt.value = allocArena(ar, sizeof(ValueExpr));
    parseValueExpr(sp->exprStmnt.value, parser, ar);
    sp->span = sp->exprStmnt.value->span;
    sp->diagnostics_length = 0;
    return;
  }
  }
}

void parseTranslationUnit(TranslationUnit *tu, Parser *parser, Arena *ar) {
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
    parseStmnt(VEC_PUSH(&statements, Stmnt), parser, ar);

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
  tu->statements = manageMemArena(ar, releaseVector(&statements));
  tu->span = SPAN(LNCOL(0, 0), end);
  tu->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  tu->diagnostics = manageMemArena(ar, releaseVector(&diagnostics));
  return;
}
