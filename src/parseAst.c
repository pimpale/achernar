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

#define EXPECT_TYPE(token, tokenType, onErrLabel)                              \
  do {                                                                         \
    if ((token).kind != (tokenType)) {                                         \
      goto onErrLabel;                                                         \
    }                                                                          \
  } while (false)

// Call this method after token error detected to skip to the next valid state

// Jump till after semicolon, taking into account parentheses, brackets,
// attributes, and braces It will ignore subexprs, but can halt at the end
static void resyncStmnt(BufferedLexer *blp, Arena *ar) {
  UNUSED(ar);
  int64_t parenDepth = 0;
  int64_t braceDepth = 0;
  int64_t bracketDepth = 0;

  while (true) {
    Token t;
    advanceToken(blp, &t);
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
        setNextToken(blp, &t);
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

// Jumps till we hit identifier or assign sign then backtracks one
// Not aware of parens, braces, brackets or attributes
// This is because all types are linear in nature in BPlus
static void resyncType(BufferedLexer *blp, Arena *ar) {
  UNUSED(ar);
  while (true) {
    Token t;
    advanceToken(blp, &t);
    switch (t.kind) {
    case TK_Assign:
    case TK_Identifier: {
      setNextToken(blp, &t);
      return;
    }
    default: {
      break;
    }
    }
  }
}

// Note that all errors resync at the statement level
static void parseStmnt(Stmnt *sp, BufferedLexer *blp, Arena *ar);
static void parseValueExpr(ValueExpr *vep, BufferedLexer *blp, Arena *ar);
static void parseTypeExpr(TypeExpr *tep, BufferedLexer *blp, Arena *ar);
static void parseBinding(Binding *bp, BufferedLexer *blp, Arena *ar);

static void parsePath(Path *pp, BufferedLexer *blp, Arena *ar) {
  Token t;
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_Identifier, HANDLE_NO_STARTING_IDENTIFIER);
  LnCol start = t.span.start;

  Vector pathSegments;
  createVector(&pathSegments);

  *VEC_PUSH(&pathSegments, char *) = INTERN(t.identifier, ar);

  while (true) {
    advanceToken(blp, &t);
    if (t.kind == TK_ScopeResolution) {
      advanceToken(blp, &t);
      EXPECT_TYPE(t, TK_Identifier, HANDLE_NO_IDENTIFIER);
      *VEC_PUSH(&pathSegments, char *) = INTERN(t.identifier, ar);
    } else {
      // we've reached the end of the path
      setNextToken(blp, &t);
      break;
    }
  }

  pp->pathSegments_length = VEC_LEN(&pathSegments, char *);
  pp->pathSegments = manageMemArena(ar, releaseVector(&pathSegments));
  pp->diagnostics_length = 0;
  pp->diagnostics = NULL;
  pp->span = SPAN(start, t.span.end);
  return;

HANDLE_NO_IDENTIFIER:
  setNextToken(blp, &t);
  pp->pathSegments_length = VEC_LEN(&pathSegments, char *);
  pp->pathSegments = manageMemArena(ar, releaseVector(&pathSegments));
  pp->diagnostics_length = 1;
  pp->diagnostics = allocArena(ar, sizeof(Diagnostic));
  pp->diagnostics[0] = DIAGNOSTIC(DK_PathExpectedIdentifier, t.span);
  pp->span = SPAN(start, t.span.end);
  return;

HANDLE_NO_STARTING_IDENTIFIER:
  INTERNAL_ERROR("called path parser where there was no path");
  PANIC();
}

static void parseIntValueExpr(ValueExpr *ivep, BufferedLexer *blp, Arena *ar) {
  UNUSED(ar);
  Token t;
  advanceToken(blp, &t);
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

static void parseBoolValueExpr(ValueExpr *bvep, BufferedLexer *blp, Arena *ar) {
  UNUSED(ar);
  Token t;
  advanceToken(blp, &t);
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

static void parseFloatValueExpr(ValueExpr *fvep, BufferedLexer *blp, Arena *ar) {
  UNUSED(ar);
  Token t;
  advanceToken(blp, &t);
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

static void parseCharValueExpr(ValueExpr *cvep, BufferedLexer *blp, Arena *ar) {
  UNUSED(ar);
  Token t;
  advanceToken(blp, &t);
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

static void parseStringValueExpr(ValueExpr *svep, BufferedLexer *blp, Arena *ar) {
  UNUSED(ar);
  Token t;
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_StringLiteral, HANDLE_NO_STRING_LITERAL);
  svep->kind = VEK_StringLiteral;
  svep->stringLiteral.value = INTERN(t.string_literal, ar);
  svep->span = t.span;
  svep->diagnostics_length = 0;
  return;

HANDLE_NO_STRING_LITERAL:
  INTERNAL_ERROR("called string literal parser where there was no "
                 "string literal");
  PANIC();
}

static void parseGroupValueExpr(ValueExpr *gvep, BufferedLexer *blp, Arena *ar) {
  ZERO(gvep);
  gvep->kind = VEK_Group;

  Token t;
  advanceToken(blp, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_ParenLeft, HANDLE_NO_LEFTPAREN);
  gvep->groupExpr.value = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(gvep->groupExpr.value, blp, ar);

  // Expect a rightparen
  advanceToken(blp, &t);
  gvep->span = SPAN(start, t.span.end);
  EXPECT_TYPE(t, TK_ParenRight, HANDLE_NO_RIGHTPAREN);
  gvep->diagnostics_length = 0;
  return;

HANDLE_NO_LEFTPAREN:
  INTERNAL_ERROR("called group parser where there was no "
                 "group");
  PANIC();

HANDLE_NO_RIGHTPAREN:
  resyncStmnt(blp, ar);
  gvep->diagnostics_length = 1;
  gvep->diagnostics = allocArena(ar, sizeof(Diagnostic));
  gvep->diagnostics[0] = DIAGNOSTIC(DK_GroupExpectRightParen, gvep->span);
}

static void parseBlockValueExpr(ValueExpr *bvep, BufferedLexer *blp, Arena *ar) {
  ZERO(bvep);
  bvep->kind = VEK_Block;

  Token t;
  advanceToken(blp, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_BraceLeft, HANDLE_NO_LEFTBRACE);

  // Create list of statements
  Vector statements;
  createVector(&statements);

  // List of diagnostics
  Vector diagnostics;
  createVector(&diagnostics);

  // Parse the elements
  while (true) {
    // Check for end paren
    advanceToken(blp, &t);
    if (t.kind == TK_BraceRight) {
      bvep->blockExpr.suppress_value = true;
      break;
    }
    // If it wasn't an end brace, we push it back
    setNextToken(blp, &t);

    // Parse and push the statement
    parseStmnt(VEC_PUSH(&statements, Stmnt), blp, ar);

    // Accept semicolon, if any
    // If there's no semicolon then it MUST be followed by an right brace
    // This also allows a trailing semicolon
    advanceToken(blp, &t);
    if (t.kind != TK_Semicolon) {
      bvep->blockExpr.suppress_value = false;
      // If the next value isn't a right brace, then we throw an error
      if (t.kind == TK_BraceRight) {
        break;
      } else if (t.kind == TK_None && t.error == DK_EOF) {
        *VEC_PUSH(&diagnostics, Diagnostic) = DIAGNOSTIC(DK_EOF, t.span);
        break;
      } else {
        // give them a missing semicolon error, but keep parsing
        setNextToken(blp, &t);
        *VEC_PUSH(&diagnostics, Diagnostic) =
            DIAGNOSTIC(DK_BlockExpectedSemicolon, t.span);
      }
    }
  }

  bvep->blockExpr.statements_length = VEC_LEN(&statements, Stmnt);
  bvep->blockExpr.statements = manageMemArena(ar, releaseVector(&statements));
  bvep->span = SPAN(start, t.span.end);
  bvep->diagnostics_length = VEC_LEN(&diagnostics, Diagnostic);
  bvep->diagnostics = manageMemArena(ar, releaseVector(&diagnostics));
  return;

HANDLE_NO_LEFTBRACE:
  INTERNAL_ERROR(
      "called a block expresion parser where there was no leftbrace");
  PANIC();
}

static void parseIfValueExpr(ValueExpr *ivep, BufferedLexer *blp, Arena *ar) {
  ZERO(ivep);
  Token t;
  advanceToken(blp, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_If, HANDLE_NO_IF);
  ivep->kind = VEK_If;

  // parse condition
  ivep->ifExpr.condition = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(ivep->ifExpr.condition, blp, ar);

  // parse body
  ivep->ifExpr.body = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(ivep->ifExpr.body, blp, ar);

  // if the next value is else
  advanceToken(blp, &t);
  if (t.kind == TK_Else) {
    ivep->ifExpr.has_else = true;
    ivep->ifExpr.else_body = allocArena(ar, sizeof(ValueExpr));
    parseValueExpr(ivep->ifExpr.else_body, blp, ar);
    ivep->span = SPAN(start, ivep->ifExpr.else_body->span.end);
  } else {
    // back up
    setNextToken(blp, &t);
    ivep->span = SPAN(start, ivep->ifExpr.body->span.end);
  }
  return;

HANDLE_NO_IF:
  INTERNAL_ERROR("called if expression parser where there was no "
                 "if expression");
  PANIC();
}

static void parsePassValueExpr(ValueExpr *pep, BufferedLexer *blp, Arena *ar) {
  UNUSED(ar);
  pep->kind = VEK_Pass;
  Token t;
  advanceToken(blp, &t);
  pep->span = t.span;
  EXPECT_TYPE(t, TK_Pass, HANDLE_NO_PASS);
  pep->diagnostics_length = 0;
  return;

HANDLE_NO_PASS:
  INTERNAL_ERROR("called pass parser where there was no pass");
  PANIC();
}

static void parseBreakValueExpr(ValueExpr *bep, BufferedLexer *blp, Arena *ar) {
  UNUSED(ar);
  bep->kind = VEK_Break;
  Token t;
  advanceToken(blp, &t);
  bep->span = t.span;
  EXPECT_TYPE(t, TK_Break, HANDLE_NO_BREAK);
  bep->diagnostics_length = 0;
  return;

HANDLE_NO_BREAK:
  INTERNAL_ERROR("called break parser where there was no break");
  PANIC();
}

static void parseContinueValueExpr(ValueExpr *cep, BufferedLexer *blp, Arena *ar) {
  UNUSED(ar);
  cep->kind = VEK_Continue;
  Token t;
  advanceToken(blp, &t);
  cep->span = t.span;
  EXPECT_TYPE(t, TK_Continue, HANDLE_NO_CONTINUE);
  cep->diagnostics_length = 0;
  return;

HANDLE_NO_CONTINUE:
  INTERNAL_ERROR("called continue parser where there was no continue");
  PANIC();
}

static void parseReturnValueExpr(ValueExpr *rep, BufferedLexer *blp, Arena *ar) {
  rep->kind = VEK_Return;
  Token t;
  advanceToken(blp, &t);
  rep->span = t.span;
  EXPECT_TYPE(t, TK_Return, HANDLE_NO_RETURN);
  rep->returnExpr.value = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(rep->returnExpr.value, blp, ar);
  rep->diagnostics_length = 0;
  return;

HANDLE_NO_RETURN:
  INTERNAL_ERROR("called return parser where there was no continue");
  PANIC();
}

static void parseWhileValueExpr(ValueExpr *wep, BufferedLexer *blp, Arena *ar) {
  wep->kind = VEK_While;
  Token t;
  advanceToken(blp, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_While, HANDLE_NO_WHILE);

  wep->whileExpr.condition = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(wep->whileExpr.condition, blp, ar);

  wep->whileExpr.body = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(wep->whileExpr.body, blp, ar);

  wep->span = SPAN(start, wep->whileExpr.body->span.end);
  wep->diagnostics_length = 0;
  return;

HANDLE_NO_WHILE:
  INTERNAL_ERROR("called continue parser where there was no continue");
  PANIC();
}

// Pattern : Expr,
static void parseMatchCaseExpr(struct MatchCaseExpr_s *mcep, BufferedLexer *blp, Arena *ar) {
  ZERO(mcep);

  Token t;
  // Get pattern
  mcep->pattern = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(mcep->pattern, blp, ar);
  // Expect colon
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_Colon, HANDLE_NO_COLON);
  // Get Value
  mcep->value = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(mcep->value, blp, ar);
  mcep->span = SPAN(mcep->pattern->span.start, mcep->value->span.end);
  mcep->diagnostics_length = 0;
  return;

HANDLE_NO_COLON:
  mcep->span = mcep->pattern->span;
  mcep->diagnostics_length = 1;
  mcep->diagnostics = allocArena(ar, sizeof(Diagnostic));
  mcep->diagnostics[0] = DIAGNOSTIC(DK_MatchNoColon, t.span);
  resyncStmnt(blp, ar);
  return;
}

static void parseMatchValueExpr(ValueExpr *mvep, BufferedLexer *blp, Arena *ar) {
  ZERO(mvep);
  mvep->kind = VEK_Match;
  Token t;
  // Ensure match
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_Match, HANDLE_NO_MATCH);

  LnCol start = t.span.start;

  // Get expression to match against
  mvep->matchExpr.value = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(mvep->matchExpr.value, blp, ar);
  // now we must parse the block containing the cases

  // Expect beginning brace
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_BraceLeft, HANDLE_NO_LEFTBRACE);

  Vector matchCases;
  createVector(&matchCases);
  while (true) {
    // Check if right brace
    advanceToken(blp, &t);
    if (t.kind == TK_BraceRight) {
      break;
    }
    setNextToken(blp, &t);

    // Parse the match case expr
    parseMatchCaseExpr(VEC_PUSH(&matchCases, struct MatchCaseExpr_s), blp, ar);

    // Accept comma, if any
    // If there's no comma then it MUST be followed by an end brace
    // This also allows trailing commas
    advanceToken(blp, &t);
    if (t.kind != TK_Comma) {
      // If the next value isn't an end brace, then we throw an error
      EXPECT_TYPE(t, TK_BraceRight, HANDLE_NO_RIGHTBRACE);
      break;
    }
  }

  // Get interior cases
  mvep->matchExpr.cases_length = VEC_LEN(&matchCases, struct MatchCaseExpr_s);
  mvep->matchExpr.cases = manageMemArena(ar, releaseVector(&matchCases));

  mvep->span = SPAN(start, t.span.end);
  mvep->diagnostics_length = 0;

  return;

HANDLE_NO_MATCH:
  INTERNAL_ERROR("called match parser where there was no match");
  PANIC();

HANDLE_NO_RIGHTBRACE:
  mvep->diagnostics_length = 1;
  mvep->diagnostics = allocArena(ar, sizeof(Diagnostic));
  mvep->diagnostics[0] = DIAGNOSTIC(DK_MatchNoRightBrace, t.span);
  mvep->span = SPAN(start, t.span.end);
  mvep->matchExpr.cases_length = VEC_LEN(&matchCases, struct MatchCaseExpr_s);
  mvep->matchExpr.cases = manageMemArena(ar, releaseVector(&matchCases));
  resyncStmnt(blp, ar);
  return;

HANDLE_NO_LEFTBRACE:
  mvep->diagnostics_length = 1;
  mvep->diagnostics = allocArena(ar, sizeof(Diagnostic));
  mvep->diagnostics[0] = DIAGNOSTIC(DK_MatchNoLeftbrace, t.span);
  mvep->span = SPAN(start, t.span.end);
  mvep->matchExpr.cases_length = VEC_LEN(&matchCases, struct MatchCaseExpr_s);
  mvep->matchExpr.cases = manageMemArena(ar, releaseVector(&matchCases));
  resyncStmnt(blp, ar);
  return;
}

static void parseReferenceValueExpr(ValueExpr *rvep, BufferedLexer *blp, Arena *ar) {
  ZERO(rvep);
  rvep->kind = VEK_Reference;
  rvep->reference.path = allocArena(ar, sizeof(Path));
  parsePath(rvep->reference.path, blp, ar);
  rvep->span = rvep->reference.path->span;
  rvep->diagnostics_length = 0;
  return;
}

// Level0ValueExpr (literals of any kind)
// Level1ValueExpr parentheses, braces
// Level2ValueExpr () [] $ @ . -> (suffixes)
// Level3ValueExpr - + ~ ! (prefixes)
// Level4ValueExpr * / % (multiplication and division)
// Level5ValueExpr + - (addition and subtraction)
// Level6ValueExpr << >> & | ^ (bitwise operators)
// Level7ValueExpr < <= > >= == != (comparators)
// Level8ValueExpr && || (Boolean Operators)

static void parseL1ValueExpr(ValueExpr *l1, BufferedLexer *blp, Arena *ar) {
  Token t;
  advanceToken(blp, &t);
  // Decide which expression it is
  switch (t.kind) {
  // Literals
  case TK_IntLiteral: {
    setNextToken(blp, &t);
    parseIntValueExpr(l1, blp, ar);
    return;
  }
  case TK_BoolLiteral: {
    setNextToken(blp, &t);
    parseBoolValueExpr(l1, blp, ar);
    return;
  }
  case TK_FloatLiteral: {
    setNextToken(blp, &t);
    parseFloatValueExpr(l1, blp, ar);
    return;
  }
  case TK_CharLiteral: {
    setNextToken(blp, &t);
    parseCharValueExpr(l1, blp, ar);
    return;
  }
  case TK_StringLiteral: {
    setNextToken(blp, &t);
    parseStringValueExpr(l1, blp, ar);
    return;
  }
  case TK_ParenLeft: {
    setNextToken(blp, &t);
    parseGroupValueExpr(l1, blp, ar);
    return;
  }
  case TK_BraceLeft: {
    setNextToken(blp, &t);
    parseBlockValueExpr(l1, blp, ar);
    return;
  }
  case TK_If: {
    setNextToken(blp, &t);
    parseIfValueExpr(l1, blp, ar);
    return;
  }
  case TK_Break: {
    setNextToken(blp, &t);
    parseBreakValueExpr(l1, blp, ar);
    return;
  }
  case TK_Continue: {
    setNextToken(blp, &t);
    parseContinueValueExpr(l1, blp, ar);
    return;
  }
  case TK_Return: {
    setNextToken(blp, &t);
    parseReturnValueExpr(l1, blp, ar);
    return;
  }
  case TK_Pass: {
    setNextToken(blp, &t);
    parsePassValueExpr(l1, blp, ar);
    return;
  }
  case TK_While: {
    setNextToken(blp, &t);
    parseWhileValueExpr(l1, blp, ar);
    return;
  }
  case TK_Match: {
    setNextToken(blp, &t);
    parseMatchValueExpr(l1, blp, ar);
    return;
  }
  case TK_Identifier: {
    setNextToken(blp, &t);
    parseReferenceValueExpr(l1, blp, ar);
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

static void parseL2ValueExpr(ValueExpr *l2, BufferedLexer *blp, Arena *ar) {

  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff
  ValueExpr currentTopLevel;
  parseL1ValueExpr(&currentTopLevel, blp, ar);
  LnCol start = currentTopLevel.span.start;

  while (true) {
    Token t;
    advanceToken(blp, &t);
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
      advanceToken(blp, &t);
      v.span = SPAN(start, t.span.end);
      if (t.kind != TK_Identifier) {
        // If we encounter an error, we bail out of this L2ValueExpr
        v.fieldAccess.field = NULL;
        setNextToken(blp, &t);
        v.diagnostics_length = 1;
        v.diagnostics = allocArena(ar, sizeof(Diagnostic));
        v.diagnostics[0] = DIAGNOSTIC(DK_FieldAccessExpectedIdentifier, v.span);
        *l2 = v;
        return;
      }
      v.fieldAccess.field = INTERN(t.identifier, ar);
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
      parseValueExpr(v.binaryOp.right_operand, blp, ar);
      // expect closing bracket
      advanceToken(blp, &t);

      // Calculate span and diagnostics
      v.span = SPAN(start, t.span.end);
      if (t.kind != TK_BracketRight) {
        // If we miss the bracket we bail out of this subexpr, setting the
        // next token
        v.diagnostics_length = 1;
        v.diagnostics = allocArena(ar, sizeof(Diagnostic));
        v.diagnostics[0] = DIAGNOSTIC(DK_ArrayAccessExpectedBracket, v.span);
        setNextToken(blp, &t);
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
        advanceToken(blp, &t);
        if (t.kind == TK_ParenRight) {
          break;
        }
        // If it wasn't an end paren, we push it back
        setNextToken(blp, &t);

        // Parse and push the argument
        parseValueExpr(VEC_PUSH(&arguments, ValueExpr), blp, ar);

        // Accept comma, if any
        // If there's no comma then it MUST be followed by an end paren
        // This also allows trailing commas
        advanceToken(blp, &t);
        if (t.kind != TK_Comma) {
          if (t.kind == TK_ParenRight) {
            break;
          } else {
            // If the next value isn't an end paren, then we throw an error
            v.kind = VEK_Call;
            v.callExpr.function = allocArena(ar, sizeof(ValueExpr));
            *v.callExpr.function = currentTopLevel;
            v.callExpr.arguments_length = VEC_LEN(&arguments, ValueExpr);
            v.callExpr.arguments = manageMemArena(ar, releaseVector(&arguments));

            // Calculate span and diagnostics
            v.span = SPAN(start, t.span.end);
            v.diagnostics_length = 1;
            v.diagnostics = allocArena(ar, sizeof(Diagnostic));
            v.diagnostics[0] = DIAGNOSTIC(DK_FunctionCallExpectedParen, v.span);
            // Bail out of the subexpr
            setNextToken(blp, &t);
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
      setNextToken(blp, &t);
      *l2 = currentTopLevel;
      return;
    }
    }
  }
}

static void parseL3ValueExpr(ValueExpr *l3, BufferedLexer *blp, Arena *ar) {
  Token t;
  advanceToken(blp, &t);
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
    setNextToken(blp, &t);
    parseL2ValueExpr(l3, blp, ar);
    return;
  }
  }

  // Now parse the rest of the expression
  l3->kind = VEK_UnaryOp;
  l3->unaryOp.operand = allocArena(ar, sizeof(ValueExpr));
  parseL3ValueExpr(l3->unaryOp.operand, blp, ar);
  l3->span = SPAN(t.span.start, l3->unaryOp.operand->span.end);
  l3->diagnostics_length = 0;
  return;
}

static void parseL4ValueExpr(ValueExpr *l4, BufferedLexer *blp, Arena *ar) {
  ValueExpr v;
  parseL3ValueExpr(&v, blp, ar);

  Token t;
  advanceToken(blp, &t);
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
    setNextToken(blp, &t);
    *l4 = v;
    return;
  }
  }

  l4->kind = VEK_BinaryOp;
  l4->binaryOp.left_operand = allocArena(ar, sizeof(ValueExpr));
  *l4->binaryOp.left_operand = v;
  l4->binaryOp.right_operand = allocArena(ar, sizeof(ValueExpr));
  parseL4ValueExpr(l4->binaryOp.right_operand, blp, ar);
  l4->span = SPAN(l4->binaryOp.left_operand->span.start,
                  l4->binaryOp.right_operand->span.end);
  l4->diagnostics_length = 0;
  return;
}

static void parseL5ValueExpr(ValueExpr *l5, BufferedLexer *blp, Arena *ar) {
  ValueExpr v;
  parseL4ValueExpr(&v, blp, ar);

  Token t;
  advanceToken(blp, &t);
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
    setNextToken(blp, &t);
    *l5 = v;
    return;
  }
  }

  l5->kind = VEK_BinaryOp;
  l5->binaryOp.left_operand = allocArena(ar, sizeof(ValueExpr));
  *l5->binaryOp.left_operand = v;
  l5->binaryOp.right_operand = allocArena(ar, sizeof(ValueExpr));
  parseL5ValueExpr(l5->binaryOp.right_operand, blp, ar);
  l5->span = SPAN(l5->binaryOp.left_operand->span.start,
                  l5->binaryOp.right_operand->span.end);
  l5->diagnostics_length = 0;
  return;
}

static void parseL6ValueExpr(ValueExpr *l6, BufferedLexer *blp, Arena *ar) {
  ValueExpr v;
  parseL5ValueExpr(&v, blp, ar);

  Token t;
  advanceToken(blp, &t);
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
    setNextToken(blp, &t);
    *l6 = v;
    return;
  }
  }

  l6->kind = VEK_BinaryOp;
  l6->binaryOp.left_operand = allocArena(ar, sizeof(ValueExpr));
  *l6->binaryOp.left_operand = v;
  l6->binaryOp.right_operand = allocArena(ar, sizeof(ValueExpr));
  parseL6ValueExpr(l6->binaryOp.right_operand, blp, ar);
  l6->span = SPAN(l6->binaryOp.left_operand->span.start,
                  l6->binaryOp.right_operand->span.end);
  l6->diagnostics_length = 0;
  return;
}

static void parseL7ValueExpr(ValueExpr *l7, BufferedLexer *blp, Arena *ar) {
  ValueExpr v;
  parseL6ValueExpr(&v, blp, ar);

  Token t;
  advanceToken(blp, &t);
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
    setNextToken(blp, &t);
    *l7 = v;
    return;
  }
  }

  l7->kind = VEK_BinaryOp;
  l7->binaryOp.left_operand = allocArena(ar, sizeof(ValueExpr));
  *l7->binaryOp.left_operand = v;
  l7->binaryOp.right_operand = allocArena(ar, sizeof(ValueExpr));
  parseL7ValueExpr(l7->binaryOp.right_operand, blp, ar);
  l7->span = SPAN(l7->binaryOp.left_operand->span.start,
                  l7->binaryOp.right_operand->span.end);
  l7->diagnostics_length = 0;
  return;
}

static void parseL8ValueExpr(ValueExpr *l8, BufferedLexer *blp, Arena *ar) {
  ValueExpr v;
  parseL7ValueExpr(&v, blp, ar);

  Token t;
  advanceToken(blp, &t);
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
    setNextToken(blp, &t);
    *l8 = v;
    return;
  }
  }

  l8->kind = VEK_BinaryOp;
  l8->binaryOp.left_operand = allocArena(ar, sizeof(ValueExpr));
  *l8->binaryOp.left_operand = v;
  l8->binaryOp.right_operand = allocArena(ar, sizeof(ValueExpr));
  parseL8ValueExpr(l8->binaryOp.right_operand, blp, ar);
  l8->span = SPAN(l8->binaryOp.left_operand->span.start,
                  l8->binaryOp.right_operand->span.end);
  l8->diagnostics_length = 0;
  return;
}

// shim method
static void parseValueExpr(ValueExpr *vep, BufferedLexer *blp, Arena *ar) {
  parseL8ValueExpr(vep, blp, ar);
}


static void parseStructTypeExpr(TypeExpr *ste, BufferedLexer *blp, Arena *ar) {
  ZERO(ste);
  ste->kind = TEK_Struct;
  Token t;
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_Struct, HANDLE_NO_STRUCT);
  LnCol start = t.span.start;

  EXPECT_TYPE(t, TK_BraceLeft, HANDLE_NO_LEFTBRACE);

  Vector bindings;
  createVector(&bindings);

  // Parse the parameters (Comma Separated list of bindings)
  while (true) {
    // Check for end brace
    advanceToken(blp, &t);
    if (t.kind == TK_BraceRight) {
      break;
    } else if (t.kind == TK_None && t.error == DK_EOF) {
      goto HANDLE_NO_RIGHTBRACE;
    }
    // If it wasn't an end brace, we push it back
    setNextToken(blp, &t);

    // Parse and push the parameter
    parseBinding(VEC_PUSH(&bindings, Binding), blp, ar);
    // Accept comma, if any
    // If there's no comma then it MUST be followed by an end brace
    // This also allows trailing commas
    advanceToken(blp, &t);
    if (t.kind != TK_Comma) {
      // If the next value isn't an end paren, then we throw an error
      EXPECT_TYPE(t, TK_ParenRight, HANDLE_NO_RIGHTBRACE);
      break;
    }
  }

  ste->structExpr.members_length = VEC_LEN(&bindings, Binding);
  ste->structExpr.members = manageMemArena(ar, releaseVector(&bindings));
  ste->span = SPAN(start, t.span.end);
  ste->diagnostics_length = 0;
  return;

HANDLE_NO_LEFTBRACE:
  ste->structExpr.members_length = 0;
  ste->structExpr.members = NULL;
  ste->span = SPAN(start, t.span.end);
  ste->diagnostics_length = 1;
  ste->diagnostics = allocArena(ar, sizeof(Diagnostic));
  ste->diagnostics[0] =
      DIAGNOSTIC(DK_StructDeclStmntExpectedLeftBrace, ste->span);
  return;

HANDLE_NO_RIGHTBRACE:
  ste->structExpr.members_length = VEC_LEN(&bindings, Binding);
  ste->structExpr.members = manageMemArena(ar, releaseVector(&bindings));
  ste->span = SPAN(start, t.span.end);
  ste->diagnostics_length = 1;
  ste->diagnostics = allocArena(ar, sizeof(Diagnostic));
  ste->diagnostics[0] =
      DIAGNOSTIC(DK_StructDeclStmntExpectedRightBrace, t.span);
  resyncStmnt(blp, ar);
  return;

HANDLE_NO_STRUCT:
  INTERNAL_ERROR("called struct type expression parser where there was no "
                 "struct declaration");
  PANIC();
}

static void parseReferenceTypeExpr(TypeExpr *rtep, BufferedLexer *blp, Arena *ar) {
  ZERO(rtep);
  rtep->kind = TEK_Reference;
  rtep->referenceExpr.path = allocArena(ar, sizeof(Path));
  parsePath(rtep->referenceExpr.path, blp, ar);
  rtep->diagnostics_length = 0;
  rtep->span = rtep->referenceExpr.path->span;
}

static void parseTypeofTypeExpr(TypeExpr *tte, BufferedLexer *blp, Arena *ar) {
  // zero-initialize ttep
  ZERO(tte);
  tte->kind = TEK_Typeof;

  Token t;
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_Typeof, HANDLE_NO_TYPEOF);
  tte->typeofExpr.value = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(tte->typeofExpr.value, blp, ar);
  tte->diagnostics_length = 0;
  tte->span = SPAN(t.span.start, tte->typeofExpr.value->span.end);
  return;

HANDLE_NO_TYPEOF:
  INTERNAL_ERROR("called typeof type expression parser where there was no "
                 "typeof");
  PANIC();
}

static void parseL1TypeExpr(TypeExpr *l1, BufferedLexer *blp, Arena *ar) {
  Token t;
  advanceToken(blp, &t);
  switch (t.kind) {
  case TK_Identifier: {
    setNextToken(blp, &t);
    parseReferenceTypeExpr(l1, blp, ar);
    return;
  }
  case TK_Struct: {
    setNextToken(blp, &t);
    parseStructTypeExpr(l1, blp, ar);
    return;
  }
  case TK_Typeof: {
    setNextToken(blp, &t);
    parseTypeofTypeExpr(l1, blp, ar);
    return;
  }
  default: {
    l1->kind = TEK_None;
    l1->span = t.span;
    l1->diagnostics_length = 1;
    l1->diagnostics = allocArena(ar, sizeof(Diagnostic));
    l1->diagnostics[0] = DIAGNOSTIC(DK_TypeExprUnexpectedToken, t.span);
    // Resync
    resyncType(blp, ar);
    return;
  }
  }
}

static void parseL2TypeExpr(TypeExpr *l2, BufferedLexer *blp, Arena *ar) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff
  TypeExpr currentTopLevel;
  parseL1TypeExpr(&currentTopLevel, blp, ar);
  LnCol start = currentTopLevel.span.start;

  while (true) {
    Token t;
    advanceToken(blp, &t);
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
      advanceToken(blp, &t);
      te.span = SPAN(start, t.span.end);
      if (t.kind != TK_Identifier) {
        // If we encounter an error, we bail out of this L2ValueExpr
        te.fieldAccess.field = NULL;
        setNextToken(blp, &t);
        te.diagnostics_length = 1;
        te.diagnostics = allocArena(ar, sizeof(Diagnostic));
        te.diagnostics[0] =
            DIAGNOSTIC(DK_TypeExprFieldAccessExpectedIdentifier, te.span);
        *l2 = te;
        return;
      }
      te.fieldAccess.field = INTERN(t.identifier, ar);
      te.diagnostics_length = 0;
      currentTopLevel = te;
      break;
    }
    default: {
      // there are no more level 2 expressions
      setNextToken(blp, &t);
      *l2 = currentTopLevel;
      return;
    }
    }
  }
}

static void parseTypeExpr(TypeExpr *tep, BufferedLexer *blp, Arena *ar) {
  parseL2TypeExpr(tep, blp, ar);
}

static void parseBinding(Binding *bp, BufferedLexer *blp, Arena *ar) {
  // zero-initialize bp
  ZERO(bp);

  // these variables will be reused
  Token t;

  // get identifier
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_Identifier, HANDLE_NO_IDENTIFIER);
  Span identitySpan = t.span;
  bp->name = INTERN(t.identifier, ar);

  // check if colon
  advanceToken(blp, &t);
  if (t.kind == TK_Colon) {
    // Get type of variable
    bp->type = allocArena(ar, sizeof(TypeExpr));
    parseTypeExpr(bp->type, blp, ar);
  } else {
    // push back whatever
    setNextToken(blp, &t);
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
  bp->span = t.span;
  bp->diagnostics_length = 1;
  bp->diagnostics = allocArena(ar, sizeof(Diagnostic));
  bp->diagnostics[0] = DIAGNOSTIC(DK_BindingExpectedIdentifier, t.span);
  resyncStmnt(blp, ar);
  return;
}

static void parseVarDeclStmnt(Stmnt *vdsp, BufferedLexer *blp, Arena *ar) {
  // zero-initialize vdsp
  ZERO(vdsp);
  vdsp->kind = SK_VarDecl;
  // these variables will be reused
  Token t;

  // Skip let declaration
  advanceToken(blp, &t);
  // The start of the variable declaration
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_Let, HANDLE_NO_LET);

  // Get Binding
  vdsp->varDecl.binding = allocArena(ar, sizeof(Binding));
  parseBinding(vdsp->varDecl.binding, blp, ar);

  // Expect Equal Sign
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_Assign, HANDLE_NO_ASSIGN);

  // Get Value;
  vdsp->varDecl.value = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(vdsp->varDecl.value, blp, ar);
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
  resyncStmnt(blp, ar);
}

static void parseFnDeclStmnt(Stmnt *fdsp, BufferedLexer *blp, Arena *ar) {
  // zero-initialize fdsp
  ZERO(fdsp);

  fdsp->kind = SK_FnDecl;

  // these variables will be reused
  Token t;
  // Skip fn declaration
  advanceToken(blp, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TK_Function, HANDLE_NO_FN);

  // get name
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_Identifier, HANDLE_NO_IDENTIFIER);
  fdsp->fnDecl.name = INTERN(t.identifier, ar);

  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_ParenLeft, HANDLE_NO_LEFTPAREN);

  Vector parameterDeclarations;
  createVector(&parameterDeclarations);

  // Parse the parameters (Comma Seperated list of bindings)
  while (true) {
    // Check for end paren
    advanceToken(blp, &t);
    if (t.kind == TK_ParenRight) {
      break;
    }
    // If it wasn't an end paren, we push it back
    setNextToken(blp, &t);

    // Parse and push the parameter
    parseBinding(VEC_PUSH(&parameterDeclarations, Binding), blp, ar);

    // Accept comma, if any
    // If there's no comma then it MUST be followed by an end paren
    // This also allows trailing commas
    advanceToken(blp, &t);
    if (t.kind != TK_Comma) {
      // If the next value isn't an end paren, then we throw an error
      EXPECT_TYPE(t, TK_ParenRight, HANDLE_NO_RIGHTPAREN);
      break;
    }
  }

  // Copy arguments in
  fdsp->fnDecl.params_length = VEC_LEN(&parameterDeclarations, Binding);
  fdsp->fnDecl.params = manageMemArena(ar, releaseVector(&parameterDeclarations));

  // Colon return type delimiter
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_Colon, HANDLE_NO_COLON);

  // Return type
  fdsp->fnDecl.type = allocArena(ar, sizeof(TypeExpr));
  parseTypeExpr(fdsp->fnDecl.type, blp, ar);

  // Equal sign expression delimiter
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_Assign, HANDLE_NO_ASSIGN);

  fdsp->fnDecl.body = allocArena(ar, sizeof(ValueExpr));
  parseValueExpr(fdsp->fnDecl.body, blp, ar);
  fdsp->span = SPAN(start, fdsp->fnDecl.body->span.end);
  fdsp->diagnostics_length = 0;
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
  resyncStmnt(blp, ar);
  return;

HANDLE_NO_LEFTPAREN:
  fdsp->diagnostics_length = 1;
  fdsp->diagnostics = allocArena(ar, sizeof(Diagnostic));
  fdsp->diagnostics[0] = DIAGNOSTIC(DK_FnDeclStmntExpectedParen, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->fnDecl.params_length = 0;
  fdsp->fnDecl.params = NULL;
  resyncStmnt(blp, ar);
  return;

HANDLE_NO_RIGHTPAREN:
  fdsp->diagnostics_length = 1;
  fdsp->diagnostics = allocArena(ar, sizeof(Diagnostic));
  fdsp->diagnostics[0] = DIAGNOSTIC(DK_FnDeclStmntExpectedParen, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->fnDecl.params_length = VEC_LEN(&parameterDeclarations, Binding);
  fdsp->fnDecl.params = manageMemArena(ar, releaseVector(&parameterDeclarations));
  resyncStmnt(blp, ar);
  return;

HANDLE_NO_COLON:
  fdsp->diagnostics_length = 1;
  fdsp->diagnostics = allocArena(ar, sizeof(Diagnostic));
  fdsp->diagnostics[0] = DIAGNOSTIC(DK_FnDeclStmntExpectedColon, t.span);
  fdsp->span = SPAN(start, t.span.end);
  resyncStmnt(blp, ar);
  return;

HANDLE_NO_ASSIGN:
  fdsp->diagnostics_length = 1;
  fdsp->diagnostics = allocArena(ar, sizeof(Diagnostic));
  fdsp->diagnostics[0] = DIAGNOSTIC(DK_FnDeclStmntExpectedAssign, t.span);
  fdsp->span = SPAN(start, t.span.end);
  resyncStmnt(blp, ar);
  return;
}

static void parseTypeAliasStmnt(Stmnt *adsp, BufferedLexer *blp, Arena *ar) {
  ZERO(adsp);
  adsp->kind = SK_TypeAliasDecl;
  Token t;
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_TypeAlias, HANDLE_NO_ALIAS);
  LnCol start = t.span.start;

  adsp->typeAliasStmnt.type = allocArena(ar, sizeof(TypeExpr));
  parseTypeExpr(adsp->typeAliasStmnt.type, blp, ar);

  advanceToken(blp, &t);
  EXPECT_TYPE(t, TK_Identifier, HANDLE_NO_IDENTIFIER);
  adsp->typeAliasStmnt.name = INTERN(t.identifier, ar);
  adsp->span = SPAN(start, t.span.end);
  adsp->diagnostics_length = 0;
  return;

HANDLE_NO_IDENTIFIER:
  adsp->typeAliasStmnt.name = NULL;
  adsp->span = SPAN(start, t.span.end);
  adsp->diagnostics_length = 1;
  adsp->diagnostics = allocArena(ar, sizeof(Diagnostic));
  adsp->diagnostics[0] =
      DIAGNOSTIC(DK_AliasDeclStmntExpectedIdentifier, t.span);
  return;

HANDLE_NO_ALIAS:
  INTERNAL_ERROR("called alias declaration parser where there was no "
                 "alias declaration");
  PANIC();
}

static void parseStmnt(Stmnt *sp, BufferedLexer *blp, Arena *ar) {
  Token t;
  // peek next token
  advanceToken(blp, &t);
  setNextToken(blp, &t);

  switch (t.kind) {
  case TK_Function: {
    parseFnDeclStmnt(sp, blp, ar);
    return;
  }
  case TK_Let: {
    parseVarDeclStmnt(sp, blp, ar);
    return;
  }
  case TK_TypeAlias: {
    parseTypeAliasStmnt(sp, blp, ar);
    return;
  }
  default: {
    // Value Expr Statement
    sp->kind = SK_Expr;
    sp->exprStmnt.value = allocArena(ar, sizeof(ValueExpr));
    parseValueExpr(sp->exprStmnt.value, blp, ar);
    sp->span = sp->exprStmnt.value->span;
    sp->diagnostics_length = 0;
    return;
  }
  }
}

void parseTranslationUnit(TranslationUnit *tu, BufferedLexer *blp, Arena *ar) {
  Token t;
  Vector statements;
  createVector(&statements);

  // List of diagnostics
  Vector diagnostics;
  createVector(&diagnostics);

  while (true) {
    // Check for EOF
    advanceToken(blp, &t);
    if (t.kind == TK_None && t.error == DK_EOF) {
      break;
    }
    // If it wasn't an EOF, we push it back
    setNextToken(blp, &t);

    // Parse and push the statement
    parseStmnt(VEC_PUSH(&statements, Stmnt), blp, ar);

    // semicolon is required
    advanceToken(blp, &t);
    if (t.kind == TK_None && t.error == DK_EOF) {
      // We've hit the end of the file
      break;
    } else if (t.kind == TK_Semicolon) {
      // Do nothing
    } else {
      // give them a missing semicolon error, but keep parsing
      setNextToken(blp, &t);
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
