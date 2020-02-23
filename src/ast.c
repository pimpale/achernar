#include "ast.h"

#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "lexer.h"
#include "token.h"
#include "vector.h"

#define EXPECT_TYPE(token, tokenType, onErrLabel)                              \
  do {                                                                         \
    if ((token).type != (tokenType)) {                                         \
      goto onErrLabel;                                                         \
    }                                                                          \
  } while (false)

#define EXPECT_NO_ERROR(thingPtr, onErrLabel)                                  \
  do {                                                                         \
    if ((thingPtr)->diagnostic.type != E_Ok) {                                 \
      goto onErrLabel;                                                         \
    }                                                                          \
  } while (false)

// Call this method after token error detected to skip to the next valid state

// Jump till after semicolon, taking into account parentheses, brackets,
// attributes, and braces It will ignore subexprs, but can halt at the end
static void resyncStmnt(BufferedLexer *blp) {
  int64_t parenDepth = 0;
  int64_t braceDepth = 0;
  int64_t bracketDepth = 0;
  int64_t attrDepth = 0;

  while (true) {
    Token t;
    advanceToken(blp, &t);
    switch (t.type) {
    case T_ParenLeft: {
      parenDepth++;
      break;
    }
    case T_ParenRight: {
      parenDepth--;
      if (parenDepth < 0) {
        return;
      }
      break;
    }
    case T_BraceLeft: {
      braceDepth++;
      break;
    }
    case T_BraceRight: {
      braceDepth--;
      if (braceDepth < 0) {
        return;
      }
      break;
    }
    case T_BracketLeft: {
      bracketDepth++;
      break;
    }
    case T_BracketRight: {
      bracketDepth--;
      if (bracketDepth < 0) {
        return;
      }
      break;
    }
    case T_AttrLeft: {
      attrDepth++;
      break;
    }
    case T_AttrRight: {
      attrDepth--;
      if (attrDepth < 0) {
        return;
      }
      break;
    }
    case T_Semicolon: // Fallthrough
    case T_Comma: {
      if (braceDepth <= 0 && bracketDepth <= 0 && parenDepth <= 0 &&
          attrDepth <= 0) {
        // put the comma or semicolon back on the stack to parse
        setNextToken(blp, &t);
        return;
      }
      break;
    }
    case T_None: {
      if (t.error == E_EOF) {
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
static void resyncType(BufferedLexer *blp) {
  while (true) {
    Token t;
    advanceToken(blp, &t);
    switch (t.type) {
    case T_Assign:
    case T_Identifier: {
      setNextToken(blp, &t);
      return;
    }
    default: {
      break;
    }
    }
  }
}

// Note that all errors resynch at the statement level
static void parseStmnt(Stmnt *sp, BufferedLexer *blp);
static void parseValueExpr(ValueExpr *vep, BufferedLexer *blp);
static void parseTypeExpr(TypeExpr *tep, BufferedLexer *blp);
static void parsePattern(Pattern *pp, BufferedLexer *blp);

static void parseTypeExpr(TypeExpr *tep, BufferedLexer *blp) {
  // zero-initialize tep
  memset(tep, 0, sizeof(*tep));

  Token t;

  advanceToken(blp, &t);
  LnCol start = t.span.start;
  switch (t.type) {
  case T_Identifier: {
    tep->type.name = strdup(t.identifier);
    while (true) {
      advanceToken(blp, &t);
      if (t.type == T_Ref) {
        tep->type.ptrCount++;
      } else {
        setNextToken(blp, &t);
        break;
      }
    }
    tep->span = SPAN(start, t.span.end);
    tep->diagnostic = DIAGNOSTIC(E_Ok, tep->span);
    break;
  }
  case T_Typeof: {
    tep->typeofExpr.value = malloc(sizeof(ValueExpr));
    parseValueExpr(tep->typeofExpr.value, blp);

    tep->span = SPAN(start, tep->typeofExpr.value->span.end);
    tep->diagnostic = DIAGNOSTIC(E_Ok, tep->span);
    break;
  }
  default: {
      tep->span = t.span;
      tep->diagnostic = DIAGNOSTIC(E_TypeExprUnexpectedToken, t.span);
      // Resync
      resyncType(blp);
      break;
  }
  }
}

static void parseBinding(Binding *bp, BufferedLexer *blp) {
  // zero-initialize bp
  memset(bp, 0, sizeof(*bp));

  // these variables will be reused
  Token t;

  // Get type of variable
  bp->type = malloc(sizeof(TypeExpr));
  parseTypeExpr(bp->type, blp);

  // get identifier
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Identifier, HANDLE_NO_IDENTIFIER);
  bp->name = strdup(t.identifier);

  // Error handling
  // Set error, and give back the error causing thing
  // Generally, we use the t token to find the last parsed token

HANDLE_NO_IDENTIFIER:
  // If it's not an identifier token, we must resync
  bp->span = bp->type->span;
  bp->diagnostic = DIAGNOSTIC(E_BindingExpectedIdentifier, t.span);
  resyncStmnt(blp);
  return;
}

static void parseVarDeclStmnt(Stmnt *vdsp, BufferedLexer *blp) {
  // zero-initialize vdsp
  memset(vdsp, 0, sizeof(*vdsp));
  vdsp->kind = S_VarDecl;
  // these variables will be reused
  Token t;

  // Skip let declaration
  advanceToken(blp, &t);
  // The start of the variable declaration
  LnCol start = t.span.start;
  EXPECT_TYPE(t, T_Let, HANDLE_NO_LET);

  // Get Binding
  vdsp->varDecl.binding = malloc(sizeof(Binding));
  parseBinding(vdsp->varDecl.binding, blp);

  // Expect Equal Sign
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Assign, HANDLE_NO_ASSIGN);

  // Get Value;
  vdsp->varDecl.value = malloc(sizeof(ValueExpr));
  parseValueExpr(vdsp->varDecl.value, blp);

HANDLE_NO_LET:
  INTERNAL_ERROR("called variable declaration parser where there was no "
                 "variable declaration");
  PANIC();

HANDLE_NO_ASSIGN:
  vdsp->span = SPAN(start, t.span.end);
  vdsp->diagnostic = DIAGNOSTIC(E_VarDeclStmntExpectedAssign, t.span);
  resyncStmnt(blp);
}

static void parseFuncDeclStmnt(Stmnt *fdsp, BufferedLexer *blp) {
  // zero-initialize fdsp
  memset(fdsp, 0, sizeof(*fdsp));
  fdsp->kind = S_FuncDecl;

  // these variables will be reused
  Token t;
  Vector parameterDeclarations;
  createVector(&parameterDeclarations);

  // Skip fn declaration
  advanceToken(blp, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, T_Function, HANDLE_NO_FN);

  // get name
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Identifier, HANDLE_NO_IDENTIFIER);
  fdsp->funcDecl.name = strdup(t.identifier);

  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_ParenLeft, HANDLE_NO_OPENING_PAREN);

  // Parse the parameters (Comma Seperated list of bindings)
  while (true) {
    // Check for end paren
    advanceToken(blp, &t);
    if (t.type == T_ParenRight) {
      break;
    }
    // If it wasn't an end paren, we push it back
    setNextToken(blp, &t);

    // Parse and push the parameter
    parseBinding(VEC_PUSH(&parameterDeclarations, Binding), blp);

    // Accept comma, if any
    // If there's no comma then it MUST be followed by an end paren
    // This also allows trailing commas
    advanceToken(blp, &t);
    if (t.type != T_Comma) {
      // If the next value isn't an end paren, then we throw an error
      EXPECT_TYPE(t, T_ParenRight, HANDLE_NO_CLOSING_PAREN);
      break;
    }
  }

  // Copy arguments in
  fdsp->funcDecl.params = releaseVector(&parameterDeclarations);
  fdsp->funcDecl.params_length = VEC_LEN(&parameterDeclarations, Binding);

  // Colon return type delimiter
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Colon, HANDLE_NO_COLON);

  // Return type
  parseTypeExpr(fdsp->funcDecl.type, blp);

  // Equal sign expression delimiter
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Assign, HANDLE_NO_ASSIGN);

  fdsp->funcDecl.body = malloc(sizeof(ValueExpr));
  parseValueExpr(fdsp->funcDecl.body, blp);
  fdsp->span = SPAN(start, fdsp->funcDecl.body->span.end);
  fdsp->diagnostic = DIAGNOSTIC(E_Ok, fdsp->span);
  return;

  // Error handlers
HANDLE_NO_FN:
  INTERNAL_ERROR("called function declaration parser where there was no "
                 "function declaration");
  PANIC();


HANDLE_NO_IDENTIFIER:
  fdsp->diagnostic = DIAGNOSTIC(E_FuncDeclStmntExpectedIdentifier, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->funcDecl.params_length = VEC_LEN(&parameterDeclarations, Binding);
  fdsp->funcDecl.params = releaseVector(&parameterDeclarations);
  return;

HANDLE_NO_OPENING_PAREN:
  fdsp->diagnostic = DIAGNOSTIC(E_FuncDeclStmntExpectedParen, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->funcDecl.params_length = VEC_LEN(&parameterDeclarations, Binding);
  fdsp->funcDecl.params = releaseVector(&parameterDeclarations);
  return;

HANDLE_NO_CLOSING_PAREN:
  fdsp->diagnostic = DIAGNOSTIC(E_FuncDeclStmntExpectedParen, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->funcDecl.params_length = VEC_LEN(&parameterDeclarations, Binding);
  fdsp->funcDecl.params = releaseVector(&parameterDeclarations);
  return;

HANDLE_NO_COLON:
  fdsp->diagnostic = DIAGNOSTIC(E_FuncDeclStmntExpectedColon, t.span);
  fdsp->span = SPAN(start, t.span.end);
  return;

HANDLE_NO_ASSIGN:
  fdsp->diagnostic = DIAGNOSTIC(E_FuncDeclStmntExpectedAssign, t.span);
  fdsp->span = SPAN(start, t.span.end);
  return;
}

static void parseBreakExpr(ValueExpr *bep, BufferedLexer *blp) {
  bep->kind = VE_Break;
  Token t;
  advanceToken(blp, &t);
  bep->span = t.span;
  EXPECT_TYPE(t, T_Break, HANDLE_NO_BREAK);
  bep->diagnostic = DIAGNOSTIC(E_Ok, t.span);
  return;

HANDLE_NO_BREAK:
  INTERNAL_ERROR("called break parser where there was no break");
  PANIC();
}

static void parseContinueExpr(ValueExpr *cep, BufferedLexer *blp) {
  cep->kind = VE_Continue;
  Token t;
  advanceToken(blp, &t);
  cep->span = t.span;
  EXPECT_TYPE(t, T_Continue, HANDLE_NO_CONTINUE);
  cep->diagnostic = DIAGNOSTIC(E_Ok, t.span);
  return;

HANDLE_NO_CONTINUE:
  INTERNAL_ERROR("called continue parser where there was no continue");
  PANIC();
}

static void parseWhileExpr(ValueExpr *wep, BufferedLexer *blp) {
  wep->kind = VE_While;
  Token t;
  advanceToken(blp, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, T_While, HANDLE_NO_WHILE);
  parseValueExpr(wep->whileExpr.condition, blp);
  parseValueExpr(wep->whileExpr.body, blp);
  wep->span = SPAN(start, wep->whileExpr.body->span.end);
  wep->diagnostic = DIAGNOSTIC(E_Ok, wep->span);
  return;

HANDLE_NO_WHILE:
  INTERNAL_ERROR("called continue parser where there was no continue");
  PANIC();
}

// Pattern : Expr,
static void parseMatchCaseExpr(MatchCaseExpr *mcep, BufferedLexer *blp) {
  memset(mcep, 0, sizeof(*mcep));
  Token t;
  // Get pattern
  parsePattern(mcep->pattern, blp);
  // Expect colon
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Colon, HANDLE_NO_COLON);
  // Get Value
  parseValueExpr(mcep->value, blp);
  mcep->span = SPAN(mcep->pattern->span.start, mcep->value->span.end);
  mcep->diagnostic = DIAGNOSTIC(E_Ok, mcep->span);
  return;

HANDLE_NO_COLON:
  mcep->span = mcep->pattern->span;
  mcep->diagnostic = DIAGNOSTIC(E_Ok, t.span);
  resyncStmnt(blp);
  return;
}

static void parseMatchExpr(ValueExpr *mep, BufferedLexer *blp) {
  Token t;
  // Ensure match
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Match, HANDLE_NO_MATCH);
  // Get expression to match against
  parseValueExpr(mep->matchExpr.value, blp);
  // now we must parse the block containing the cases

  // Expect beginning brace
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_BraceLeft, HANDLE_NO_LEFTBRACE);

  // TODO how do i add commas?
  Vector matchCases;
  createVector(&matchCases);
  while (true) {
    advanceToken(blp, &t);
    if (t.type == T_BraceRight) {
      break;
    }
    setNextToken(blp, &t);
    parseMatchCaseExpr(VEC_PUSH(&matchCases, MatchCaseExpr), blp);
  }

  // Get interior cases
  mep->matchExpr.cases_length = VEC_LEN(&matchCases, MatchCaseExpr);
  mep->matchExpr.cases = releaseVector(&matchCases);

  // TODO Diagnostics and Span
  return;

HANDLE_NO_MATCH:
  INTERNAL_ERROR("called match parser where there was no match");
  PANIC();

HANDLE_NO_LEFTBRACE:
  // TODO fricking errors
  return;
}

void parseTranslationUnit(TranslationUnit *tu, BufferedLexer *blp) {
  Vector data;
  createVector(&data);

  while (true) {
    Stmnt s;
    parseStmnt(&s, blp);
    *VEC_PUSH(&data, Stmnt) = s;

    if (s.diagnostic.type == E_EOF) {
      break;
    }
  }

  tu->statements_length = VEC_LEN(&data, Stmnt);
  tu->statements = releaseVector(&data);
  return;
}
