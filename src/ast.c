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

// Call these methods after token error detected to skip to the next valid state

// Note that all errors resynch at the statement level
static void parseStmnt(Stmnt *sp, BufferedLexer *blp);
static void parseValueExpr(ValueExpr *vep, BufferedLexer *blp);
static void parseTypeExpr(TypeExpr *tep, BufferedLexer *blp);
static void parsePlaceExpr(PlaceExpr *pep, BufferedLexer *blp);
static void parsePattern(Pattern *pp, BufferedLexer *blp);

static void parseBinding(Binding *bp, BufferedLexer *blp) {
  // zero-initialize vdsp
  memset(bp, 0, sizeof(*bp));
  // these variables will be reused
  Token t;

  // Get type of variable
  parseTypeExpr(bp->type, blp);
  EXPECT_NO_ERROR(bp->type, HANDLE_TYPE_ERR);

  // Now we get the identifier
  EXPECT_TYPE(t, T_Identifier, HANDLE_NO_IDENTIFIER);
  bp->name = strdup(t.identifier);

  // Error handling
  // Set error, and give back the error causing thing
  // Generally, we use the t token to find the last parsed token

HANDLE_NO_IDENTIFIER:
  bp->span = SPAN(bp->type->span.start, t.span.end);
  bp->diagnostic = DIAGNOSTIC(E_VarDeclStmntExpectedIdentifier, t.span);
  bp->name = NULL;
  setNextT_(blp, &t);
  return;

HANDLE_TYPE_ERR:
  bp->span = t.span;
  bp->diagnostic = DIAGNOSTIC(E_VarDeclStmntExpectedType, t.span);
  bp->name = NULL;
  return;
}

static void parseVarDeclPlaceExpr(PlaceExpr *vdpep, BufferedLexer *blp) {
  // zero-initialize vdsp
  memset(vdpep, 0, sizeof(*vdpep));
  vdpep->kind = PE_VarDecl;
  // these variables will be reused
  Token t;

  // Skip let declaration
  advanceToken(blp, &t);
  // The start of the variable declaration
  LnCol start = t.span.start;
  EXPECT_TYPE(t, T_Let, HANDLE_NO_LET);

  // Get Binding
  parseBinding(vdpep->varDecl.binding, blp);
  EXPECT_NO_ERROR(vdpep->varDecl.binding, HANDLE_BINDING_ERR);

  // Error handling
  // Set error, and give back the error causing thing
  // Generally, we use the t token to find the last parsed token

HANDLE_NO_LET:
  INTERNAL_ERROR("called variable declaration parser where there was no "
                 "variable declaration");
  PANIC();

HANDLE_BINDING_ERR:
  vdpep->span = SPAN(start, t.span.end);
  vdpep->diagnostic = DIAGNOSTIC(E_VarDeclStmntExpectedType, t.span);
  return;
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

  // Parse the parameters

  // Note that when parsing these declarations, the let is omitted, and we don't
  // look for a value
  while (true) {
    // Check for end paren
    advanceToken(blp, &t);
    if (t.type == T_ParenRight) {
      break;
    }

    // If it wasn't an end paren, we push it back
    setNextT_(blp, &t);

    // Parse and push the parameter
    parseBinding(VEC_PUSH(&parameterDeclarations, Binding), blp);

    // Accept comma, if any
    // If there's no comma then we must bail
    // This also allows trailing commas
    advanceToken(blp, &t);
    if (t.type != T_Comma) {
      // If the next value isn't an end paren, then we throw an error
      EXPECT_TYPE(t, T_ParenRight, HANDLE_NO_COMMA);
      setNextT_(blp, &t);
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
  EXPECT_NO_ERROR(fdsp->funcDecl.type, HANDLE_TYPE_ERR);

  // Equal sign expression delimiter
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Assign, HANDLE_NO_ASSIGN);

  fdsp->span = SPAN(start, t.span.end);
  parseValueExpr(fdsp->funcDecl.body, blp);
  fdsp->diagnostic = DIAGNOSTIC(E_Ok, fdsp->span);
  return;

HANDLE_NO_FN:
  INTERNAL_ERROR("called function declaration parser where there was no "
                 "function declaration");
  PANIC();

  // Error handlers

HANDLE_NO_IDENTIFIER:
  fdsp->diagnostic =
      DIAGNOSTIC(E_FuncDeclStmntExpectedIdentifier, t.span);
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

HANDLE_NO_COMMA:
  fdsp->diagnostic = DIAGNOSTIC(E_FuncDeclStmntExpectedParen, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->funcDecl.params_length = VEC_LEN(&parameterDeclarations, Binding);
  fdsp->funcDecl.params = releaseVector(&parameterDeclarations);
  return;

HANDLE_NO_COLON:
  fdsp->diagnostic =
      DIAGNOSTIC(E_FuncDeclStmntExpectedColon, t.span);
  fdsp->span = SPAN(start, t.span.end);
  return;

HANDLE_NO_ASSIGN:
  fdsp->diagnostic =
      DIAGNOSTIC(E_FuncDeclStmntExpectedAssign, t.span);
  fdsp->span = SPAN(start, t.span.end);
  return;

HANDLE_TYPE_ERR:
  fdsp->diagnostic =
      DIAGNOSTIC(E_FuncDeclStmntExpectedType, t.span);
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

static void parseForExpr(ValueExpr *fep, BufferedLexer *blp) {
  fep->kind = VE_For;
  Token t;
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_For, HANDLE_NO_FOR);

  parseValueExpr(fep->forExpr.init, blp);
  parseValueExpr(fep->forExpr.condition, blp);
  parseValueExpr(fep->forExpr.update, blp);
  parseValueExpr(fep->forExpr.body, blp);
  // TODO probably want to implement generators... frick...
  return;

HANDLE_NO_FOR:
  INTERNAL_ERROR("called for parser where there was no for");
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
  // TODO we need to redo the error handling system
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

    if(s.diagnostic.type == E_EOF) {
      break;
    }
  }

  tu->statements_length = VEC_LEN(&data, Stmnt);
  tu->statements = releaseVector(&data);
  return;
}
