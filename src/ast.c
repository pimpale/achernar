#include "ast.h"

#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "lexer.h"
#include "token.h"
#include "vector.h"

#define EXPECT_TYPE(token, tokenType, onErrLabel)                           \
  do {                                                                         \
    if ((token).type != (tokenType)) {                                     \
      goto onErrLabel;                                                         \
    }                                                                          \
  } while (false)

// Note that all errors resynch at the statement level
static void parseStmntProxy(StmntProxy *expr, BufferedLexer *blp);
static void parseExprProxy(ExprProxy *expr, BufferedLexer *blp);

static void parseVarDeclStmnt(VarDeclStmnt *vdsp, BufferedLexer *blp) {
  // zero-initialize vdsp
  memset(vdsp, 0, sizeof(*vdsp));

  // these variables will be reused
  Token t;

  // Skip let declaration
  advanceToken(blp, &t);
  // The start of the variable declaration
  LnCol start = t.span.start;
  EXPECT_TYPE(&t, TokenLet, HANDLE_NO_LET);

  // This might be a mutable or type
  advanceToken(blp, &t);
  if (t.type == TokenMut) {
    vdsp->isMutable = true;
    // If it was a mutable, Now grab the actual type
    advanceToken(blp, &t);
  }

  EXPECT_TYPE(&t, TokenIdentifier, HANDLE_NO_TYPE);
  vdsp->type = strdup(t.identifier);

  // Now check for any pointer layers
  // Loop will exit with t holding the first nonref token

  vdsp->pointerCount = 0;
  while (true) {
    advanceToken(blp, &t);
    if (t.type == TokenRef) {
      vdsp->pointerCount++;
    } else {
      break;
    }
  }

  // When the loop breaks out, t should be an identifier
  EXPECT_TYPE(&t, Identifier, HANDLE_NO_IDENTIFIER);
  vdsp->name = strdup(t.identifier);

  // Expect Assign or semicolon
  advanceToken(blp, &t);

  // If the thing is a semicolon, we end here
  if(t.type == TokenSemicolon) {
    vdsp->span = SPAN(start, t.span.end);
    vdsp->hasValue = false;
    return;
  }

  // Otherwise, we expect an assign
  EXPECT_TYPE(&t, TokenAssign, HANDLE_NO_ASSIGN);

  // Expect Expression (no implicit undefined)
  vdsp->hasValue = true;
  parseExprProxy(&vdsp->value, blp);
  vdsp->span = SPAN(start, vdsp->value.span.end);
  return;

  // Error handling
  // Set error, and give back the error causing thing
  // Generally, we use the t token to find the last parsed token

  HANDLE_NO_LET:
  INTERNAL_ERROR("called variable declaration parser where there was no variable declaration");
  PANIC();
  return;

  HANDLE_NO_TYPE:
  vdsp->span = SPAN(start, t.span.end);
  vdsp->error = DIAGNOSTIC(ErrorVarDeclStmntExpectedTypeNameOrModifer, t.span);
  setNextToken(blp, &t);
  return;

  HANDLE_NO_IDENTIFIER:
  vdsp->span = SPAN(start, t.span.end);
  vdsp->error = DIAGNOSTIC(ErrorVarDeclStmntExpectedIdentifier, t.span);
  setNextToken(blp, &t);
  return;

  HANDLE_NO_ASSIGN:
  vdsp->span = SPAN(start, t.span.end);
  vdsp->error = DIAGNOSTIC(ErrorVarDeclStmntExpectedAssign, t.span);
  setNextToken(blp, &t);
  return;
}

static void parseParamVarDeclStmnt(VarDeclStmnt *vdsp, BufferedLexer *blp) {
  // these variables will be reused
  Token t;

  memset(vdsp, 0, sizeof(*vdsp));
  advanceToken(blp, &t);
  LnCol paramStart = t.span.start;

  // This might be a mutable or type or closing paren
  if (t.type == TokenMut) {
    vdsp->isMutable = true;
    // Now grab the actual type
    advanceToken(blp, &t);
  }

  EXPECT_TYPE(t, TokenIdentifier, HANDLE_NO_TYPE);

  // Copy type
  vdsp->type = strdup(t.identifier);

  // Now check for any pointer layers
  // Loop will exit with t holding the first nonref token
  vdsp->pointerCount = 0;
  while (true) {
    advanceToken(blp, &t);
    if (t.type == TokenRef) {
      vdsp->pointerCount++;
    } else {
      break;
    }
  }

  // Copy identifier
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TokenIdentifier, HANDLE_NO_IDENTIFIER);
  vdsp->name = strdup(t.identifier);
  // Now set span to the space
  vdsp->span = SPAN(start, t.span.end);
  vdsp->error = DIAGNOSTIC(ErrorOk, vdsp->span);
  return;

  HANDLE_NO_TYPE:
  vdsp->span = SPAN(start, t.span.end);
  vdsp->error = DIAGNOSTIC(ErrorFuncDeclStmntParamExpectedTypeOrModifier, t.span);
  setNextToken(blp, &t);
  return;

  HANDLE_NO_IDENTIFIER:
  vdsp->span = SPAN(start, t.span.end);
  vdsp->error = DIAGNOSTIC(ErrorFuncDeclStmntParamExpectedIdentifier, t.span);
  setNextToken(blp, &t);
  return;
}

static void parseFuncDeclStmnt(FuncDeclStmnt *fdsp, BufferedLexer *blp) {
  // zero-initialize fdsp
  memset(fdsp, 0, sizeof(*fdsp));

  // these variables will be reused
  Token t;
  Diagnostic d;
  Vector parameterDeclarations;
  createVector(&parameterDeclarations);

  // Skip fn declaration
  advanceToken(blp, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, TokenFunction, HANDLE_NO_FN);

  // get name
  advanceToken(blp, &t);
  EXPECT_TYPE(&t, TokenIdentifier, HANDLE_NO_IDENTIFIER);
  fdsp->name = strdup(t.identifier);

  advanceToken(blp, &t);
  EXPECT_TYPE(&t, TokenParenLeft, HANDLE_NO_OPENING_PAREN);

  // Parse the varDeclStatements

  // Note that when parsing these declarations, the let is omitted, and we don't
  // look for a value
  while (true) {
    // Check for end paren
    advanceToken(blp, &t);
    if(t.type == TokenParenRight) {
      break;
    }

    setNextToken(blp, &t);

    // Parse and push the parameter
    parseParamVarDeclStmnt(VEC_PUSH(&parameterDeclarations, VarDeclStmnt), blp);

    // Accept comma, if any
    // If there's no comma then we must bail
    // This also allows trailing commas
    advanceToken(blp, &t);
    if(t.type != TokenComma) {
      setNextToken(blp, &t);
      EXPECT_TYPE(t, TokenParenRight, HANDLE_NO_COMMA);
      break;
    }
  }

  // Copy arguments in
  fdsp->arguments_length = VEC_LEN(&parameterDeclarations, VarDeclStmnt);
  fdsp->arguments = releaseVector(&parameterDeclarations);

  // Colon return type delimiter
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TokenColon, HANDLE_NO_TYPE_SPECIFIER);

  // Return type
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TokenIdentifier, HANDLE_NO_TYPE_SPECIFIER);
  fdsp->type = strdup(t.identifier);
  fdsp->span = SPAN(start, t.span.end);
  parseExprProxy(&fdsp->body, blp);
  fdsp->error = DIAGNOSTIC(ErrorOk, fdsp->span);
  return;

  HANDLE_NO_FN:
  INTERNAL_ERROR("called function declaration parser where there was no function declaration");
  PANIC();
  return;

  // Error handlers

  HANDLE_NO_IDENTIFIER:
  fdsp->error= DIAGNOSTIC(ErrorFuncDeclStmntExpectedTypeIdentifier, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->arguments_length = VEC_LEN(&parameterDeclarations, VarDeclStmnt);
  fdsp->arguments = releaseVector(&parameterDeclarations);
  return;

  HANDLE_NO_OPENING_PAREN:
  fdsp->error= DIAGNOSTIC(ErrorFuncDeclStmntExpectedParen, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->arguments_length = VEC_LEN(&parameterDeclarations, VarDeclStmnt);
  fdsp->arguments = releaseVector(&parameterDeclarations);
  return;

  HANDLE_NO_COMMA:
  fdsp->error= DIAGNOSTIC(ErrorFuncDeclStmntExpectedParen, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->arguments_length = VEC_LEN(&parameterDeclarations, VarDeclStmnt);
  fdsp->arguments = releaseVector(&parameterDeclarations);
  return;

  HANDLE_NO_TYPE_SPECIFIER:
  fdsp->error= DIAGNOSTIC(ErrorFuncDeclStmntExpectedTypeIdentifier, t.span);
  fdsp->span = SPAN(start, t.span.end);
  return;
}

static void parseBreakExpr(BreakExpr *bep, BufferedLexer *blp) {
  Token t;
  advanceToken(blp, &t);
  bep->span = t.span;
  EXPECT_TYPE(t, TokenBreak, HANDLE_NO_BREAK);
  bep->error = DIAGNOSTIC(ErrorOk, t.span);
  return;

  HANDLE_NO_BREAK:
  INTERNAL_ERROR("called break parser where there was no break");
  PANIC();
}

static void parseContinueExpr(ContinueExpr *cep, BufferedLexer *blp) {
  Token t;
  advanceToken(blp, &t);
  cep->span = t.span;
  EXPECT_TYPE(t, TokenContinue, HANDLE_NO_CONTINUE);
  cep->error = DIAGNOSTIC(ErrorOk, t.span);
  return;

  HANDLE_NO_CONTINUE:
  INTERNAL_ERROR("called continue parser where there was no continue");
  PANIC();
}

static void parseWhileExpr(WhileExpr *wep, BufferedLexer *blp) {
  Token t;
  advanceToken(blp, &t);
  EXPECT_TYPE(t, TokenWhile, HANDLE_NO_WHILE);
  parseExprProxy(&wep->condition, blp);
  parseExprProxy(&wep->body, blp);
  wep->span = t.span;

  HANDLE_NO_WHILE:
  INTERNAL_ERROR("called continue parser where there was no continue");
  PANIC();
}

static void parseForExpr(ForExpr *fep, BufferedLexer *blp) {
  Token t;
  advanceToken(blp, &t);
  EXPECT_TYPE(&t, TokenFor, HANDLE_NO_FOR);

  parseStmntProxy(&fep->init, blp);
  parseExprProxy(&fep->test, blp);
  parseStmntProxy(&fep->update, blp);
  parseExprProxy(&fep->body, blp);
  // TODO
  return;
}

static void parseMatchCaseExpr(MatchCaseExpr *mcep, Parser *p) {
  Token t;
  // Get pattern
  parseExprProxy(&mcep->pattern, p);
  // Expect colon
  d = advanceParser(p, &t);
  EXPECT_TYPE(&t, TokenColon);
  // Get Value
  parseExprProxy(&mcep->pattern, p);
  mcep->errorLength = 0;
  return d;
}

static Diagnostic parseMatchExpr(MatchExpr *mep, Parser *p) {
  Token t;
  Diagnostic d;
  // Ensure match
  d = advanceParser(p, &t);
  EXPECT_TYPE(&t, TokenMatch);
  // Get expression to match against
  d = parseExprProxy(&mep->value, p);
  // now we must parse the block containing the cases

  // Expect beginning brace
  d = advanceParser(p, &t);
  EXPECT_TYPE(&t, TokenBraceLeft);

  // TODO how do i add commas?
  Vector cases;
  createVector(&cases);
  while (true) {
    d = peekParser(p, &t);
    if (t.type == TokenBraceRight) {
      break;
    }
    d = parseMatchCaseExpr(VEC_PUSH(&cases, MatchCaseExpr), p);
  }

  // Get interior cases
  mep->cases_length = VEC_LEN(&cases, MatchCaseExpr);
  mep->cases = releaseVector(&cases);

  // TODO errors
  mep->errors = 0;
  return d;
}

// Shunting yard algorithm
static Diagnostic parseExprProxy(ExprProxy *expr, Parser *p) {
  Token t;
  Diagnostic d;

  // get thing
  d = peekParser(p, &t);

  switch (t.type) {
  case TokenBreak: {
    expr->type = ExprBreak;
    expr->value = malloc(sizeof(ExprBreak));
    return parseBreakExpr((BreakExpr *)expr->value, p);
  }
  case TokenContinue: {
    expr->type = ExprContinue;
    expr->value = malloc(sizeof(ExprContinue));
    return parseContinueExpr((ContinueExpr *)expr->value, p);
  }
  case TokenWhile: {
    expr->type = ExprWhile;
    expr->value = malloc(sizeof(ExprWhile));
    return parseWhileExpr((WhileExpr *)expr->value, p);
  }
  case TokenFor: {
    expr->value = ExprFor;
    expr->value = malloc(sizeof(ExprFor));
    return parseForExpr((ForExpr *)expr->value, p);
  }
  case TokenMatch: {
    expr->value = ExprMatch;
    expr->value = malloc(sizeof(ExprMatch));
    return parseMatchExpr((MatchExpr *)expr->value, p);
  }
  }
}

static Diagnostic parseStmntProxy(StmntProxy *s, Parser *p) {
  // these variables will be reused
  Token t;
  Diagnostic d;

  // get thing
  d = peekParser(p, &t);

  switch (t.type) {
  case TokenFunction: {
    // Initialize statement
    s->type = StmntFuncDecl;
    s->value = malloc(sizeof(FuncDeclStmnt));
    return parseFuncDeclStmnt((FuncDeclStmnt *)s->value, p);
  }
  case TokenLet: {
    // Initialize statement
    s->type = StmntVarDecl;
    s->value = malloc(sizeof(StmntVarDecl));
    d = parseVarDeclStmnt((VarDeclStmnt *)s->value, p);
    return (Diagnostic){.type = ErrorOk, .ln = d.ln, .col = d.col};
  }
  default: {
    advanceParser(p, &t);
    // TODO logDiagnostic(p->dl, d);
    return d;
  }
  }
}

Diagnostic parseTranslationUnit(TranslationUnit *tu, Parser *p) {
  // reused
  Diagnostic d;

  Vector data;
  createVector(&data);

  while (true) {
    StmntProxy s;
    d = parseStmntProxy(&s, p);
    if (d.type == ErrorEOF) {
      break;
    } else if (d.type != ErrorOk) {
      // TODO logDiagnostic(p->dl, d);
    }
    *VEC_PUSH(&data, StmntProxy) = s;
  }

  tu->statements_length = VEC_LEN(&data, StmntProxy);
  tu->statements = releaseVector(&data);
  return (Diagnostic){.type = ErrorOk, .ln = 0, .col = 0};
}
