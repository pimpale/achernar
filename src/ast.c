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
  Diagnostic d;

  // Skip let declaration
  advanceToken(blp, &t);
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
    vdsp->hasValue = false;
    return;
  }

  // Otherwise, we expect an assign
  EXPECT_TYPE(&t, TokenAssign, HANDLE_NO_ASSIGN);

  // Expect Expression (no implicit undefined)
  vdsp->hasValue = true;
  parseExprProxy(&vdsp->value, blp);

  return;

  // Error handling
  // Set error, and give back the error causing thing

  HANDLE_NO_LET:
  INTERNAL_ERROR("called variable declaration parser where there was no variable declaration");
  PANIC();
  return;

  HANDLE_NO_TYPE:
  vdsp->error = ErrorVarDeclStmntExpectedTypeNameOrModifer;
  setNextToken(blp, &t);
  return;

  HANDLE_NO_IDENTIFIER:
  vdsp->error = ErrorVarDeclStmntExpectedTypeNameOrModifer;
  setNextToken(blp, &t);
  return;

  HANDLE_NO_ASSIGN:
  vdsp->error = ErrorVarDeclStmntExpectedAssign;
  setNextToken(blp, &t);
  return;
}

static Diagnostic parseFuncDeclStmnt(FuncDeclStmnt *fdsp, BufferedLexer *blp) {
  // zero-initialize fdsp
  memset(fdsp, 0, sizeof(*fdsp));

  // these variables will be reused
  Token t;
  Diagnostic d;

  // Skip fn declaration
  advanceToken(blp, &t);
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
  Vector parameterDeclarations;
  createVector(&parameterDeclarations);
  while (true) {
    VarDeclStmnt vds;
    advanceToken(blp, &t);
    // This might be a mutable or type or closing paren
    if (t.type == TokenMut) {
      vds.isMutable = true;
      // Now grab the actual type
      advanceToken(blp, &t);

    } else if (t.type == TokenParenRight) {
      // Bailing once we hit the other end
      break;
    } else {
      EXPECT_TYPE(t, TokenIdentifier, HANDLE_PARAM_NO_TYPE);
    }

    // Copy type
    vds.type = strdup(t.identifier);

    // Now check for any pointer layers
    // Loop will exit with t holding the first nonref token
    vds.pointerCount = 0;
    while (true) {
      advanceToken(blp, &t);
      if (t.type == TokenRef) {
        vds.pointerCount++;
      } else {
        break;
      }
    }

    // Copy identifier
    advanceToken(blp, &t);
    EXPECT_TYPE(&t, TokenIdentifier);
    vds.name = strdup(t.identifier);

    // No errors
    vds.errorLength = 0;
    vds.errors = NULL;

    // Insert into array
    *VEC_PUSH(&parameterDeclarations, VarDeclStmnt) = vds;
  }

  // Copy arguments in
  fdsp->arguments_length = VEC_LEN(&parameterDeclarations, VarDeclStmnt);
  fdsp->arguments = releaseVector(&parameterDeclarations);

  // Colon return type delimiter
  d = advanceParser(p, &t);
  EXPECT_TYPE(&t, TokenColon);

  // Return type
  d = advanceParser(p, &t);
  EXPECT_TYPE(&t, TokenIdentifier);
  fdsp->type = strdup(t.identifier);

  d = parseExprProxy(&fdsp->body, p);

  return (Diagnostic){.type = ErrorOk, .ln = ln, .col = col};
}

static Diagnostic parseBreakExpr(BreakExpr *bep, Parser *p) {
  Token t;
  Diagnostic d;
  d = advanceParser(p, &t);
  EXPECT_TYPE(&t, TokenBreak);
  bep->errorLength = 0;
  return (Diagnostic){.type = ErrorOk, .ln = d.ln, .col = d.col};
}

static Diagnostic parseContinueExpr(ContinueExpr *cep, Parser *p) {
  Token t;
  Diagnostic d;
  d = advanceParser(p, &t);
  EXPECT_TYPE(&t, TokenContinue);
  cep->errorLength = 0;
  return (Diagnostic){.type = ErrorOk, .ln = d.ln, .col = d.col};
}

static Diagnostic parseWhileExpr(WhileExpr *wep, Parser *p) {
  Token t;
  Diagnostic d;
  d = advanceParser(p, &t);
  EXPECT_TYPE(&t, TokenWhile);
  d = parseExprProxy(&wep->condition, p);
  d = parseExprProxy(&wep->body, p);

  // TODO errors
  wep->errorLength = 0;
  return d;
}

static Diagnostic parseForExpr(ForExpr *fep, Parser *p) {
  Token t;
  Diagnostic d;
  d = advanceParser(p, &t);
  EXPECT_TYPE(&t, TokenFor);
  d = parseStmntProxy(&fep->init, p);
  d = parseExprProxy(&fep->test, p);
  d = parseStmntProxy(&fep->update, p);
  d = parseExprProxy(&fep->body, p);
  fep->errorLength = 0;
  return d;
}

static Diagnostic parseMatchCaseExpr(MatchCaseExpr *mcep, Parser *p) {
  Token t;
  Diagnostic d;
  // Get pattern
  d = parseExprProxy(&mcep->pattern, p);
  // Expect colon
  d = advanceParser(p, &t);
  EXPECT_TYPE(&t, TokenColon);
  // Get Value
  d = parseExprProxy(&mcep->pattern, p);
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
