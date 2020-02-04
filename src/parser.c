#include "parser.h"

#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "token.h"
#include "vector.h"

// Creates a new parser based on parsing a file on demand
Parser *createParserLexer(Parser *parser, DiagnosticLogger *dl, Lexer *lexer) {
  parser->backing = ParserBackingLexer;
  parser->lex.lexer = lexer;
  parser->lex.loc = 0;
  createVector(&parser->lex.tokVec);
  parser->dl = dl;
  return parser;
}

// Creates a new parser based on parsing an array.
Parser *createParserMemory(Parser *parser, DiagnosticLogger *dl, Token *tokens,
                           size_t tokenCount) {
  parser->backing = ParserBackingMemory;
  parser->mem.ptr = malloc(tokenCount * sizeof(Token));
  memcpy(parser->mem.ptr, tokens, tokenCount * sizeof(Token));
  parser->mem.len = tokenCount;
  parser->mem.loc = 0;
  parser->dl = dl;
  return parser;
}

// TODO also have to destroy the nested tokens
Parser *destroyParser(Parser *parser) {
  switch (parser->backing) {
  case ParserBackingMemory: {
    free(parser->mem.ptr);
    break;
  }
  case ParserBackingLexer: {
    destroyVector(&parser->lex.tokVec);
    break;
  }
  }
  return parser;
}

// Returns a copy of next token, unless end of file. Returns ErrorEOF if hits
// end of file Will print lexing errors
static Diagnostic advanceParser(Parser *p, Token *token) {
  switch (p->backing) {
  case ParserBackingLexer: {
    if (p->lex.loc < VEC_LEN(&p->lex.tokVec, Token)) {
      // Return cached value
      *token = *VEC_GET(&p->lex.tokVec, p->lex.loc++, Token);
      return (Diagnostic){.type = ErrorOk, .ln = token->ln, .col = token->col};
    } else {
      // Iterate till we get a non broken integer
      while (true) {
        Diagnostic diag = lexNextToken(p->lex.lexer, token);
        switch (diag.type) {
        case ErrorOk: {
          // Increment the location
          p->lex.loc++;
          // Append it to the vector cache
          *VEC_PUSH(&p->lex.tokVec, Token) = *token;
          return diag;
        }
        case ErrorEOF: {
          return diag;
        }
        default: {
          logDiagnostic(p->dl, diag);
        }
        }
      }
    }
  }
  case ParserBackingMemory: {
    if (p->mem.loc < p->mem.len) {
      // Increment and return
      *token = p->mem.ptr[p->mem.loc++];
      return (Diagnostic){.type = ErrorOk, .ln = token->ln, .col = token->col};
    } else {
      // Issue Eof Error
      // If we don't have a last token to issue it at, issue it at 0,0
      if (p->mem.len != 0) {
        Token lastToken;
        lastToken = p->mem.ptr[p->mem.len - 1];
        return (Diagnostic){
            .type = ErrorEOF, .ln = lastToken.ln, .col = lastToken.col};
      } else {
        return (Diagnostic){.type = ErrorEOF, .ln = 0, .col = 0};
      }
    }
  }
  }
}

// Gets current token
static Diagnostic peekParser(Parser *p, Token *token) {
  Diagnostic diag = advanceParser(p, token);
  if (diag.type == ErrorOk) {
    switch (p->backing) {
    case ParserBackingLexer: {
      p->lex.loc--;
      break;
    }
    case ParserBackingMemory: {
      p->mem.loc--;
      break;
    }
    }
  }
  return diag;
}

#define EXPECT_NO_ERROR(diag)                                                  \
  do {                                                                         \
    if ((diag).type != ErrorOk) {                                              \
      return (diag);                                                           \
    }                                                                          \
  } while (false)

#define EXPECT_TYPE(tokenPtr, tokenType)                                       \
  do {                                                                         \
    if ((tokenPtr)->type != (tokenType)) {                                     \
      return (Diagnostic){.type = ErrorUnexpectedToken,                        \
                          .ln = (tokenPtr)->ln,                                \
                          .col = (tokenPtr)->col};                             \
    }                                                                          \
  } while (false)

// TODO
static Diagnostic parseExpr(Expr *expr, Parser *p);

static Diagnostic parseFuncDeclStmnt(FuncDeclStmnt *fdsp, Parser *p) {
  // these variables will be reused
  Token t;
  Diagnostic d;

  // Skip fn declaration
  advanceParser(p, &t);
  EXPECT_NO_ERROR(d);
  EXPECT_TYPE(&t, TokenFunction);

  // the location of the whole function
  uint64_t ln = d.ln;
  uint64_t col = d.col;

  // get name
  d = advanceParser(p, &t);
  EXPECT_NO_ERROR(d);
  EXPECT_TYPE(&t, TokenIdentifier);
  fdsp->name = strdup(t.identifier);

  d = advanceParser(p, &t);
  EXPECT_NO_ERROR(d);
  EXPECT_TYPE(&t, TokenParenLeft);

  // TODO parse parameter expressions

  d = advanceParser(p, &t);
  EXPECT_NO_ERROR(d);
  EXPECT_TYPE(&t, TokenParenLeft);

  // TODO parse expressions

  return (Diagnostic){.type = ErrorOk, .ln = ln, .col = col};
}

static Diagnostic parseStmnt(Stmnt *s, Parser *p) {
  // these variables will be reused
  Token t;
  Diagnostic d;

  // get thing
  d = peekParser(p, &t);
  EXPECT_NO_ERROR(d);

  switch (t.type) {
  case TokenFunction: {
    FuncDeclStmnt fds;
    d = parseFuncDeclStmnt(&fds, p);
    EXPECT_NO_ERROR(d);

    // Initialize statement
    s->type = StmntFuncDecl;
    s->value = malloc(sizeof(FuncDeclStmnt));
    memcpy(s->value, &t, sizeof(FuncDeclStmnt));

    return (Diagnostic){.type = ErrorOk, .ln = d.ln, .col = d.col};
  }
  default: {
    logDiagnostic(p->dl, d);
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
    Stmnt s;
    d = parseStmnt(&s, p);
    if(d.type == ErrorEOF) {
      break;
    } else if(d.type != ErrorOk) {
      logDiagnostic(p->dl, d);
    }
    *VEC_PUSH(&data, Stmnt) = s;
  }

  size_t length = VEC_LEN(&data, Stmnt);
  Stmnt *statements = releaseVector(&data);

  return (Diagnostic){.type=ErrorOk, .ln=0,.col=0};
}
