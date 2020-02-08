#include "parser.h"

#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "token.h"
#include "vector.h"

// Creates a new parser based on parsing a file on demand
Parser *createParserLexer(Parser *parser, Lexer *lexer) {
  parser->backing = ParserBackingLexer;
  parser->lex.lexer = lexer;
  parser->lex.loc = 0;
  createVector(&parser->lex.tokVec);
  return parser;
}

// Creates a new parser based on parsing an array.
Parser *createParserMemory(Parser *parser, Token *tokens,
                           size_t tokenCount) {
  parser->backing = ParserBackingMemory;
  parser->mem.ptr = malloc(tokenCount * sizeof(Token));
  memcpy(parser->mem.ptr, tokens, tokenCount * sizeof(Token));
  parser->mem.len = tokenCount;
  parser->mem.loc = 0;
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
        if(diag.type == ErrorOk) {
          // Increment the location
          p->lex.loc++;
          // Append it to the vector cache
          *VEC_PUSH(&p->lex.tokVec, Token) = *token;
        }
        return diag;
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

// Shunting yard algorithm
static Diagnostic parseExprProxy(ExprProxy *expr, Parser *p) {
  Vector opStack;
  Vector output;
  createVector(&opStack);
  createVector(&output);

  while(true) {
    puts("parsing expr");
  }
}

static Diagnostic parseVarDeclStmnt(VarDeclStmnt *vdsp, Parser *p) {
  // these variables will be reused
  Token t;
  Diagnostic d;

  // Skip let declaration
  advanceParser(p, &t);
  EXPECT_NO_ERROR(d);
  EXPECT_TYPE(&t, TokenLet);

  // the location of the whole function
  uint64_t ln = d.ln;
  uint64_t col = d.col;

  // This might be a mutable or type
  d = advanceParser(p, &t);
  EXPECT_NO_ERROR(d);
  if(t.type == TokenMut) {
    vdsp->isMutable = true;
  }
  // Now grab the actual type
  d = advanceParser(p, &t);
  EXPECT_NO_ERROR(d);

  // Now check for any pointer layers
  // Loop will exit with t holding the first nonref token
  vdsp->pointerCount = 0;
  while(true) {
    d = advanceParser(p, &t);
    EXPECT_NO_ERROR(d);
    if(t.type == TokenRef) {
      vdsp->pointerCount++;
    } else {
      break;
    }
  }

  // Copy identifier
  d = advanceParser(p, &t);
  EXPECT_NO_ERROR(d);
  EXPECT_TYPE(&t, TokenIdentifier);
  vdsp->name = strdup(t.identifier);

  // Expect Assign
  d = advanceParser(p, &t);
  EXPECT_NO_ERROR(d);
  EXPECT_TYPE(&t, TokenAssign);

  // Expect Expression (no implicit undefined)
  vdsp->hasValue = true;
  parseExprProxy(&vdsp->value, p);

  // Expect Semicolon
  d = advanceParser(p, &t);
  EXPECT_NO_ERROR(d);
  EXPECT_TYPE(&t, TokenSemicolon);

  return (Diagnostic){.type = ErrorOk, .ln = ln, .col = col};
}

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

  // Parse the varDeclStatements

  // Note that when parsing these declarations, the let is omitted, and we don't look for a value
  Vector parameterDeclarations;
  createVector(&parameterDeclarations);
  while(true) {
    puts("looking for args");
    VarDeclStmnt vds;
    d = advanceParser(p, &t);
    EXPECT_NO_ERROR(d);
    // This might be a mutable or type or closing paren
    if(t.type == TokenMut) {
      vds.isMutable = true;
      // Now grab the actual type
      d = advanceParser(p, &t);
      EXPECT_NO_ERROR(d);

    } else if(t.type == TokenParenRight) {
      // Bailing once we hit the other end
      break;
    }

    // Copy type
    EXPECT_TYPE(&t, TokenIdentifier);
    vds.type = strdup(t.identifier);

    // Now check for any pointer layers
    // Loop will exit with t holding the first nonref token
    vds.pointerCount = 0;
    while(true) {
      d = advanceParser(p, &t);
      EXPECT_NO_ERROR(d);
      if(t.type == TokenRef) {
        vds.pointerCount++;
      } else {
        break;
      }
    }

    // Copy identifier
    d = advanceParser(p, &t);
    EXPECT_NO_ERROR(d);
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
  EXPECT_NO_ERROR(d);
  EXPECT_TYPE(&t, TokenColon);

  // Return type
  d = advanceParser(p, &t);
  EXPECT_NO_ERROR(d);
  EXPECT_TYPE(&t, TokenIdentifier);
  fdsp->type = strdup(t.identifier);



  d = parseExprProxy(&fdsp->body, p);
  EXPECT_NO_ERROR(d);

  return (Diagnostic){.type = ErrorOk, .ln = ln, .col = col};
}

static Diagnostic parseStmntProxy(StmntProxy *s, Parser *p) {
  // these variables will be reused
  Token t;
  Diagnostic d;

  // get thing
  d = peekParser(p, &t);
  EXPECT_NO_ERROR(d);

  switch (t.type) {
  case TokenFunction: {
    // Initialize statement
    s->type = StmntFuncDecl;
    s->value = malloc(sizeof(FuncDeclStmnt));
    d = parseFuncDeclStmnt((FuncDeclStmnt*)s->value, p);
    EXPECT_NO_ERROR(d);
    return (Diagnostic){.type = ErrorOk, .ln = d.ln, .col = d.col};
  }
  case TokenLet: {
    // Initialize statement
    s->type = StmntVarDecl;
    s->value = malloc(sizeof(StmntVarDecl));
    d = parseVarDeclStmnt((VarDeclStmnt*)s->value, p);
    EXPECT_NO_ERROR(d);
    return (Diagnostic){.type = ErrorOk, .ln = d.ln, .col = d.col};
  }
  default: {
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
    if(d.type == ErrorEOF) {
      break;
    } else if(d.type != ErrorOk) {
      // TODO logDiagnostic(p->dl, d);
    }
    *VEC_PUSH(&data, StmntProxy) = s;
  }

  tu->statements_length = VEC_LEN(&data, StmntProxy);
  tu->statements = releaseVector(&data);
  return (Diagnostic){.type=ErrorOk, .ln=0,.col=0};
}
