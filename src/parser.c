#include "parser.h"

#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "token.h"
#include "vector.h"

Parser *createParserLexer(Parser *parser, Lexer* lexer) {
  parser->backing = ParserBackingLexer;
  parser->lexer = lexer;
  parser->dl = lexer->dl;
  return parser;
}

Parser *createParserMemory(Parser *parser, DiagnosticLogger* dl, Token *tokens, size_t tokenCount) {
  parser->backing = ParserBackingMemory;
  parser->memory.ptr = malloc(tokenCount*sizeof(Token));
  memcpy(parser->memory.ptr, tokens, tokenCount * sizeof(Token));
  parser->memory.len = tokenCount;
  parser->memory.loc = 0;
  parser->dl = dl;
  return parser;
}

Parser *destroyParser(Parser* parser) {
  switch(parser->backing) {
    case ParserBackingMemory: {
      free(parser->memory.ptr);
      break;
    }
    case ParserBackingLexer: {
      break;
    }
  }
  return parser;
}

static ResultTokenPtr advanceParser(Parser *p) {
  if (p->currentToken < p->tokenCount) {
    // Increment and return
    // clang-format off
    return (ResultTokenPtr){
      .val = &p->tokens[p->currentToken++],
      .err = ErrorOk
    };
    // clang-format on
  } else {
    // Issue Eof Error
    return (ResultTokenPtr){.err = ErrorEOF};
  }
}

// Gets current token
static ResultTokenPtr peekParser(Parser *p) {
  if (p->currentToken < p->tokenCount) {
    // Increment and return
    // clang-format off
    return (ResultTokenPtr){
      .val = &p->tokens[p->currentToken],
      .err = ErrorOk
    };
    // clang-format on
  } else {
    // Issue Eof Error
    return (ResultTokenPtr){.err = ErrorEOF};
  }
}

static ResultFuncDeclStmnt parseFuncDeclStmnt(Parser *p) {
  // TODO
  return (ResultFuncDeclStmnt){.err = ErrorOk};
}

static ResultStmnt parseStmnt(Parser *p) {
  ResultTokenPtr ret = peekParser(p);
  if (ret.err != ErrorOk) {
    return (ResultStmnt){.err = ret.err};
  }

  Stmnt s;
  switch (ret.val->type) {
  case TokenFunction: {
    ResultFuncDeclStmnt ret = parseFuncDeclStmnt(p);
    if (ret.err != ErrorOk) {
      // TODO probably
      return (ResultStmnt){.err = ret.err};
    }

    s.type = StmntFuncDecl;
    s.value = malloc(sizeof(FuncDeclStmnt));
    memcpy(s.value, &ret.val, sizeof(ret.val));
    // clang-format off
    return (ResultStmnt) {
      .val = s,
      .err = ErrorOk
    };
    // clang-format on
  }
  default: {
    logError(ErrLevelError,
             "Unrecognized token, TODO add error handling system");
    return (ResultStmnt){.err = ErrUnknown};
  }
  }
}

ResultTranslationUnit parseTranslationUnit(Parser *p) {
  Vector data;
  createVector(&data);
  while (true) {
    ResultStmnt ret = parseStmnt(p);
    if (ret.err == ErrorEOF) {
      logError(ErrLevelError, "finished reading file!");
      break;
    } else if (ret.err != ErrorOk) {
      logError(ErrLevelError, "TODO add error handling system: %s",
               strErrVal(ret.err));
      break;
    }
    // Only reachable if it is equal to err ok
    *VEC_PUSH(&data, Stmnt) = ret.val;
  }

  size_t length = VEC_LEN(&data, Stmnt);
  Stmnt *statements = releaseVector(&data);

  // clang-format off
  return (ResultTranslationUnit) {
    .val = (TranslationUnit) {
      .statements = statements,
      .length = length
    },
    .err = ErrorOk
  };
  // clang-format on
}
