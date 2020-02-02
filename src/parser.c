#include "parser.h"

#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "token.h"
#include "vector.h"

Parser *createParser(Parser *parser, Token *tokens, size_t tokenCount) {
  parser->tokens = tokens;
  parser->tokenCount = tokenCount;
  parser->currentToken = 0;
  return parser;
}

static ResultTokenPtr advanceParser(Parser *p) {
  if (p->currentToken < p->tokenCount) {
    // Increment and return
    // clang-format off
    return (ResultTokenPtr){
      .val = &p->tokens[p->currentToken++],
      .err = ErrOk
    };
    // clang-format on
  } else {
    // Issue Eof Error
    return (ResultTokenPtr){.err = ErrEof};
  }
}

// Gets current token
static ResultTokenPtr peekParser(Parser *p) {
  if (p->currentToken < p->tokenCount) {
    // Increment and return
    // clang-format off
    return (ResultTokenPtr){
      .val = &p->tokens[p->currentToken],
      .err = ErrOk
    };
    // clang-format on
  } else {
    // Issue Eof Error
    return (ResultTokenPtr){.err = ErrEof};
  }
}

static bool accept(Parser *p, TokenType type) {
  ResultTokenPtr rtp = peekParser(p);
  if (rtp.err != ErrOk) {
    return false;
  }
  return rtp.val->type == type;
}

static bool consume(Parser *p, TokenType type) {
  bool accepted = accept(p, type);
  advanceParser(p);
  return accepted;
}

static ResultFuncDeclStmnt parseFuncDeclStmnt(Parser *p) {
  // TODO
  return (ResultFuncDeclStmnt){.err = ErrUnknown};
}

static ResultStmnt parseStmnt(Parser *p) {
  ResultTokenPtr ret = peekParser(p);
  if (ret.err != ErrOk) {
    return (ResultStmnt){.err = ret.err};
  }

  Stmnt s;
  switch (ret.val->type) {
  case TokenFunction: {
    ResultFuncDeclStmnt ret = parseFuncDeclStmnt(p);
    if (ret.err != ErrOk) {
      // TODO probably
      return (ResultStmnt){.err = ret.err};
    }

    s.type = StmntFuncDecl;
    s.value = malloc(sizeof(FuncDeclStmnt));
    memcpy(s.value, &ret.val, sizeof(ret.val));
    // clang-format off
    return (ResultStmnt) {
      .val = s,
      .err = ErrOk
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
    if (ret.err == ErrEof) {
      logError(ErrLevelError, "finished reading file!");
      break;
    } else if (ret.err != ErrOk) {
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
    .err = ErrOk
  };
  // clang-format on
}
