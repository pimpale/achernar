#include "parser.h"
#include "error.h"
#include "token.h"

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

static bool accept(Parser* p, TokenType type) {
  ResultTokenPtr rtp = peekParser(p);
  if(rtp.err != ErrOk) {
    return false;
  }
  return rtp.val->type == type;
}

static bool consume(Parser* p, TokenType type) {
  bool accepted = accept(p, type);
  advanceParser(p);
  return accepted;
}

ResultTranslationUnit parseTranslationUnit(Parser* parser) {
  switch
}
