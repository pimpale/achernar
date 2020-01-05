#include <stdio.h>
#include <inttypes.h>

#include "vector.h"
#include "lex.h"
#include "error.h"


static void lexComment(Parseable* stream, Vector* tokens) {
  UNUSED(tokens);
  if (nextValue(stream) != '#') {
    LOG_ERROR_ARGS(ERR_LEVEL_ERROR,
        "malformed comment found at line " PRIu64 " and column " PRIu64 "\n",
        stream->lineNumber,
        stream->charNumber);
  }
  int32_t c;
  while ((c = nextValue(stream)) != EOF) {
    if (c == '\n') {
      break;
    }
  }
}

static void lexStringLiteral(Parseable* stream, Vector* tokens) {
  if (nextValue(stream) != '\"') {
    LOG_ERROR_ARGS(ERR_LEVEL_ERROR,
        "malformed string found at line " PRIu64 " and column " PRIu64 "\n",
        stream->lineNumber,
        stream->charNumber);
  }
  Vector* stringVector = newVector();

  int32_t c;
  while ((c = nextValue(stream)) != EOF) {
    if (c == '\"') {
      break;
    } else {
      *VEC_PUSH(string, char) = (char)c;
    }
  }
  // Copy data to token
  char* string = malloc(lengthVector(stringVector));
  memcpy(string, getVector(string,0));
  deleteVector(stringVector);

  Token *stringToken = newToken(SymStringLiteral, string);
  *VEC_PUSH(tokens, Token*) = stringToken;
}

static void lexFloat(Parseable* stream, Vector* tokens) {

Token* newToken(SymType type, void* payload) {
  Token *t = malloc(sizeof(Token));
  t->type = type;
  t->payload = payload;
  return t;
}

void freeToken(Token* token) {
  if(token->type == SymStringLiteral
      || token->type == SymFloatLiteral
      || token->type == SymIntLiteral) {
    free(token->payload);
  }
  free(token);
}

void lex(Parseable* stream, Vector* tokens) {
  int32_t c;
  while ((c = nextValue(stream)) != EOF) {
    // Unget c for the next function
    backValue(c, stream);
    if (isblank(c) || c == '\n') {
      // Move on till next token
      nextValue(stream);
    } else if (c == '#') {
      lexComment(stream, tokens);
    } else if (c == '\"') {
      lexStringLiteral(stream, tokens);
    } else if (isdigit(c)) {
      parseNumber(stream, tokens);
    } else {
      parseIdentifier(stream, tokens);
    }
  }
}
