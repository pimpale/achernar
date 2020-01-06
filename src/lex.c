#include "lex.h"

#include <ctype.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "vector.h"

/* Token Functions */

Token* newToken(SymType type, void* payload) {
  Token* t = malloc(sizeof(Token));
  t->type = type;
  t->payload = payload;
  return t;
}

void freeToken(Token* token) {
  if (token->type == SymStringLiteral || token->type == SymFloatLiteral ||
      token->type == SymIntLiteral) {
    free(token->payload);
  }
  free(token);
}

static void lexComment(Parseable* stream, Vector* tokens) {
  UNUSED(tokens);
  if (nextValue(stream) != '#') {
    LOG_ERROR_ARGS(ERR_LEVEL_ERROR,
                   "malformed comment found at line " PRIu64
                   " and column " PRIu64 "\n",
                   stream->lineNumber, stream->charNumber);
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
                   "malformed string found at line " PRIu64
                   " and column " PRIu64 "\n",
                   stream->lineNumber, stream->charNumber);
  }
  Vector* stringVector = newVector();

  int32_t c;
  while ((c = nextValue(stream)) != EOF) {
    if (c == '\"') {
      break;
    } else {
      *VEC_PUSH(stringVector, char) = (char)c;
    }
  }
  // Copy data to token
  char* string = malloc(lengthVector(stringVector));
  memcpy(string, getVector(stringVector, 0), lengthVector(stringVector));
  deleteVector(stringVector);

  Token* stringToken = newToken(SymStringLiteral, string);
  *VEC_PUSH(tokens, Token*) = stringToken;
}

static void lexIntegerLiteral(Parseable* stream, Vector* tokens) {}
static void lexFloatLiteral(Parseable* stream, Vector* tokens) {}

static void parseNumberLiteral(Parseable* stream, Vector* tokens) {
  Vector* data = newVector();

  bool has
  int32_t c;
  while ((c = nextValue(stream)) != EOF) {
    if (isdigit(c) || c == '.') {
      *VEC_PUSH(data, char) = (char)c;
    } else {
      break;
    }
  }
  // Copy data to token
  char* string = malloc(lengthVector(stringVector));
  memcpy(string, getVector(stringVector, 0), lengthVector(stringVector));
  deleteVector(stringVector);

  Token* stringToken = newToken(SymStringLiteral, string);
  *VEC_PUSH(tokens, Token*) = stringToken;
}

void lex(Parseable* stream, Vector* tokens) {
  int32_t c;
  while ((c = nextValue(stream)) != EOF) {
    // Unget c for the next function
    backValue(stream);
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
