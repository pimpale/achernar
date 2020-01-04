#include <stdio.h>

#include "vector.h"
#include "lex.h"

void freeToken(Token* token) {
  if(token->type == SymStringLiteral
      || token->type == SymFloatLiteral
      || token->type == SymIntLiteral) {
    free(token->payload);
  }
  free(token);
}
void lexIdent

void lexFile(FILE* input, Vector* tokens) {
  //
  int32_t c;
  while ((c = nextValue(input)) != EOF) {
    // Unget c for the next function
    backValue(stream);
    if (isblank(c) || c == '\n') {
      nextValue(stream);
    } else if (c == '#') {
      parseComment(stream, stack);
    } else if (c == '(') {
      parseString(stream, stack);
    } else if (isdigit(c)) {
      parseNumber(stream, stack);
    } else {
      parseFunction(stream, stack, funtab, vartab);
    }
  }
}
