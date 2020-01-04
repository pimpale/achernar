#include <stdio.h>

#include "vector.h"
#include "lex.h"
#include "error.h"


static void lexComment(FILE* input, Vector* tokens) {
  int32_t c;
  if(getc(c) != '#') {
    
  if() {

  }


void freeToken(Token* token) {
  if(token->type == SymStringLiteral
      || token->type == SymFloatLiteral
      || token->type == SymIntLiteral) {
    free(token->payload);
  }
  free(token);
}

void lexFile(FILE* input, Vector* tokens) {
  int32_t c;
  while ((c = getc(input)) != EOF) {
    // Unget c for the next function
    ungetc(c, input);
    if (isblank(c) || c == '\n') {
      // Move on till next token
      nextValue(input);
    } else if (c == '#') {
      lexComment(input, tokens);
    } else if (c == '\"') {
      lexStringLiteral(input, tokens);
    } else if (isdigit(c)) {
      parseNumber(input, tokens);
    } else {
      parseIdentifier(input, tokens);
    }
  }
}
