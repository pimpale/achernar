#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "vector.h"
#include "token.h"
#include "lex.h"

static char* newAstString(FILE* stream) {
  Parseable* p = newParseableFile(stream);
  Vector* tokens = newVector();
  lex(p, tokens);
  for(size_t i = 0; i < VEC_LEN(tokens, Token*); i++) {
    printToken(*VEC_GET(tokens, i, Token*));
  }
  deleteParseable(p);
  return "";
}


int main(int argc, char** argv) {
  char* ast = newAstString(stdin);
  puts(ast);
}
