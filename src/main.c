#include <stdio.h>
#include <stdlib.h>

#include "vector.h"
#include "lex.h"

char* newAstString(FILE* stream) {
  Parseable* p = newParseableFile(stream);
  Vector* tokens = newVector();
  lex(p, tokens);
  freeParseable(p);
  return "";
}


int main(int argc, char** argv) {
  char* ast = newAstString(stdin);
  puts(ast);
  free(ast);
}
