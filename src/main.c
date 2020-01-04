#include <stdio.h>
#include <stdlib.h>

#include "vector.h"
#include "log.h"
#include "lex.h"

char* newAstString(FILE* input) {
  Vector* tokens = newVector();
  lex(input, tokens);
  return "";
}


int main(int argc, char** argv) {
  char* ast = newAstString(stdin);
  puts(ast);
  free(ast);
}
