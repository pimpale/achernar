#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

#include "error.h"
#include "vector.h"
#include "token.h"
#include "lex.h"

static char* newAstString(FILE* stream) {
  Parseable* p = newParseableFile(stream);
  while(true) {
    ResultTokenPtr ret = nextToken(p);
    if(ret.err == ErrOk) {
      printToken(ret.val);
      deleteToken(ret.val);
    } else if(ret.err == ErrEof) {
      break;
    }
  }
  deleteParseable(p);
  return "";
}


int main(int argc, char** argv) {
  char* ast = newAstString(stdin);
  puts(ast);
}
