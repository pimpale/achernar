#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

#include "error.h"
#include "vector.h"
#include "token.h"

static char* newAstString(FILE* stream) {
  Parseable* p = newParseableFile(stream);
  while(true) {
    ResultToken ret = nextToken(p);
    if(ret.err == ErrOk) {
      printToken(&ret.val);
      destroyToken(&ret.val);
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
