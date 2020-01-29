#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

#include "error.h"
#include "lexer.h"
#include "vector.h"
#include "token.h"

static char* newAstString(FILE* stream) {
  Lexer* l = createLexerFile(malloc(sizeof(Lexer)), stream);
  while(true) {
    ResultToken ret = lexNextToken(l);
    if(ret.err == ErrOk) {
      printToken(&ret.val);
      destroyToken(&ret.val);
    } else if(ret.err == ErrEof) {
      break;
    }
  }
  destroyLexer(l);
  free(l);
  return "";
}


int main(int argc, char** argv) {
  char* ast = newAstString(stdin);
  puts(ast);
}
