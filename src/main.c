#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "lexer.h"
#include "token.h"
#include "vector.h"

static char *newAstString(FILE *stream) {
  DiagnosticLogger *dl =
      createDiagnosticLogger(malloc(sizeof(DiagnosticLogger)), stderr);
  Lexer *l = createLexerFile(malloc(sizeof(Lexer)), stream);
  while (true) {
    ResultToken ret = lexNextToken(l, dl);
    if (ret.err == ErrorOk) {
      printToken(&ret.val);
      destroyToken(&ret.val);
    } else if (ret.err == ErrorEOF) {
      break;
    }
  }
  free(destroyLexer(l));
  free(destroyDiagnosticLogger(dl));
  return "";
}

int main(int argc, char **argv) {
  char *ast = newAstString(stdin);
  puts(ast);
}
