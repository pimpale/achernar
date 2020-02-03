#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "lexer.h"
#include "parser.h"
#include "token.h"
#include "vector.h"

static char *newAstString(FILE *stream) {
  DiagnosticLogger *dl =
      createDiagnosticLogger(malloc(sizeof(DiagnosticLogger)), stderr);
  Lexer *l = createLexerFile(malloc(sizeof(Lexer)), dl, stream);
  Parser *p = createParserLexer(malloc(sizeof(Parser)), l);

  ResultTranslationUnit rtu = parseTranslationUnit(p);


  free(destroyParser(p));
  free(destroyLexer(l));
  free(destroyDiagnosticLogger(dl));
  return "";
}

int main(int argc, char **argv) {
  char *ast = newAstString(stdin);
  puts(ast);
}
