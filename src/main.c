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
      createDiagnosticLogger(malloc(sizeof(DiagnosticLogger)), stdout);
  Lexer *l = createLexerFile(malloc(sizeof(Lexer)), stream);
  Parser *p = createParserLexer(malloc(sizeof(Parser)), dl, l);

  TranslationUnit tu;
  parseTranslationUnit(&tu, p);


  free(destroyParser(p));
  free(destroyDiagnosticLogger(dl));
  free(destroyLexer(l));
  return "";
}

int main() {
  char *ast = newAstString(stdin);
  puts(ast);
}
