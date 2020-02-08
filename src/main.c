#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "lexer.h"
#include "parser.h"
#include "token.h"
#include "vector.h"

static char *newAstString(FILE *stream) {
  Lexer *l = createLexerFile(malloc(sizeof(Lexer)), stream);
  Parser *p = createParserLexer(malloc(sizeof(Parser)), l);

  TranslationUnit tu;
  parseTranslationUnit(&tu, p);


  free(destroyParser(p));
  free(destroyLexer(l));
  return "";
}

int main() {
  char *ast = newAstString(stdin);
  puts(ast);
}
