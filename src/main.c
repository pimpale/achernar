#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "ast.h"
#include "error.h"
#include "lexer.h"
#include "token.h"
#include "vector.h"

static char *newAstString(FILE *stream) {
  Lexer *l = createLexerFile(malloc(sizeof(Lexer)), stream);
  BufferedLexer *blp = createBufferedLexer(malloc(sizeof(BufferedLexer)), l);

  TranslationUnit tu;
  parseTranslationUnit(&tu, blp);

  //printTranslationUnit(tu);

  free(destroyBufferedLexer(blp));
  free(destroyLexer(l));
  return "";
}

int main() {
  char *ast = newAstString(stdin);
  puts(ast);
}
