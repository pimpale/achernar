#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "ast.h"
#include "printAst.h"
#include "parseAst.h"
#include "error.h"
#include "lexer.h"
#include "token.h"
#include "vector.h"

static char *newAstString(FILE *stream) {
  Arena *mem = createArena(malloc(sizeof(Arena)));

  Lexer *lexer = createLexerFile(malloc(sizeof(Lexer)), stream);

  BufferedLexer *blp = createBufferedLexer(malloc(sizeof(BufferedLexer)), lexer, mem);

  TranslationUnit tu;
  parseTranslationUnit(&tu, blp, mem);

  char* str = printTranslationUnit(&tu);

  free(destroyBufferedLexer(blp));
  free(destroyLexer(lexer));
  free(destroyArena(mem));
  return str;
}

int main() {
  char *ast = newAstString(stdin);
  puts(ast);
  free(ast);
}
