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
  Lexer *lexer = createLexerFile(malloc(sizeof(Lexer)), stream);
  Arena *token_arena = createArena(malloc(sizeof(Arena)));
  BufferedLexer *blp = createBufferedLexer(malloc(sizeof(BufferedLexer)), lexer, token_arena);

  TranslationUnit tu;
  parseTranslationUnit(&tu, blp);

  char* str = printTranslationUnit(&tu);

  free(destroyBufferedLexer(blp));
  free(destroyLexer(lexer));
  free(destroyArena(token_arena));
  return str;
}

int main() {
  char *ast = newAstString(stdin);
  puts(ast);
}
