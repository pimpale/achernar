#ifndef PARSE_AST_H_
#define PARSE_AST_H_

#include "ast.h"

typedef struct Parser_s {
  Arena *ar;
  Lexer *lexer;
  bool has_next_token;
  Token next_token;
  // Vector<Vector<Comment>> (will comments push to the topmost scope)
  Vector comments;
} Parser;

void createParser(Parser *pp, Lexer *lp, Arena* ar);
void parseTranslationUnitParser(Parser *pp, TranslationUnit *tu);
Arena* releaseParser(Parser* pp);

#endif
