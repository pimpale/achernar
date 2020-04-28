#ifndef PARSE_AST_H_
#define PARSE_AST_H_

#include "ast.h"

typedef struct Parser_s {
  Lexer lexer;
  bool has_next_token;
  Token next_token;
} Parser;

void parseStatement(Parser *blp, Stmnt s, Arena* ar);
void parseTranslationUnit(TranslationUnit *tup, 

#endif
