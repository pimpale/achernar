#ifndef PARSE_AST_H_
#define PARSE_AST_H_

#include "ast.h"

typedef struct Parser_s {
  Arena *ar;
  Lexer *lexer;
  bool has_next_token;
  Token next_token;
} Parser;

void createParser(Parser *pp, Lexer *lp, Arena* ar);
void parseStatementParser(Parser *pp, Stmnt *s);
Arena* releaseParser(Parser* pp);

#endif
