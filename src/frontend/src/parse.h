#ifndef PARSE_AST_H_
#define PARSE_AST_H_

#include "ast.h"

typedef struct Parser_s {
  Arena *ar;
  Lexer *lexer;
  // Vector<Token>
  Vector next_tokens_stack;
  // Vector<Vector<Comment>> vector of peeked comments for each peeked token
  Vector next_comments_stack;
  // Vector<Vector<Comment>> (stack of vectors of comments)
  Vector comments;
  // list of nested scopes for breaking out of stuff
  int64_t paren_depth;
  int64_t brace_depth;
  int64_t bracket_depth;
} Parser;

void createParser(Parser *pp, Lexer *lp, Arena *ar);
void parseTranslationUnitParser(Parser *pp, TranslationUnit *tu);
Arena *releaseParser(Parser *pp);

#endif
