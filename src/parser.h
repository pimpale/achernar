#ifndef PARSER_H
#define PARSER_H

#include <stdint.h>
#include <stdio.h>

#include "token.h"
#include "ast.h"


// Do not manually modify any of these values
typedef struct {
  Token* tokens;
  size_t tokenCount;
  size_t currentToken;
} Parser;

// initializes a parser using a list of tokens (copies)
Parser* createParser(Parser* parser, Token* tokens, size_t tokenCount);
// deletes parser
void destroyParser(Parser* parser);

// Creates and parses a TranslationUnit
ResultTranslationUnit parseTranslationUnit(Parser* parser);

#endif
