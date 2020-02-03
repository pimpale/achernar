#ifndef PARSER_H
#define PARSER_H

#include <stdint.h>
#include <stdio.h>

#include "ast.h"
#include "error.h"
#include "lexer.h"
#include "token.h"

typedef enum ParserBacking_e {
  ParserBackingLexer,
  ParserBackingMemory,
} ParserBacking;

// Do not manually modify any of these values
typedef struct {
  ParserBacking backing;
  union {
    Lexer *lexer;
    struct {
      Token *ptr;
      size_t len;
      size_t loc;
    } memory;
  };
  DiagnosticLogger* dl;
} Parser;

// initializes a parser using a list of tokens (copies)
Parser *createParserLexer(Parser *parser, Lexer *lexer);
Parser *createParserMemory(Parser *parser, DiagnosticLogger* dl, Token *tokens, size_t tokenCount);
// deletes parser
Parser* destroyParser(Parser *parser);

// Creates and parses a TranslationUnit
ResultTranslationUnit parseTranslationUnit(Parser *parser);

#endif
