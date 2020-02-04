#ifndef LEXER_H
#define LEXER_H

#include "token.h"

#include <stdint.h>
#include <stdio.h>

#include "error.h"

typedef enum LexerBacking_e {
  LexerBackingMemory,
  LexerBackingFile,
} LexerBacking;

// Do not manually modify any of these values
typedef struct {
  // The backing data structure
  LexerBacking backing;
  // Can either be a File ptr or a memory with location and length
  union {
    FILE *file;
    struct {
      char *ptr;
      size_t len;
      size_t loc;
    } memory;
  };
  // Caches the number of newlines encountered
  uint64_t ln;
  // Caches the current column of the code
  uint64_t col;
} Lexer;

Lexer *createLexerFile(Lexer *lexer, FILE *file);
Lexer *createLexerMemory(Lexer *lexer, char *ptr, size_t len);

Lexer *destroyLexer(Lexer *lexer);

// Initializes Token to the value of the next token if there has not been an error.
// returns the type of any failure
Diagnostic lexNextToken(Lexer *lexer, Token* token);

#endif
