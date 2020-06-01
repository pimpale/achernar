#ifndef LEXER_H
#define LEXER_H

#include <stdint.h>
#include <stdio.h>

#include "lncol.h"

// Do not manually modify or access any of these values
typedef struct {
  // The backing data structure
  enum {
    lex_BackingMemory,
    lex_BackingFile,
  } backing;
  // Can either be a File ptr or a memory with location and length
  union {
    FILE *file;
    struct {
      char *ptr;
      size_t len;
      size_t loc;
    } memory;
  };
  // Caches the current location in file
  LnCol position;
} Lexer;

// Creates new lexer from file
void lex_fromFile(Lexer *lexer, FILE *file);
// Creates new lexer from 
void lex_fromMemory(Lexer *lexer, char *ptr, size_t len);
void lex_destroy(Lexer *lexer);

// returns the span of the peeked character
Span lex_peekSpan(Lexer* lexer);

// Returns the next char available
int32_t lex_next(Lexer *lexer);

// Peeks the next char from the lexer
int32_t lex_peek(Lexer *lexer);

#endif
