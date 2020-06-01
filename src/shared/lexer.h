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

void lex_fromFile(Lexer *lexer, FILE *file);
void lex_fromMemory(Lexer *lexer, char *ptr, size_t len);
void lex_destroy(Lexer *lexer);

int32_t lex_next(Lexer *lexer);
int32_t lex_peek(Lexer *lexer);

#endif
