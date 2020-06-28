#ifndef LEXER_H
#define LEXER_H

#include <stdint.h>
#include <stdio.h>

#include "queue.h"
#include "lncol.h"

// Do not manually modify or access any of these values
typedef struct {
  // The backing data structure
  enum {
    lex_BackingMemory,
    lex_BackingFile,
  } _backing;
  // Can either be a File ptr or a memory with location and length
  union {
    FILE *_file;
    struct {
      char *ptr;
      size_t len;
      size_t loc;
    } _memory;
  };
  // Caches the current location in file
  LnCol _position;
  // Queue of peeked characters
  Queue peeked_chars;
} Lexer;


// Creates new lexer from file
Lexer lex_fromFile(FILE *file);
// Creates new lexer from 
Lexer lex_from_memory(char *ptr, size_t len);
void lex_destroy(Lexer *lexer);

// returns the span of the next character
Span lex_getSpan(Lexer* lexer);

LnCol lex_position(Lexer *lexer);

// Returns the next char available
int32_t lex_next(Lexer *lexer);

// Peeks the nth char from the lexer
// n >= 1
int32_t lex_peekNth(Lexer *lexer, size_t n);

#define LEX_PEEK(lexer) lex_peekNth((lexer), 1)

#endif
