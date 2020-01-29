#ifndef LEXER_H
#define LEXER_H

#include <stdint.h>
#include <stdio.h>

#include "token.h"

typedef enum {
  LexerBackingMemory,
  LexerBackingFile,
} LexerBacking;

// Do not manually modify any of these values
typedef struct {
  // The backing data structure
  LexerBacking backing;
  // Can either be a File ptr or a memory with location and length
  union {
    FILE* file;
    struct {
      char* ptr;
      size_t len;
      size_t loc;
    } memory;
  };
  // Caches the number of newlines encountered
  uint64_t lineNumber;
  // Caches the number of characters encountered in this line
  uint64_t charNumber;
} Lexer;


Lexer* createLexerFile(Lexer* lexer, FILE* file);
Lexer* createLexerMemory(Lexer* lexer, char* ptr, size_t len);

void destroyLexer(Lexer* lexer);

// Returns the next available token, or a EofError
ResultToken lexNextToken(Lexer* lexer);

#endif
