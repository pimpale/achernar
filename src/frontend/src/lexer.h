#ifndef LEXER_H
#define LEXER_H

#include "token.h"

#include <stdint.h>
#include <stdio.h>

#include "arena.h"
#include "error.h"

typedef enum {
  LBK_LexerBackingMemory,
  LBK_LexerBackingFile,
} LexerBackingKind;

// Do not manually modify or access any of these values
typedef struct {
  // The backing data structure
  LexerBackingKind backing;
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
  // Stores the data of each token
  Arena *ar;
} Lexer;

void createLexerFile(Lexer *lexer, FILE *file, Arena *ar);
void createLexerMemory(Lexer *lexer, char *ptr, size_t len, Arena *ar);
Arena *releaseLexer(Lexer *lexer);

// Initializes Token to the value of the next token if there has not been an
// error. Otherwise the token will have the type TokenNone and will contain a
// diagnostic. Tokens will be deallocated when the lexer is deallocated.
void lexNextToken(Lexer *lexer, Token *token);

#endif
