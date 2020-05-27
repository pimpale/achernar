#ifndef LEXER_H
#define LEXER_H

#include <stdint.h>
#include <stdio.h>

#include "arena.h"
#include "lncol.h"

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

int32_t nextValueLexer(Lexer *lexer);
int32_t peekValueLexer(Lexer *lexer);

#endif
