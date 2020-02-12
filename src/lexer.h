#ifndef LEXER_H
#define LEXER_H

#include "token.h"

#include <stdint.h>
#include <stdio.h>

#include "linearAllocator.h"
#include "error.h"

typedef enum LexerBacking_e {
  LexerBackingMemory,
  LexerBackingFile,
} LexerBacking;

// Do not manually modify or access any of these values
typedef struct {
  LinearAllocator tokenData;
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
// Otherwise the token will have the type TokenNone and will contain a diagnostic.
// Tokens will be deallocated when the lexer is deallocated.
void lexNextToken(Lexer *lexer, Token* token);


typedef struct {
  Lexer *l;
  bool hasNextToken;
  Token nextToken;
} BufferedLexer;

BufferedLexer* createBufferedLexer(BufferedLexer* blp, Lexer* l);
void advanceToken(BufferedLexer* bl, Token* token);
void setNextToken(BufferedLexer* bl, Token* token);
BufferedLexer* destroyBufferedLexer(BufferedLexer* blp);


#endif
