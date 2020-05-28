#include "lexer.h"

#include <stdlib.h>
#include <stdint.h>

void createLexerFile(Lexer *lexer, FILE *file, Allocator *a) {
  lexer->position = LNCOL(1, 1);
  lexer->a= a;

  // Files
  lexer->file = file;
  lexer->backing = LBK_LexerBackingFile;
}

void createLexerMemory(Lexer *lexer, char *ptr, size_t len, Allocator *a) {
  lexer->position = LNCOL(1, 1);
  lexer->a = a;

  // Copy memory
  lexer->backing = LBK_LexerBackingMemory;
  lexer->memory.len = len;
  lexer->memory.loc = 0;
  lexer->memory.ptr = ptr;
}

void releaseLexer(Lexer *lexer) {
  switch (lexer->backing) {
  case LBK_LexerBackingMemory: {
    free(lexer->memory.ptr);
    break;
  }
  case LBK_LexerBackingFile: {
    break;
  }
  }
}

int32_t nextValueLexer(Lexer *lexer) {
  int32_t nextValue;
  switch (lexer->backing) {
  case LBK_LexerBackingMemory: {
    if (lexer->memory.loc + 1 >= lexer->memory.len) {
      nextValue = EOF;
    } else {
      // Return the element at the location, and increment location
      nextValue = (lexer->memory.ptr[lexer->memory.loc]);
      lexer->memory.loc++;
      if (nextValue == '\n') {
        lexer->position.ln += 1;
        lexer->position.col = 1;
      } else {
        lexer->position.col += 1;
      }
    }
    break;
  }
  case LBK_LexerBackingFile: {
    nextValue = getc(lexer->file);
    if (nextValue != EOF) {
      if (nextValue == '\n') {
        lexer->position.ln += 1;
        lexer->position.col = 1;
      } else {
        lexer->position.col += 1;
      }
    }
    break;
  }
  }
  return nextValue;
}

int32_t peekValueLexer(Lexer *lexer) {
  switch (lexer->backing) {
  case LBK_LexerBackingMemory: {
    // If it's within the bounds, return the value ahead of us
    if (lexer->memory.loc + 2 < lexer->memory.len) {
      return (lexer->memory.ptr[lexer->memory.loc + 1]);
    } else {
      return EOF;
    }
  }
  case LBK_LexerBackingFile: {
    int32_t val = getc(lexer->file);
    ungetc(val, lexer->file);
    return val;
  }
  }
}

