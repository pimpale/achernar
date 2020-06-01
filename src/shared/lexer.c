#include "lexer.h"

#include <stdlib.h>
#include <stdint.h>

void lex_fromFile(Lexer *lexer, FILE *file) {
  lexer->position = LNCOL(1, 1);

  // Files
  lexer->file = file;
  lexer->backing = lex_BackingFile;
}

void lex_fromMemory(Lexer *lexer, char *ptr, size_t len) {
  lexer->position = LNCOL(1, 1);

  // Copy memory
  lexer->backing = lex_BackingMemory;
  lexer->memory.len = len;
  lexer->memory.loc = 0;
  lexer->memory.ptr = ptr;
}

void lex_destroy(Lexer *lexer) {
  switch (lexer->backing) {
  case lex_BackingMemory: {
    free(lexer->memory.ptr);
    break;
  }
  case lex_BackingFile: {
    break;
  }
  }
}

int32_t lex_next(Lexer *lexer) {
  int32_t nextValue;
  switch (lexer->backing) {
  case lex_BackingMemory: {
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
  case lex_BackingFile: {
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

int32_t lex_peek(Lexer *lexer) {
  switch (lexer->backing) {
  case lex_BackingMemory: {
    // If it's within the bounds, return the value ahead of us
    if (lexer->memory.loc + 2 < lexer->memory.len) {
      return (lexer->memory.ptr[lexer->memory.loc + 1]);
    } else {
      return EOF;
    }
  }
  case lex_BackingFile: {
    int32_t val = getc(lexer->file);
    ungetc(val, lexer->file);
    return val;
  }
  }
}

