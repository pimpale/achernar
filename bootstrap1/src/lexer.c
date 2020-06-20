#include "lexer.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

Lexer lex_fromFile(FILE *file) {
  return (Lexer){.position = LNCOL(LN(1), COL(1)),
                 .file = file,
                 .backing = lex_BackingFile};
}

Lexer lex_fromMemory(char *ptr, size_t len) {
  return (Lexer){
      .position = LNCOL(LN(1), COL(1)),
      .backing = lex_BackingMemory,
      .memory.len = len,
      .memory.loc = 0,
      .memory.ptr = ptr,
  };
}

void lex_destroy(Lexer *lexer) {
  switch (lexer->backing) {
  case lex_BackingMemory: {
    break;
  }
  case lex_BackingFile: {
    break;
  }
  }
}

static void lex_incrementLn(Lexer *lexer) {
  lexer->position.ln = LN(lexer->position.ln.val + 1);
  lexer->position.col = COL(1);
}

static void lex_incrementCol(Lexer *lexer) {
  lexer->position.col = COL(lexer->position.col.val + 1);
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
    }
    break;
  }
  case lex_BackingFile: {
    nextValue = getc(lexer->file);
    break;
  }
  }

  switch (nextValue) {
  case EOF: {
    // just return EOF
    return EOF;
  }
  case '\r': {
    // if
    if (lex_peek(lexer) == '\n') {
      lex_incrementCol(lexer);
    } else {
      lex_incrementLn(lexer);
    }
    return '\r';
  }
  case '\n': {
    lex_incrementLn(lexer);
    return '\n';
  }
  default: {
    lex_incrementCol(lexer);
    return nextValue;
  }
  }
}

int32_t lex_peek(Lexer *lexer) {
  int32_t ret = EOF;
  switch (lexer->backing) {
  case lex_BackingMemory: {
    // If it's within the bounds, return the value ahead of us
    if (lexer->memory.loc + 2 < lexer->memory.len) {
      ret = (lexer->memory.ptr[lexer->memory.loc + 1]);
    } else {
      ret = EOF;
    }
    break;
  }
  case lex_BackingFile: {
    int32_t val = getc(lexer->file);
    ungetc(val, lexer->file);
    ret = val;
    break;
  }
  }
  return ret;
}

Span lex_peekSpan(Lexer *lexer) {
  if (lex_peek(lexer) == '\n') {
    return SPAN(lexer->position, LNCOL(LN(lexer->position.ln.val + 1), COL(1)));
  } else {
    return SPAN(lexer->position,
                LNCOL(lexer->position.ln, COL(lexer->position.col.val + 1)));
  }
}
