#include "lexer.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

Lexer lex_fromFile(FILE *file) {
  return (Lexer){._position = LNCOL(LN(1), COL(1)),
                 ._file = file,
                 ._backing = lex_BackingFile};
}

Lexer lex_fromMemory(char *ptr, size_t len) {
  return (Lexer){
      ._position = LNCOL(LN(1), COL(1)),
      ._backing = lex_BackingMemory,
      ._memory.len = len,
      ._memory.loc = 0,
      ._memory.ptr = ptr,
  };
}

void lex_destroy(Lexer *lexer) {
  switch (lexer->_backing) {
  case lex_BackingMemory: {
    break;
  }
  case lex_BackingFile: {
    break;
  }
  }
}

static LnCol lex_incrementLn(LnCol position) {
  return LNCOL(LN(position.ln.val + 1), COL(1));
}

static LnCol lex_incrementCol(LnCol position) {
  return LNCOL(position.ln, COL(position.col.val + 1));
}

static int32_t lex_nextRaw(Lexer *lexer) {
  int32_t nextValue;
  switch (lexer->_backing) {
  case lex_BackingMemory: {
    if (lexer->_memory.loc + 1 >= lexer->_memory.len) {
      nextValue = EOF;
    } else {
      // Return the element at the location, and increment location
      nextValue = (lexer->_memory.ptr[lexer->_memory.loc]);
      lexer->_memory.loc++;
    }
    break;
  }
  case lex_BackingFile: {
    nextValue = getc(lexer->_file);
    break;
  }
  }
  return nextValue;
}

int32_t lex_next(Lexer *lexer) {
  int32_t nextValue;

  if (QUEUE_LEN(&lexer->peeked_chars, int32_t) > 0) {
    // if there's something in the queue
    QUEUE_POP(&lexer->peeked_chars, &nextValue, int32_t);
  } else {
    // if the queue is empty
    nextValue = lex_nextRaw(lexer);
  }

  switch (nextValue) {
  case EOF: {
    // just return EOF
    return EOF;
  }
  case '\r': {
    // if
    if (LEX_PEEK(lexer) == '\n') {
      lexer->_position = lex_incrementCol(lexer->_position);
    } else {
      lexer->_position = lex_incrementLn(lexer->_position);
    }
    return '\r';
  }
  case '\n': {
    lexer->_position = lex_incrementLn(lexer->_position);
    return '\n';
  }
  default: {
    lexer->_position = lex_incrementCol(lexer->_position);
    return nextValue;
  }
  }
}

// gets the k'th token
// k must be greater than 0
int32_t lex_peekNth(Lexer *lexer, size_t k) {
  assert(k > 0);

  for (size_t i = QUEUE_LEN(&lexer->peeked_chars, int32_t); i < k; i++) {
    // get the next character and enqueue it
    *QUEUE_PUSH(&lexer->peeked_chars, int32_t) = lex_nextRaw(lexer);
  }

  // return the most recently queued character
  return *QUEUE_GET(&lexer->peeked_chars,
                    QUEUE_LEN(&lexer->peeked_chars, int32_t) - k, int32_t);
}

Span lex_getSpan(Lexer *lexer) {
  int32_t n1 = lex_peekNth(lexer, 1);

  switch (n1) {
  case '\r': {
    switch (lex_peekNth(lexer, 2)) {
    case '\n': {
      // increment col if it's part of a \r\n ending
      return SPAN(lexer->_position, lex_incrementCol(lexer->_position));
    }
    default: {
      // increment line if it's an only \r ending
      return SPAN(lexer->_position, lex_incrementLn(lexer->_position));
    }
    }
  }
  case '\n': {
    return SPAN(lexer->_position, lex_incrementLn(lexer->_position));
  }
  default: {
    return SPAN(lexer->_position, lex_incrementCol(lexer->_position));
  }
  }
}
