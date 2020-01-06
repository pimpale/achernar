#include "parseable.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "constants.h"
#include "error.h"

Parseable* newParseableFile(FILE* file) {
  Parseable* parseable = malloc(sizeof(Parseable));
  parseable->backing = PARSEABLE_BACKING_FILE;
  parseable->memory = NULL;
  parseable->len = 0;
  parseable->loc = 0;
  parseable->file = file;
  parseable->lineNumber = 0;
  parseable->charNumber = 0;
}

Parseable* newParseableMemory(char* ptr, size_t len) {
  Parseable* parseable = malloc(sizeof(Parseable));
  parseable->backing = PARSEABLE_BACKING_FILE;
  parseable->backing = PARSEABLE_BACKING_MEMORY;
  parseable->file = NULL;
  parseable->memory = malloc(len);
  memcpy(parseable->memory, ptr, len);
  parseable->len = len;
  parseable->loc = 0;
  parseable->lineNumber = 0;
  parseable->charNumber = 0;
}

int32_t nextValue(Parseable* p) {
  int32_t nextValue;
  switch (p->backing) {
    case PARSEABLE_BACKING_MEMORY: {
      if (p->loc + 1 >= p->len) {
        nextValue = EOF;
      } else {
        // Return the element at the location, and increment location
        nextValue = (p->memory[p->loc]);
        p->loc += 1;
        if (nextValue == '\n') {
          p->lineNumber += 1;
          p->charNumber = 0;
        } else {
          p->charNumber += 1;
        }
      }
      break;
    }
    case PARSEABLE_BACKING_FILE: {
      nextValue = getc(p->file);
      if (nextValue == '\n') {
        p->lineNumber += 1;
        p->charNumber = 0;
      } else {
        p->charNumber += 1;
      }
      break;
    }
  }
  return nextValue;
}

int32_t peekValue(Parseable* p) {
  int32_t peekValue = nextValue(p);
  switch (p->backing) {
    case PARSEABLE_BACKING_MEMORY: {
      if (p->loc != 0) {
        p->loc -= 1;
      }
      break;
    }
    case PARSEABLE_BACKING_FILE: {
      ungetc(peekValue, p->file);
      break;
    }
  }
  return peekValue;
}

void deleteParseable(Parseable* p) {
  switch (p->backing) {
    case PARSEABLE_BACKING_MEMORY: {
      free(p->memory);
      break;
    }
    case PARSEABLE_BACKING_FILE: {
      break;
    }
  }
  free(p);
}
