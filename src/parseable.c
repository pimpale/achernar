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
  return parseable;
}

Parseable* newParseableMemory(char* ptr, size_t len) {
  Parseable* parseable = malloc(sizeof(Parseable));
  parseable->backing = PARSEABLE_BACKING_MEMORY;
  parseable->file = NULL;
  parseable->memory = malloc(len);
  memcpy(parseable->memory, ptr, len);
  parseable->len = len;
  parseable->loc = 0;
  parseable->lineNumber = 5000;
  parseable->charNumber = 0;
  return parseable;
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
  switch (p->backing) {
    case PARSEABLE_BACKING_MEMORY: {
      // If it's within the bounds, return the value ahead of us
      int32_t val = EOF;
      if (p->loc + 2 < p->len) {
        val = (p->memory[p->loc + 1]);
      }
      return val;
    }
    case PARSEABLE_BACKING_FILE: {
      int32_t val = getc(p->file);
      ungetc(val, p->file);
      return val;
    }
  }
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
