#include <stdint.h>
#include <stdio.h>

#include "parseable.h"
#include "error.h"
#include "constants.h"


Parseable* newParseableFile(FILE* file) {
  Parseable* parseable = malloc(sizeof(Parseable));
  parseable->backing = PARSEABLE_BACKING_FILE;
  parseable->memory = NULL;
  parseable->len = 0;
  parseable->loc = 0;
  parseable->file = file;
  parseable->lastVal = 0; // null byte
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
  parseable->lastVal = 0; // null byte
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
        if(nextValue  == '\n') {
          p->lineNumber += 1;
          p->charNumber = 0;
        } else {
          p->charNumber += 1;
        }
      }
      break;
    }
    case PARSEABLE_BACKING_FILE: {
      p->lastVal = getc(p->file);
      nextValue = (p->lastVal);
      if(nextValue  == '\n') {
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

void backValue(Parseable* p) {
  switch (p->backing) {
    case PARSEABLE_BACKING_MEMORY: {
      if (p->loc != 0) {
        p->loc -= 1;
      }
      break;
    }
    case PARSEABLE_BACKING_FILE: {
      ungetc(p->lastVal, p->file);
      break;
    }
  }
}

void deleteParseable(Parseable* p) {
  switch(p->backing) {
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
