#ifndef LNCOL_H
#define LNCOL_H

#include <stdint.h>

typedef struct LnCol_s {
  uint64_t ln;
  uint64_t col;
} LnCol;

// [start, end)
typedef struct Span_s {
  LnCol start;
  LnCol end;
} Span;

#define LNCOL(ln, col) ((LnCol){ln, col})
#define SPAN(start, end) ((Span){start, end})

#endif
