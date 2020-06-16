#ifndef LNCOL_H
#define LNCOL_H

#include <stdint.h>

// Wrap Lns and Cols in their own struct so you can't get confused
typedef struct {
  uint64_t val;
} Ln;

typedef struct {
  uint64_t val;
} Col;

typedef struct LnCol_s {
  Ln ln;
  Col col;
} LnCol;

// [start, end)
typedef struct Span_s {
  LnCol start;
  LnCol end;
} Span;

#define LN(ln) ((Ln){ln})
#define COL(col) ((Col){col})
#define LNCOL(ln, col) ((LnCol){ln, col})
#define SPAN(start, end) ((Span){start, end})

#endif
