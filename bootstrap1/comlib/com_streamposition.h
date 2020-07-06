#ifndef COM_STREAMPOSITION_H
#define COM_STREAMPOSITION_H

#include "com_define.h"

// Wrap Lns and Cols in their own struct so you can't get confused
typedef struct {
  uint64_t val;
} com_streamposition_Ln;

typedef struct {
  uint64_t val;
} com_streamposition_Col;

typedef struct LnCol_s {
  com_streamposition_Ln ln;
  com_streamposition_Col col;
} com_streamposition_LnCol;

// [start, end)
typedef struct Span_s {
  com_streamposition_LnCol start;
  com_streamposition_LnCol end;
} com_streamposition_Span;

#define com_streamposition_ln_m(ln) ((com_streamposition_Ln){ln})
#define com_streamposition_col_m(col) ((com_streamposition_Col){col})
#define com_streamposition_lncol_m(ln, col)                                    \
  ((com_streamposition_LnCol){ln, col})
#define com_streamposition_span_m(start, end)                                  \
  ((com_streamposition_Span){start, end})

#endif
