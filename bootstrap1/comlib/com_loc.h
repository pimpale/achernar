#ifndef COM_LOC_H
#define COM_LOC_H

#include "com_define.h"

// Wrap Lns and Cols in their own struct so you can't get confused
typedef struct {
  u64 val;
} com_loc_Ln;

typedef struct {
  u64 val;
} com_loc_Col;

typedef struct {
  com_loc_Ln ln;
  com_loc_Col col;
} com_loc_LnCol;

// [start, end)
typedef struct {
  com_loc_LnCol start;
  com_loc_LnCol end;
} com_loc_Span;

#define com_loc_ln_m(ln) ((com_loc_Ln){ln})
#define com_loc_col_m(col) ((com_loc_Col){col})
#define com_loc_lncol_m(ln, col)                                    \
  ((com_loc_LnCol){ln, col})
#define com_loc_span_m(start, end)                                  \
  ((com_loc_Span){start, end})

#endif
