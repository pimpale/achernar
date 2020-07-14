#ifndef COM_BIGINT_H
#define COM_BIGINT_H

#include "com_biguint.h"

typedef struct {
  // can be -1, 0, or 1
  i8 _sign;
  com_biguint magnitude;
} com_bigint;



#endif

