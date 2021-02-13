#ifndef EVAL_HIR_H
#define EVAL_HIR_H

#include "hir.h"
#include "com_allocator.h"


// Evaluate compile time expressions and deduce typing (expression downwards)
void fully_eval(hir_Expr* expr, com_allocator* a);

#endif

