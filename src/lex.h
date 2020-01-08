#ifndef LEX_H
#define LEX_H

#include "parseable.h"
#include "vector.h"
#include "token.h"

// Appends all tokens found in this file to the tokens vector
ResultTokenPtr nextToken(Parseable* stream);

#endif
