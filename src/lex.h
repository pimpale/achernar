#ifndef LEX_H
#define LEX_H

#include "parseable.h"
#include "vector.h"


// Appends all tokens found in this file to the tokens vector
void lex(Parseable* stream, Vector* tokens);

#endif
