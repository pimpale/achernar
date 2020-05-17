#ifndef PRINT_AST_H_
#define PRINT_AST_H_

#include <stdio.h>

#include "ast.h"
#include "parseAst.h"

typedef struct {
  Parser *parser;
  Arena *arena;
} Printer;

void createPrinter(Printer *printer, Parser *parser, Arena *arena);

void printJsonPrinter(Printer *printer, FILE *file);
Arena *releasePrinter(Printer *printer);

#endif
