#ifndef PRINT_AST_H_
#define PRINT_AST_H_

#include <stdio.h>

#include "ast.h"
#include "ast_parse.h"
#include "allocator.h"

typedef struct {
  Parser *parser;
  Allocator *a;
} Printer;

void createPrinter(Printer *printer, Parser *parser, Allocator *allocator);

void printJsonPrinter(Printer *printer, FILE *file);
Allocator *releasePrinter(Printer *printer);

#endif
