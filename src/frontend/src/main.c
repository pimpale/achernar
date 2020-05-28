#include <stdio.h>

#include "allocator.h"
#include "arena_allocator.h"
#include "lex.h"
#include "parse.h"
#include "printAst.h"
#include <stdio.h>

int main() {
  Allocator pool;
  arena_a_create(&pool);

  Lexer lexer;
  createLexerFile(&lexer, stdin, &pool);

  Parser parser;
  createParser(&parser, &lexer, &pool);

  Printer printer;
  createPrinter(&printer, &parser, &pool);

  // Print
  printJsonPrinter(&printer, stdout);

  // Clean up
  releasePrinter(&printer);
  releaseParser(&parser);
  releaseLexer(&lexer);

  a_destroy(&pool);
}
