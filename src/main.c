#include <stdio.h>

#include "arena.h"
#include "lexer.h"
#include "parseAst.h"
#include "printAst.h"

int main() {
  Arena mem;
  createArena(&mem);

  Lexer lexer;
  createLexerFile(&lexer, stdin, &mem);

  Parser parser;
  createParser(&parser, &lexer, &mem);

  Printer printer;
  createPrinter(&printer, &parser, &mem);

  // Print
  printJsonPrinter(&printer, stdout);

  // Clean up
  releasePrinter(&printer);
  releaseParser(&parser);
  releaseLexer(&lexer);
  destroyArena(&mem);
}
