#include <stdio.h>

#include "allocator.h"
#include "std_allocator.h"
#include "lex.h"
#include "ast_parse.h"
#include "ast_print.h"
#include <stdio.h>

int main() {
  Allocator pool = std_allocator();
  Lexer lexer = lex_fromFile(stdin);
  Parser parser = parse_create(&lexer, &pool);

  // Print
  print_stream(&parser, stdout);

  // Clean up
  parse_release(&parser);
  lex_destroy(&lexer);
  a_destroy(&pool);
}
