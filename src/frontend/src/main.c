#include <stdio.h>

#include "allocator.h"
#include "arena_allocator.h"
#include "lex.h"
#include "ast_parse.h"
#include "ast_print.h"
#include <stdio.h>

int main() {
  Allocator pool = arena_a_create();
  Lexer lexer = lex_fromFile(stdin);
  Parser parser = parse_create(&lexer, &pool);

  // Print
  print_stream(&parser, stdout);

  // Clean up
  parse_release(&parser);
  lex_destroy(&lexer);
  a_destroy(&pool);
}
