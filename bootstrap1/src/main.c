#include <stdio.h>

#include "allocator.h"
#include "std_allocator.h"
#include "lexer.h"
#include "code_to_tokens.h"
#include "tokens_to_ast.h"
#include "ast_to_json.h"

int main() {
  Allocator pool = std_allocator();
  Lexer lexer = lex_fromFile(stdin);
  AstFromCodeConstructor parser = ast_create(&lexer, &pool);

	

  // Print
  print_stream(&parser, stdout);

  // Clean up
  ast_destroy(&parser);
  lex_destroy(&lexer);
  a_destroy(&pool);
}
