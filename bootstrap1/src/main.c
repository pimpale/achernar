#include "com_allocator.h"
#include "code_to_tokens.h"
#include "tokens_to_ast.h"
#include "ast_to_json.h"

int main() {
  com_allocator pool = std_allocator();
  Lexer lexer = lex_fromFile(stdin);
  AstFromCodeConstructor parser = ast_create(&lexer, &pool);

	

  // Print
  print_stream(&parser, stdout);

  // Clean up
  ast_destroy(&parser);
  lex_destroy(&lexer);
  a_destroy(&pool);
}
