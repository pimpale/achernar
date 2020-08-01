#include "com_os_allocator.h"
#include "com_os_iostream.h"
#include "code_to_tokens.h"
#include "tokens_to_ast.h"
#include "ast_to_json.h"

int main() {
  com_allocator a = com_os_allocator();
  com_reader r = com_os_iostream_in();
  com_writer w = com_os_iostream_out();

  AstConstructor ast = ast_create(&r, &a);

  // Print
  print_stream(&ast, &w);

  // Clean up
  ast_destroy(&ast);
  com_allocator_destroy(&a);
}
