#include "ast_to_json.h"
#include "code_to_tokens.h"
#include "com_os_allocator.h"
#include "com_os_iostream.h"
#include "com_reader_buffered.h"
#include "tokens_to_ast.h"


#include "com_mem.h"
#include "stdlib.h"

int main() {
  com_allocator a = com_os_allocator();

  // create buffered reader
  com_reader r = com_os_iostream_in();
  com_reader br = com_reader_buffered(&r, &a) ;

  ast_Constructor ast = ast_create(&br, &a);

  // Print
  com_writer w = com_os_iostream_out();

  print_stream(&ast, &a, &w);

  // Clean up
  ast_destroy(&ast);
  com_writer_destroy(&w);
  com_reader_destroy(&br);
  com_reader_destroy(&r);
  com_allocator_destroy(&a);
}
