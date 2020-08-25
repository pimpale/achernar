#ifndef AST_TO_JSON
#define AST_TO_JSON

#include "com_define.h"
#include "com_writer.h"
#include "tokens_to_ast.h"
#include "com_allocator.h"

void print_stream(AstConstructor *parser, com_allocator* a, com_writer* writer);

#endif
