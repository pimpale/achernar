#ifndef AST_TO_HIR_H
#define AST_TO_HIR_H

#include "tokens_to_ast.h"
#include "com_allocator.h"
#include "diagnostic.h"
#include "hir.h"

hir_Expr* hir_constructExpr(const ast_Expr* vep, DiagnosticLogger* diagnostics, com_allocator *a);

#endif // AST_TO_HIR_H

