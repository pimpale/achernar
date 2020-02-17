#ifndef AST_H_
#define AST_H_

#include <stdbool.h>
#include <stdint.h>

#include "error.h"
#include "lexer.h"
#include "token.h"

typedef enum {
  S_FuncDecl,
  S_VarDecl,
  S_StructDecl,
  S_TypeAliasDecl,
  S_Entry,
  S_Expr,
} StmntKind;

typedef enum {
  VE_IntLiteral,
  VE_FloatLiteral,
  VE_CharLiteral,
  VE_StringLiteral,
  VE_ArrayLiteral,
  VE_StructLiteral,
  VE_BinaryOp,
  VE_UnaryOp,
  VE_Call,
  VE_If,
  VE_While,
  VE_For,
  VE_With,
  VE_Break,
  VE_Continue,
  VE_Return,
  VE_Match,
  VE_Block,
} ValueExprKind;

typedef enum {
  PE_VarDecl,     // let
  PE_FieldAccess, // .
  PE_ArrayIndex
} PlaceExprKind;


typedef enum {
  TE_VarDecl,     // let
  TE_FieldAccess, // .
  TE_ArrayIndex
} TypeExprKind;

typedef enum {
  EBO_Add,              // +
  EBO_Sub,              // -
  EBO_Mul,              // *
  EBO_Div,              // /
  EBO_Mod,              // %
  EBO_BitAnd,           // &
  EBO_BitOr,            // |
  EBO_BitXor,           // ^
  EBO_BitShl,           // <<
  EBO_BitShr,           // >>
  EBO_LogicalAnd,       // &&
  EBO_LogicalOr,        // ||
  EBO_CompEqual,        // ==
  EBO_CompNotEqual,     // !=
  EBO_CompLess,         // <
  EBO_CompLessEqual,    // <=
  EBO_CompGreater,      // >
  EBO_CompGreaterEqual, // >=
  EBO_FieldAccess,      // .
  EBO_ArrayAccess,      // []
  EBO_Pipeline,         // ->
} ExprBinOpKind;

typedef enum {
  EUO_Negate,     // -
  EUO_LogicalNot, // !
  EUO_BitNot,     // ~
  EUO_Ref,        // $
  EUO_Deref       // @
} ExprUnOpKind;

typedef struct Attr_s {
  // TODO what goes in here?
  Span span;
  Diagnostic diagnostic;
} Attr;

// Expressions and operations yielding a matchable pattern
typedef struct Pattern_s {
  Span span;
  Diagnostic diagnostic;
} Pattern;

// Expressions and operations yielding a type
typedef struct TypeExpr_s {
  TypeExprKind kind;
  Span span;
  Diagnostic diagnostic;
  // TODO
} TypeExpr;

// Expressions and operations yielding a memory location
typedef struct PlaceExpr_s {
  PlaceExprKind kind;
  Span span;
  Diagnostic diagnostic;
  union {
    struct VarDecl_s {
      char* name;
      struct TypeExpr_s* type;
    } VarDecl;
  };
} PlaceExpr;

typedef struct ValueExpr_s {
  ValueExprKind kind;
  Span span;
  Diagnostic diagnostic;
  union {
    struct IntLiteral_s {
      uint64_t value;
    } IntLiteral;
    struct FloatLiteral_s {
      double value;
    } FloatLiteral;
    struct CharLiteral_s {
      char value;
    } CharLiteral;
    struct StringLiteral_s {
      char* value;
      size_t value_length;
    } StringLiteral;
    struct ArrayLiteral_s {
      struct Expr_s* elements;
      size_t elements_length;
    } ArrayLiteral;
    struct StructLiteralEntry_s {
      char* field;
      struct Expr_s* value;
    } StructLiteralEntry;
    struct StructLiteral_s {
      struct Expr_s* entries; // MUST be of type StructLiteralEntry
      size_t entries_length;
    } StructLiteral;
    struct UnaryOp_s {
      ExprUnOpKind operator;
      struct Expr_s* operand;
    } UnaryOp;
    struct BinaryOp_s {
      ExprBinOpKind operator;
      struct Expr_s* operand_1;
      struct Expr_s* operand_2;
    } BinaryOp;
    struct If_s {
      struct Expr_s* condition;
      struct Expr_s* body;
      bool has_expr;
      struct Expr_s* else_body;
    } If;
    struct While_s {
      struct Expr_s* condition;
      struct Expr_s* body;
    } While;
    struct For_s {
      struct Expr_s* init;
      struct Expr_s* condition;
      struct Expr_s* update;
    } For;
    struct Call_s {
      char* function;
      struct Expr_s* arguments;
      size_t arguments_length;
    } Call;
    struct Return_s {
      bool has_value;
      struct Expr_s* value;
    } Return;
    struct MatchCase_s {
      struct Pattern_s* pattern;
      struct Expr_s* value;
    } MatchCase;
    struct Match_s {
      struct Expr_s* value;
      struct Expr_s* cases; // MUST be of type matchCase
      size_t cases_length;
    } Match;
    struct Block_s {
      struct Stmnt_s* statements;
      size_t statements_length;
      struct Expr_s* expr;
    } Block;
  };
} ValueExpr;

typedef struct Stmnt_s {
  StmntKind kind;
  Span span;
  Diagnostic diagnostic;
  union {
    struct FuncDecl_s {
      char* name;
      struct PlaceExpr_s* params; // MUST be of type VarDecl
      size_t params_length;
      struct Expr_s* expr;
    } FuncDecl;
    struct StructDecl_s {
      char* field;
      struct Decl_s* members; // MUST be of type VarDecl
      size_t members_length;
    } StructDecl;
    struct ExprStmnt_s {
      struct ValueExpr_s* value;
    } ExprStmnt;
  };
} Stmnt;

typedef struct TranslationUnit_s {
  struct Stmnt_s
      *statements;            // The top level is just a series of statements
  uint64_t statements_length; // The number of statements
} TranslationUnit;

void parseTranslationUnit(TranslationUnit *tup, BufferedLexer *blp);

#endif
