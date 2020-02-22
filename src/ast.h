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

typedef struct TypeExpr_s TypeExpr;
typedef struct ValueExpr_s ValueExpr;
typedef struct PlaceExpr_s PlaceExpr;
typedef struct StructEntryExpr_s StructEntryExpr;
typedef struct MatchCaseExpr_s MatchCaseExpr;
typedef struct Binding_s Binding;
typedef struct Pattern_s Pattern;

typedef struct Attr_s {
  // TODO what goes in here?
  Span span;
  Diagnostic diagnostic;
} Attr;

// Expressions and operations yielding a type
typedef struct TypeExpr_s {
  TypeExprKind kind;
  Span span;
  Diagnostic diagnostic;
  // TODO
} TypeExpr;

typedef struct Binding_s {
  Span span;
  Diagnostic diagnostic;
  // Elements
  char *name;
  TypeExpr *type;
} Binding;

// Expressions and operations yielding a matchable pattern
typedef struct Pattern_s {
  Span span;
  Diagnostic diagnostic;
} Pattern;

// Expressions and operations yielding a match case
typedef struct MatchCaseExpr_s {
  Span span;
  Diagnostic diagnostic;
  Pattern* pattern;
  ValueExpr* value;
} MatchCaseExpr;

// Expressions and operations yielding a struct entry case
typedef struct StructEntryExpr_s {
  Span span;
  Diagnostic diagnostic;
  char* name;
  ValueExpr *value;
} StructEntryExpr;


// Expressions and operations yielding a memory location
typedef struct PlaceExpr_s {
  PlaceExprKind kind;
  Span span;
  Diagnostic diagnostic;
  union {
    struct {
      Binding *binding;
    } varDecl;
  };
} PlaceExpr;

typedef struct ValueExpr_s {
  ValueExprKind kind;
  Span span;
  Diagnostic diagnostic;
  union {
    struct {
      uint64_t value;
    } intLiteral;
    struct {
      double value;
    } floatLiteral;
    struct {
      char value;
    } charLiteral;
    struct {
      char* value;
      size_t value_length;
    } stringLiteral;
    struct {
      struct ValueExpr_s* elements;
      size_t elements_length;
    } arrayLiteral;
    struct {
      struct StructEntryExpr_s* entries;
      size_t entries_length;
    } structLiteral;
    struct {
      ExprUnOpKind operator;
      struct ValueExpr_s* operand;
    } unaryOp;
    struct {
      ExprBinOpKind operator;
      struct ValueExpr_s* operand_1;
      struct ValueExpr_s* operand_2;
    } binaryOp;
    struct {
      struct ValueExpr_s* condition;
      struct ValueExpr_s* body;
      bool has_expr;
      struct ValueExpr_s* else_body;
    } ifExpr;
    struct {
      struct ValueExpr_s* condition;
      struct ValueExpr_s* body;
    } whileExpr;
    struct {
      struct ValueExpr_s* init;
      struct ValueExpr_s* condition;
      struct ValueExpr_s* update;
      struct ValueExpr_s* body;
    } forExpr;
    struct {
      char* function;
      struct ValueExpr_s* arguments;
      size_t arguments_length;
    } callExpr;
    struct {
      bool has_value;
      struct ValueExpr_s* value;
    } returnExpr;
    struct Match_s {
      struct ValueExpr_s* value;
      struct MatchCaseExpr_s* cases;
      size_t cases_length;
    } matchExpr;
    struct Block_s {
      struct Stmnt_s* statements;
      size_t statements_length;
      struct ValueExpr_s* expr;
    } Block;
  };
} ValueExpr;

typedef struct Stmnt_s {
  StmntKind kind;
  Span span;
  Diagnostic diagnostic;
  union {
    struct {
      char* name;
      struct Binding_s* params;
      size_t params_length;
      struct TypeExpr_s* type;
      struct ValueExpr_s* body;
    } funcDecl;
    struct {
      char* field;
      struct Binding_s* members;
      size_t members_length;
    } structDecl;
    struct {
      struct ValueExpr_s* value;
    } exprStmnt;
  };
} Stmnt;

typedef struct TranslationUnit_s {
  struct Stmnt_s
      *statements;            // The top level is just a series of statements
  uint64_t statements_length; // The number of statements
} TranslationUnit;

void parseTranslationUnit(TranslationUnit *tup, BufferedLexer *blp);

#endif
