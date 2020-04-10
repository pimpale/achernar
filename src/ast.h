#ifndef AST_H_
#define AST_H_

#include <stdbool.h>
#include <stdint.h>

#include "error.h"
#include "lexer.h"
#include "token.h"

typedef enum {
  SK_None,
  SK_FnDecl,
  SK_VarDecl,
  SK_TypeAliasDecl,
  SK_Expr,
} StmntKind;

typedef enum {
  VEK_None,
  VEK_IntLiteral,
  VEK_BoolLiteral,
  VEK_FloatLiteral,
  VEK_CharLiteral,
  VEK_StringLiteral,
  VEK_ArrayLiteral,
  VEK_StructLiteral,
  VEK_BinaryOp,
  VEK_UnaryOp,
  VEK_Call,
  VEK_If,
  VEK_While,
  VEK_For,
  VEK_With,
  VEK_Pass,
  VEK_Break,
  VEK_Continue,
  VEK_Return,
  VEK_Match,
  VEK_Block,
  VEK_Group,
  VEK_FieldAccess,
  VEK_Reference,
} ValueExprKind;

typedef enum {
  TEK_None,      // Error type
  TEK_Reference, // Reference
  TEK_Struct,    // struct
  TEK_Omitted,   // Omitted
  TEK_Typeof,    // typeof
} TypeExprKind;

typedef enum {
  BOK_Add,              // +
  BOK_Sub,              // -
  BOK_Mul,              // *
  BOK_Div,              // /
  BOK_Mod,              // %
  BOK_BitAnd,           // &
  BOK_BitOr,            // |
  BOK_BitXor,           // ^
  BOK_BitShl,           // <<
  BOK_BitShr,           // >>
  BOK_LogicalAnd,       // &&
  BOK_LogicalOr,        // ||
  BOK_CompEqual,        // ==
  BOK_CompNotEqual,     // !=
  BOK_CompLess,         // <
  BOK_CompLessEqual,    // <=
  BOK_CompGreater,      // >
  BOK_CompGreaterEqual, // >=
  BOK_ArrayAccess,      // []
  BOK_Pipeline,         // ->
} BinaryOpKind;

typedef enum {
  UOK_Negate,     // -
  UOK_Posit,      // +
  UOK_LogicalNot, // !
  UOK_BitNot,     // ~
  UOK_Ref,        // $
  UOK_Deref       // @
} UnaryOpKind;

typedef struct TypeExpr_s TypeExpr;
typedef struct ValueExpr_s ValueExpr;
typedef struct StructEntryExpr_s StructEntryExpr;
typedef struct MatchCaseExpr_s MatchCaseExpr;
typedef struct Binding_s Binding;
typedef struct Path_s Path;
typedef struct Stmnt_s Stmnt;

// Attributes that may be attached to statements or certain expressions
typedef struct Attr_s {
  Span span;
  Diagnostic *diagnostics;
  size_t diagnostics_length;
} Attr;

typedef struct Path_s {
  Span span;
  Diagnostic *diagnostics;
  size_t diagnostics_length;
  char **pathSegments;
  size_t pathSegments_length;
} Path;

// Expressions and operations yielding a type
typedef struct TypeExpr_s {
  TypeExprKind kind;
  Span span;
  Diagnostic *diagnostics;
  size_t diagnostics_length;
  union {
    struct {
      Path *name;
      uint64_t ptrCount;
    } referenceExpr;
    struct {
      Binding *members;
      size_t members_length;
    } structExpr;
    struct {
      Binding *members;
      size_t members_length;
    } enumExpr;
    struct {
      ValueExpr *value;
    } typeofExpr;
  };
} TypeExpr;

typedef struct Binding_s {
  Span span;
  Diagnostic *diagnostics;
  size_t diagnostics_length;
  // Elements
  char *name;
  TypeExpr *type;
} Binding;

// Expressions and operations yielding a match case
typedef struct MatchCaseExpr_s {
  Span span;
  Diagnostic *diagnostics;
  size_t diagnostics_length;
  ValueExpr *pattern;
  ValueExpr *value;
} MatchCaseExpr;

// Expressions and operations yielding a struct entry case
typedef struct StructEntryExpr_s {
  Span span;
  Diagnostic *diagnostics;
  size_t diagnostics_length;
  char *name;
  ValueExpr *value;
} StructEntryExpr;

typedef struct ValueExpr_s {
  ValueExprKind kind;
  Span span;
  Diagnostic *diagnostics;
  size_t diagnostics_length;
  union {
    struct {
      uint64_t value;
    } intLiteral;
    struct {
      bool value;
    } boolLiteral;
    struct {
      double value;
    } floatLiteral;
    struct {
      char value;
    } charLiteral;
    struct {
      char *value;
      size_t value_length;
    } stringLiteral;
    struct {
      ValueExpr *elements;
      size_t elements_length;
    } arrayLiteral;
    struct {
      StructEntryExpr *entries;
      size_t entries_length;
    } structLiteral;
    struct {
      ValueExpr *value;
      char *field;
    } fieldAccess;
    struct {
      Path *value;
    } reference;
    struct {
      UnaryOpKind operator;
      ValueExpr *operand;
    } unaryOp;
    struct {
      BinaryOpKind operator;
      ValueExpr *left_operand;
      ValueExpr *right_operand;
    } binaryOp;
    struct {
      ValueExpr *condition;
      ValueExpr *body;
      bool has_else;
      ValueExpr *else_body;
    } ifExpr;
    struct {
      ValueExpr *condition;
      ValueExpr *body;
    } whileExpr;
    struct {
      ValueExpr *init;
      ValueExpr *condition;
      ValueExpr *update;
      ValueExpr *body;
    } forExpr;
    struct {
      ValueExpr *function;
      ValueExpr *arguments;
      size_t arguments_length;
    } callExpr;
    struct {
      ValueExpr *value;
    } returnExpr;
    struct Match_s {
      ValueExpr *value;
      MatchCaseExpr *cases;
      size_t cases_length;
    } matchExpr;
    struct Group_s {
      ValueExpr *value;
    } groupExpr;
    struct Block_s {
      Stmnt *statements;
      size_t statements_length;
      bool suppress_value;
    } blockExpr;
  };
} ValueExpr;

typedef struct Stmnt_s {
  StmntKind kind;
  Span span;
  Diagnostic *diagnostics;
  size_t diagnostics_length;
  union {
    struct {
      char *name;
      Binding *params;
      size_t params_length;
      TypeExpr *type;
      ValueExpr *body;
    } fnDecl;
    struct {
      Binding *binding;
      ValueExpr *value;
    } varDecl;
    struct {
      ValueExpr *lvalue;
      ValueExpr *rvalue;
    } assignStmnt;
    struct {
      TypeExpr *type;
      char *name;
    } aliasStmnt;
    struct {
      ValueExpr *value;
    } exprStmnt;
  };
} Stmnt;

typedef struct TranslationUnit_s {
  Span span;               // span of the translation unit
  Diagnostic *diagnostics; // any errors that occur during parsing
  size_t diagnostics_length;
  Stmnt *statements;          // The top level is just a series of statements
  uint64_t statements_length; // The number of statements
} TranslationUnit;

#endif
