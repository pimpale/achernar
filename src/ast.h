#ifndef AST_H_
#define AST_H_

#include <stdbool.h>
#include <stdint.h>

#include "error.h"
#include "lexer.h"
#include "token.h"

typedef enum {
  S_FnDecl,
  S_VarDecl,
  S_StructDecl,
  S_TypeAliasDecl,
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
  VE_Pass,
  VE_Break,
  VE_Continue,
  VE_Return,
  VE_Match,
  VE_Block,
  VE_Group,
  VE_FieldAccess,
  VE_Reference,
} ValueExprKind;

typedef enum {
  TE_Type,   // type
  TE_Typeof, // typeof
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
  EBO_ArrayAccess,      // []
  EBO_Pipeline,         // ->
} ExprBinOpKind;

typedef enum {
  EUO_Negate,     // -
  EUO_Posit,      // +
  EUO_LogicalNot, // !
  EUO_BitNot,     // ~
  EUO_Ref,        // $
  EUO_Deref       // @
} ExprUnOpKind;

typedef struct TypeExpr_s TypeExpr;
typedef struct ValueExpr_s ValueExpr;
typedef struct StructEntryExpr_s StructEntryExpr;
typedef struct MatchCaseExpr_s MatchCaseExpr;
typedef struct Binding_s Binding;
typedef struct Stmnt_s Stmnt;

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
  union {
    struct {
      char *name;
      uint64_t ptrCount;
    } type;
    struct {
      ValueExpr *value;
    } typeofExpr;
  };
} TypeExpr;

typedef struct Binding_s {
  Span span;
  Diagnostic diagnostic;
  // Elements
  char *name;
  TypeExpr *type;
} Binding;

// Expressions and operations yielding a match case
typedef struct MatchCaseExpr_s {
  Span span;
  Diagnostic diagnostic;
  ValueExpr *pattern;
  ValueExpr *value;
} MatchCaseExpr;

// Expressions and operations yielding a struct entry case
typedef struct StructEntryExpr_s {
  Span span;
  Diagnostic diagnostic;
  char *name;
  ValueExpr *value;
} StructEntryExpr;

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
      char *identifier;
    } reference;
    struct {
      ExprUnOpKind operator;
      ValueExpr *operand;
    } unaryOp;
    struct {
      ExprBinOpKind operator;
      ValueExpr *operand_1;
      ValueExpr *operand_2;
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
      bool has_value;
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
      bool trailing_semicolon;
    } blockExpr;
  };
} ValueExpr;

typedef struct Stmnt_s {
  StmntKind kind;
  Span span;
  Diagnostic diagnostic;
  union {
    struct {
      char *name;
      Binding *params;
      size_t params_length;
      TypeExpr *type;
      ValueExpr *body;
    } fnDecl;
    struct {
      bool has_name;
      char* name;
      Binding *members;
      size_t members_length;
      bool trailing_comma;
    } structDecl;
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
      char* name;
    } aliasStmnt;
    struct {
      ValueExpr *value;
    } exprStmnt;
  };
} Stmnt;

typedef struct TranslationUnit_s {
  Span span; // span of the translation unit
  Diagnostic diagnostic;      // any errors that occur during parsing
  Stmnt *statements;          // The top level is just a series of statements
  uint64_t statements_length; // The number of statements
} TranslationUnit;

void parseTranslationUnit(TranslationUnit *tup, BufferedLexer *blp);
char* printTranslationUnit(TranslationUnit *tup);

#endif
