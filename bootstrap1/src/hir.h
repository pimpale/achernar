#ifndef HIR_H
#define HIR_H

#include <stdint.h>

#include "ast.h"
#include "vector.h"


typedef struct {
  size_t id;
} hir_Binding;

typedef struct {
  size_t id;
} hir_Reference;

typedef struct {
  size_t id;
} hir_Field;

typedef struct hir_Type_s hir_Type;
typedef struct hir_Val_s hir_Val;
typedef struct hir_Pat_s hir_Pat;
typedef struct hir_Stmnt_s hir_Stmnt;

typedef enum {
  hir_PEVRK_CompEqual,        // ==
  hir_PEVRK_CompNotEqual,     // !=
  hir_PEVRK_CompLess,         // <
  hir_PEVRK_CompLessEqual,    // <=
  hir_PEVRK_CompGreater,      // >
  hir_PEVRK_CompGreaterEqual, // >=
} hir_PatValRestrictionKind;

typedef enum {
  hir_PK_None,                   // Error type
  hir_PK_Macro,                  // a macro representing a pattern
  hir_PK_ValRestriction,         // matches a constant val
  hir_PK_TypeRestriction,        // matches a type, without binding
  hir_PK_TypeRestrictionBinding, // matches a type, and binds it
  hir_PK_Struct,                 // a container for struct based patterns
  hir_PK_Group,                  // ()
  hir_PK_UnaryOp,                // !
  hir_PK_BinaryOp,               // , |
} hir_PatKind;

typedef enum {
  hir_PEBOK_Tuple,
  hir_PEBOK_Union,
  hir_PEBOK_And,
  hir_PEBOK_Or,
} hir_PatBinaryOpKind;

typedef enum { hir_PEUOK_Not } hir_PatUnaryOpKind;

typedef enum {
  hir_PSMK_Dummy,
  hir_PSMK_Field,
  hir_PSMK_Rest,
} hir_PatStructMemberKind;

typedef struct {
  // where it came from
  ast_PatStructMember* source;

  hir_PatStructMemberKind kind;
  union {
    struct {
      hir_Pat *pattern;
      hir_Field *field;
    } field;
    struct {
      hir_Pat *pattern;
    } rest;
  };
} hir_PatStructMember;

typedef struct hir_Pat_s {
  ast_Pat* source;

  hir_PatKind kind;
  union {
    struct {
      hir_PatValRestrictionKind restriction;
      ast_Val *val;
    } valRestriction;
    struct {
      ast_Macro *macro;
    } macro;
    struct {
      ast_Type *type;
    } typeRestriction;
    struct {
      hir_Type *type;
      hir_Binding name;
    } typeRestrictionBinding;
    struct {
      ast_PatStructMember *members;
      size_t members_len;
    } structExpr;
    struct {
      hir_PatUnaryOpKind op;
      hir_Pat *operand;
    } unaryOp;
    struct {
      hir_PatBinaryOpKind op;
      hir_Pat *left_operand;
      hir_Pat *right_operand;
    } binaryOp;
  };
} hir_Pat;


#endif

