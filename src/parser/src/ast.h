#ifndef AST_H
#define AST_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "lncol.h"
#include "token.h"

typedef struct  {
  Span span;
  char *scope;
  char *data;
} Comment;

typedef struct  {
  Span span;
  Comment *comments;
  size_t comments_len;
} AstNode;

typedef struct  {
  AstNode node;

  char *name;
  Token *tokens;
  size_t tokens_len;
} MacroExpr;

typedef struct  {
  AstNode node;

  char **pathSegments;
  size_t pathSegments_len;
} Path;

typedef struct TypeExpr_s TypeExpr;
typedef struct ValExpr_s ValExpr;
typedef struct PatExpr_s PatExpr;
typedef struct Stmnt_s Stmnt;

typedef enum PatExprValRestrictionKind_e {
  PEVRK_CompEqual,        // ==
  PEVRK_CompNotEqual,     // !=
  PEVRK_CompLess,         // <
  PEVRK_CompLessEqual,    // <=
  PEVRK_CompGreater,      // >
  PEVRK_CompGreaterEqual, // >=
} PatExprValRestrictionKind;

typedef enum {
  PEK_None,            // Error type
  PEK_Macro,           // a macro representing a pattern
  PEK_ValRestriction,  // matches a constant val, and optionally binds it
  PEK_TypeRestriction, // matches a type, and optionally binds it
  PEK_Struct,          // a container for struct based patterns
  PEK_Group,           // ()
  PEK_UnaryOp,         // !
  PEK_BinaryOp,        // , |
} PatExprKind;

typedef enum {
  PEBOK_Tuple,
  PEBOK_Union,
  PEBOK_And,
  PEBOK_Or,
} PatExprBinaryOpKind;

typedef enum { PEUOK_Not } PatExprUnaryOpKind;

typedef enum {
  PSMEK_Field,
  PSMEK_Rest,
  PSMEK_Macro,
} PatStructMemberExprKind;

typedef struct  {
  AstNode node;
  PatStructMemberExprKind kind;
  PatExpr *pattern;
  union {
      struct {
        MacroExpr data;
      } macro;
      struct {
        char *field;
      } field;
  };
} PatStructMemberExpr;

typedef struct PatExpr_s {
  AstNode node;

  PatExprKind kind;
  union {
    struct {
      PatExprValRestrictionKind restriction;
      ValExpr *valExpr;
    } valRestriction;
    struct {
        MacroExpr macro;
    } macro;
    struct {
      bool has_binding;
      char *binding;
      TypeExpr *type;
    } typeRestriction;
    struct {
      PatStructMemberExpr *members;
      size_t members_len;
    } structExpr;
    struct {
      PatExpr *value;
    } groupExpr;
    struct {
      PatExprUnaryOpKind op;
      PatExpr *operand;
    } unaryOp;
    struct {
      PatExprBinaryOpKind op;
      PatExpr *left_operand;
      PatExpr *right_operand;
    } binaryOp;
  };
} PatExpr;

typedef enum {
  TEK_None,        // Error type
  TEK_Omitted,     // Omitted
  TEK_Macro,       //MacroExpr Type
  TEK_Nil,         // Nil type
  TEK_Reference,   // Reference (primitive or aliased or path)
  TEK_Struct,      // struct
  TEK_Fn,          // function pointer
  TEK_UnaryOp,     // & or @
  TEK_BinaryOp,    // , or |
  TEK_FieldAccess, // .
} TypeExprKind;

typedef enum  {
  TSEK_Struct,
  TSEK_Enum,
} TypeStructExprKind;

typedef enum {
  TSMEK_None,
  TSMEK_Macro,
  TSMEK_StructMember
} TypeStructMemberExprKind;

typedef struct TypeStructMemberExpr_s {
  AstNode node;
  TypeStructMemberExprKind kind;

  union {
      struct {
      char *name;
      TypeExpr *type;
      } structMember;
      struct {
         MacroExpr* macro;
      } macro;
  };
} TypeStructMemberExpr;

typedef enum {
  TEUOK_Ref,  // $
  TEUOK_Deref // @
} TypeExprUnaryOpKind;

typedef enum {
  TEBOK_Tuple, // ,
  TEBOK_Union  // |
} TypeExprBinaryOpKind;

// Expressions and operations yielding a type
typedef struct TypeExpr_s {
  AstNode node;
  TypeExprKind kind;

  union {
    struct {
      Path *path;
    } referenceExpr;
    struct {
      TypeStructExprKind *kind;
      TypeStructMemberExpr *members;
      size_t members_len;
    } structExpr;
    struct {
      TypeExpr *parameters;
      size_t parameters_len;
      TypeExpr *type;
    } fnExpr;
    struct {
      TypeExprUnaryOpKind op;
      struct TypeExpr_s *operand;
    } unaryOp;
    struct {
      TypeExprBinaryOpKind op;
      struct TypeExpr_s *left_operand;
      struct TypeExpr_s *right_operand;
    } binaryOp;
    struct {
      struct TypeExpr_s *val;
      char *field;
    } fieldAccess;
  };
} TypeExpr;

typedef enum {
  VEK_None,
  VEK_NilLiteral,
  VEK_BoolLiteral,
  VEK_IntLiteral,
  VEK_FloatLiteral,
  VEK_CharLiteral,
  VEK_Fn,
  VEK_Loop,
  VEK_As,
  VEK_StringLiteral,
  VEK_StructLiteral,
  VEK_BinaryOp,
  VEK_UnaryOp,
  VEK_Call,
  VEK_Continue,
  VEK_Return,
  VEK_Match,
  VEK_Block,
  VEK_FieldAccess,
  VEK_Reference,
} ValExprKind;

typedef enum {
  LEK_Omitted,
  LEK_Label,
} LabelExprKind;

typedef struct {
    AstNode node;
    LabelExprKind kind;
    union {
        struct {
            char *label;
        } label;
    };
} Label;

typedef enum {
  MCK_None,
  MCK_Case,
  MCK_Macro,
} MatchCaseKind;

typedef struct  {
  AstNode node;
  MatchCaseKind kind;

  union {
      struct {
  PatExpr *pattern;
  ValExpr *val;
      } matchCase;
      struct {
          MacroExpr* macro;
      } macro;
  };

} MatchCaseExpr;

typedef enum {
    VSMEK_None,
    VSMEK_Macro,
    VSMEK_ValStructMemberExpr,
} ValStructMemberExprKind;

typedef struct  {
  AstNode node;

  ValStructMemberExprKind kind;

  union {
      struct {
          char *name;
          ValExpr *val;
      } valStructMemberExpr;
      struct {
         MacroExpr *macro;
      } macro;
  };
} ValStructMemberExpr;

typedef enum {
  VEUOK_Negate,
  VEUOK_Posit,
  VEUOK_Not,
  VEUOK_Ref,
  VEUOK_Deref,
} ValExprUnaryOpKind;

typedef enum {
  VEBOK_Add,
  VEBOK_Sub,
  VEBOK_Mul,
  VEBOK_Div,
  VEBOK_Mod,
  VEBOK_And,
  VEBOK_Or,
  VEBOK_CompEqual,
  VEBOK_CompNotEqual,
  VEBOK_CompLess,
  VEBOK_CompLessEqual,
  VEBOK_CompGreater,
  VEBOK_CompGreaterEqual,
  VEBOK_Pipeline,
  VEBOK_Assign,
  VEBOK_AssignAdd,
  VEBOK_AssignSub,
  VEBOK_AssignMul,
  VEBOK_AssignDiv,
  VEBOK_AssignMod,
  VEBOK_Tuple,
} ValExprBinaryOpKind;

typedef struct ValExpr_s {
  AstNode node;
  ValExprKind kind;

  union {
    struct {
      bool value;
    } boolLiteral;
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
      size_t val_len;
    } stringLiteral;
    struct {
      ValStructMemberExpr *members;
      size_t members_len;
    } structExpr;
    struct {
      ValExpr *value;
      TypeExpr *type;
      char *field;
    } asExpr;
    struct {
      ValExpr *value;
      Label *label;
    } loopExpr;
    struct {
      ValExpr *value;
      char *field;
    } fieldAccess;
    struct {
      Path *path;
    } reference;
    struct {
      ValExprUnaryOpKind op;
      ValExpr *operand;
    } unaryOp;
    struct {
      ValExprBinaryOpKind op;
      ValExpr *left_operand;
      ValExpr *right_operand;
    } binaryOp;
    struct {
      ValExpr *function;
      ValExpr *parameters;
      size_t parameters_len;
    } callExpr;
    struct {
      PatExpr *parameters;
      size_t parameters_len;
      TypeExpr *type;
      ValExpr *body;
    } fnExpr;
    struct {
      ValExpr *value;
      Label *label;
    } returnExpr;
    struct {
      ValExpr *val;
      MatchCaseExpr *cases;
      size_t cases_len;
    } matchExpr;
    struct {
      Stmnt *statements;
      size_t statements_len;
      Label *label;
    } blockExpr;
  };
} ValExpr;

typedef enum {
  SK_None,
  SK_Use,
  SK_Macro,
  SK_Namespace,
  SK_ValDecl,
  SK_TypeDecl,
  SK_ValExpr,
  SK_DeferStmnt,
} StmntKind;

typedef struct Stmnt_s {
  AstNode node;
  StmntKind kind;

  size_t comments_len;
  union {
    // Declarations
    struct {
      PatExpr *pattern;
      bool has_val;
      ValExpr *val;
    } valDecl;
    struct {
      TypeExpr *type;
      char *name;
    } typeDecl;
    // Things
    struct {
      Path *path;
    } useStmnt;
    struct {
      Path *path;
      Stmnt *stmnt;
    } namespaceStmnt;
    struct {
        MacroExpr* macro;
    } macroStmnt;
    // Expressions
    struct {
      ValExpr *val;
    } valExpr;
    struct {
      char *scope;
      ValExpr *val;
    } deferStmnt;
  };
} Stmnt;

#endif
