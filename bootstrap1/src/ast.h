#ifndef AST_H
#define AST_H

#include "com_bigdecimal.h"
#include "com_bigint.h"
#include "com_define.h"
#include "com_loc.h"
#include "com_str.h"

typedef struct ast_Expr_s ast_Expr;
typedef struct ast_Pat_s ast_Pat;
typedef struct ast_Stmnt_s ast_Stmnt;

typedef enum {
  ast_RK_None,      // some kind of error
  ast_RK_Reference, // an actual reference
} ast_ReferenceKind;

typedef struct ast_Reference_s {
  com_loc_Span span;
  ast_ReferenceKind kind;
  union {
    struct {
      com_str name;
    } reference;
  };
} ast_Reference;

typedef enum {
  ast_BK_None,
  ast_BK_Bind,
  ast_BK_Ignore,
} ast_BindingKind;

typedef struct {
  com_loc_Span span;
  ast_BindingKind kind;
  union {
    struct {
      com_str name;
      ast_Expr *constraint;
    } bind;
  };
} ast_Binding;

typedef enum {
  ast_FK_None,  // some kind of error
  ast_FK_Field, // an actual reference
} ast_FieldKind;

typedef struct ast_Field_s {
  com_loc_Span span;
  ast_FieldKind kind;
  union {
    struct {
      com_str name;
    } field;
  };
} ast_Field;

typedef struct {
  com_loc_Span span;
  bool significant;
  com_str data;
} ast_Metadata;

typedef struct {
  com_loc_Span span;
  ast_Metadata *metadata;
  usize metadata_len;
} ast_Common;

typedef enum {
  ast_PK_None,        // Error type
  ast_PK_Wildcard,    // ignores a single element
  ast_PK_Reference,   // compares element to external variable
  ast_PK_Let,         // binds a single element to new variable
  ast_PK_LetExpr,     // matches previous
  ast_PK_Record,      // a container for struct based patterns
  ast_PK_New,         // Destructure
  ast_PK_Group,       // {}
  ast_PK_UnaryOp,     // !
  ast_PK_BinaryOp,    // , |
  ast_PK_Constructor, // Expr constructor with matching
} ast_PatKind;

typedef enum {
  ast_PEBOK_Product, // Matches if the scrutinee is a product type, and attempts
                     // to match subexpression
  ast_PEBOK_Sum,     // Tries to match LHS, then tries to match RHS
} ast_PatBinaryOpKind;

typedef enum {
  ast_PEUOK_Not,
  ast_PEUOK_Ref,
  ast_PEUOK_Deref,
} ast_PatUnaryOpKind;

typedef struct ast_Pat_s {
  ast_Common common;

  ast_PatKind kind;
  union {
    struct {
      ast_Reference *path;
    } reference;
    struct {
      ast_Binding *binding;
    } let;
    struct {
      ast_Pat *pat;
      ast_Binding *binding;
    } letExpr;
    struct {
      ast_Pat *pat;
      ast_Field *field;
    } record;
    struct {
      ast_Pat *inner;
    } group;
    struct {
      ast_PatUnaryOpKind op;
      ast_Pat *operand;
    } unaryOp;
    struct {
      ast_PatBinaryOpKind op;
      ast_Pat *left_operand;
      ast_Pat *right_operand;
    } binaryOp;
  };
} ast_Pat;

typedef enum {
  ast_LBK_Omitted,
  ast_LBK_Label,
} ast_LabelBindingKind;

typedef struct {
  com_loc_Span span;
  ast_LabelBindingKind kind;
  union {
    struct {
      com_str label;
    } label;
  };
} ast_LabelBinding;

typedef enum {
  ast_LRK_None,
  ast_LRK_Label,
} ast_LabelReferenceKind;

typedef struct {
  com_loc_Span span;
  ast_LabelReferenceKind kind;
  union {
    struct {
      com_str label;
    } label;
  };
} ast_LabelReference;

typedef enum {
  ast_MCK_None,
  ast_MCK_Case,
} ast_MatchCaseKind;

typedef struct {
  ast_Common common;
  ast_MatchCaseKind kind;
  union {
    struct {
      ast_Pat *pat;
      ast_Expr *val;
    } matchCase;
  };
} ast_MatchCase;

typedef enum {
  ast_EUOK_Not,
  ast_EUOK_Ref,
  ast_EUOK_Deref,
  ast_EUOK_IneqGreater,
  ast_EUOK_IneqLesser,
  ast_EUOK_IneqLesserInclusive,
} ast_ExprUnaryOpKind;

typedef enum {
  // Math
  ast_EBOK_Add,
  ast_EBOK_Sub,
  ast_EBOK_Mul,
  ast_EBOK_Div,
  ast_EBOK_Rem,
  // Booleans
  ast_EBOK_And,
  ast_EBOK_Or,
  ast_EBOK_Xor,
  // Comparison
  ast_EBOK_CompEqual,
  ast_EBOK_CompNotEqual,
  ast_EBOK_CompLess,
  ast_EBOK_CompLessEqual,
  ast_EBOK_CompGreater,
  ast_EBOK_CompGreaterEqual,
  // Set Operations
  ast_EBOK_Union,
  ast_EBOK_Difference,
  ast_EBOK_Intersection,
  ast_EBOK_SymDifference,
  // Type Manipulation
  ast_EBOK_Product,
  ast_EBOK_Sum,
  // Misc
  ast_EBOK_Assign,
  ast_EBOK_Range,
  ast_EBOK_RangeInclusive,
} ast_ExprBinaryOpKind;

typedef enum {
  ast_EK_None,
  ast_EK_Omitted,
  ast_EK_Lhs,
  ast_EK_NeverType,
  ast_EK_IntLiteral,
  ast_EK_RealLiteral,
  ast_EK_StringLiteral,
  ast_EK_Fn,
  ast_EK_FnType,
  ast_EK_Loop,
  ast_EK_Record,
  ast_EK_BinaryOp,
  ast_EK_UnaryOp,
  ast_EK_Call,
  ast_EK_Pipe,
  ast_EK_Ret,
  ast_EK_Match,
  ast_EK_Block,
  ast_EK_FieldAccess,
  ast_EK_Reference,
  ast_EK_Has,
} ast_ExprKind;

typedef struct ast_Expr_s {
  ast_Common common;
  ast_ExprKind kind;
  union {
    struct {
      bool value;
    } boolLiteral;
    struct {
      com_bigint value;
    } intLiteral;
    struct {
      com_bigdecimal value;
    } realLiteral;
    struct {
      com_str value;
    } stringLiteral;
    struct {
      ast_Field *field;
      ast_Expr *val;
    } record;
    struct {
      ast_Expr *body;
      ast_LabelBinding *label;
    } loop;
    struct {
      ast_Expr *root;
      ast_Field *field;
    } fieldAccess;
    struct {
      ast_Expr *root;
      ast_Expr *fn;
      ast_Expr *parameters;
      usize parameters_len;
    } pipe;
    struct {
      ast_Reference *path;
    } reference;
    struct {
      ast_ExprUnaryOpKind op;
      ast_Expr *operand;
    } unaryOp;
    struct {
      ast_ExprBinaryOpKind op;
      ast_Expr *left_operand;
      ast_Expr *right_operand;
    } binaryOp;
    struct {
      ast_Expr *function;
      ast_Expr *parameters;
      usize parameters_len;
    } call;
    struct {
      ast_Binding *name;
      ast_Pat *parameters;
      usize parameters_len;
      ast_Expr *type;
      ast_Expr *body;
    } fn;
    struct {
      ast_Expr *parameters;
      usize parameters_len;
      ast_Expr *type;
    } fnType;
    struct {
      ast_Expr *expr;
      ast_LabelReference *label;
    } ret;
    struct {
      ast_Expr *root;
      ast_MatchCase *cases;
      usize cases_len;
    } match;
    struct {
      ast_LabelBinding *label;
      ast_Stmnt *stmnts;
      usize stmnts_len;
    } block;
  };
} ast_Expr;

typedef enum {
  ast_SK_None,
  ast_SK_Use,
  ast_SK_Mod,
  ast_SK_Def,
  ast_SK_Expr,
  ast_SK_DeferStmnt,
} ast_StmntKind;

typedef struct ast_Stmnt_s {
  ast_Common common;
  ast_StmntKind kind;
  union {
    // Declarations
    struct {
      ast_Pat *pat;
      ast_Expr *val;
    } def;
    struct {
      ast_Reference *path;
    } useStmnt;
    struct {
      ast_Binding *name;
      ast_Stmnt *stmnts;
      usize stmnts_len;
    } mod;
    struct {
      ast_Expr *expr;
    } expr;
    struct {
      ast_LabelReference *label;
      ast_Expr *val;
    } deferStmnt;
  };
} ast_Stmnt;

com_str ast_strPatKind(ast_PatKind val);
com_str ast_strPatBinaryOpKind(ast_PatBinaryOpKind val);
com_str ast_strExprKind(ast_ExprKind val);
com_str ast_strLabelReferenceKind(ast_LabelReferenceKind val);
com_str ast_strLabelBindingKind(ast_LabelBindingKind val);
com_str ast_strMatchCaseKind(ast_MatchCaseKind val);
com_str ast_strExprUnaryOpKind(ast_ExprUnaryOpKind val);
com_str ast_strExprBinaryOpKind(ast_ExprBinaryOpKind val);
com_str ast_strStmntKind(ast_StmntKind val);
com_str ast_strPatUnaryOpKind(ast_PatUnaryOpKind val);
com_str ast_strBindingKind(ast_BindingKind val);
com_str ast_strReferenceKind(ast_ReferenceKind val);

#endif
