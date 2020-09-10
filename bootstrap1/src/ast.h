#ifndef AST_H
#define AST_H

#include "com_define.h"
#include "com_loc.h"
#include "com_str.h"
#include "token.h"

typedef struct {
  com_loc_Span span;
  bool significant;
  com_str data;
} ast_Metadata;

typedef enum {
  ast_RK_None,      // some kind of error
  ast_RK_Reference, // an actual reference
} ast_ReferenceKind;

typedef struct ast_Reference_s {
  com_loc_Span span;
  ast_ReferenceKind kind;
  union {
    struct {
      com_str* segments;
      usize segments_len;
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
      com_str val;
    } bind;
  };
} ast_Binding;

typedef enum {
  ast_FK_None,
  ast_FK_FieldStr,
  ast_FK_FieldInt,
} ast_FieldKind;

typedef struct {
  com_loc_Span span;
  ast_FieldKind kind;
  union {
    struct {
      com_str val;
    } strField;
  };
} ast_Field;

typedef struct {
  com_loc_Span span;
  ast_Metadata *metadata;
  usize metadata_len;
} ast_Common;

typedef struct ast_Expr_s ast_Expr;
typedef struct ast_Pat_s ast_Pat;
typedef struct ast_Stmnt_s ast_Stmnt;

typedef enum {
  ast_PK_None,            // Error type
  ast_PK_ExprBinding,     // binds a whole subexpression, optionally constraining type 
  ast_PK_Record,          // a container for struct based patterns
  ast_PK_Range,           // Matches a range of integers or numbers
  ast_PK_Group,           // {}
  ast_PK_UnaryOp,         // !
  ast_PK_BinaryOp,        // , |
  ast_PK_Constructor,     // Expr constructor with matching  
  ast_PK_Reference,       // Matches if it is equal
} ast_PatKind;

typedef enum {
  ast_PEBOK_Product, // Matches if the scrutinee is a product type, and attempts to match subexpression
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
      ast_Binding *binding;
      ast_Expr *type;
    } valBinding;
    struct {
      ast_Pat* pat;
      ast_Binding *binding;
      ast_Expr *type;
    } subExprBinding;
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
      ast_Pat *pattern;
      ast_Expr *val;
    } matchCase;
  };

} ast_MatchCase;

typedef enum {
  ast_VEUOK_Not,
  ast_VEUOK_Ref,
  ast_VEUOK_Deref,
} ast_ExprUnaryOpKind;

typedef enum {
  ast_VEBOK_Add,
  ast_VEBOK_Sub,
  ast_VEBOK_Mul,
  ast_VEBOK_IDiv,
  ast_VEBOK_FDiv,
  ast_VEBOK_IRem,
  ast_VEBOK_FRem,
  ast_VEBOK_And,
  ast_VEBOK_Or,
  ast_VEBOK_Xor,
  ast_VEBOK_CompEqual,
  ast_VEBOK_CompNotEqual,
  ast_VEBOK_CompLess,
  ast_VEBOK_CompLessEqual,
  ast_VEBOK_CompGreater,
  ast_VEBOK_CompGreaterEqual,
  ast_VEBOK_Pipeline,
  ast_VEBOK_Assign,
  ast_VEBOK_AssignAdd,
  ast_VEBOK_AssignSub,
  ast_VEBOK_AssignMul,
  ast_VEBOK_AssignIDiv,
  ast_VEBOK_AssignFDiv,
  ast_VEBOK_AssignIRem,
  ast_VEBOK_AssignFRem,
  ast_VEBOK_Product,
  ast_VEBOK_Union,
  ast_VEBOK_Intersection,
} ast_ExprBinaryOpKind;

typedef enum {
  ast_VK_None,
  ast_VK_Omitted,
  ast_VK_NilLiteral,
  ast_VK_BoolLiteral,
  ast_VK_IntLiteral,
  ast_VK_FloatLiteral,
  ast_VK_CharLiteral,
  ast_VK_Fn,
  ast_VK_Loop,
  ast_VK_As,
  ast_VK_Record,
  ast_VK_StringLiteral,
  ast_VK_BinaryOp,
  ast_VK_UnaryOp,
  ast_VK_Call,
  ast_VK_Pipe,
  ast_VK_Ret,
  ast_VK_Match,
  ast_VK_Block,
  ast_VK_FieldAccess,
  ast_VK_Reference,
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
    } floatLiteral;
    struct {
      u8 value;
    } charLiteral;
    struct {
      com_str value;
    } stringLiteral;
    struct {
      ast_Field *field;
      ast_Expr *val;
    } record;
    struct {
      ast_Expr *root;
      ast_Expr *type;
    } as;
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
      ast_Binding* name;
      ast_Pat *parameters;
      usize parameters_len;
      ast_Expr *type;
      ast_Expr *body;
    } fn;
    struct {
      ast_Expr *value;
      ast_LabelReference *label;
    } returnExpr;
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
  ast_SK_ExprDecl,
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
    } valDecl;
    struct {
      ast_Reference *path;
    } useStmnt;
    struct {
      ast_Binding *name;
      ast_Stmnt *stmnts;
      usize stmnts_len;
    } modStmnt;
    struct {
      ast_Expr *val;
    } val;
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
com_str ast_strFieldKind(ast_FieldKind val);
com_str ast_strReferenceKind(ast_ReferenceKind val);

#endif
