#ifndef AST_H
#define AST_H

#include "com_bigdecimal.h"
#include "com_bigint.h"
#include "com_define.h"
#include "com_loc.h"
#include "com_str.h"
#include "token.h"

typedef struct ast_Expr_s ast_Expr;
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
      ast_Expr *pat;
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
  ast_EK_Self,
  ast_EK_NeverType,
  ast_EK_IntLiteral,    // Literal for an integer number
  ast_EK_RealLiteral,   // A literal for a real (floating point) number
  ast_EK_StringLiteral, // A string literal
  ast_EK_Fn,            // Creates a new function
  ast_EK_FnType,        // Creates a new type of a function
  ast_EK_Loop,          // Loops until a scope is returned
  ast_EK_New,           // Constructs a new record type
  ast_EK_BinaryOp,      // Binary operation
  ast_EK_UnaryOp,       // Unary operation
  ast_EK_Call,          // Call a function with a product type
  ast_EK_Pipe,          // Syntactic sugar operator to evaluate a function postfix
  ast_EK_Ret,           // Returns from a scope with a value
  ast_EK_Match,         // Matches an expression to the first matching pattern and destructures it
  ast_EK_Block,         // Groups together several statements and returns the last statement's value, or nil
  ast_EK_FieldAccess,   // Accessing the field of a record object
  ast_EK_Reference,     // A reference to a previously defined module, function, or variable
  ast_PK_Wildcard,      // (PATTERN) ignores a single element
  ast_PK_Let,           // (PATTERN) binds a single element to new variable
  ast_PK_AtLet,         // (PATTERN) matches previous
} ast_ExprKind;

typedef struct ast_Expr_s {
  ast_Common common;
  ast_ExprKind kind;
  union {
    struct {
      com_bigint value;
    } intLiteral;
    struct {
      com_bigdecimal value;
    } realLiteral;
    struct {
      com_str value;
      tk_StringLiteralKind kind;
    } stringLiteral;
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
      ast_Expr *parameters;
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
    struct {
      ast_Binding *binding;
    } let;
    struct {
      ast_Expr *pat;
      ast_Binding *binding;
    } atLet;  };
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
      ast_Expr *pat;
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

com_str ast_strExprKind(ast_ExprKind val);
com_str ast_strLabelReferenceKind(ast_LabelReferenceKind val);
com_str ast_strLabelBindingKind(ast_LabelBindingKind val);
com_str ast_strMatchCaseKind(ast_MatchCaseKind val);
com_str ast_strExprUnaryOpKind(ast_ExprUnaryOpKind val);
com_str ast_strExprBinaryOpKind(ast_ExprBinaryOpKind val);
com_str ast_strStmntKind(ast_StmntKind val);
com_str ast_strBindingKind(ast_BindingKind val);
com_str ast_strReferenceKind(ast_ReferenceKind val);

#endif
