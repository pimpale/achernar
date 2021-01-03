#ifndef AST_H
#define AST_H

#include "com_bigdecimal.h"
#include "com_bigint.h"
#include "com_define.h"
#include "com_loc.h"
#include "com_str.h"
#include "token.h"

typedef enum {
  ast_IK_None,
  ast_IK_Identifier,
} ast_IdentifierKind;

typedef struct {
  com_loc_Span span;
  ast_IdentifierKind kind;
  union {
    struct {
      com_str name;
    } id;
  };
} ast_Identifier;

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
  ast_LK_None,
  ast_LK_Label,
} ast_LabelKind;

typedef struct {
  com_loc_Span span;
  ast_LabelKind kind;
  union {
    struct {
      com_str label;
    } label;
  };
} ast_Label;

typedef struct ast_Expr_s ast_Expr;

typedef enum {
  ast_EBOK_None,
  // Type coercion
  ast_EBOK_Constrain,
  // Function definition
  ast_EBOK_Defun,
  // CaseOption
  ast_EBOK_CaseOption,
  // Function call
  ast_EBOK_Apply,
  ast_EBOK_RevApply,
  // Function composition
  ast_EBOK_Compose,
  // Function Piping
  ast_EBOK_PipeForward,
  ast_EBOK_PipeBackward,
  // Math
  ast_EBOK_Add,
  ast_EBOK_Sub,
  ast_EBOK_Mul,
  ast_EBOK_Div,
  ast_EBOK_Rem,
  ast_EBOK_Pow,
  // Booleans
  ast_EBOK_And,
  ast_EBOK_Or,
  // Comparison
  ast_EBOK_CompEqual,
  ast_EBOK_CompNotEqual,
  ast_EBOK_CompLess,
  ast_EBOK_CompLessEqual,
  ast_EBOK_CompGreater,
  ast_EBOK_CompGreaterEqual,
  // Set Operations
  ast_EBOK_Union,
  ast_EBOK_Intersection,
  ast_EBOK_Difference,
  ast_EBOK_In,
  // Type Manipulation
  ast_EBOK_Cons,
  ast_EBOK_Sum,
  // Range
  ast_EBOK_Range,
  ast_EBOK_RangeInclusive,
  // Assign
  ast_EBOK_Assign,
  // Sequence
  ast_EBOK_Sequence,
  // Pattern rename
  ast_EBOK_As,
  // Module Access
  ast_EBOK_ModuleAccess,
} ast_ExprBinaryOpKind;

typedef enum {
  ast_EK_None,     // An error when parsing
  ast_EK_Void,      // Literal for void
  ast_EK_VoidType,  // Literal for void type
  ast_EK_NeverType, // Literal for never type
  ast_EK_Int,      // Literal for an integer number
  ast_EK_Bool,     // Literal for a boolean
  ast_EK_Real,     // Literal for a real (floating point) number
  ast_EK_String,   // A string literal
  ast_EK_Loop,     // Loops until a scope is returned
  ast_EK_Val,      // embed a computed value in a pattern
  ast_EK_Pat,      // quote a pattern
  ast_EK_Label,    // Wraps a term in a label that can be deferred or returned from
  ast_EK_Defer,    // Defer until label
  ast_EK_Struct,   // Constructs a new compound type
  ast_EK_BinaryOp, // Binary operation
  ast_EK_Ret,      // Returns from a scope with a value
  ast_EK_IfThen,       // if then else expression
  ast_EK_CaseOf,   // Matches an expression to the first matching pattern and
                   // destructures it
  ast_EK_Group,    // Introduces new scope and label
  ast_EK_Reference,    // A reference to a previously defined variable
  ast_EK_BindIgnore,   // (PATTERN ONLY) ignores a single element
  ast_EK_Bind,         // (PATTERN ONLY) matches a single element to new variable
  ast_EK_BindSplat,    // (PATTERN ONLY) Automagically deconstructs and binds a struct
} ast_ExprKind;

typedef struct ast_Expr_s {
  ast_Common common;
  ast_ExprKind kind;
  union {
    struct {
      com_bigint value;
    } intLiteral;
    struct {
      bool value;
    } boolLiteral;
    struct {
      com_bigdecimal value;
    } realLiteral;
    struct {
      com_str value;
      tk_StringLiteralKind kind;
    } stringLiteral;
    struct {
      ast_Expr *expr;
    } structLiteral;
    struct {
      ast_Expr *body;
    } loop;
    struct {
      ast_Expr *val;
    } val;
    struct {
      ast_Expr *pat;
    } pat;
    struct {
      ast_Label *label;
      ast_Expr *val;
    } label;
    struct {
      ast_Identifier *reference;
    } reference;
    struct {
      ast_ExprBinaryOpKind op;
      ast_Expr *left_operand;
      ast_Expr *right_operand;
    } binaryOp;
    struct {
      ast_Expr *expr;
      ast_Label *label;
    } ret;
    struct {
      ast_Expr *expr;
      ast_Expr *cases;
    } caseof;
    struct {
      ast_Expr *expr;
      ast_Expr *then_expr;
      ast_Expr *else_expr;
    } ifthen;
    struct {
      ast_Expr *expr;
    } group;
    struct {
      ast_Label *label;
      ast_Expr *val;
    } defer;
    struct {
      ast_Identifier *bind;
    } bind;
    struct {
      ast_Identifier *mutate;
    } mutate;
  };
} ast_Expr;

com_str ast_strExprKind(ast_ExprKind val);
com_str ast_strIdentifierKind(ast_IdentifierKind val);
com_str ast_strLabelKind(ast_LabelKind val);
com_str ast_strExprBinaryOpKind(ast_ExprBinaryOpKind val);

#endif
