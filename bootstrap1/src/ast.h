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
  ast_LK_Omitted,
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
typedef struct ast_Stmnt_s ast_Stmnt;

typedef enum {
  ast_CEK_None,
  ast_CEK_Element,
} ast_CompoundElementKind;

typedef struct {
  ast_Common common;
  ast_CompoundElementKind kind;
  union {
    struct {
      ast_Identifier *name;
      ast_Expr *val;
    } element;
  };
} ast_CompoundElement;

typedef enum {
  ast_EUOK_Not,
  ast_EUOK_Ref,
  ast_EUOK_Deref,
  ast_EUOK_Copy,
  ast_EUOK_Neg,
  ast_EUOK_Pos,
} ast_ExprUnaryOpKind;

typedef enum {
  // scoping & precedence
  ast_EBOK_In,
  // Type coercion
  ast_EBOK_Constrain,
  // Function
  ast_EBOK_Fn,
  ast_EBOK_Call,
  ast_EBOK_Pipe,
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
  // Range
  ast_EBOK_Range,
  ast_EBOK_RangeInclusive,
} ast_ExprBinaryOpKind;

typedef enum {
  ast_EK_None,        // An error when parsing
  ast_EK_Int,         // Literal for an integer number
  ast_EK_Real,        // Literal for a real (floating point) number
  ast_EK_String,      // A string literal
  ast_EK_Loop,        // Loops until a scope is returned
  ast_EK_Struct,      // Constructs a new compound type
  ast_EK_BinaryOp,    // Binary operation
  ast_EK_UnaryOp,     // Unary operation
  ast_EK_Ret,         // Returns from a scope with a value
  ast_EK_Match,       // Matches an expression to the first matching pattern and
                      // destructures it
  ast_EK_Block,       // Groups together several statements, returning last val
  ast_EK_FieldAccess, // Accessing the field of a record object
  ast_EK_Reference,   // A reference to a previously defined variable
  ast_EK_BindIgnore,  // (PATTERN ONLY) ignores a single element
  ast_EK_Bind,        // (PATTERN ONLY) binds a single element to new variable
  ast_EK_AtBind,      // (PATTERN ONLY) matches previous
  ast_EK_Mutate,      // (PATTERN ONLY) matches a single variable and mutates the variable
  ast_EK_AtMutate,    // (PATTERN ONLY) matches previous and then mutates variable
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
      ast_CompoundElement *elements;
      usize elements_len;
    } structLiteral;
    struct {
      ast_Expr *body;
      ast_Label *label;
    } loop;
    struct {
      ast_Expr *root;
      ast_Identifier *field;
    } fieldAccess;
    struct {
      ast_Identifier *reference;
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
      ast_Expr *expr;
      ast_Label *label;
    } ret;
    struct {
      ast_Expr *root;
      ast_Expr *cases;
      usize cases_len;
    } match;
    struct {
      ast_Label *label;
      ast_Stmnt *stmnts;
      usize stmnts_len;
    } block;
    struct {
      ast_Identifier *mutate;
    } mutate;
    struct {
      ast_Expr *pat;
      ast_Identifier *mutate;
    } atMutate;
    struct {
      ast_Identifier *binding;
    } binding;
    struct {
      ast_Expr *pat;
      ast_Identifier *binding;
    } atBinding;
  };
} ast_Expr;

typedef enum {
  ast_SK_None,
  ast_SK_Assign,
  ast_SK_Expr,
  ast_SK_Defer,
} ast_StmntKind;

typedef struct ast_Stmnt_s {
  ast_Common common;
  ast_StmntKind kind;
  union {
    // Declarations
    struct {
      ast_Expr *pat;
      ast_Expr *val;
    } assign;
    struct {
      ast_Expr *expr;
    } expr;
    struct {
      ast_Label *label;
      ast_Expr *val;
    } defer;
  };
} ast_Stmnt;

com_str ast_strExprKind(ast_ExprKind val);
com_str ast_strIdentifierKind(ast_IdentifierKind val);
com_str ast_strLabelKind(ast_LabelKind val);
com_str ast_strCompoundElementKind(ast_CompoundElementKind val);
com_str ast_strExprUnaryOpKind(ast_ExprUnaryOpKind val);
com_str ast_strExprBinaryOpKind(ast_ExprBinaryOpKind val);
com_str ast_strStmntKind(ast_StmntKind val);

#endif
