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
  ast_CTEK_None,
  ast_CTEK_Element,
} ast_CompoundTypeElementKind;

typedef struct {
  ast_Common common;
  ast_CompoundTypeElementKind kind;
  union {
    struct {
      ast_Identifier *name;
      ast_Expr *type;
    } element;
  };
} ast_CompoundTypeElement;

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
  ast_EUOK_IneqGreater,
  ast_EUOK_IneqLesser,
  ast_EUOK_IneqLesserInclusive,
} ast_ExprUnaryOpKind;

typedef enum {
  // Type assertion
  ast_EBOK_Constrain,
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
  ast_EK_None,        // An error when parsing
  ast_EK_NeverType,   // The type of the special value returned by ret,
                      // neverending loops, and functions that dont return
  ast_EK_Nil,         // Literal for nil
  ast_EK_NilType,     // Literal for the type of nil, Nil
  ast_EK_Int,         // Literal for an integer number
  ast_EK_Real,        // Literal for a real (floating point) number
  ast_EK_String,      // A string literal
  ast_EK_StructType,  // Creates a new struct type from fields
  ast_EK_EnumType,    // Creates a new enum type from fields
  ast_EK_Fn,          // Creates a new function
  ast_EK_FnType,      // Creates a new type of a function
  ast_EK_Loop,        // Loops until a scope is returned
  ast_EK_New,         // Constructs a new compound type of another type
  ast_EK_Struct,      // Constructs a new compound type
  ast_EK_BinaryOp,    // Binary operation
  ast_EK_UnaryOp,     // Unary operation
  ast_EK_Call,        // Call a function with a product type
  ast_EK_Pipe,        // Syntactic sugar operator to evaluate a function postfix
  ast_EK_Ret,         // Returns from a scope with a value
  ast_EK_Match,       // Matches an expression to the first matching pattern and
                      // destructures it
  ast_EK_Block,       // Groups together several statements, returning last val
  ast_EK_FieldAccess, // Accessing the field of a record object
  ast_EK_Reference,   // A reference to a previously defined variable
  ast_EK_BindIgnore,  // (PATTERN ONLY) ignores a single element
  ast_EK_Bind,        // (PATTERN ONLY) binds a single element to new variable
  ast_EK_AtBind,      // (PATTERN ONLY) matches previous
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
      ast_CompoundTypeElement *elements;
      usize elements_len;
    } structType;
    struct {
      ast_CompoundTypeElement *elements;
      usize elements_len;
    } enumType;
    struct {
      ast_CompoundElement *elements;
      usize elements_len;
    } structLiteral;
    struct {
      ast_Expr *root;
      ast_CompoundElement *elements;
      usize elements_len;
    } new;
    struct {
      ast_Expr *body;
      ast_Label *label;
    } loop;
    struct {
      ast_Expr *root;
      ast_Identifier *field;
    } fieldAccess;
    struct {
      ast_Expr *root;
      ast_Expr *fn;
      ast_Expr *parameters;
      usize parameters_len;
    } pipe;
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
      ast_Expr *function;
      ast_Expr *parameters;
      usize parameters_len;
    } call;
    struct {
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
      ast_Label *label;
    } ret;
    struct {
      ast_Expr *root;
      ast_MatchCase *cases;
      usize cases_len;
    } match;
    struct {
      ast_Label *label;
      ast_Stmnt *stmnts;
      usize stmnts_len;
    } block;
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
  ast_SK_Let,
  ast_SK_Def,
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
    } let;
    struct {
      ast_Expr *pat;
    } def;
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
com_str ast_strMatchCaseKind(ast_MatchCaseKind val);
com_str ast_strCompoundTypeElementKind(ast_CompoundTypeElementKind val);
com_str ast_strCompoundElementKind(ast_CompoundElementKind val);
com_str ast_strExprUnaryOpKind(ast_ExprUnaryOpKind val);
com_str ast_strExprBinaryOpKind(ast_ExprBinaryOpKind val);
com_str ast_strStmntKind(ast_StmntKind val);

#endif
