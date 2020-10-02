#ifndef HIR_H
#define HIR_H

#include "com_bigdecimal.h"
#include "com_bigint.h"
#include "com_define.h"
#include "com_loc.h"
#include "com_str.h"

typedef enum {
  hir_IK_None,
  hir_IK_Identifier,
} hir_IdentifierKind;

typedef struct {
  com_loc_Span span;
  hir_IdentifierKind kind;
  union {
    struct {
      com_str name;
    } id;
  };
} hir_Identifier;

typedef struct {
  com_loc_Span span;
  bool significant;
  com_str data;
} hir_Metadata;

typedef struct {
  com_loc_Span span;
  hir_Metadata *metadata;
  usize metadata_len;
} hir_Common;

typedef enum {
  hir_LK_None,
  hir_LK_Omitted,
  hir_LK_Label,
} hir_LabelKind;

typedef struct {
  com_loc_Span span;
  hir_LabelKind kind;
  union {
    struct {
      com_str label;
    } label;
  };
} hir_Label;

typedef struct hir_Expr_s hir_Expr;
typedef struct hir_Stmnt_s hir_Stmnt;

typedef enum {
  hir_MCK_None,
  hir_MCK_Case,
} hir_MatchCaseKind;

typedef struct {
  hir_Common common;
  hir_MatchCaseKind kind;
  union {
    struct {
      hir_Expr *pat;
      hir_Expr *val;
    } matchCase;
  };
} hir_MatchCase;

typedef enum {
  hir_CTEK_None,
  hir_CTEK_Element,
} hir_CompoundTypeElementKind;

typedef struct {
  hir_Common common;
  hir_CompoundTypeElementKind kind;
  union {
    struct {
      hir_Identifier *name;
      hir_Expr *type;
    } element;
  };
} hir_CompoundTypeElement;

typedef enum {
  hir_CEK_None,
  hir_CEK_Element,
} hir_CompoundElementKind;

typedef struct {
  hir_Common common;
  hir_CompoundElementKind kind;
  union {
    struct {
      hir_Identifier *name;
      hir_Expr *val;
    } element;
  };
} hir_CompoundElement;

typedef enum {
  hir_EUOK_Not,
  hir_EUOK_Ref,
  hir_EUOK_Deref,
  hir_EUOK_IneqGreater,
  hir_EUOK_IneqLesser,
  hir_EUOK_IneqLesserInclusive,
} hir_ExprUnaryOpKind;

typedef enum {
  // Type assertion
  hir_EBOK_Constrain,
  // Math
  hir_EBOK_Add,
  hir_EBOK_Sub,
  hir_EBOK_Mul,
  hir_EBOK_Div,
  hir_EBOK_Rem,
  // Booleans
  hir_EBOK_And,
  hir_EBOK_Or,
  hir_EBOK_Xor,
  // Comparison
  hir_EBOK_CompEqual,
  hir_EBOK_CompNotEqual,
  hir_EBOK_CompLess,
  hir_EBOK_CompLessEqual,
  hir_EBOK_CompGreater,
  hir_EBOK_CompGreaterEqual,
  // Set Operations
  hir_EBOK_Union,
  hir_EBOK_Difference,
  hir_EBOK_Intersection,
  hir_EBOK_SymDifference,
  // Type Manipulation
  hir_EBOK_Product,
  hir_EBOK_Sum,
  // Misc
  hir_EBOK_Assign,
  hir_EBOK_Range,
  hir_EBOK_RangeInclusive,
} hir_ExprBinaryOpKind;

typedef enum {
  hir_EK_None,        // An error when parsing
  hir_EK_NeverType,   // The type of the special value returned by ret,
                      // neverending loops, and functions that dont return
  hir_EK_Nil,         // Literal for nil
  hir_EK_NilType,     // Literal for the type of nil, Nil
  hir_EK_Int,         // Literal for an integer number
  hir_EK_Real,        // Literal for a real (floating point) number
  hir_EK_String,      // A string literal
  hir_EK_StructType,  // Creates a new struct type from fields
  hir_EK_EnumType,    // Creates a new enum type from fields
  hir_EK_Fn,          // Creates a new function
  hir_EK_FnType,      // Creates a new type of a function
  hir_EK_Loop,        // Loops until a scope is returned
  hir_EK_New,         // Constructs a new compound type of another type
  hir_EK_Struct,      // Constructs a new compound type
  hir_EK_BinaryOp,    // Binary operation
  hir_EK_UnaryOp,     // Unary operation
  hir_EK_Call,        // Call a function with a product type
  hir_EK_Pipe,        // Syntactic sugar operator to evaluate a function postfix
  hir_EK_Ret,         // Returns from a scope with a value
  hir_EK_Match,       // Matches an expression to the first matching pattern and
                      // destructures it
  hir_EK_Block,       // Groups together several statements, returning last val
  hir_EK_FieldAccess, // Accessing the field of a record object
  hir_EK_Reference,   // A reference to a previously defined variable
  hir_EK_BindIgnore,  // (PATTERN ONLY) ignores a single element
  hir_EK_Bind,        // (PATTERN ONLY) binds a single element to new variable
  hir_EK_AtBind,      // (PATTERN ONLY) matches previous
} hir_ExprKind;

typedef struct hir_Expr_s {
  hir_Common common;
  hir_ExprKind kind;
  union {
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
      hir_CompoundTypeElement *elements;
      usize elements_len;
    } structType;
    struct {
      hir_CompoundTypeElement *elements;
      usize elements_len;
    } enumType;
    struct {
      hir_CompoundElement *elements;
      usize elements_len;
    } structLiteral;
    struct {
      hir_Expr *root;
      hir_CompoundElement *elements;
      usize elements_len;
    } new;
    struct {
      hir_Expr *body;
      hir_Label *label;
    } loop;
    struct {
      hir_Expr *root;
      hir_Identifier *field;
    } fieldAccess;
    struct {
      hir_Expr *root;
      hir_Expr *fn;
      hir_Expr *parameters;
    } pipe;
    struct {
      hir_Identifier *reference;
    } reference;
    struct {
      hir_ExprUnaryOpKind op;
      hir_Expr *operand;
    } unaryOp;
    struct {
      hir_ExprBinaryOpKind op;
      hir_Expr *left_operand;
      hir_Expr *right_operand;
    } binaryOp;
    struct {
      hir_Expr *root;
      hir_Expr *parameters;
    } call;
    struct {
      hir_Expr *parameters;
      hir_Expr *body;
    } fn;
    struct {
      hir_Expr *parameters;
      hir_Expr *type;
    } fnType;
    struct {
      hir_Expr *expr;
      hir_Label *label;
    } ret;
    struct {
      hir_Expr *root;
      hir_MatchCase *cases;
      usize cases_len;
    } match;
    struct {
      hir_Label *label;
      hir_Stmnt *stmnts;
      usize stmnts_len;
    } block;
    struct {
      hir_Identifier *binding;
    } binding;
    struct {
      hir_Expr *pat;
      hir_Identifier *binding;
    } atBinding;
  };
} hir_Expr;

typedef enum {
  hir_SK_None,
  hir_SK_Let,
  hir_SK_Def,
  hir_SK_Expr,
  hir_SK_Defer,
} hir_StmntKind;

typedef struct hir_Stmnt_s {
  hir_Common common;
  hir_StmntKind kind;
  union {
    // Declarations
    struct {
      hir_Expr *pat;
      hir_Expr *val;
    } let;
    struct {
      hir_Expr *pat;
    } def;
    struct {
      hir_Expr *expr;
    } expr;
    struct {
      hir_Label *label;
      hir_Expr *val;
    } defer;
  };
} hir_Stmnt;

com_str hir_strExprKind(hir_ExprKind val);
com_str hir_strIdentifierKind(hir_IdentifierKind val);
com_str hir_strLabelKind(hir_LabelKind val);
com_str hir_strMatchCaseKind(hir_MatchCaseKind val);
com_str hir_strCompoundTypeElementKind(hir_CompoundTypeElementKind val);
com_str hir_strCompoundElementKind(hir_CompoundElementKind val);
com_str hir_strExprUnaryOpKind(hir_ExprUnaryOpKind val);
com_str hir_strExprBinaryOpKind(hir_ExprBinaryOpKind val);
com_str hir_strStmntKind(hir_StmntKind val);

#endif
