#ifndef HIR_H
#define HIR_H

#include "com_bigdecimal.h"
#include "com_bigint.h"
#include "com_define.h"
#include "com_loc.h"
#include "com_str.h"

typedef struct {
  bool generated;
  com_loc_Span span;
  com_str *metadata;
  usize metadata_len;
} hir_Common;

typedef enum {
  hir_IK_None,
  hir_IK_Identifier,
} hir_IdentifierKind;

typedef struct {
  hir_Common common;
  hir_IdentifierKind kind;
  union {
    struct {
      com_str name;
    } id;
  };
} hir_Identifier;

typedef struct hir_Expr_s hir_Expr;
typedef struct hir_Pat_s hir_Pat;
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
  hir_EK_None,        // An error when parsing
  hir_EK_NeverType,   // The type of the special value returned by ret,
                      // neverending loops, and functions that dont return
  hir_EK_Nil,         // Literal for nil
  hir_EK_NilType,     // Literal for the type of nil, Nil
  hir_EK_Int,         // Literal for an integer number
  hir_EK_Real,        // Literal for a real (floating point) number
  hir_EK_String,      // A string literal
  hir_EK_Fn,          // Creates a new function
  hir_EK_FnType,      // Creates a new type of a function
  hir_EK_Loop,        // Loops until a scope is returned
  hir_EK_New,         // Constructs a new compound type of another type
  hir_EK_Struct,      // Constructs a new compound type ad hoc
  hir_EK_Call,        // Call a function with a product type
  hir_EK_Pipe,        // Syntactic sugar operator to evaluate a function postfix
  hir_EK_Ret,         // Returns from a scope with a value
  hir_EK_Match,       // Matches an expression to the first matching pattern and
                      // destructures it
  hir_EK_Block,       // Groups together several statements, returning last val
  hir_EK_FieldAccess, // Accessing the field of a record object
  hir_EK_Reference,   // A reference to a previously defined variable
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

      hir_Expr *defers;
      usize defers_len;
    } loop;
    struct {
      hir_Expr *root;
      hir_Identifier *field;
    } fieldAccess;
    struct {
      hir_Expr *root;
      hir_Expr *type;
    } typeConstrain;
    struct {
      hir_Pat *target;
      hir_Expr* value;
    } assign;
    struct {
      hir_Identifier *reference;
    } reference;
    struct {
      hir_Expr *root;
      hir_Expr *parameters;
    } call;
    struct {
      hir_Pat *parameters;
      hir_Expr *body;
    } fn;
    struct {
      hir_Expr *parameters;
      hir_Expr *type;
    } fnType;
    struct {
      hir_Expr *expr;
      hir_Expr *targetConstruct;
    } ret;
    struct {
      hir_Expr *root;
      hir_MatchCase *cases;
      usize cases_len;
    } match;
    struct {
      hir_Stmnt *stmnts;
      usize stmnts_len;

      hir_Expr *retval;

      hir_Expr *defers;
      usize defers_len;
    } block;
  };
} hir_Expr;

typedef enum {
  hir_PK_None,        // an error in compuation
  hir_PK_Value,       // A Expr with no holes
  hir_PK_RevFn,       // Calling the reverse function
  hir_PK_Wildcard,         // matches a value
  hir_PK_WildcardBind,        // binds a single element to new variable
} hir_PatKind;

typedef struct hir_Pat_s {
  hir_Common common;
  hir_PatKind kind;
  union {
    struct {
      hir_Pat *pat;
    } any;
    struct {
      hir_Pat *pat;
      hir_Identifier *binding;
    } bind;
  };
} hir_Pat;

typedef enum {
  hir_SK_None,
  hir_SK_Assign,
  hir_SK_Expr,
} hir_StmntKind;

typedef struct hir_Stmnt_s {
  hir_StmntKind kind;
  union {
    // Declarations
    struct {
      hir_Pat *pat;
      hir_Expr *val;
    } assign;
    struct {
      hir_Expr *expr;
    } expr;
  };
} hir_Stmnt;

com_str hir_strExprKind(hir_ExprKind val);
com_str hir_strIdentifierKind(hir_IdentifierKind val);
com_str hir_strMatchCaseKind(hir_MatchCaseKind val);
com_str hir_strCompoundElementKind(hir_CompoundElementKind val);
com_str hir_strStmntKind(hir_StmntKind val);

#endif
