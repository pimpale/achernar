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

typedef struct {
  hir_Common common;
  union {
    struct {
      com_str name;
    } id;
  };
} hir_Identifier;

typedef struct hir_Expr_s hir_Expr;
typedef struct hir_Pat_s hir_Pat;
typedef struct hir_Stmnt_s hir_Stmnt;

typedef struct {
  hir_Common common;
  union {
    struct {
      hir_Expr *pat;
      hir_Expr *val;
    } matchCase;
  };
} hir_MatchCase;

typedef struct {
  hir_Common common;
  union {
    struct {
      hir_Identifier *name;
      hir_Expr *val;
    } element;
  };
} hir_CompoundElement;

typedef enum {
  // Filesystem Abstractions
  hir_IIK_Import, // Evaluates a source file, yielding an object
  // Type stuff
  hir_IIK_Struct, // creates a struct from an ad hoc compound object
  hir_IIK_Enum,   // creates a disjoint union from an ad hoc compound object
  hir_IIK_New,    // Creates a function constructing the compound type provided
  // Type Reflection
  hir_IIK_Int, // compile time only arbitrary size integer
  // Math with integers
  hir_IIK_IntAdd,
  hir_IIK_IntSub,
  hir_IIK_IntMul,
  hir_IIK_IntDiv,
  hir_IIK_IntRem,
  hir_IIK_IntDivRem,
  hir_IIK_Real, // compile time only arbitrary size real number
  // Math with reals
  hir_IIK_RealAdd,
  hir_IIK_RealSub,
  hir_IIK_RealMul,
  hir_IIK_RealDiv,
  hir_IIK_RealRem,
  // Conversion between integers and reals
  hir_IIK_RealRound,
  hir_IIK_RNE, // round to nearest
  hir_IIK_RTZ, // round to zero
  hir_IIK_RDN, // round down
  hir_IIK_RUP, // round up
  hir_IIK_IntPromote,
  // Bit Vectors
  hir_IIK_SignedBitVec, // creates a bitvector from an integer
  // Unsigned Operations
  hir_IIK_UnsignedBitVecAdd,
  hir_IIK_UnsignedBitVecAddOverflow,
  hir_IIK_UnsignedBitVecSub,
  hir_IIK_UnsignedBitVecSubOverflow,
  hir_IIK_UnsignedBitVecMul,
  hir_IIK_UnsignedBitVecMulOverflow,
  hir_IIK_UnsignedBitVecDiv,
  hir_IIK_UnsignedBitVecRem,
  hir_IIK_UnsignedBitVecDivRem,
  hir_IIK_UnsignedBitVecShr,
  hir_IIK_UnsignedBitVecShrOverflow,
  hir_IIK_UnsignedBitVecShl,
  hir_IIK_UnsignedBitVecShlOverflow,
  hir_IIK_UnsignedBitVecRol,
  hir_IIK_UnsignedBitVecRor,
  hir_IIK_UnsignedBitVec, // creates a bitvector from an integer
  // Signed Operations
  hir_IIK_SignedBitVecAdd,
  hir_IIK_SignedBitVecAddOverflow,
  hir_IIK_SignedBitVecSub,
  hir_IIK_SignedBitVecSubOverflow,
  hir_IIK_SignedBitVecMul,
  hir_IIK_SignedBitVecMulOverflow,
  hir_IIK_SignedBitVecDiv,
  hir_IIK_SignedBitVecRem,
  hir_IIK_SignedBitVecDivRem,
  hir_IIK_SignedBitVecShr,
  hir_IIK_SignedBitVecShrOverflow,
  hir_IIK_SignedBitVecShl,
  hir_IIK_SignedBitVecShlOverflow,
  hir_IIK_SignedBitVecRol,
  hir_IIK_SignedBitVecRor,

  // Handle memory addresses
  hir_IIK_GetMemAddr,
  hir_IIK_DerefMemAddr,

  // Returns a place! from a memory address
  hir_IIK_MutateMemAddr,
} hir_IntrinsicIdentifierKind;

typedef enum {
  hir_EK_NeverType,   // The type of the special value returned by ret,
                      // neverending loops, and functions that dont return
  hir_EK_Nil,         // Literal for nil
  hir_EK_Int,         // Literal for an integer number
  hir_EK_Real,        // Literal for a real (floating point) number
  hir_EK_String,      // A string literal
  hir_EK_Fn,          // Creates a new function
  hir_EK_Loop,        // Loops until a scope is returned
  hir_EK_Struct,      // Constructs a new compound type ad hoc
  hir_EK_Call,        // Call a function with a product type
  hir_EK_Pipe,        // Syntactic sugar operator to evaluate a function postfix
  hir_EK_Ret,         // Returns from a scope with a value
  hir_EK_Match,       // Matches an expression to the first matching pattern and
                      // destructures it
  hir_EK_Scope,       // Groups together several statements, returning last val
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
      hir_Expr *body;
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
      hir_Expr *value;
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
    } label;
  };
} hir_Expr;

typedef enum {
  hir_PK_Fn,   // Calls function with current scrutinee and then matches on the
               // results
  hir_PK_Wildcard,  // matches any value
  hir_PK_Bind,      // binds a single element to new variable
  hir_PK_Mutate,    // binds a single element to new variable
  hir_PK_MutatePtr, // binds a single element to new variable
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
    struct {
      hir_Pat *pat;
      hir_Identifier *binding;
    } mutate;
    struct {
      hir_Pat *pat;
      hir_Expr *ptr;
    } mutatePtr;
  };
} hir_Pat;

typedef enum {
  hir_SK_None,
  hir_SK_Assign,
  hir_SK_Expr,
} hir_StmntKind;

typedef struct hir_Stmnt_s {
  hir_Common common;
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
com_str hir_strStmntKind(hir_StmntKind val);

#endif
