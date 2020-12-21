#ifndef HIR_H
#define HIR_H

#include "ast.h"
#include "com_bigdecimal.h"
#include "com_bigint.h"
#include "com_define.h"
#include "com_loc.h"
#include "com_str.h"

typedef struct hir_Expr_s hir_Expr;

typedef enum {
  // Filesystem Abstractions
  hir_IK_Import, // Evaluates a source file, yielding an object
  // Type stuff
  hir_IK_Struct, // creates a struct from an ad hoc compound object
  hir_IK_Enum,   // creates a disjoint union from an ad hoc compound object
  hir_IK_New,    // Creates a function constructing the compound type provided
  // Type Reflection
  hir_IK_Int, // compile time only arbitrary size integer
  // Math with integers
  hir_IK_IntAdd,
  hir_IK_IntSub,
  hir_IK_IntMul,
  hir_IK_IntDiv,
  hir_IK_IntRem,
  hir_IK_IntDivRem,
  hir_IK_Real, // compile time only arbitrary size real number
  // Math with reals
  hir_IK_RealAdd,
  hir_IK_RealSub,
  hir_IK_RealMul,
  hir_IK_RealDiv,
  hir_IK_RealRem,
  // Conversion between integers and reals
  hir_IK_RealRound,
  hir_IK_RNE, // round to nearest
  hir_IK_RTZ, // round to zero
  hir_IK_RDN, // round down
  hir_IK_RUP, // round up
  hir_IK_IntPromote,
  // Bit Vectors
  hir_IK_SignedBitVec, // creates a bitvector from an integer
  // Unsigned Operations
  hir_IK_UnsignedBitVecAdd,
  hir_IK_UnsignedBitVecAddOverflow,
  hir_IK_UnsignedBitVecSub,
  hir_IK_UnsignedBitVecSubOverflow,
  hir_IK_UnsignedBitVecMul,
  hir_IK_UnsignedBitVecMulOverflow,
  hir_IK_UnsignedBitVecDiv,
  hir_IK_UnsignedBitVecRem,
  hir_IK_UnsignedBitVecDivRem,
  hir_IK_UnsignedBitVecShr,
  hir_IK_UnsignedBitVecShrOverflow,
  hir_IK_UnsignedBitVecShl,
  hir_IK_UnsignedBitVecShlOverflow,
  hir_IK_UnsignedBitVecRol,
  hir_IK_UnsignedBitVecRor,
  hir_IK_UnsignedBitVec, // creates a bitvector from an integer
  // Signed Operations
  hir_IK_SignedBitVecAdd,
  hir_IK_SignedBitVecAddOverflow,
  hir_IK_SignedBitVecSub,
  hir_IK_SignedBitVecSubOverflow,
  hir_IK_SignedBitVecMul,
  hir_IK_SignedBitVecMulOverflow,
  hir_IK_SignedBitVecDiv,
  hir_IK_SignedBitVecRem,
  hir_IK_SignedBitVecDivRem,
  hir_IK_SignedBitVecShr,
  hir_IK_SignedBitVecShrOverflow,
  hir_IK_SignedBitVecShl,
  hir_IK_SignedBitVecShlOverflow,
  hir_IK_SignedBitVecRol,
  hir_IK_SignedBitVecRor,

  // Handle memory addresses
  hir_IK_GetMemAddr,
  hir_IK_DerefMemAddr,

  // Assign value to place
  hir_IK_Assign,

  // Create Function
  hir_IK_Defun,

  // Returns a place! from a memory address
  hir_IK_MutateMemAddr,
} hir_IntrinsicKind;

typedef enum {
  hir_EK_None,  // An error when parsing
  hir_EK_Nil,   // Literal for nil
  hir_EK_Int,   // Literal for an integer number
  hir_EK_Real,  // Literal for a real (floating point) number
  hir_EK_Loop,  // Loops until a scope is returned
  hir_EK_Apply, // applies a function
  hir_EK_Label, // Wraps a term in a label that can be deferred or returned from
  hir_EK_Defer, // Defer until label
  hir_EK_Struct, // Constructs a new compound type
  hir_EK_Ret,    // Returns from a scope with a value
  hir_EK_CaseOf, // Matches an expression to the first matching pattern and
                 // destructures it
  hir_EK_ModuleAccess, // Accessing the module of a module object
  hir_EK_Reference,    // A reference to a previously defined variable
  hir_EK_BindIgnore,   // (PATTERN ONLY) ignores a single element
  hir_EK_Bind,      // (PATTERN ONLY) matches a single element to new variable
  hir_EK_At,        // matches expression and assigns to another
  hir_EK_Constrain, // matches expression and assigns to another
} hir_ExprKind;

typedef struct hir_Expr_s {
  hir_ExprKind kind;
  ast_Expr *from;
  union {
    struct {
      com_bigint value;
    } intLiteral;
    struct {
      com_bigdecimal value;
    } realLiteral;
    struct {
      hir_Expr *expr;
    } structLiteral;
    struct {
      hir_Expr *body;
    } loop;
    struct {
      hir_Expr *expr;
      hir_Expr *defer;
      usize defer_len;
    } label;
    struct {
      hir_Expr *fn;
      hir_Expr *param;
    } apply;
    struct {
      hir_Expr *module;
      com_str field;
    } moduleAccess;
    struct {
      hir_IntrinsicKind kind;
    } intrinsic;
    struct {
      com_str reference;
    } reference;
    struct {
      hir_Expr *expr;
      hir_Expr *scope;
    } ret;
    struct {
      hir_Expr *expr;
      hir_Expr *cases;
      usize cases_len;
    } caseof;
    struct {
      com_str mutate;
    } mutate;
    struct {
      com_str bind;
    } bind;
    struct {
      hir_Expr *pat;
      hir_Expr *binding;
    } at;
  };
} hir_Expr;

com_str hir_strExprKind(hir_ExprKind val);

#endif
