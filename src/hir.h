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
  hir_EK_None,  // An error when parsing
  hir_EK_Loop,  // Loops until a scope is returned
  hir_EK_Apply, // applies a function
  hir_EK_Label, // Wraps a term in a label that can be deferred or returned from
  hir_EK_StructLiteral, // Constructs a new compound type
  hir_EK_Ret,           // Returns from a scope with a value
  hir_EK_ModuleAccess,  // Accessing the module of a module object
  hir_EK_Reference,     // A reference to a previously defined variable
  hir_EK_CaseOf,     // Switches on a pattern
  hir_EK_CaseOption,     // Switches on a pattern
  hir_EK_BindIgnore,    // (PATTERN ONLY) ignores a single element
  hir_EK_BindSplat,     // (PATTERN ONLY) automagically deconstructs a struct
  hir_EK_Bind,      // (PATTERN ONLY) matches a single element to new variable
  hir_EK_At,        // matches expression and assigns to another
  hir_EK_Constrain, // matches expression and assigns to another

  // Literals for values
  hir_EK_Void,      // Literal for void
  hir_EK_VoidType,  // Literal for type of void
  hir_EK_NeverType, // Literal for never type
  hir_EK_Int,       // Literal for an integer number
  hir_EK_IntType,   // Literal for the type of hir_EK_Int
  hir_EK_Real,      // Literal for a real (floating point) number
  hir_EK_RealType,  // Literal for the type of hir_EK_Real

  // Filesystem Abstractions
  hir_EK_ImportFn, // Evaluates a source file, yielding an object
  // Type stuff
  hir_EK_StructFn, // creates a struct from an ad hoc compound object
  hir_EK_EnumFn,   // creates a disjoint union from an ad hoc compound object
  hir_EK_NewFn,    // Creates a function constructing the compound type provided

  // Math with integers
  hir_EK_IntAddFn,
  hir_EK_IntSubFn,
  hir_EK_IntMulFn,
  hir_EK_IntDivFn,
  hir_EK_IntRemFn,
  hir_EK_IntDivRemFn,
  // Math with reals
  hir_EK_RealAddFn,
  hir_EK_RealSubFn,
  hir_EK_RealMulFn,
  hir_EK_RealDivFn,
  hir_EK_RealRemFn,
  // Conversion between integers and reals
  hir_EK_RealRoundFn,
  hir_EK_RNE, // round to nearest
  hir_EK_RTZ, // round to zero
  hir_EK_RDN, // round down
  hir_EK_RUP, // round up
  hir_EK_IntPromoteFn,
  // Bit Vectors
  hir_EK_SignedBitVecFn, // creates a bitvector from an integer
  // Unsigned Operations
  hir_EK_UnsignedBitVecAddFn,
  hir_EK_UnsignedBitVecAddOverflowFn,
  hir_EK_UnsignedBitVecSubFn,
  hir_EK_UnsignedBitVecSubOverflowFn,
  hir_EK_UnsignedBitVecMulFn,
  hir_EK_UnsignedBitVecMulOverflowFn,
  hir_EK_UnsignedBitVecDivFn,
  hir_EK_UnsignedBitVecRemFn,
  hir_EK_UnsignedBitVecDivRemFn,
  hir_EK_UnsignedBitVecShrFn,
  hir_EK_UnsignedBitVecShrOverflowFn,
  hir_EK_UnsignedBitVecShlFn,
  hir_EK_UnsignedBitVecShlOverflowFn,
  hir_EK_UnsignedBitVecRolFn,
  hir_EK_UnsignedBitVecRorFn,
  hir_EK_UnsignedBitVecFn, // creates a bitvector from an integer
  // Signed Operations
  hir_EK_SignedBitVecAddFn,
  hir_EK_SignedBitVecAddOverflowFn,
  hir_EK_SignedBitVecSubFn,
  hir_EK_SignedBitVecSubOverflowFn,
  hir_EK_SignedBitVecMulFn,
  hir_EK_SignedBitVecMulOverflowFn,
  hir_EK_SignedBitVecDivFn,
  hir_EK_SignedBitVecRemFn,
  hir_EK_SignedBitVecDivRemFn,
  hir_EK_SignedBitVecShrFn,
  hir_EK_SignedBitVecShrOverflowFn,
  hir_EK_SignedBitVecShlFn,
  hir_EK_SignedBitVecShlOverflowFn,
  hir_EK_SignedBitVecRolFn,
  hir_EK_SignedBitVecRorFn,

  // Create Function
  hir_EK_Defun, 

  // Handle memory address + ownership
  hir_EK_PlaceType, // this is the type of a valid place that may be assigned to or take reference of
  hir_EK_PatternType, // PlaceType | StructPattern | IntPatternType | RealPatternType | Splat | TODO


  // Handle memory addresses
  hir_EK_GetMemAddrFn, // PlaceType($x) -> Ref(x)
  hir_EK_DerefMemAddrFn, // Ref($x) -> PlaceType(x)

  // Assign value to place
  hir_EK_AssignFn, // Pattern($x) -> x -> void

  // Returns a place from a memory address
  hir_EK_MutateMemAddrFn, 
} hir_ExprKind;

// reserved for stuff that isn't just a reference to
typedef struct hir_Expr_s {
  hir_ExprKind kind;
  const ast_Expr *from;
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
      hir_Expr **defer;
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
      com_str reference;
    } reference;
    struct {
      hir_Expr *expr;
      hir_Expr *scope;
    } ret;
    struct {
      hir_Expr *pattern;
      hir_Expr *result;
    } caseoption;
    struct {
      hir_Expr *expr;
      hir_Expr **cases;
      usize cases_len;
    } caseof;
    struct {
      hir_Expr *value;
      hir_Expr *type;
    } constrain;
    struct {
      hir_Expr *tomatch;
      hir_Expr *target;
    } at;
    struct {
      hir_Expr *pattern;
      hir_Expr *value;
    } defun;
    struct {
      com_str mutate;
    } mutate;
    struct {
      com_str bind;
    } bind;
    struct {
      hir_Expr **expr;
      usize expr_len;
    } sequence;
  };
} hir_Expr;

com_str hir_strExprKind(hir_ExprKind val);

#endif
