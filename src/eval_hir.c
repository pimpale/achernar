#include "eval_hir.h"
#include "com_bigdecimal.h"
#include "com_bigint.h"
#include "com_define.h"
#include "com_hashtable.h"
// Notes:
// This stage is responsible for converting the HIR into a typed MIR
// the output should be a typed MIR
// To accomplish this, we first need to partially evaluate the HIR using
// abstract interpretation Defintions Every variable has a set of potential
// values and potential types Every variable has a set of





// This part of evaluation fully evaluates a Procedure type

typedef struct eval_Val_s eval_Val;

typedef struct eval_Val_s {
  bool setType;
  union {
    struct {
      enum {
        eval_VK_VoidType,
        eval_VK_BoolType,
        eval_VK_IntType,
        eval_VK_RealType,
        eval_VK_BitVecType,
        eval_VK_FloatType,
        eval_VK_StructType,
        eval_VK_FunctionTypeType,
      } setKind;
      union {
        struct {
          bool hasSign;
          u8 bits;
        } bitVecType;
        struct {
          u8 bits;
        } floatType;
      };
    } set;
  };
  struct {
    eval_Val *type;
    union {
      bool boolVal;
      com_bigint intVal;
      com_bigdecimal realVal;
      u8 unsignedBitVec8Val;
      i8 signedBitVec8Val;
      u16 unsignedBitVec16Val;
      i16 signedBitVec16Val;
      u32 unsignedBitVec32Val;
      i32 signedBitVec32Val;
      u64 unsignedBitVec64Val;
      i64 signedBitVec64Val;
      u32 float32Val;
      u64 float64Val;
      struct {
        // HashTable<com_str, eval_Val>
        com_hashtable fields;
      } structTypeVal;
      struct {
        // HashTable<com_str, eval_Val>
        com_hashtable fields;
      } structVal;
      struct {
        eval_Val* inType;
        eval_Val* outType;
      } functionTypeVal;
      struct {
        // HashTable<com_str, eval_Val>
        com_hashtable params;

        eval_Val* bind;
        eval_Val* outType;
        eval_Val* state;
      } functionVal;
    };
  } val;
} eval_Val;

// This function evals a hir section completely using the context given
static void fully_eval() {}
