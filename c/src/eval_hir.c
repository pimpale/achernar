#include "eval_hir.h"
#include "com_assert.h"
#include "com_bigdecimal.h"
#include "com_bigint.h"
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
  enum eval_ValKind_e {
    eval_VK_Type,
    eval_VK_Nil,
    eval_VK_NilType,
    eval_VK_StructLiteral,
    eval_VK_StructType,
    eval_VK_Bool,
    eval_VK_BoolType,
    eval_VK_Uint8,
    eval_VK_Uint16,
    eval_VK_Uint32,
    eval_VK_Uint64,
    eval_VK_Int8,
    eval_VK_Int16,
    eval_VK_Int32,
    eval_VK_Int64,
    eval_VK_BitVecType,
    eval_VK_Float32,
    eval_VK_Float64,
    eval_VK_FloatType,
    eval_VK_Never,
    eval_VK_NeverType,
    eval_VK_Fn,
    eval_VK_FnType,
    eval_VK_FnTypeType,
  } kind;
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
    f32 float32Val;
    f64 float64Val;
    struct {
      bool hasSign;
      // INVARIANT: bits is 8 | 16 | 32 | 64
      u8 bits;
    } bitVecType;
    struct {
      // INVARIANT: bits is 32 | 64
      u8 bits;
    } floatType;
    struct {
      eval_Val *a;
      eval_Val *b;
    } orType;
    struct {
      // represents a return from a lower value.
      com_str label;
      eval_Val *returned;
    } never;
    struct {
      // HashTable<com_str, eval_Val>
      com_hashtable fields;
    } structType;
    struct {
      // HashTable<com_str, eval_Val>
      com_hashtable fields;
    } structLiteral;
    struct {
      eval_Val *inType;
      eval_Val *outType;
    } fnType;
    struct {
      eval_Val *bind;
      hir_Expr *body;
    } fn;
  };
} eval_Val;

/* Internal functions for getType */

static eval_Val eval_simpleVal(enum eval_ValKind_e vk) {
  return (eval_Val){.kind = vk};
}

static eval_Val eval_floatVal(u8 bits) {
  return (eval_Val){.kind = eval_VK_FloatType, .floatType = {.bits = bits}};
}

static eval_Val eval_floatType(u8 bits) {
  return (eval_Val){.kind = eval_VK_FloatType, .floatType = {.bits = bits}};
}

static eval_Val eval_BitVecType(u8 bits, bool hasSign) {
  return (eval_Val){.kind = eval_VK_FloatType,
                    .bitVecType = {.bits = bits, .hasSign = hasSign}};
}

static eval_Val getType(eval_Val val, com_allocator *a) {
  switch (val.kind) {
  case eval_VK_Type:
  case eval_VK_BoolType:
  case eval_VK_BitVecType:
  case eval_VK_FloatType:
  case eval_VK_FnTypeType:
  case eval_VK_NeverType:
  case eval_VK_StructType:
  case eval_VK_NilType:
    return eval_simpleVal(eval_VK_Type);
  case eval_VK_Bool:
    return eval_simpleVal(eval_VK_BoolType);
  case eval_VK_FnType:
    return eval_simpleVal(eval_VK_FnType);
  case eval_VK_Uint8:
    return eval_BitVecType(8, false);
  case eval_VK_Uint16:
    return eval_BitVecType(16, false);
  case eval_VK_Uint32:
    return eval_BitVecType(32, false);
  case eval_VK_Uint64:
    return eval_BitVecType(64, false);
  case eval_VK_Int8:
    return eval_BitVecType(8, true);
  case eval_VK_Int16:
    return eval_BitVecType(16, true);
  case eval_VK_Int32:
    return eval_BitVecType(32, true);
  case eval_VK_Int64:
    return eval_BitVecType(64, true);
  case eval_VK_Float32:
    return eval_floatType(32);
  case eval_VK_Float64:
    return eval_floatType(64);
  case eval_VK_Never:
    return eval_simpleVal(eval_VK_NeverType);
  case eval_VK_Nil:
    return eval_simpleVal(eval_VK_NilType);
  case eval_VK_StructLiteral: {
    // TODO put struct fields in
    com_hashtable_create(com_allocator_alloc(
        a, (com_allocator_HandleData){.len = 30,
                                      .flags = com_allocator_defaults(a) |
                                               com_allocator_NOLEAK |
                                               com_allocator_REALLOCABLE}));
    for(int i = 0; i < val.structLiteral.fields)
      return eval_simpleVal(eval_VK_NilType);
  }
  }
}

static bool eval_isType(eval_Val val, eval_Val type) {
  // TODO please fix me
  // should return true if val can fit type
  return true;
}

typedef struct {
  com_str identifier;
  bool isLabel;
  union {
    struct {
      com_vec defers;
    } label;
    struct {
      eval_Val *value;
    } variable;
  };
} eval_Variable;

// this is an environement
typedef struct eval_Env_s eval_Env;
typedef struct eval_Env_s {
  eval_Env *old_env;
  eval_Variable var;
} eval_Env;

// utility macro  to create a vector
#define hir_alloc_vec_m(a)                                                     \
  com_vec_create(com_allocator_alloc(                                          \
      a, (com_allocator_HandleData){.len = 10,                                 \
                                    .flags = com_allocator_defaults(a) |       \
                                             com_allocator_NOLEAK |            \
                                             com_allocator_REALLOCABLE}))

// Evaluates Type Terms concretely
// INVARIANT: returned value will fit in contextType
eval_Val pure_concrete_interpret(hir_Expr *hir, eval_Val contextType,
                                 eval_Env env, com_allocator *a) {
  switch (hir->kind) {
  case hir_EK_None: {
    com_assert_unreachable_m("hir_ek_none reached");
    break;
  }
  case hir_EK_Loop: {
    while (true) {
      eval_Val v = pure_concrete_interpret(
          hir->loop.expr, eval_simpleVal(eval_VK_NilType), env, a);
      if (eval_isNever(v)) {
        return v;
      }
    }
  }
  case hir_EK_Apply: {
    eval_Val any = eval_simpleVal(eval_VK_AnyType);
    eval_Val ft =
        (eval_Val){.kind = eval_VK_FnType,
                   .fnType = {.inType = &any, .outType = &contextType}};
    eval_Val fn = pure_concrete_interpret(hir->apply.fn, ft, env, a);
    if (fn.kind == eval_VK_Never) {
      return fn;
    }
    eval_Val param = pure_concrete_interpret(
        hir->apply.param, *fn.val.type->val.fnType.inType, env, a);
    if (param.kind == eval_VK_Never) {
      return param;
    }

    // TODO bind param to fn and actually apply it
    return any;
  }
  case hir_EK_Label: {

    eval_Env newenv = (eval_Env){
        .old_env = &env,
        .var = (eval_Variable){.identifier = hir->label.identifer,
                               .isLabel = true,
                               .label = {.defers = hir_alloc_vec_m(a)}}};
    eval_Val v =
        pure_concrete_interpret(hir->label.expr, contextType, newenv, a);
    if (v.kind == eval_VK_Never &&
        com_str_equal(v.never.label, hir->label.identifer)) {
      eval_Val returned = *v.never.returned;
      if (!eval_isType(returned, contextType)) {
        // TODO give proper error
        com_assert_unreachable_m("incompatible type reached");
      }
      return returned;
    }
    // if the label didn't catch it, return either the error or the value.
    return v;
  }
  case hir_EK_Defer: {
    eval_Env *current = &env;
    while (current != NULL) {
      if (current->var.isLabel &&
          com_str_equal(hir->defer.label, current->var.identifier)) {
        // push pointer to expression
        *com_vec_push_m(&current->var.label.defers, hir_Expr *) =
            hir->defer.expr;
        return eval_simpleVal(eval_VK_Nil);
      }
      current = current->old_env;
    }
    // this will run if we didn't find the label
    com_assert_unreachable_m("couldn't find label");
  }
  }
}
// This function evals a hir section completely using the context givenvoid
// fully_eval(hir_Expr *expr, com_allocator *a) {}
