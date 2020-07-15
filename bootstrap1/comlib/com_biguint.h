#ifndef COM_BIGUINT_H
#define COM_BIGUINT_H

#include "com_allocator.h"
#include "com_define.h"
#include "com_math.h"
#include "com_vec.h"

/* Data-holding structure: array of u32s */
typedef struct {
  // vector of u32s
  // not permitted to have empty zeros in the front
  // This makes comparing magnitudes faster and memory usage down
  com_vec _array;
} com_biguint;

// biguint creation

///  creates a biguint from an allocator (default value is 0)
/// REQUIRES: `h` a valid com_allocator_Handle that is reallocable
/// with GUARANTEES: returns a valid `com_biguint` with value 0
com_biguint com_biguint_create(com_allocator_Handle h);

// biguint destruction

///  frees biguint
/// REQUIRES: `a` is a valid pointer to a `com_biguint`
/// GUARANTEES:  `a` is no longer a valid `com_biguint`
/// GUARANTEES: all memory associated with `a` will be deallocated
void com_biguint_destroy(com_biguint *a);

///  Sets the value of a biguint to a u64
/// REQUIRES: `dest` is a valid pointer to a valid `com_biguint`
/// GUARANTEES: the value of  `dest` is now equal to `val`
void com_biguint_set_u64(com_biguint *dest, u64 val);

///  Returns the nearest value the value of a biguint as an u64
/// REQUIRES: `a` is a valid pointer to a valid `com_biguint`
/// GUARANTEES: returns the value of `u64` closest to the value of `a`
u64 com_biguint_get_u64(const com_biguint *a);

///  Returns if `a` can be losslessly represented as a u64
/// REQUIRES: `a` is a valid pointer to a valid `com_biguint`
/// GUARANTEES: returns `true` if the value of `a` is between u64_min_m and
/// u64_max_m else false
bool com_biguint_fits_u64(const com_biguint *a);

///  Returns the nearest value of a biguint as a f64
/// REQUIRES: `a` is a valid pointer to a valid `com_biguint`
/// GUARANTEES: returns the value of `f64` closest to the value of `a`
f64 com_biguint_get_f64(const com_biguint *a);

///  Copies the value of a biguint to a biguint
/// REQUIRES: `dest` is a valid pointer to a valid `com_biguint`
/// GUARANTEES: the value of  `dest` is now equal to `val`
void com_biguint_set(com_biguint *dest, const com_biguint *src);

/* Basic arithmetic operations: */

/// dest := a + b
/// REQUIRES: `a` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `b` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `dest` is a valid pointer to a valid `com_biguint`
/// GUARANTEES: `dest` will be overwritten with the sum of `a` and `b`
void com_biguint_add(com_biguint *dest, const com_biguint *a,
                     const com_biguint *b);

/// dest := a - b
/// REQUIRES: `a` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `b` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `dest` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `a` > `b`
/// GUARANTEES: `dest` will be overwritten by `a` - `b`
void com_biguint_sub(com_biguint *dest, const com_biguint *a,
                     const com_biguint *b);

/// dest := a * b
/// REQUIRES: `a` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `b` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `dest` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `dest` will be overwritten by `a` * `b`
void com_biguint_mul(com_biguint *dest, const com_biguint *a,
                     const com_biguint *b, com_Allocator *allocator);
/// dest := a / b
/// REQUIRES: `a` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `b` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `dest` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `dest` will be overwritten by `a` / `b`
void com_biguint_div(com_biguint *dest, const com_biguint *a,
                     const com_biguint *b, com_Allocator *allocator);

/// REQUIRES: `a` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `b` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `dest` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `dest` will be overwritten by `a` % `b`
void com_biguint_rem(com_biguint *dest, const com_biguint *a,
                     const com_biguint *b, com_Allocator *allocator);

/// REQUIRES: `a` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `b` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `quotient` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `remainder` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `quotient` will be overwritten by `a` / `b`
/// GUARANTEES: `remainder` will be overwritten by `a` % `b`
void com_biguint_div_rem(com_biguint *quotient, com_biguint *remainder,
                         const com_biguint *a, const com_biguint *b,
                         com_Allocator *allocator);

/* Bitwise operations: */
void com_biguint_and(com_biguint *dest, const com_biguint *a,
                     const com_biguint *b);
void com_biguint_or(com_biguint *dest, const com_biguint *a,
                    const com_biguint *b);
void com_biguint_xor(com_biguint *dest, const com_biguint *a,
                     const com_biguint *b);
void com_biguint_lshift(com_biguint *dest, const com_biguint *a,
                        const u64 nbits);
void com_biguint_rshift(com_biguint *dest, const com_biguint *a,
                        const u64 nbits);

// compares the magnitude of b with reference to a
/// REQUIRES: `a` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `b` is a valid pointer to a valid `com_biguint`
/// GUARANTEES: if `b` is greater than `a` will return com_math_GREATER
/// GUARANTEES: if `b` is less than `a` will return com_math_LESS
/// GUARANTEES: if `b` is equal to `a` will return com_math_EQUAL
com_math_cmptype com_biguint_cmp(const com_biguint *a, const com_biguint *b);

// constant version
void com_biguint_add_u32(com_biguint *dest, const com_biguint *a, u32 b);
void com_biguint_sub_u32(com_biguint *dest, const com_biguint *a, u32 b);
void com_biguint_mul_u32(com_biguint *dest, const com_biguint *a, u32 b);

com_math_cmptype com_biguint_cmp_u64(const com_biguint *a, u64 b);

bool com_biguint_is_zero(const com_biguint *a);

#endif
