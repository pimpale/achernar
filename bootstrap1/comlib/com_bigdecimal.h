#ifndef COM_BIGDECIMAL_H
#define COM_BIGDECIMAL_H

#include "com_allocator.h"
#include "com_bigint.h"

typedef struct {
  // how much to downscale bigdecimal
  // measured in words
  u32 _downscale;
  com_bigint _value;
} com_bigdecimal;

/// creates a bigdecimal from a magnitude and the sign
/// REQUIRES: `magnitude` is a valid `value`
/// GUARANTEES: returns a valid `com_biguint` using `value`
/// GUARANTEES: if `negative`, then the returned value will be negative
/// GUARANTEES: `value` is consumed
com_bigdecimal com_bigdecimal_from(com_bigint value);

/// creates a bigdecimal from an allocator handle, with value 0
/// REQUIRES: `h` is a valid com_allocator_Handle
/// GUARANTEES: returns a valid `com_bigdecimal` with value 0 and sign 0
com_bigdecimal com_bigdecimal_create(com_allocator_Handle h);

/// returns a pointer to the magnitude of bigint
/// REQUIRES: `a` is a valid com_bigint
/// REQUIRES: `dest` is a valid com_biguint
/// GUARANTEES: sets dest to the magnitude of `a`
void com_bigint_magnitude(com_biguint* dest, const com_bigint *a);

/// frees bigdecimal
/// REQUIRES: `a` is a valid pointer to a `com_bigdecimal`
/// GUARANTEES: all memory associated with `a` will be deallocated
/// GUARANTEES: `a` is no longer a valid com_bigdecimal
void com_bigdecimal_destroy(com_bigdecimal *a);

/// sets `dest` to `a`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: `dest` is set to the value of `a`
void com_bigdecimal_set_i64(com_bigdecimal *dest, i64 a);

/// returns the nearest value of a bigdecimal as an i64
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal `
/// GUARANTEES: `a` is no longer a
i64 com_bigdecimal_get_i64(const com_bigdecimal *a);

/// returns true if the com_bigdecimal can be losslessly respresented as an i64
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: returns true if `a` can be represented by a i64
bool com_bigdecimal_fits_i64(const com_bigdecimal *a);

///  Returns the nearest value of a bigdecimal as a f64
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: returns the value of `f64` closest to the value of `a`
f64 com_bigdecimal_get_f64(const com_bigdecimal *a);

/// sets `dest` to `a`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: `dest` is set to the value of `a`
void com_bigdecimal_set_f64(com_bigdecimal *dest, f64 a);

///  Copies the value of a bigdecimal to a bigdecimal
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: the value of  `dest` is now equal to `val`
void com_bigdecimal_set(com_bigdecimal *dest, const com_bigdecimal *src);

/* Basic arithmetic operations: */

/// dest := a + b
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: `dest` will be overwritten with the sum of `a` and `b`
void com_bigdecimal_add(com_bigdecimal *dest, const com_bigdecimal *a, const com_bigdecimal *b);

/// dest := a - b
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: `dest` will be overwritten by `a` - `b`
void com_bigdecimal_sub(com_bigdecimal *dest, const com_bigdecimal *a, const com_bigdecimal *b);

/// dest := a * b
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `dest` will be overwritten by `a` * `b`
void com_bigdecimal_mul(com_bigdecimal *dest, const com_bigdecimal *a, const com_bigdecimal *b,
                    com_allocator *allocator);
/// dest := a / b
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `dest` will be overwritten by `a` / `b`
void com_bigdecimal_div(com_bigdecimal *dest, const com_bigdecimal *a, const com_bigdecimal *b,
                    com_allocator *allocator);

/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `dest` will be overwritten by `a` % `b`
void com_bigdecimal_rem(com_bigdecimal *dest, const com_bigdecimal *a, const com_bigdecimal *b,
                    com_allocator *allocator);

/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `quotient` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `remainder` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `quotient` will be overwritten by `a` / `b`
/// GUARANTEES: `remainder` will be overwritten by `a` % `b`
void com_bigdecimal_div_rem(com_bigdecimal *quotient, com_bigdecimal *remainder,
                        const com_bigdecimal *a, const com_bigdecimal *b,
                        com_allocator *allocator);

/* Special operators and comparison */

com_math_cmptype com_bigdecimal_cmp(const com_bigdecimal *a, const com_bigdecimal *b);

/// returns the sign of a bigdecimal
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: returns `com_math_ZERO` if `a` is zero
/// GUARANTEES: returns `com_math_POSITIVE` if `a` is greater than 0
/// GUARANTEES: returns `com_math_NEGATIVE` if `a` is less than 0
com_math_signtype com_bigdecimal_sign(const com_bigdecimal *a);

/// returns true if `a` is zero
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: if `a` is zero, returns true
/// GUARANTEES: if `a` is not zero, returns false
bool com_bigdecimal_is_zero(const com_bigdecimal *a);

// constant version
void com_bigint_add_i32(com_bigint *dest, const com_bigint *a, i32 b);
void com_bigint_sub_i32(com_bigint *dest, const com_bigint *a, i32 b);
void com_bigint_mul_i32(com_bigint *dest, const com_bigint *a, i32 b);

#endif
