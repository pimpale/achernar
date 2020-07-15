#ifndef COM_BIGINT_H
#define COM_BIGINT_H

#include "com_allocator.h"
#include "com_biguint.h"

typedef struct {
  bool _negative;
  com_biguint _magnitude;
} com_bigint;

/// creates a bigint from a magnitude and the sign
/// REQUIRES: `magnitude` is a valid `com_biguint`
/// GUARANTEES: returns a valid `com_biguint` using `magnitude`
/// GUARANTEES: if `negative`, then the returned value will be negative
com_bigint com_bigint_from(com_biguint magnitude, bool negative);

/// creates a bigint from an allocator handle, with value 0
/// REQUIRES: `h` is a valid com_allocator_Handle
/// GUARANTEES: returns a valid `com_bigint` with value 0 and sign 0
com_bigint com_bigint_create(com_allocator_Handle h);

/// returns a pointer to the magnitude of bigint
/// REQUIRES: `a` is a valid com_bigint
/// GUARANTEES: returns a pointer to a valid `com_biguint` storing the value of
/// the magnitude
const com_biguint *com_bigint_magnitude(const com_bigint *a);

/// frees bigint
/// REQUIRES: `a` is a valid pointer to a `com_bigint`
/// GUARANTEES: all memory associated with `a` will be deallocated
/// GUARANTEES: `a` is no longer a valid com_bigint
void com_bigint_destroy(com_bigint *a);

/// sets `dest` to `a`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigint`
/// GUARANTEES: `dest` is set to the value of `a`
void com_bigint_set_i64(com_bigint *dest, i64 a);

/// returns the nearest value of a bigint as an i64
/// REQUIRES: `a` is a valid pointer to a valid `com_bigint `
/// GUARANTEES: `a` is no longer a
i64 com_bigint_get_i64(const com_bigint *a);

/// returns true if the com_bigint can be losslessly respresented as an i64
/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// GUARANTEES: returns true if `a` can be represented by a i64
bool com_bigint_fits_i64(const com_bigint *a);

///  Returns the nearest value of a bigint as a f64
/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// GUARANTEES: returns the value of `f64` closest to the value of `a`
f64 com_bigint_get_f64(const com_bigint *a);

///  Copies the value of a bigint to a bigint
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigint`
/// GUARANTEES: the value of  `dest` is now equal to `val`
void com_bigint_set(com_bigint *dest, const com_bigint *src);

/* Basic arithmetic operations: */

/// dest := a + b
/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigint`
/// GUARANTEES: `dest` will be overwritten with the sum of `a` and `b`
void com_bigint_add(com_bigint *dest, const com_bigint *a, const com_bigint *b);

/// dest := a - b
/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigint`
/// GUARANTEES: `dest` will be overwritten by `a` - `b`
void com_bigint_sub(com_bigint *dest, const com_bigint *a, const com_bigint *b);

/// dest := a * b
/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `dest` will be overwritten by `a` * `b`
void com_bigint_mul(com_bigint *dest, const com_bigint *a, const com_bigint *b,
                    com_Allocator *allocator);
/// dest := a / b
/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `dest` will be overwritten by `a` / `b`
void com_bigint_div(com_bigint *dest, const com_bigint *a, const com_bigint *b,
                    com_Allocator *allocator);

/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `dest` will be overwritten by `a` % `b`
void com_bigint_rem(com_bigint *dest, const com_bigint *a, const com_bigint *b,
                    com_Allocator *allocator);

/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `quotient` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `remainder` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `quotient` will be overwritten by `a` / `b`
/// GUARANTEES: `remainder` will be overwritten by `a` % `b`
void com_bigint_div_rem(com_bigint *quotient, com_bigint *remainder,
                        const com_bigint *a, const com_bigint *b,
                        com_Allocator *allocator);

/* Special operators and comparison */

com_math_cmptype com_bigint_cmp(const com_bigint *a, const com_bigint *b);

/// returns the sign of a bigint
/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// GUARANTEES: returns `com_math_ZERO` if `a` is zero
/// GUARANTEES: returns `com_math_POSITIVE` if `a` is greater than 0
/// GUARANTEES: returns `com_math_NEGATIVE` if `a` is less than 0
com_math_signtype com_bigint_sign(const com_bigint *a);

/// returns true if `a` is zero
/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// GUARANTEES: if `a` is zero, returns true
/// GUARANTEES: if `a` is not zero, returns false
bool com_bigint_is_zero(const com_bigint *a);

#endif
