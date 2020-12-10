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
/// GUARANTEES: `magnitude` is consumed
com_bigint com_bigint_from(com_biguint magnitude, bool negative);

/// creates a bigint from an allocator handle, with value 0
/// REQUIRES: `h` is a valid com_allocator_Handle
/// GUARANTEES: returns a valid `com_bigint` with value 0 and sign 0
com_bigint com_bigint_create(com_allocator_Handle h);

/// returns a pointer to the magnitude of bigint
/// REQUIRES: `a` is a valid com_bigint
/// REQUIRES: `dest` is a valid com_biguint
/// GUARANTEES: sets dest to the magnitude of `a`
void com_bigint_magnitude(com_biguint* dest, const com_bigint *a);

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
                    com_allocator *allocator);
/// dest := a / b
/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `dest` will be overwritten by `a` / `b`
void com_bigint_div(com_bigint *dest, const com_bigint *a, const com_bigint *b,
                    com_allocator *allocator);

/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `dest` will be overwritten by `a` % `b`
void com_bigint_rem(com_bigint *dest, const com_bigint *a, const com_bigint *b,
                    com_allocator *allocator);

/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `quotient` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `remainder` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `quotient` will be overwritten by `a` / `b`
/// GUARANTEES: `remainder` will be overwritten by `a` % `b`
void com_bigint_div_rem(com_bigint *quotient, com_bigint *remainder,
                        const com_bigint *a, const com_bigint *b,
                        com_allocator *allocator);


/* Bitwise operations on magnitude: */
void com_bigint_and(com_bigint *dest, const com_bigint *a,
                     const com_bigint *b);
void com_bigint_or(com_bigint *dest, const com_bigint *a,
                    const com_bigint *b);
void com_bigint_xor(com_bigint *dest, const com_bigint *a,
                     const com_bigint *b);
void com_bigint_lshift(com_bigint *dest, const com_bigint *a,
                        const usize nbits);
void com_bigint_rshift(com_bigint *dest, const com_bigint *a,
                        const usize nbits);

// constant operators
void com_bigint_add_i32(com_bigint *dest, const com_bigint *a, i32 b);
void com_bigint_sub_i32(com_bigint *dest, const com_bigint *a, i32 b);
void com_bigint_mul_i32(com_bigint *dest, const com_bigint *a, i32 b);
void com_bigint_div_i32(com_bigint *dest, const com_bigint *a, i32 b);

/* Special operators and comparison */

// compares b with reference to a
/// REQUIRES: `a` is a valid pointer to a valid `com_biguint`
/// REQUIRES: `b` is a valid pointer to a valid `com_biguint`
/// GUARANTEES: if `b` is greater than `a` will return com_math_GREATER
/// GUARANTEES: if `b` is less than `a` will return com_math_LESS
/// GUARANTEES: if `b` is equal to `a` will return com_math_EQUAL
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

/// negates `a`
/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// GUARANTEES: if `a` != 0 the sign has been flipped
void com_bigint_negate(com_bigint *a);

/* Functions to inspect the layout of the bigint */

/// Returns the number of u32 words in `a`
/// REQUIRES: `a` must be a valid pointer to a valid com_bigint
/// GUARANTEES: returns the number of words in the internal representation of `a`
/// GUARANTEES: the number returned is equal to ceil(log(a)/log(u32_max_m))
usize com_bigint_len(const com_bigint *a);

/// Get value of bigint's internal little endian representation at index i
/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `i` < `com_bigint_len(a)`
/// GUARANTEES: returns the `i`'th most significant word of `a`
u32 com_bigint_get_at(const com_bigint *a, usize i);

/// Set value of bigint's internal little endian representation at index i to val
/// REQUIRES: `a` is a valid pointer to a valid `com_bigint`
/// REQUIRES: `i` < `com_bigint_len(a)`
/// REQUIRES: `val` is the value to set
/// GUARANTEES: sets the `i`'th most significant word of `a` to `val
void com_bigint_set_at(com_bigint *a, usize i, u32 val);

#endif
