#ifndef COM_BIGDECIMAL_H
#define COM_BIGDECIMAL_H

#include "com_allocator.h"
#include "com_bigint.h"

typedef struct {
  // the precision of the integer
  // this is essentially how many words the decimal point has been moved up
  usize _precision;
  com_bigint _value;
} com_bigdecimal;

/// creates a bigdecimal from a magnitude and the sign
/// REQUIRES: `magnitude` is a valid `value`
/// GUARANTEES: returns a valid `com_biguint` using `value`
/// GUARANTEES: if `negative`, then the returned value will be negative
/// GUARANTEES: `value` is consumed
/// GUARANTEES: precision of returned value is zero
com_bigdecimal com_bigdecimal_from(com_bigint value);

/// creates a bigdecimal from an allocator handle, with value 0
/// REQUIRES: `h` is a valid com_allocator_Handle
/// GUARANTEES: returns a valid `com_bigdecimal` with value 0 and sign 0
com_bigdecimal com_bigdecimal_create(com_allocator_Handle h);

/// gets the unscaled magnitude of bigdecimal
/// REQUIRES: `a` is a valid com_bigdecimal
/// REQUIRES: `dest` is a valid com_biguint
/// GUARANTEES: sets dest to the magnitude of `a`
void com_bigdecimal_magnitude(com_biguint* dest, const com_bigdecimal *a);

/// gets the precision of a
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: returns `a.precision`
usize com_bigdecimal_get_precision(const com_bigdecimal* a);

/// sets the precision of a
/// REQUIRES: `a` is a valid pointer to a valid com_bigdecimal
/// REQUIRES: `prec` is the precision to set to 
/// GUARANTEES: `a.precision` is now `prec`
/// GUARANTEES: if `prec > a.precision` then no data is lost
void com_bigdecimal_set_precision(com_bigdecimal *a, usize prec);

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
/// REQUIRES: `a` and `b` have the same precision
/// GUARANTEES: `dest` will be overwritten with the sum of `a` and `b`
/// GUARANTEES: `dest` will have the same precision as a and b
void com_bigdecimal_add(com_bigdecimal *dest, const com_bigdecimal *a, const com_bigdecimal *b);

/// dest := a - b
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `a` and `b` have the same precision
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: `dest` will be overwritten by `a` - `b`
/// GUARANTEES: `dest` will have the same precision as a and b
void com_bigdecimal_sub(com_bigdecimal *dest, const com_bigdecimal *a, const com_bigdecimal *b);

/// dest := a * b
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `dest` will be overwritten by `a` * `b`
/// GUARANTEES: `dest` will have the a's precision + b's precision
void com_bigdecimal_mul(com_bigdecimal *dest, const com_bigdecimal *a, const com_bigdecimal *b,
                    com_allocator *allocator);
/// dest := a / b
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `dest` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `a`'s precision must be greater than or equal to `b`'s precision
/// REQUIRES: `allocator` is a valid `com_allocator`
/// GUARANTEES: `dest` will be overwritten by `a` / `b`
/// GUARANTEES: `dest` will have the a's precision - b's precision
void com_bigdecimal_div(com_bigdecimal *dest, const com_bigdecimal *a, const com_bigdecimal *b,
                    com_allocator *allocator);

/* Special operators and comparison */

// compares b with reference to a
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `b` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `a` and `b` have the same precision
/// GUARANTEES: if `b` is greater than `a` will return com_math_GREATER
/// GUARANTEES: if `b` is less than `a` will return com_math_LESS
/// GUARANTEES: if `b` is equal to `a` will return com_math_EQUAL
com_math_cmptype com_bigdecimal_cmp(const com_bigdecimal *a, const com_bigdecimal *b);

/// returns the sign of a bigdecimal
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: returns `com_math_ZERO` if `a` is zero
/// GUARANTEES: returns `com_math_POSITIVE` if `a` is greater than 0
/// GUARANTEES: returns `com_math_NEGATIVE` if `a` is less than 0
com_math_signtype com_bigdecimal_sign(const com_bigdecimal *a);

/// negates `a`
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: if `a` != 0 the sign has been flipped
void com_bigdecimal_negate(com_bigdecimal *a);

/// returns true if `a` is zero
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: if `a` is zero, returns true
/// GUARANTEES: if `a` is not zero, returns false
bool com_bigdecimal_is_zero(const com_bigdecimal *a);

/* Functions to inspect the layout of the bigdecimal */

/// Returns the number of u32 words in `a`
/// REQUIRES: `a` must be a valid pointer to a valid com_bigdecimal
/// GUARANTEES: returns the number of words in the internal representation of `a`
/// GUARANTEES: the number returned is equal to ceil(log(a)/log(u32_max_m))
usize com_bigdecimal_len(const com_bigdecimal *a);

/// Get value of bigdecimal's internal little endian representation at index i
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `i` < `com_bigdecimal_len(a)`
/// GUARANTEES: returns the `i`'th most significant word of `a`
u32 com_bigdecimal_get_at(const com_bigdecimal *a, usize i);

/// Set value of bigdecimal's internal little endian representation at index i to val
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// REQUIRES: `i` < `com_bigdecimal_len(a)`
/// REQUIRES: `val` is the value to set
/// GUARANTEES: sets the `i`'th most significant word of `a` to `val
void com_bigdecimal_set_at(com_bigdecimal *a, usize i, u32 val);

/// Removes extra bits of precision from `a`
/// REQUIRES: `a` is a valid pointer to a valid `com_bigdecimal`
/// GUARANTEES: the value of `a` is unchanged
/// GUARANTEES: any extra words of precision are removed
void com_bigdecimal_remove_trailing_zero(com_bigdecimal *a);

#endif
