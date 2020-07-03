#ifndef COM_BIGINT_H
#define COM_BIGINT_H


#include "com_define.h"
#include "com_vec.h"
#include "com_math.h"
#include "com_allocator.h"

/* Data-holding structure: array of u32s */
typedef struct {
  // vector of u32s
  com_vec _array;
  // if the num is negative
  bool _negative;
} com_bigint;

// bigint creation

/** creates a bigint from an allocator (default value is 0)
 * REQUIRES: `h` a valid com_allocator_Handle suitable to create a `com_vec` with
 * GUARANTEES: returns a valid `com_bigint` with value 0
 */
com_bigint com_bigint_create(com_allocator_Handle h);

// bigint destruction

/** frees bigint and returns the closest valid i64
 * REQUIRES: `a` is a valid pointer to a `com_bigint`
 * GUARANTEES:  `a` is no longer a valid `com_bigint`
 * GUARANTEES: all memory associated with `a` will be deallocated
 */
i64 com_bigint_release(com_bigint* a);

/** Sets the value of a bigint to a i64
 * REQUIRES: `dest` is a valid pointer to a valid `com_bigint`
 * GUARANTEES: the value of  `dest` is now equal to `val`
 */
void com_bigint_set_i64(com_bigint* dest, i64 val);

/** Returns the nearest value the value of a bigint 
 * REQUIRES: `a` is a valid pointer to a valid `com_bigint`
 * GUARANTEES: returnst the value of `i64` closest to the value of `a`
 */
i64 com_bigint_get_i64(const com_bigint* a);


/** Copies the value of a bigint to a bigint
 * REQUIRES: `dest` is a valid pointer to a valid `com_bigint`
 * GUARANTEES: the value of  `dest` is now equal to `val`
 */
void com_bigint_set(com_bigint* dest, const com_bigint* src);

/* Basic arithmetic operations: */
void com_bigint_add(com_bigint* dest, const com_bigint* a, const com_bigint* b);
void com_bigint_sub(com_bigint* dest, const com_bigint* a, const com_bigint* b);
void com_bigint_mul(com_bigint* dest, const com_bigint* a, const com_bigint* b);
void com_bigint_div(com_bigint* quotient, com_bigint* remainder, const com_bigint* a, const com_bigint* b);

/* Bitwise operations: */
void com_bigint_and(com_bigint* dest, const com_bigint* a, const com_bigint* b); 
void com_bigint_or(com_bigint* dest, const com_bigint* a, const com_bigint* b);  
void com_bigint_xor(com_bigint* dest, const com_bigint* a, const com_bigint* b); 
void com_bigint_lshift(com_bigint* dest, const com_bigint* a, const u64 nbits); 
void com_bigint_rshift(com_bigint* dest, const com_bigint* a, const u64 nbits); 

/* Special operators and comparison */
com_math_cmptype com_bigint_cmp(const com_bigint* a, const com_bigint* b);
com_math_cmptype com_bigint_cmp_i64(const com_bigint* a, i64 b);
com_math_signtype com_bigint_sign(com_bigint* n);

#endif
