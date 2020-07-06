#ifndef COM_STR
#define COM_STR

#include "com_define.h"

typedef struct {
  u8* data;
  usize len;
} com_str;

/* creates a new `com_str`
 * REQUIRES: `data` is a pointer to a valid string with length at least `len`
 * REQUIRES: `len` is the number of bytes to include in `len`
 * GUARANTEES: returns a valid `com_str` with len `len` and data `data`
 */
com_str com_str_create(u8* data, usize len);

/* converts a null terminated string to a valid `com_str`
 * REQUIRES: `asciiz` is a pointer to a null terminated string
 * GUARANTEES: returns a valid `com_str` with a length equal to the length of `asciiz` not including the null
 */
com_str com_str_asciiz(u8* asciiz);

/* converts a string literal to a valid `com_str`
 * REQUIRES: `literal` is a valid string literal
 * GUARANTEES: returns a valid `com_str` with a length equal to the size of the literal`
 */
#define com_str_lit_m(literal) com_str_create((u8*)(literal), sizeof(literal)-1)

/* compares two strs with each other
 * REQUIRES: `a` and `b` are both valid com_strs
 * GUARANTEES: returns true if `a` and `b` are equal in value, else returns `false` 
 */
bool com_str_equal(const com_str a, const com_str b);

#endif

