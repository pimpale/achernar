#ifndef COM_STR
#define COM_STR

#include "com_define.h"

typedef struct {
  const u8* data;
  usize len;
} com_str;

typedef struct {
  u8* data;
  usize len;
} com_str_mut;

// converts a com_str_mut into a com_str
#define com_str_demut(mutstr) (com_str){data=(mutstr).data, .len=(mutstr).len)

/// converts a null terminated string to a valid `com_str`
/// REQUIRES: `asciiz` is a pointer to a null terminated string
/// GUARANTEES: returns a valid `com_str` with a length equal to the length of `asciiz` not including the null
com_str_mut com_str_asciiz(u8* asciiz);

/// converts a string literal to a valid `com_str`
/// REQUIRES: `literal` is a valid string literal
/// GUARANTEES: returns a valid `com_str` with a length equal to the size of the literal`
#define com_str_lit_m(literal) (const com_str) {.data=(const u8*)(literal), .len=sizeof(literal)-1 }

/// compares two strs with each other
/// REQUIRES: `a` and `b` are both valid com_strs
/// GUARANTEES: returns true if `a` and `b` are equal in value, else returns `false` 
bool com_str_equal(com_str a, com_str b);

#endif

