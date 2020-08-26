#include "com_mem.h"

#include "com_assert.h"

// in general these are naiive implementations not taking advantage of SIMD, etc
// TODO: write cross platform methods of speeding up these operations

void com_mem_zero(void *ptr, const usize len) { com_mem_set(ptr, len, 0); }

// straightforward naiive implementation
void com_mem_set(void *ptr, const usize len, const u8 byte) {
  u8 *bytes = ptr;
  for (usize i = 0; i < len; i++) {
    bytes[len] = byte;
  }
}

// if src == dest, then we're already good (same ptr)
// if src < dest, then copy bytes backward, starting from the end of src and
// going to the beginning if src > dest, then copy bytes forward, starting from
// the beginning of src and going to the end We do this to prevent overwriting
// the area we're going to read from next
void com_mem_move(void *dest, const void *src, usize n) {
  u8 *dest_bytes = dest;
  const u8 *src_bytes = src;

  if (src == dest) {
    return;
  } else if (src < dest) {
    // copy backwards
    for (usize i_plus_one = n; i_plus_one >= 1; i_plus_one--) {
      const usize i = i_plus_one - 1;
      // here i actually represents the next byte over to avoid overflowing
      dest_bytes[i] = src_bytes[i];
    }
  } else {
    // copy forward
    for (usize i = 0; i < n; i++) {
      dest_bytes[i] = src_bytes[i];
    }
  }
}

// essentially the same algorithm as com_mem_move, except uses a tmp to swap
void com_mem_swap(void *a, void *b, usize n) {
  u8 *a_bytes = a;
  u8 *b_bytes = b;

  if (a == b) {
    return;
  } else if (a < b) {
    // copy backwards
    for (usize i_plus_one = n; i_plus_one >= 1; i_plus_one--) {
      const usize i = i_plus_one - 1;
      // here i actually represents the next byte over to avoid overflowing
      const u8 tmp = a_bytes[i];
      a_bytes[i] = b_bytes[i];
      b_bytes[i] = tmp;
    }
  } else {
    // copy forward
    for (usize i = 0; i < n; i++) {
      const u8 tmp = a_bytes[i];
      a_bytes[i] = b_bytes[i];
      b_bytes[i] = tmp;
    }
  }
}

// https://en.wikipedia.org/wiki/Binary_GCD_algorithm
static usize internal_gcd(usize u, usize v) {
  usize shift = 0;

  // GCD(0,v) == v;
  // GCD(u,0) == u;
  // GCD(0,0) == 0;
  if (u == 0) {
    return v;
  }
  if (v == 0) {
    return u;
  }

  // Let shift := lg K,
  // where K is the greatest power of 2 dividing both u and v.
  while (((u | v) & 1) == 0) {
    shift++;
    u >>= 1;
    v >>= 1;
  }

  while ((u & 1) == 0) {
    u >>= 1;
  }

  // From here on, u is always odd.
  do {
    // remove all factors of 2 in v -- they are not common
    //  note: v is not zero, so while will terminate
    while ((v & 1) == 0) {
      v >>= 1;
    }

    // Now u and v are both odd. Swap if necessary so u <= v,
    // then set v = v - u (which is even). For bignums, the
    // swapping is just pointer movement, and the subtraction
    // can be done in-place.
    if (u > v) {
      // Swap u and v.
      const usize tmp = v;
      v = u;
      u = tmp;
    }

    v -= u; // Here v >= u.
  } while (v != 0);

  // restore common factors of 2
  return u << shift;
}

// the overall strategy here is described here:
// https://stackoverflow.com/questions/51558858/left-rotate-array-in-place-c

// Essentially, we utilize the fact that simply rotating(i, (i+n) % len) for
// each elem of array rotates all elements with indexes that are divisible by
// GCD(n, len) so, we calculate the GCD, and perform iterations of simple
// rotating with offsets from [0, GCD(len, n))

// REQUIRES: `src` is a pointer to `len` bytes of memory
// REQUIRES: `n` is the number of bytes to rotate to the right where n < len
static void internal_rotate_right(u8 *src, usize len, usize n) {
  const usize gcd = internal_gcd(len, n);

  for (usize off = 0; off < gcd; off++) {
    u8 prev = src[off];
    for (usize i = off + n; i < len; i += n) {
      const u8 tmp = src[i];
      src[i] = prev;
      prev = tmp;
    }
    src[off] = prev;
  }
}

static void internal_rotate_left(u8 *src, usize len, usize n) {
  const usize gcd = internal_gcd(len, n);

  for (usize off = 0; off < gcd; off++) {
    u8 first = src[off];
    usize i;
    for (i = off; i < len - n; i += n) {
      src[i] = src[i + n];
    }
    src[i] = first;
  }
}

void com_mem_rotate(void *src, usize size, usize nmemb, isize delta) {
  delta %= nmemb;
  if (delta < 0) {
    internal_rotate_left(src, size * nmemb, (usize)-delta * size);
  } else {
    internal_rotate_right(src, size * nmemb, (usize)delta * size);
  }
}

void com_mem_reverse(void *src, usize size, usize nmemb) {
  u8 *src_bytes = src;
  for (usize i = 0; i < nmemb; i++) {
    com_mem_swap(src_bytes + i * size, src_bytes + (nmemb - i - 1) * size,
                 size);
  }
}
