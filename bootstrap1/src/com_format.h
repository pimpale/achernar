#ifndef COM_FORMAT
#define COM_FORMAT

#include "com_define.h"
#include "com_allocator.h"
#include "com_vec.h"
#include "com_str.h"

typedef enum {
  com_format_FLAGS_NONE = 0,
  // if true will display plus
  // ignored for unsigned
  com_format_PLUS_VISIBLE = 1<<2,
  // if true will display minus
  // ignored for unsigned
  com_format_MINUS_VISIBLE = 1<<3,
  // which direction to pad in
  com_format_HEX_UPPER= 1<<4,
} com_format_Flags;

/** Interprets `data` as a char and appends it to `builder` without escaping
 * REQUIRES: `builder` is a pointer to a valid vector of u8s
 * GUARANTEES: `data` will be interpreted as an ascii character and appended to the builder
 */
void com_format_u8_char(com_vec* builder, u8 data);
/** Appends the contents of `data` to `builder` by copying
 * REQUIRES: `builder` is a valid pointer to a valid com_vec
 * REQUIRES: `data` is a valid pointer to a valid `com_str`
 * GUARANTEES: `data` is unaltered
 * GUARANTEES: `builder` is data->len bytes longer
 * GUARANTEES: the `data->len` bytes of the contents of `data` are appended to `builder`
 */
void com_format_str(com_vec* builder, const com_str data);

/** Converts `data` to a string format with radix `radix` and then append to builder
 * REQUIRES: `builder` is a valid pointer to a valid com_vec
 * REQUIRES: `flags` is a valid com_format_Flags object
 * REQUIRES: `radix` > 1 && `radix` <= 36
 * GUARANTEES: will losslessly append data to `builder`
 */
void com_format_i64(com_vec* builder, u8 radix, i64 data, com_format_Flags flags, u8 min_width, u8 pad_char);
void com_format_u64(com_vec* builder, u8 radix, u64 data, com_format_Flags flags, u8 min_width, u8 pad_char);
void com_format_f32(com_vec* builder, u8 radix, f32 data, com_format_Flags flags, u8 min_width, u8 pad_char);
void com_format_f64(com_vec* builder, u8 radix, f64 data, com_format_Flags flags, u8 min_width, u8 pad_char);

// may be lossy
// TODO: write full description from wikipedia
//  https://en.wikipedia.org/wiki/Printf_format_string
void com_format_f32_exp(com_vec* builder, u8 radix, f32 data, com_format_Flags flags);
void com_format_f64_exp(com_vec* builder, u8 radix, f64 data, com_format_Flags flags);

/** Interprets `data` as a char and appends it to `builder` escaping non plaintext characters
 * REQUIRES: `builder` is a pointer to a valid vector of u8s
 * GUARANTEES: `data` will be interpreted as an ascii character and appended to the builder, with exceptions for:
 * \b
 * \f
 * \n
 * \r
 * \t
 * "
 * \
 * And any character under 0x1F
 */
void com_format_u8_char_checked(com_vec* builder, u8 data); 
// does the same thing but for a string
void com_format_str_checked(com_vec* builder, const com_str data);

/** Converts a utf code point to characters and puts it into `builder`
 * REQUIRES: `utf` is a valid utf codepoint
 * REQUIRES: `builder` is a valid pointer to a valid com_vec
 * GUARANTEES: `utf` will be converted into utf8 and appended to builder
 */
void com_format_append_utf_codepoint(com_vec* builder, u32 utf);


#endif

