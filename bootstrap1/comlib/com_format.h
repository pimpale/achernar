#ifndef COM_FORMAT
#define COM_FORMAT

#include "com_allocator.h"
#include "com_define.h"
#include "com_str.h"
#include "com_writer.h"

// all unsigned numbers and zero count as positive
typedef enum {
  // will display a plus before positive numbers
  // will display a minus sign before negative numbers
  com_format_SignPlusMinus,
  // will display nothing before positive numbers
  // will display a minus before negative numbers
  com_format_SignMinus,
  // will display neither a sign on positive or negative numbers
  com_format_SignNone
} com_format_SignKind;

typedef enum {
  // Default is to right align
  com_format_AlignmentRight,
  // Can also selected left align
  com_format_AlignmentLeft,
} com_format_AlignmentKind;

typedef struct {
  // whether to pad left or right
  com_format_AlignmentKind alignment;
  // radix between 2 and 36, inclusive
  u8 radix;
  // character to pad with
  u8 pad_char;
  bool upper;
  // sign to use
  com_format_SignKind sign;
  // will be padded up to this number
  // use zero for no padding
  u32 min_width;
  // how many digits of precision are required
  // use u32_max_m for max precision
  u32 precision;
} com_format_FormatData;

#define com_format_DEFAULT_SETTING                                             \
  (com_format_FormatData) {                                                    \
    .alignment = com_format_AlignmentLeft, .radix = 10, .pad_char = ' ',       \
    .upper = true, .sign = com_format_SignMinus, .min_width = 0,               \
    .precision = 16                                                            \
  }

///  Appends the contents of `data` to `w` by copying
/// REQUIRES: `w` is a valid pointer to a valid com_writer
/// REQUIRES: `data` is a valid pointer to a valid `com_str`
/// GUARANTEES: `data` is unaltered
/// GUARANTEES: `w` is data->len bytes longer
/// GUARANTEES: the `data->len` bytes of the contents of `data` are appended to
/// `w`
void com_format_str(com_writer *w, const com_str data);

///  Converts `data` to a string format with radix `radix` and then append to w
/// REQUIRES: `w` is a valid pointer to a valid com_writer
/// REQUIRES: `setting` is a valid com_format_Settings object
/// REQUIRES: `pad_data` is a valid com_format_PadData object
/// REQUIRES: `setting.radix` >= 2 && `setting.radix` <= 36
/// GUARANTEES: will losslessly append data to `w`
void com_format_i64(com_writer *w, i64 data, com_format_FormatData fmtdata);
void com_format_u64(com_writer *w, u64 data, com_format_FormatData fmtdata);

typedef enum {
  com_format_FloatFixed,
  com_format_FloatScientific,
  com_format_FloatDefault,
} com_format_FloatKind;

void com_format_f32(com_writer *w, f32 data, com_format_FormatData fmtdata,
                    com_format_FloatKind kind);
void com_format_f64(com_writer *w, f64 data, com_format_FormatData fmtdata,
                    com_format_FloatKind kind);

///  Interprets `data` as a char and appends it to `w` escaping non plaintext
///  characters
/// REQUIRES: `w` is a pointer to a valid vector of u8s
/// GUARANTEES: `data` will be interpreted as an ascii character and appended to
/// the w, with exceptions for: '\b' '\f' '\n' '\r' '\' '\"' '\\'
/// And any character under 0x1F
void com_format_u8_char_checked(com_writer *w, u8 data);
// does the same thing but for a string
void com_format_str_checked(com_writer *w, const com_str data);

///  Converts a utf code point to characters and puts it into `w`
/// REQUIRES: `utf` is a valid utf codepoint
/// REQUIRES: `w` is a valid pointer to a valid com_writer
/// GUARANTEES: `utf` will be converted into utf8 and appended to w
void com_format_append_utf_codepoint(com_writer *w, u32 utf);

// GUARANTEES:  returns true if c is a digit (0-9) else false
bool com_format_is_digit(u8 c);
// GUARANTEES: returns true if c is a-z else false
bool com_format_is_lower_alpha(u8 c);
// GUARANTEES: returns true if c is A-Z else false
bool com_format_is_upper_alpha(u8 c);
// GUARANTEES: returns true if c is A-Z or a-z else false
bool com_format_is_alpha(u8 c);
// GUARANTEES: returns true if c is A-Z, a-z, or 0-9 else false
bool com_format_is_alphanumeric(u8 c);
// GUARANTEES: follows https://tools.ietf.org/html/rfc4627 specification
// GUARANTEES: returns true if `c` is \u0020, \u009, \u00A, \u000D else false
bool com_format_is_whitespace(u8 c);

// GUARANTEES: returns true if `c` is 0-9 a-f A-F else false
bool com_format_is_hex(u8 c);

// REQUIRES: `c` is 0-9 a-f A-F
// GUARANTEES: returns a number 0-15
u8 com_format_from_hex(u8 c);

// REQUIRES: `c` is less than 16
// GUARANTEES: returns digits '0'-'9' for numbers 0-9
// GUARANTEES: for 10-15 and upper=true, returns A-F
// GUARANTEES: for 10-15 and upper=false returns a-f
u8 com_format_to_hex(u8 c, bool upper);

#endif
