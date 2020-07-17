#ifndef COM_FORMAT
#define COM_FORMAT

#include "com_define.h"
#include "com_allocator.h"
#include "com_writer.h"
#include "com_str.h"

typedef struct {
  // radix between 2 and 36, inclusive
  u8 radix;
  bool uppercaseAlphabet;
  bool showMinus;
  bool showPlus;
} com_format_Settings;

#define com_format_HEX_UNSIGNED_SETTING ((com_format_Settings) {.radix=16, .uppercaseAlphabet=true, .showMinus=false, .showPlus=false})
#define com_format_DEFAULT_SETTING ((com_format_Settings) {.radix=10, .uppercaseAlphabet=false, .showMinus=true, .showPlus=false})

typedef struct {
    // char to print on the left side if the length of the printed string is less than `min_width`
    u8 pad_char;
    // the minimum number of bytes to pad up to
    u32 min_width;
} com_format_PadData;

#define com_format_NO_PADDING ((com_format_PadData) { .pad_char=' ', .min_width=0})
#define com_format_ZERO_PADDING(len) ((com_format_PadData) { .pad_char='0', .min_width=(len)})
#define com_format_SPACE_PADDING(len) ((com_format_PadData) { .pad_char=' ', .min_width=(len)})

///  Interprets `data` as a char and appends it to `w` without escaping
/// REQUIRES: `w` is a pointer to a valid vector of u8s
/// GUARANTEES: `data` will be interpreted as an ascii character and appended to the w
void com_format_u8_char(com_writer* w, u8 data);
///  Appends the contents of `data` to `w` by copying
/// REQUIRES: `w` is a valid pointer to a valid com_writer
/// REQUIRES: `data` is a valid pointer to a valid `com_str`
/// GUARANTEES: `data` is unaltered
/// GUARANTEES: `w` is data->len bytes longer
/// GUARANTEES: the `data->len` bytes of the contents of `data` are appended to `w`
void com_format_str(com_writer* w, const com_str data);

///  Converts `data` to a string format with radix `radix` and then append to w
/// REQUIRES: `w` is a valid pointer to a valid com_writer
/// REQUIRES: `setting` is a valid com_format_Settings object
/// REQUIRES: `pad_data` is a valid com_format_PadData object
/// REQUIRES: `setting.radix` >= 2 && `setting.radix` <= 36
/// GUARANTEES: will losslessly append data to `w`
void com_format_i64(com_writer* w, i64 data, com_format_Settings setting, com_format_PadData pad_data);
void com_format_u64(com_writer* w, u64 data, com_format_Settings setting, com_format_PadData pad_data);


typedef struct {
  // how many digits to print (means max width)
  u32 significant_digits;
  // if the 
  bool exponential;
} com_format_FloatData;


void com_format_f32(com_writer* w, f32 data, com_format_Settings setting, com_format_PadData pad_data, com_format_FloatData float_data);
void com_format_f64(com_writer* w, f64 data, com_format_Settings setting, com_format_PadData pad_data, com_format_FloatData float_data);


///  Interprets `data` as a char and appends it to `w` escaping non plaintext characters
/// REQUIRES: `w` is a pointer to a valid vector of u8s
/// GUARANTEES: `data` will be interpreted as an ascii character and appended to the w, with exceptions for:
/// \b
/// \f
/// \n
/// \r
/// \t
/// "
/// \
/// And any character under 0x1F
void com_format_u8_char_checked(com_writer* w, u8 data); 
// does the same thing but for a string
void com_format_str_checked(com_writer* w, const com_str data);

///  Converts a utf code point to characters and puts it into `w`
/// REQUIRES: `utf` is a valid utf codepoint
/// REQUIRES: `w` is a valid pointer to a valid com_writer
/// GUARANTEES: `utf` will be converted into utf8 and appended to w
void com_format_append_utf_codepoint(com_writer* w, u32 utf);

#endif

