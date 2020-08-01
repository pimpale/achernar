#include "com_format.h"
#include "com_assert.h"
#include "com_mem.h"
#include "com_str.h"

// Accepts Vector<char>, pushes as many chars points as needed to encode the
// data
void com_format_append_utf_codepoint(com_writer *w, u32 utf) {
  com_assert_m(utf <= 0x10FFFF, "utf is not a valid codepoint");

  if (utf <= 0x7F) {
    // Plain ASCII
    u8 bytes[] = {(u8)utf};
    com_writer_append_str(w, com_str_lit_m(bytes));
  } else if (utf <= 0x07FF) {
    // 2-byte unicode
    u8 bytes[] = {(u8)(((utf >> 6) & 0x1F) | 0xC0),
                  (u8)(((utf >> 0) & 0x3F) | 0x80)};
    com_writer_append_str(w, com_str_lit_m(bytes));
  } else if (utf <= 0xFFFF) {
    // 3-byte unicode
    u8 bytes[] = {
        (u8)(((utf >> 12) & 0x0F) | 0xE0),
        (u8)(((utf >> 6) & 0x3F) | 0x80),
        (u8)(((utf >> 0) & 0x3F) | 0x80),
    };
    com_writer_append_str(w, com_str_lit_m(bytes));
  } else if (utf <= 0x10FFFF) {
    u8 bytes[] = {
        (u8)(((utf >> 18) & 0x07) | 0xF0),
        (u8)(((utf >> 12) & 0x3F) | 0x80),
        (u8)(((utf >> 6) & 0x3F) | 0x80),
        (u8)(((utf >> 0) & 0x3F) | 0x80),
    };
    com_writer_append_str(w, com_str_lit_m(bytes));
  }
}

// GUARANTEES:  returns true if c is a digit (0-9) else false
bool com_format_is_digit(u8 c) { return c >= '0' && c <= '9'; }

// GUARANTEES: returns true if c is a-z else false
bool com_format_is_lower_alpha(u8 c) { return c >= 'a' && c <= 'z'; }
// GUARANTEES: returns true if c is A-Z else false
bool com_format_is_upper_alpha(u8 c) { return c >= 'A' && c <= 'Z'; }
// GUARANTEES: returns true if c is A-Z or a-z else false
bool com_format_is_alpha(u8 c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

// GUARANTEES: returns true if c is A-Z, a-z, or 0-9 else false
bool com_format_is_alphanumeric(u8 c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
         (c >= '0' && c <= '9');
}

// GUARANTEES: follows https://tools.ietf.org/html/rfc4627 specification
// GUARANTEES: returns true if `c` is 0x20, 0x09, 0x0A, 0x0D else false
bool com_format_is_whitespace(u8 c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

// GUARANTEES: returns true if `c` is 0-9 a-f A-F else false
bool com_format_is_hex(u8 c);

// REQUIRES: `c` is 0-9 a-f A-F
// GUARANTEES: returns a number 0-15
u8 com_format_from_hex(u8 c) {
  switch (c) {
  case '0': {
    return 0x0;
  }
  case '1': {
    return 0x1;
  }
  case '2': {
    return 0x2;
  }
  case '3': {
    return 0x3;
  }
  case '4': {
    return 0x4;
  }
  case '5': {
    return 0x5;
  }
  case '6': {
    return 0x6;
  }
  case '7': {
    return 0x7;
  }
  case '8': {
    return 0x8;
  }
  case '9': {
    return 0x9;
  }
  case 'a': {
    return 0xa;
  }
  case 'b': {
    return 0xb;
  }
  case 'c': {
    return 0xc;
  }
  case 'd': {
    return 0xd;
  }
  case 'e': {
    return 0xe;
  }
  case 'f': {
    return 0xf;
  }
  case 'A': {
    return 0xA;
  }
  case 'B': {
    return 0xB;
  }
  case 'C': {
    return 0xC;
  }
  case 'D': {
    return 0xD;
  }
  case 'E': {
    return 0xE;
  }
  case 'F': {
    return 0xF;
  }
  default: {
    com_assert_unreachable_m("c has an invalid value");
  }
  }
}

u8 com_format_to_hex(u8 c, bool upper) {
  if (c < 10) {
    return '0' + c;
  } else {
    if (upper) {
      return 'A' + c;
    } else {
      return 'a' + c;
    }
  }
}

void com_format_u8_char_checked(com_writer *w, u8 data) {
  // convert stuff that could break a string into non string breaking
  switch (data) {
  case '\b': {
    com_writer_append_str(w, com_str_lit_m("\\b"));
    break;
  }
  case '\f': {
    com_writer_append_str(w, com_str_lit_m("\\f"));
    break;
  }
  case '\n': {
    com_writer_append_str(w, com_str_lit_m("\\n"));
    break;
  }
  case '\r': {
    com_writer_append_str(w, com_str_lit_m("\\r"));
    break;
  }
  case '\t': {
    com_writer_append_str(w, com_str_lit_m("\\t"));
    break;
  }
  case '\"': {
    com_writer_append_str(w, com_str_lit_m("\\\""));
    break;
  }
  case '\\': {
    com_writer_append_str(w, com_str_lit_m("\\\\"));
    break;
  }
  default: {
    // convert low unicode to safe representation
    if (data <= 0x001F) {
      com_writer_append_str(w, com_str_lit_m("\\u"));
      com_format_FormatData fmtd = com_format_DEFAULT_SETTING;
      fmtd.min_width = 4;
      fmtd.pad_char = '0';
      com_format_u64(w, 16, fmtd);
    } else {
      com_writer_append_u8(w, data);
    }
    break;
  }
  }
}

// Checks for special characters
void com_format_str_checked(com_writer *w, const com_str data) {
  for (usize i = 0; i < data.len; i++) {
    com_format_u8_char_checked(w, data.data[i]);
  }
}

// internal method to handle both i64 and u64
static void internal_u64_negative(com_writer *w, u64 data,
                                  bool negative, com_format_FormatData s) {

  com_assert_m(s.radix >= 2 && s.radix <= 36, "radix must be between 2 and 36");

  // buffer to push to (even with base 2 should be enough since there are
  // still only 64 bits) extra byte is for the 65
  u8 buffer[65];

  u64 digit = data;

  i64 index = 0;
  while (index < 64) {
    buffer[index] = com_format_to_hex(digit % s.radix, s.upper);
    digit /= s.radix;
    if (digit == 0) {
      break;
    }
    index++;
  }

  // handle signs by pushing them at the end
  if (negative && (s.sign == com_format_SignMinus || com_format_SignPlusMinus)) {
    buffer[index] = '-';
    index++;
  } else if (s.sign == com_format_SignPlusMinus) {
    buffer[index] = '+';
    index++;
  }

  i64 num_pad_needed = (i64)s.min_width - index;

	if(s.alignment == com_format_AlignmentLeft) {
    // this always pads left
    for (i64 i = 0; i < num_pad_needed; i++) {
      com_writer_append_u8(w, s.pad_char);
    }
	}

  // push buffer in reverse order
  while (index >= 0) {
    com_writer_append_u8(w, buffer[index]);
    index--;
  }

	if(s.alignment == com_format_AlignmentRight) {
    // this always pads left
    for (i64 i = 0; i < num_pad_needed; i++) {
      com_writer_append_u8(w, s.pad_char);
    }
	}
}


void com_format_u64(com_writer *w, u64 data, com_format_FormatData fmtdata) {
	internal_u64_negative(w, data, false, fmtdata);
}

void com_format_i64(com_writer *w, i64 data, com_format_FormatData fmtdata) {
  if (data < 0) {
    internal_u64_negative(w, (u64)-data, true, fmtdata);
  } else {
    internal_u64_negative(w, (u64)data, false, fmtdata);
  }
}
