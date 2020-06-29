#include "com_format.h"
#include "com_assert.h"
#include "com_mem.h"
#include "com_str.h"

// Accepts Vector<char>, pushes as many chars points as needed to encode the
// data
void com_format_append_utf_codepoint(com_vec *builder, u32 utf) {
  com_assert_m(utf <= 0x10FFFF, "utf is not a valid codepoint");

  if (utf <= 0x7F) { // Plain ASCII
    u8 *out = com_vec_push(builder, sizeof(u8) * 1);
    out[0] = (u8)utf;
  } else if (utf <= 0x07FF) {
    // 2-byte unicode
    u8 *out = com_vec_push(builder, sizeof(u8) * 2);
    out[0] = (u8)(((utf >> 6) & 0x1F) | 0xC0);
    out[1] = (u8)(((utf >> 0) & 0x3F) | 0x80);
  } else if (utf <= 0xFFFF) {
    // 3-byte unicode
    u8 *out = com_vec_push(builder, sizeof(u8) * 3);
    out[0] = (u8)(((utf >> 12) & 0x0F) | 0xE0);
    out[1] = (u8)(((utf >> 6) & 0x3F) | 0x80);
    out[2] = (u8)(((utf >> 0) & 0x3F) | 0x80);
  } else if (utf <= 0x10FFFF) {
    // 4-byte unicode
    u8 *out = com_vec_push(builder, sizeof(u8) * 4);
    out[0] = (u8)(((utf >> 18) & 0x07) | 0xF0);
    out[1] = (u8)(((utf >> 12) & 0x3F) | 0x80);
    out[2] = (u8)(((utf >> 6) & 0x3F) | 0x80);
    out[3] = (u8)(((utf >> 0) & 0x3F) | 0x80);
  }
}

void com_format_append_u8_char(com_vec *builder, u8 data) {
  *com_vec_push_m(builder, u8) = data;
}

static u8 toHex(u8 x, bool capital) {
  if (x < 10) {
    return '0' + (char)x;
  } else {
    if (capital) {
      return 'A' + (char)x;
    } else {
      return 'a' + (char)x;
    }
  }
}

void com_format_str(com_vec *builder, const com_str data) {
  com_mem_move(com_vec_push(builder, data.len), data.data, data.len);
}

void com_format_u8_char_checked(com_vec *builder, u8 data) {
  // convert stuff that could break a string into non string breaking
  switch (data) {
  case '\b': {
    com_format_str(builder, com_str_from_literal_m("\\b"));
    break;
  }
  case '\f': {
    com_format_str(builder, com_str_from_literal_m("\\f"));
    break;
  }
  case '\n': {
    com_format_str(builder, com_str_from_literal_m("\\n"));
    break;
  }
  case '\r': {
    com_format_str(builder, com_str_from_literal_m("\\r"));
    break;
  }
  case '\t': {
    com_format_str(builder, com_str_from_literal_m("\\t"));
    break;
  }
  case '\"': {
    com_format_str(builder, com_str_from_literal_m("\\\""));
    break;
  }
  case '\\': {
    com_format_str(builder, com_str_from_literal_m("\\\\"));
    break;
  }
  default: {
    // convert low unicode to safe representation
    if (data <= 0x001F) {
      com_format_str(builder, com_str_from_literal_m("\\u"));
      com_format_u64(builder, 16, data, com_format_FLAGS_NONE, com_format_ZERO_PADDING(4));
    } else {
      com_format_u8_char(builder, data);
    }
    break;
  }
  }
}

// Checks for special characters
void com_format_str_checked(com_vec *builder, const com_str data) {
  for (usize i = 0; i < data.len; i++) {
    com_format_u8_char_checked(builder, data.data[i]);
  }
}

// internal method to handle both i64 and u64
static void format_u64_negative(com_vec *builder, u8 radix, u64 data,
                                bool negative, com_format_Flags flags,
                                com_format_PadData pad_data) {
  com_assert_m(radix >= 2 && radix <= 36, "radix must be between 2 and 36");

  // buffer to push to (even with base 2 should be enough since there are still
  // only 64 bits)
  // extra byte is for the 65
  char buffer[65];

  u64 digit = data;

  i64 index = 0;
  while (index < 64) {
    buffer[index] = toHex(digit % radix, flags & com_format_HEX_UPPER);
    digit /= radix;
    if (digit == 0) {
      break;
    }
    index++;
  }

  // handle signs by pushing them at the end
  if (negative && (flags & com_format_MINUS_VISIBLE)) {
    buffer[index] = '-';
    index++;
  } else if (flags & com_format_PLUS_VISIBLE) {
    buffer[index] = '+';
    index++;
  }

  // this always pads left

  // how many pad_chars to push
  i64 num_pad_needed = (i64)pad_data.min_width - index;
  if (num_pad_needed > 0) {
    // reserve space and then write the pad char
    com_mem_set(com_vec_push(builder, num_pad_needed), num_pad_needed,
                pad_data.pad_char);
  }

  // push buffer in reverse order
  while (index >= 0) {
    com_format_u8_char(builder, buffer[index]);
    index--;
  }
}

u64 safe_abs(i64 val) {
  if (val < 0) {
    return (u64)-val;
  } else {
    return (u64)val;
  }
}

void com_format_i64(com_vec *builder, u8 radix, i64 data,
                    com_format_Flags flags, com_format_PadData pad_data) {
  bool negative = data < 0;
  format_u64_negative(builder, radix, safe_abs(data), negative, flags,
                      pad_data);
}

void com_format_u64(com_vec *builder, u8 radix, u64 data,
                    com_format_Flags flags, com_format_PadData pad_data) {
  format_u64_negative(builder, radix, data, false, flags, pad_data);
}

void com_format_f32(com_vec *builder, u8 radix, f32 data,
                    com_format_Flags flags, com_format_PadData pad_data);
void com_format_f64(com_vec *builder, u8 radix, f64 data,
                    com_format_Flags flags, com_format_PadData pad_data);

void com_format_f32_exp(com_vec *builder, u8 radix, f32 data,
                        com_format_Flags flags, com_format_PadData pad_data,
                        u32 sig_digits) {}

void com_format_f64_exp(com_vec *builder, u8 radix, f64 data,
                        com_format_Flags flags, com_format_PadData pad_data,
                        u32 sig_digits) {}
