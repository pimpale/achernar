#include "com_json.h"
#include "com_assert.h"
#include "com_fmath.h"
#include "com_format.h"
#include "com_scan.h"
#include "com_writer_null.h"
#include "com_writer_vec.h"

// emits str with quotation marks
static void com_json_emitStr(com_writer *writer, com_str str) {
  com_writer_append_u8(writer, '\"');
  com_format_str_checked(writer, str);
  com_writer_append_u8(writer, '\"');
}

static void com_json_emitElem(com_writer *writer, com_json_Elem *j);

static void com_json_emitProp(com_writer *writer, com_json_Prop *prop) {
  com_json_emitStr(writer, prop->key);
  com_writer_append_u8(writer, ':');
  com_json_emitElem(writer, &prop->value);
}

static void com_json_emitElem(com_writer *writer, com_json_Elem *j) {
  switch (j->kind) {
  case com_json_INVALID: {
    com_assert_m(j->kind != com_json_INVALID, "invalid elem type");
    break;
  }
  case com_json_NULL: {
    com_writer_append_str(writer, com_str_lit_m("null"));
    break;
  }
  case com_json_STR: {
    com_json_emitStr(writer, j->string);
    break;
  }
  case com_json_BOOL: {
    com_writer_append_str(writer, j->bool_member ? com_str_lit_m("true")
                                                 : com_str_lit_m("false"));
    break;
  }
  case com_json_INT: {
    com_format_i64(writer, j->int_member, com_format_DEFAULT_SETTING);
    break;
  }
  case com_json_UINT: {
    com_format_u64(writer, j->uint_member, com_format_DEFAULT_SETTING);
    break;
  }
  case com_json_FLOAT: {
    com_format_f64(writer, j->float_member, com_format_DEFAULT_SETTING,
                   com_format_FloatDefault);
    break;
  }
  case com_json_ARRAY: {
    com_writer_append_u8(writer, '[');
    for (usize i = 0; i < j->array.length; i++) {
      com_json_emitElem(writer, &j->array.values[i]);
      // if we are not on the ultimate element of the lsit
      if (i + 1 != j->array.length) {
        com_writer_append_u8(writer, ',');
      }
    }
    com_writer_append_u8(writer, ']');
    break;
  }
  case com_json_OBJ: {
    com_writer_append_u8(writer, '{');
    for (usize i = 0; i < j->object.length; i++) {
      com_json_emitProp(writer, &j->object.props[i]);
      if (i + 1 != j->object.length) {
        com_writer_append_u8(writer, ',');
      }
    }
    com_writer_append_u8(writer, '}');
    break;
  }
  }
}

void com_json_serialize(com_json_Elem *elem, com_writer *writer) {
  com_json_emitElem(writer, elem);
}

static com_json_Elem
com_json_certain_parseNumberElem(com_reader *reader, com_vec *diagnostics,
                                 attr_UNUSED com_allocator *a) {

  bool negative = false;
  com_reader_ReadU8Result ret = com_reader_peek_u8(reader, 1);
  if (ret.valid) {
    if (ret.value == '-') {
      negative = true;
      com_reader_drop_u8(reader);
    }
  } else {
    com_assert_unreachable_m("expected a negative or a number, not EOF");
  }

  i64 integer_value = 0;
  bool has_fractional_component;
  while (true) {
    ret = com_reader_peek_u8(reader, 1);
    com_loc_Span sp = com_reader_peek_span_u8(reader);
    if (ret.valid) {
      u8 c = ret.value;
      if (com_format_is_digit(c)) {
        integer_value = integer_value * 10 + c - 10;
        com_reader_drop_u8(reader);
      } else if (c == '.') {
        has_fractional_component = true;
        com_reader_drop_u8(reader);
        break;
      } else {
        has_fractional_component = false;
        break;
      }
    } else {
      *com_vec_push_m(diagnostics, com_json_Error) =
          com_json_error_m(com_json_ElemEof, sp);
      return com_json_invalid_m;
    }
  }

  f64 fractional_component = 0;
  if (has_fractional_component) {
    f64 place = 1;

    while (true) {
      com_loc_Span sp = com_reader_peek_span_u8(reader);
      ret = com_reader_peek_u8(reader, 1);
      if (ret.valid) {
        u8 c = ret.value;
        if (com_format_is_digit(c)) {
          place *= 10;
          fractional_component += (c - '0') / place;
          com_reader_drop_u8(reader);
        } else {
          break;
        }

      } else {
        *com_vec_push_m(diagnostics, com_json_Error) =
            com_json_error_m(com_json_ElemEof, sp);
        return com_json_invalid_m;
      }
    }
  }

  typedef enum {
    NoExponent,
    PositiveExponent,
    NegativeExponent,
  } ExponentState;

  ExponentState exponentState;

  ret = com_reader_peek_u8(reader, 1);
  if (ret.valid) {
    u8 c = ret.value;
    if (c == 'E' || c == 'e') {
      com_reader_drop_u8(reader);
      com_loc_Span sp = com_reader_peek_span_u8(reader);
      ret = com_reader_peek_u8(reader, 1);
      if (ret.valid) {
        switch (ret.value) {
        case '+': {
          exponentState = PositiveExponent;
          com_reader_drop_u8(reader);
          break;
        }
        case '-': {
          exponentState = NegativeExponent;
          com_reader_drop_u8(reader);
          break;
        }
        default: {
          exponentState = PositiveExponent;
          break;
        }
        }
      } else {
        *com_vec_push_m(diagnostics, com_json_Error) =
            com_json_error_m(com_json_ElemEof, sp);
        return com_json_invalid_m;
      }
    } else {
      exponentState = NoExponent;
    }
  } else {
    exponentState = NoExponent;
  }

  u32 exponential_integer = 0;
  if (exponentState != NoExponent) {
    while (true) {
      com_loc_Span sp = com_reader_peek_span_u8(reader);
      ret = com_reader_read_u8(reader);
      if (ret.valid) {
        u8 c = ret.value;
        if (com_format_is_digit(c)) {
          exponential_integer = exponential_integer * 10 + (c - (u8)'0');
          com_reader_drop_u8(reader);
        } else {
          break;
        }
      } else {
        *com_vec_push_m(diagnostics, com_json_Error) =
            com_json_error_m(com_json_ElemEof, sp);
        return com_json_invalid_m;
      }
    }
  }

  if (has_fractional_component || (exponentState != NoExponent)) {
    // means we have to be floating point
    f64 num = (f64)integer_value + fractional_component;

    if (exponentState != NoExponent) {
      f64 exponent = exponential_integer;
      if (exponentState == NegativeExponent) {
        exponent = -exponent;
      }
      num = com_fmath_f64_pow(num, exponent);
    }

    if (negative) {
      num = -num;
    }
    return com_json_float_m(num);
  } else {
    if (negative) {
      integer_value = -integer_value;
    }
    return com_json_int_m(integer_value);
  }
}

static com_json_Elem
com_json_certain_parseLiteralElem(com_reader *reader, com_vec *diagnostics,
                                  attr_UNUSED com_allocator *a) {
  com_loc_LnCol start = com_reader_position(reader);

  bool overflow = false;

  u8 buffer[6];
  usize index = 0;

  while (true) {
    com_reader_ReadU8Result ret = com_reader_peek_u8(reader, 1);
    if (ret.valid && com_format_is_alphanumeric(ret.value)) {
      u8 c = ret.value;
      // if we are able to put it in the buffer
      if (index < sizeof(buffer)) {
        // insert it in
        buffer[index++] = c;
        // get next
        com_reader_drop_u8(reader);
      } else {
        // overflow
        overflow = true;
        break;
      }
    } else {
      break;
    }
  }

  // if we have an overflow we need to read out the rest of the term and then
  // return an invalid
  if (overflow) {
    while (true) {
      com_reader_ReadU8Result ret = com_reader_peek_u8(reader, 1);
      if (ret.valid && com_format_is_alphanumeric(ret.value)) {
        // drop if is an alph or digit
        com_reader_drop_u8(reader);
      } else {
        break;
      }
    }
    // push malormed literal error
    *com_vec_push_m(diagnostics, com_json_Error) =
        com_json_error_m(com_json_MalformedLiteral,
                         com_loc_span_m(start, com_reader_position(reader)));
    return com_json_invalid_m;
  }

  com_str data_str = (com_str){.data = buffer, .len = index};

  if (com_str_equal(data_str, com_str_lit_m("null"))) {
    return com_json_null_m;
  } else if (com_str_equal(data_str, com_str_lit_m("true"))) {
    return com_json_bool_m(true);
  } else if (com_str_equal(data_str, com_str_lit_m("false"))) {
    return com_json_bool_m(false);
  } else {
    *com_vec_push_m(diagnostics, com_json_Error) =
        com_json_error_m(com_json_MalformedLiteral,
                         com_loc_span_m(start, com_reader_position(reader)));
    return com_json_invalid_m;
  }
}
static com_json_Prop com_json_parseProp(com_reader *l, com_vec *diagnostics,
                                        com_allocator *a);

static com_json_Elem com_json_certain_parseArrayElem(com_reader *l,
                                                     com_vec *diagnostics,
                                                     com_allocator *a) {
  {
    com_reader_ReadU8Result ret = com_reader_read_u8(l);
    com_assert_m(ret.valid && ret.value == '[', "expected [");
  }

  // vector of elements
  com_vec elems = com_vec_create(com_allocator_alloc(
      a, (com_allocator_HandleData){.flags = com_allocator_defaults(a) |
                                             com_allocator_NOLEAK |
                                             com_allocator_REALLOCABLE,
                                    .len = 12}));

  typedef enum {
    ArrayParseStart,
    ArrayParseExpectCommaOrEnd,
    ArrayParseExpectElem,
  } ArrayParseState;

  ArrayParseState state = ArrayParseStart;

  while (true) {
    switch (state) {
    case ArrayParseStart: {
      com_scan_skip_whitespace(l);
      com_loc_Span sp = com_reader_peek_span_u8(l);
      com_reader_ReadU8Result ret = com_reader_peek_u8(l, 1);
      if (!ret.valid) {
        *com_vec_push_m(diagnostics, com_json_Error) =
            com_json_error_m(com_json_ArrayExpectedJsonElem, sp);
        return com_json_invalid_m;
      }
      if (ret.value == ']') {
        com_reader_drop_u8(l);
        goto CLEANUP;
      } else {
        state = ArrayParseExpectElem;
      }
      break;
    }
    case ArrayParseExpectCommaOrEnd: {
      com_scan_skip_whitespace(l);

      com_loc_Span sp = com_reader_peek_span_u8(l);
      com_reader_ReadU8Result ret = com_reader_peek_u8(l, 1);
      if (!ret.valid) {
        *com_vec_push_m(diagnostics, com_json_Error) =
            com_json_error_m(com_json_ArrayExpectedRightBracket, sp);
        return com_json_invalid_m;
      }
      switch (ret.value) {
      case ',': {
        com_reader_drop_u8(l);
        state = ArrayParseExpectElem;
        break;
      }
      case ']': {
        com_reader_drop_u8(l);
        goto CLEANUP;
      }
      default: {
        // emit error
        *com_vec_push_m(diagnostics, com_json_Error) =
            com_json_error_m(com_json_ArrayExpectedRightBracket, sp);
        // drop char
        com_reader_drop_u8(l);
        break;
      }
      }
      break;
    }
    case ArrayParseExpectElem: {
      *com_vec_push_m(&elems, com_json_Elem) =
          com_json_parseElem(l, diagnostics, a);
      state = ArrayParseExpectCommaOrEnd;
      break;
    }
    }
  }

CLEANUP:;
  usize len = com_vec_len_m(&elems, com_json_Elem);
  return com_json_array_m(com_vec_release(&elems), len);
}

static com_str_mut certain_internal_str_parse(com_reader *l,
                                              com_vec *diagnostics,
                                              com_allocator *a) {
  // note that we don't want to read the first quote
  {
    com_reader_ReadU8Result ret = com_reader_read_u8(l);
    com_assert_m(ret.valid && ret.value == '\"', "expected \"");
  }

  // parse into vec writer until we get a success or a read error log errors
  // along the way

  // create vector writer
  com_vec vec = com_vec_create(com_allocator_alloc(
      a, (com_allocator_HandleData){
             .flags = com_allocator_defaults(a) | com_allocator_NOLEAK |
                      com_allocator_REALLOCABLE,
             // let's just go with 12 for now as initial capacity
             .len = 12}));
  com_writer writer = com_writer_vec_create(&vec);

  while (true) {
    com_scan_CheckedStrResult ret =
        com_scan_checked_str_until_quote(&writer, l);
    switch (ret.result) {
    case com_scan_CheckedStrSuccessful: {
      // We're finished, we can deallocate writer and release vec into a string
      // that we return as an element
      com_writer_destroy(&writer);
      return com_vec_to_str(&vec);
    }
    case com_scan_CheckedStrReadFailed: {
      // deallocate resources
      com_writer_destroy(&writer);
      // give error
      *com_vec_push_m(diagnostics, com_json_Error) =
          com_json_error_m(com_json_StrExpectedDoubleQuote, ret.span);
      // return invalid
      return com_vec_to_str(&vec);
    }
    case com_scan_CheckedStrInvalidControlChar: {
      *com_vec_push_m(diagnostics, com_json_Error) =
          com_json_error_m(com_json_StrInvalidControlChar, ret.span);
      break;
    }
    case com_scan_CheckedStrInvalidUnicodeSpecifier: {
      *com_vec_push_m(diagnostics, com_json_Error) =
          com_json_error_m(com_json_StrInvalidUnicodeSpecifier, ret.span);
      break;
    }
    }
  }
}

static com_json_Elem com_json_certain_parseStrElem(com_reader *reader,
                                                   com_vec *diagnostics,
                                                   com_allocator *a) {
  return com_json_str_m(
      com_str_demut(certain_internal_str_parse(reader, diagnostics, a)));
}

static com_json_Prop com_json_parseProp(com_reader *l, com_vec *diagnostics,
                                        com_allocator *a) {
  com_scan_skip_whitespace(l);
  com_loc_Span quotespan = com_reader_peek_span_u8(l);
  com_reader_ReadU8Result ret = com_reader_peek_u8(l, 1);

  // short circuit prevents issues
  if (!(ret.valid && ret.value == '\"')) {
    *com_vec_push_m(diagnostics, com_json_Error) =
        com_json_error_m(com_json_PropExpectedDoubleQuote, quotespan);
    return com_json_prop_m(com_str_lit_m(""), com_json_invalid_m);
  }

  com_str key = com_str_demut(certain_internal_str_parse(l, diagnostics, a));

  // resync up to colon
  com_scan_skip_whitespace(l);

  com_loc_Span colonspan = com_reader_peek_span_u8(l);
  ret = com_reader_peek_u8(l, 1);

  if (!ret.valid) {
    *com_vec_push_m(diagnostics, com_json_Error) =
        com_json_error_m(com_json_PropExpectedColon, colonspan);
    return com_json_prop_m(key, com_json_invalid_m);
  }

  if (ret.value != ':') {
    *com_vec_push_m(diagnostics, com_json_Error) =
        com_json_error_m(com_json_PropExpectedDoubleQuote, colonspan);
    // must sync to a colon in order to be robust
    com_writer nw = com_writer_null();
    // zoom up to and accept colon
    com_scan_until(&nw, l, ':');
  }

  com_json_Elem value = com_json_parseElem(l, diagnostics, a);
  return com_json_prop_m(key, value);
}

static com_json_Elem com_json_certain_parseObjectElem(com_reader *l,
                                                      com_vec *diagnostics,
                                                      com_allocator *a) {
  {
    com_reader_ReadU8Result ret = com_reader_read_u8(l);
    com_assert_m(ret.valid && (ret.value == '{'), "expected {'");
  }

  // vector of properties
  com_vec props = com_vec_create(com_allocator_alloc(
      a, (com_allocator_HandleData){.len = 0,
                                    .flags = com_allocator_defaults(a) |
                                             com_allocator_REALLOCABLE |
                                             com_allocator_NOLEAK}));

  typedef enum {
    ObjectParseStart,
    ObjectParseExpectCommaOrEnd,
    ObjectParseExpectProp,
  } ObjectParseState;

  ObjectParseState state = ObjectParseStart;

  while (true) {
    switch (state) {
    case ObjectParseStart: {
      com_scan_skip_whitespace(l);
      com_loc_Span sp = com_reader_peek_span_u8(l);
      com_reader_ReadU8Result ret = com_reader_peek_u8(l, 1);
      if (!ret.valid) {
        *com_vec_push_m(diagnostics, com_json_Error) =
            com_json_error_m(com_json_ObjectExpectedProp, sp);
        return com_json_invalid_m;
      }
      if (ret.value == '}') {
        com_reader_drop_u8(l);
        goto CLEANUP;
      } else {
        state = ObjectParseExpectProp;
      }
      break;
    }
    case ObjectParseExpectProp: {
      *com_vec_push_m(&props, com_json_Prop) =
          com_json_parseProp(l, diagnostics, a);
      state = ObjectParseExpectCommaOrEnd;
      break;
    }
    case ObjectParseExpectCommaOrEnd: {
      com_scan_skip_whitespace(l);
      com_loc_Span sp = com_reader_peek_span_u8(l);
      com_reader_ReadU8Result ret = com_reader_peek_u8(l, 1);
      if (!ret.valid) {
        *com_vec_push_m(diagnostics, com_json_Error) =
            com_json_error_m(com_json_ObjectExpectedRightBrace, sp);
        return com_json_invalid_m;
      }
      switch (ret.value) {
      case ',': {
        com_reader_drop_u8(l);
        state = ObjectParseExpectCommaOrEnd;
        break;
      }
      case '}': {
        com_reader_drop_u8(l);
        goto CLEANUP;
      }
      default: {
        // emit error
        *com_vec_push_m(diagnostics, com_json_Error) =
            com_json_error_m(com_json_ObjectExpectedRightBrace, sp);
        // drop char
        com_reader_drop_u8(l);
        break;
      }
      }
      break;
    }
    }
  }
CLEANUP:;
  usize len = com_vec_len_m(&props, com_json_Prop);
  return com_json_obj_m(com_vec_release(&props), len);
}

com_json_Elem com_json_parseElem(com_reader *l, com_vec *diagnostics,
                                 com_allocator *a) {
  com_assert_m(com_reader_flags(l) & com_reader_BUFFERED,
               "reader is not buffered");
  com_scan_skip_whitespace(l);

  com_loc_Span sp = com_reader_peek_span_u8(l);
  com_reader_ReadU8Result ret = com_reader_peek_u8(l, 1);
  if (!ret.valid) {
    *com_vec_push_m(diagnostics, com_json_Error) =
        com_json_error_m(com_json_ElemEof, sp);
    return com_json_invalid_m;
  }

  switch (ret.value) {
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
  case '-': {
    return com_json_certain_parseNumberElem(l, diagnostics, a);
  }
  case 't':
  case 'f':
  case 'n': {
    return com_json_certain_parseLiteralElem(l, diagnostics, a);
  }
  case '\"': {
    return com_json_certain_parseStrElem(l, diagnostics, a);
  }
  case '[': {
    return com_json_certain_parseArrayElem(l, diagnostics, a);
  }
  case '{': {
    return com_json_certain_parseObjectElem(l, diagnostics, a);
  }
  default: {
    *com_vec_push_m(diagnostics, com_json_Error) =
        com_json_error_m(com_json_ElemUnknownCharacter, sp);
    return com_json_invalid_m;
  }
  }
}
