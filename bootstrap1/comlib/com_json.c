#include "com_json.h"
#include "com_format.h"
#include "com_assert.h"

#define ERROR(k, l) ((com_json_Error){.kind = k, .span = l})


// emits str with quotation marks
static void com_json_emitStr(com_writer *writer, com_str str) {
    com_format_u8_char(writer, '\"');
    com_format_str_checked(writer, str);
    com_format_u8_char(writer, '\"');
}

static void com_json_emitElem(com_writer *writer, com_json_Elem *j);

static void com_json_emitProp(com_writer *writer, com_json_Prop *prop) {
  com_json_emitStr(writer, prop->key);
  com_format_u8_char(writer, ':');
  com_json_emitElem(writer, &prop->value);
}

static void com_json_emitElem(com_writer *writer, com_json_Elem *j) {
  switch (j->kind) {
  case com_json_NULL: {
    com_format_str(writer, com_str_lit_m("null"));
    break;
  }
  case com_json_STR: {
      com_json_emitStr(writer, j->string);
    break;
  }
  case com_json_BOOL: {
    com_format_str(writer, j->boolean ? com_str_lit_m("true") : com_str_lit_m("false"));
    break;
  }
  case com_json_INT: {
    // there are a max 20 digits in an integer
    com_bigint_format(writer, j->integer);
    break;
  }
  case com_json_NUM: {
    com_format_f64(writer, j->number, 10, com_format_MINUS_VISIBLE, com_format_NO_PADDING);
    break;
  }
  case com_json_ARRAY: {
    com_format_u8_char(writer, '[');
    for (size_t i = 0; i < j->array.length; i++) {
      com_json_emitElem(writer, &j->array.values[i]);
      // if we are not on the ultimate element of the lsit
      if (i + 1 != j->array.length) {
        com_format_u8_char(writer, ',');
      }
    }
    com_format_u8_char(writer, ']');
    break;
  }
  case com_json_OBJ: {
    com_format_u8_char(writer, '{');
    for (size_t i = 0; i < j->object.length; i++) {
      com_json_emitProp(writer, &j->object.props[i]);
      if (i + 1 != j->object.length) {
        com_format_u8_char(writer, ',');
      }
    }
    com_format_u8_char(writer, '}');
    break;
  }
  }
}

void com_json_serialize(com_json_Elem *elem, com_writer* writer) {
  com_json_emitElem(writer, elem);
}

// Parsing
static void skipWhitespace(com_reader *reader) {
  u8 c;
  while (com_reader_peek_u8(reader, 1, &c)) {
    switch (c) {
    case ' ':
    case '\t':
    case '\r':
    case '\n': {
      // discard whitespace
      com_reader_read_u8(reader, &c);
      break;
    }
    default: {
      return;
    }
    }
  }
}

static bool com_json_isdigit(u8 c) {
  return c >= '0' && c <= '9';
}


static com_json_Elem com_json_certain_parseNumberElem(com_reader *reader, com_vec *diagnostics,
                                        com_Allocator *a) {
  bool readsuccess;
  u8 c;

  readsuccess = com_reader_peek_u8(reader, 1, &c);
  bool negative = false;
  if (c  == '-') {
    negative = true;
    // drop char
    com_reader_read_u8(reader, &c);
  }

  if(rea

  com_bigint integer_value = com_bigint_create(com_allocator_alloc(a, (com_allocator_HandleData) { 
      // no hard cap on number size
      .flags = com_allocator_defaults(a) | com_allocator_REALLOCABLE, 
      // 99% of the time we really won't need more than 8 bytes
      .len = 8
  }));

  while ((readsuccess = com_reader_peek_u8(reader, 1, &c))) {
    if (com_json_isdigit(c)) {
      com_bigint_fma_u32_u32(&integer_value, 10, c - (u8)'0');
      com_reader_read_u8(reader, &c);
    } else {
      break;
    }
  }

  // c will be set to whatever the last thing was 

  bool fractional = false;
  if (c  == '.') {
    fractional = true;
    lex_next(l);
  }

  double fractional_component = 0;
  if (fractional) {
    double place = 1;
    while ((c = lex_peek(l)) != EOF) {
      if (isdigit(c)) {
        place *= 10;
        fractional_component += (c - '0')/place;
        lex_next(l);
      } else {
        break;
      }
    }
  }

  bool positive_exponent = false;
  bool negative_exponent = false;
  c = lex_peek(l);
  if (c == 'E' || c == 'e') {
    lex_next(l);
    switch (lex_next(l)) {
    case '+': {
      positive_exponent = true;
      break;
    }
    case '-': {
      negative_exponent = true;
      break;
    }
    default: {
      *VEC_PUSH(diagnostics, com_json_Error) =
          ERROR(com_json_NumExponentExpectedSign, l->position);
      break;
    }
    }
  }

  uint32_t exponential_integer = 0;
  if (positive_exponent || negative_exponent) {
    while ((c = lex_peek(l)) != EOF) {
      if (isdigit(c)) {
        exponential_integer = exponential_integer * 10 + (uint8_t)(c - '0');
        lex_next(l);
      } else {
        break;
      }
    }
  }

  if (fractional || negative_exponent || positive_exponent) {
    // Decimalish
    double num = (double)integer_value + fractional_component;
    if (positive_exponent) {
      for (size_t i = 0; i < exponential_integer; i++) {
        num *= 10;
      }
    }
    if (negative_exponent) {
      for (size_t i = 0; i < exponential_integer; i++) {
        num /= 10;
      }
    }
    return J_NUM_ELEM(num);
  } else {
    // Integer
    uint64_t num = integer_value;
    return J_INT_ELEM(J_INT(negative, num));
  }
}

static com_json_Elem com_json_certain_parseLiteralElem(com_reader *l, Vector *diagnostics,
                                         UNUSED Allocator *a) {
  LnCol start = l->position;

  // Fixed buffer size
  char buffer[6]; // this is long enough to hold false\0
  bool toolong = false;
  size_t index = 0;
  while (true) {
    int32_t c = lex_peek(l);
    if (isalpha(c)) {
      // Fill up buffer
      if (index < 5) {
        buffer[index] = (char)c;
        index++;
      } else {
        toolong = true;
      }
      // even if the buffer is finished we must continue on (but mark it as
      // (toolong)
      lex_next(l);
    } else {
      break;
    }
  }
  // Terminate with string length
  buffer[5] = '\0';

  if (!toolong && !strcmp("null", buffer)) {
    return J_NULL_ELEM;
  } else if (!toolong && !strcmp("true", buffer)) {
    return J_BOOL_ELEM(true);
  } else if (!toolong && !strcmp("false", buffer)) {
    return J_BOOL_ELEM(false);
  } else {
    *VEC_PUSH(diagnostics, com_json_Error) = ERROR(com_json_MalformedLiteral, start);
    return J_NULL_ELEM;
  }
}

static com_json_Str com_json_parseStr(com_reader *l, Vector *diagnostics, Allocator *a) {
  LnCol start = l->position;
  skipWhitespace(l);
  int32_t c = lex_next(l);
  if (c != '\"') {
    *VEC_PUSH(diagnostics, com_json_Error) = ERROR(com_json_StrExpectedDoubleQuote, start);
  }

  typedef enum {
    StringParserText,
    StringParserBackslash,
    StringParserUnicode,
  } StringParserState;

  Vector data = vec_create(a);

  StringParserState state = StringParserText;

  while (true) {
    switch (state) {
    case StringParserText: {
      c = lex_next(l);
      switch (c) {
      case '\\': {
        state = StringParserBackslash;
        break;
      }
      case '\"': {
        goto LOOPEND;
      }
      case EOF: {
        *VEC_PUSH(diagnostics, com_json_Error) =
            ERROR(com_json_StrExpectedDoubleQuote, l->position);
        goto LOOPEND;
      }
      default: {
        *VEC_PUSH(&data, char) = (char)c;
        break;
      }
      }
      break;
    }
    case StringParserBackslash: {
      c = lex_next(l);
      switch (c) {
      case '\"': {
        *VEC_PUSH(&data, char) = '\"';
        state = StringParserText;
        break;
      }
      case '\\': {
        *VEC_PUSH(&data, char) = '\\';
        state = StringParserText;
        break;
      }
      case '/': {
        *VEC_PUSH(&data, char) = '/';
        state = StringParserText;
        break;
      }
      case 'b': {
        *VEC_PUSH(&data, char) = '\b';
        state = StringParserText;
        break;
      }
      case 'f': {
        *VEC_PUSH(&data, char) = '\f';
        state = StringParserText;
        break;
      }
      case 'n': {
        *VEC_PUSH(&data, char) = '\n';
        state = StringParserText;
        break;
      }
      case 'r': {
        *VEC_PUSH(&data, char) = '\r';
        state = StringParserText;
        break;
      }
      case 't': {
        *VEC_PUSH(&data, char) = '\t';
        state = StringParserText;
        break;
      }
      case 'u': {
        state = StringParserUnicode;
        break;
      }
      default: {
        *VEC_PUSH(diagnostics, com_json_Error) =
            ERROR(com_json_StrInvalidControlChar, l->position);
        state = StringParserText;
        break;
      }
      }
      break;
    }
    case StringParserUnicode: {
      uint32_t code_point = 0;
      for (int i = 0; i < 4; i++) {
        c = lex_next(l);
        if (c == EOF) {
          *VEC_PUSH(diagnostics, com_json_Error) =
              ERROR(com_json_StrExpectedDoubleQuote, l->position);
          goto LOOPEND;
        }
        int8_t value = fromHex((char)c);
        if (value < 0) {
          *VEC_PUSH(diagnostics, com_json_Error) =
              ERROR(com_json_StrInvalidUnicodeSpecifier, l->position);
          value = 0;
        }
        code_point += code_point * 16 + (uint8_t)value;
      }
      encodeUTFPoint(&data, code_point);
      state = StringParserText;
      break;
    }
    }
  }

LOOPEND:;
  size_t len = VEC_LEN(&data, char);
  *VEC_PUSH(&data, char) = '\0';
  return J_STR(vec_release(&data), len);
}

static com_json_Prop com_json_parseProp(com_reader *l, Vector *diagnostics, Allocator *a);

static com_json_Elem com_json_certain_parseArrayElem(com_reader *l, Vector *diagnostics,
                                       Allocator *a) {
  assert(lex_next(l) == '[');

  // vector of elements
  Vector elems = vec_create(a);

  typedef enum {
    ArrayParseStart,
    ArrayParseExpectCommaOrEnd,
    ArrayParseExpectElem,
  } ArrayParseState;

  ArrayParseState state = ArrayParseStart;

  while (true) {
    switch (state) {
    case ArrayParseStart: {
      skipWhitespace(l);
      if (lex_peek(l) == ']') {
        goto CLEANUP;
      } else {
        state = ArrayParseExpectElem;
      }
      break;
    }
    case ArrayParseExpectCommaOrEnd: {
      skipWhitespace(l);
      int32_t c = lex_peek(l);
      switch (c) {
      case ',': {
        lex_next(l);
        state = ArrayParseExpectElem;
        break;
      }
      case ']': {
        lex_next(l);
        goto CLEANUP;
      }
      case EOF: {
        *VEC_PUSH(diagnostics, com_json_Error) =
            ERROR(com_json_ArrayExpectedJsonElem, l->position);
        goto CLEANUP;
      }
      default: {
        *VEC_PUSH(diagnostics, com_json_Error) =
            ERROR(com_json_ArrayExpectedRightBracket, l->position);
        lex_next(l);
        break;
      }
      }
      break;
    }
    case ArrayParseExpectElem: {
      *VEC_PUSH(&elems, com_json_Elem) = com_json_parseElem(l, diagnostics, a);
      state = ArrayParseExpectCommaOrEnd;
      break;
    }
    }
  }
CLEANUP:;
  size_t len = VEC_LEN(&elems, com_json_Elem);
  return J_ARRAY_ELEM(vec_release(&elems), len);
}

static com_json_Elem com_json_certain_parseStrElem(com_reader *l, Vector *diagnostics,
                                     Allocator *a) {
  assert(lex_peek(l) == '\"');
  return J_STR_ELEM(com_json_parseStr(l, diagnostics, a));
}

static com_json_Prop com_json_parseProp(com_reader *l, Vector *diagnostics, Allocator *a) {
  com_json_Str key = com_json_parseStr(l, diagnostics, a);
  skipWhitespace(l);
  if (lex_next(l) != ':') {
    *VEC_PUSH(diagnostics, com_json_Error) = ERROR(com_json_PropExpectedColon, l->position);
  }
  com_json_Elem value = com_json_parseElem(l, diagnostics, a);
  return J_PROP(key, value);
}

static com_json_Elem com_json_certain_parseObjectElem(com_reader *l, Vector *diagnostics,
                                        Allocator *a) {
  assert(lex_next(l) == '{');

  // vector of properties
  Vector props = vec_create(a);

  typedef enum {
    ObjectParseStart,
    ObjectParseExpectCommaOrEnd,
    ObjectParseExpectProp,
  } ObjectParseState;

  ObjectParseState state = ObjectParseStart;

  while (true) {
    switch (state) {
    case ObjectParseStart: {
      skipWhitespace(l);
      if (lex_peek(l) == '}') {
        goto CLEANUP;
      } else {
        state = ObjectParseExpectProp;
      }
      break;
    }
    case ObjectParseExpectCommaOrEnd: {
      skipWhitespace(l);
      switch (lex_peek(l)) {
      case ',': {
        lex_next(l);
        state = ObjectParseExpectProp;
        break;
      }
      case '}': {
        lex_next(l);
        goto CLEANUP;
      }
      case EOF: {
        *VEC_PUSH(diagnostics, com_json_Error) =
            ERROR(com_json_ArrayExpectedJsonElem, l->position);
        goto CLEANUP;
      }
      default: {
        *VEC_PUSH(diagnostics, com_json_Error) =
            ERROR(com_json_ArrayExpectedRightBracket, l->position);
        lex_next(l);
        break;
      }
      }
      break;
    }
    case ObjectParseExpectProp: {
      *VEC_PUSH(&props, com_json_Prop) = com_json_parseProp(l, diagnostics, a);
      state = ObjectParseExpectCommaOrEnd;
      break;
    }
    }
  }
CLEANUP:;
  size_t len = VEC_LEN(&props, com_json_Prop);
  return J_OBJECT_ELEM(vec_release(&props), len);
}

com_json_Elem com_json_parseElem(com_reader *l, Vector *diagnostics, Allocator *a) {
  skipWhitespace(l);

  int32_t c = lex_peek(l);
  switch (c) {
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
  case EOF: {
    *VEC_PUSH(diagnostics, com_json_Error) = ERROR(com_json_ElemEof, l->position);
    return J_NULL_ELEM;
  }
  default: {
    *VEC_PUSH(diagnostics, com_json_Error) =
        ERROR(com_json_ElemUnknownCharacter, l->position);
    lex_next(l);
    return J_NULL_ELEM;
  }
  }
}
