#include "json.h"

#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

#include "arena.h"
#include "error.h"
#include "vector.h"

// GLORIOUS UTILS

// Accepts Vector<char>, pushes as many chars points as needed to encode the
// data
void encodeUTFPoint(Vector *data, uint32_t utf) {
  if (utf <= 0x7F) { // Plain ASCII
    char *out = pushVector(data, sizeof(char) * 1);
    out[0] = (char)utf;
  } else if (utf <= 0x07FF) {
    // 2-byte unicode
    char *out = pushVector(data, sizeof(char) * 2);
    out[0] = (char)(((utf >> 6) & 0x1F) | 0xC0);
    out[1] = (char)(((utf >> 0) & 0x3F) | 0x80);
  } else if (utf <= 0xFFFF) {
    // 3-byte unicode
    char *out = pushVector(data, sizeof(char) * 3);
    out[0] = (char)(((utf >> 12) & 0x0F) | 0xE0);
    out[1] = (char)(((utf >> 6) & 0x3F) | 0x80);
    out[2] = (char)(((utf >> 0) & 0x3F) | 0x80);
  } else if (utf <= 0x10FFFF) {
    // 4-byte unicode
    char *out = pushVector(data, sizeof(char) * 4);
    out[0] = (char)(((utf >> 18) & 0x07) | 0xF0);
    out[1] = (char)(((utf >> 12) & 0x3F) | 0x80);
    out[2] = (char)(((utf >> 6) & 0x3F) | 0x80);
    out[3] = (char)(((utf >> 0) & 0x3F) | 0x80);
  }
  // TODO gracefully handle error
}

JsonKV KVJson(char *key, JsonElem value) {
  return (JsonKV){.key = (key), .value = (value)};
}

JsonElem nullJson() { return (JsonElem){.kind = JE_null}; }

JsonElem boolJson(bool x) {
  return (JsonElem){.kind = JE_boolean, .boolean = (x)};
}

JsonElem intJson(uint64_t x) {
  return (JsonElem){.kind = JE_integer, .integer = (x)};
}

JsonElem numJson(double x) {
  return (JsonElem){.kind = JE_number, .number = (x)};
}

JsonElem strJson(char *x, size_t len) {
  if (x != NULL) {
    return (JsonElem){.kind = JE_string,
                      .string = {.string = (x), .length = len}};
  } else {
    return nullJson();
  }
}

JsonElem arrDefJson(JsonElem *ptr, size_t len) {
  return (JsonElem){.kind = JE_array,
                    .array = {.values = (ptr), .length = (len)}};
}

JsonElem objDefJson(JsonKV *ptr, size_t len) {
  return (JsonElem){.kind = JE_object,
                    .object = {.values = (ptr), .length = (len)}};
}

// JSON TO STRING
static void unchecked_emitCharJson(Vector *vptr, char c) {
  *VEC_PUSH(vptr, char) = c;
}

static void unchecked_emitStrJson(Vector *vptr, char *str, size_t len) {
  // length of string in bytes
  memcpy(pushVector(vptr, len), str, len);
}

// Convert from int to string
static void emitIntJson(Vector *vptr, int64_t digit) {
  // handle negative numbers
  if (digit < 0) {
    *VEC_PUSH(vptr, char) = '-';
    digit = -digit;
  }

  while (true) {
    uint8_t rem = digit % 10;
    digit /= 10;
    *VEC_PUSH(vptr, char) = '0' + rem;
    if (digit == 0) {
      break;
    }
  }
}

static void emitNumberJson(Vector *vptr, double number) {
  // up to 328 digits in a float
  char str[350];
  snprintf(str, 350, "%f", number);
  unchecked_emitStrJson(vptr, str, strlen(str));
}

static char toHex(uint8_t x) {
  if (x < 10) {
    return '0' + x;
  } else {
    return 'a' + x;
  }
}

// Checks for special characters
static void emitStrJson(Vector *vptr, char *str, size_t len) {
  unchecked_emitCharJson(vptr, '\"');
  for (size_t i = 0; i < len; i++) {
    char c = str[i];
    switch (c) {
    case '\b': {
      char *ptr = pushVector(vptr, sizeof(char) * 2);
      ptr[0] = '\\';
      ptr[1] = 'b';
      break;
    }
    case '\f': {
      char *ptr = pushVector(vptr, sizeof(char) * 2);
      ptr[0] = '\\';
      ptr[1] = 'f';
      break;
    }
    case '\n': {
      char *ptr = pushVector(vptr, sizeof(char) * 2);
      ptr[0] = '\\';
      ptr[1] = 'n';
      break;
    }
    case '\r': {
      char *ptr = pushVector(vptr, sizeof(char) * 2);
      ptr[0] = '\\';
      ptr[1] = 'r';
      break;
    }
    case '\t': {
      char *ptr = pushVector(vptr, sizeof(char) * 2);
      ptr[0] = '\\';
      ptr[1] = 't';
      break;
    }
    case '\"': {
      char *ptr = pushVector(vptr, sizeof(char) * 2);
      ptr[0] = '\\';
      ptr[1] = '\"';
      break;
    }
    case '\\': {
      char *ptr = pushVector(vptr, sizeof(char) * 2);
      ptr[0] = '\\';
      ptr[1] = '\\';
      break;
    }
    default: {
      if (c <= 0x001F) {
        char *ptr = pushVector(vptr, sizeof(char) * 6);
        ptr[0] = '\\';
        ptr[1] = 'u';
        ptr[2] = '0';
        ptr[3] = '0';
        ptr[4] = toHex(c / 16);
        ptr[5] = toHex(c % 16);
      } else {
        *VEC_PUSH(vptr, char) = c;
      }
      break;
    }
    }
  }
  unchecked_emitCharJson(vptr, '\"');
}

static void emitJsonElem(JsonElem *j, Vector *data) {
  switch (j->kind) {
  case JE_null: {
    unchecked_emitStrJson(data, "null", 4);
    break;
  }
  case JE_string: {
    emitStrJson(data, j->string.string, j->string.length);
    break;
  }
  case JE_boolean: {
    unchecked_emitStrJson(data, j->boolean ? "true" : "false",
                          j->boolean ? 4 : 5);
    break;
  }
  case JE_integer: {
    // there are a max 20 digits in an integer
    emitIntJson(data, j->integer);
    break;
  }
  case JE_number: {
    emitNumberJson(data, j->number);
    break;
  }
  case JE_array: {
    unchecked_emitCharJson(data, '[');
    for (size_t i = 0; i < j->array.length; i++) {
      emitJsonElem(&j->array.values[i], data);
      // if we are not on the ultimate element of the lsit
      if (i + 1 != j->array.length) {
        unchecked_emitCharJson(data, ',');
      }
    }
    unchecked_emitCharJson(data, ']');
    break;
  }
  case JE_object: {
    unchecked_emitCharJson(data, '{');
    for (size_t i = 0; i < j->object.length; i++) {
      JsonKV jkv = j->object.values[i];
      emitStrJson(data, jkv.key, strlen(jkv.key));
      unchecked_emitCharJson(data, ':');
      emitJsonElem(&jkv.value, data);
      if (i + 1 != j->object.length) {
        unchecked_emitCharJson(data, ',');
      }
    }
    unchecked_emitCharJson(data, '}');
    break;
  }
  }
}

char *toStringJsonElem(JsonElem *j) {
  Vector data;
  createVector(&data);
  emitJsonElem(j, &data);
  // terminate string
  *VEC_PUSH(&data, char) = '\0';
  return releaseVector(&data);
}

// Parsing

void parseJsonKV(JsonKV *jkv, Lexer *l, Vector *diagnostics) {}

void certain_parseNumberJson(JsonElem *je, Lexer *l, Vector *diagnostics) {
  LnCol start = l->position;

  bool negative = false;
  if (peekValueLexer(l) == '-') {
    negative = true;
    nextValueLexer(l);
  }

  int64_t integer_value = 0;
  int32_t c;
  while ((c = peekValueLexer(l)) != EOF) {
    if (isdigit(c)) {
      integer_value = integer_value * 10 + (c - '0');
      nextValueLexer(l);
    } else {
      break;
    }
  }

  if (negative) {
    integer_value = -integer_value;
  }

  bool fractional = false;
  if (peekValueLexer(l) == '.') {
    fractional = true;
    nextValueLexer(l);
  }

  double fractional_component = 0;
  if (fractional) {
    double place = 0.1;
    while ((c = peekValueLexer(l)) != EOF) {
      if (isdigit(c)) {
        fractional_component += place * (c - '0');
        place /= 10;
        nextValueLexer(l);
      } else {
        break;
      }
    }
  }

  bool positive_exponent = false;
  bool negative_exponent = false;
  c = peekValueLexer(l);
  if (c == 'E' || c == 'e') {
    nextValueLexer(l);
    switch (nextValueLexer(l)) {
    case '+': {
      positive_exponent = true;
      break;
    }
    case '-': {
      negative_exponent = true;
      break;
    }
    default: {
      *VEC_PUSH(diagnostics, JsonParseDiagnostic) =
          JSONPARSEDIAGNOSTIC(JPDK_JsonNumberExponentExpectedSign, l->position);
      break;
    }
    }
  }

  uint32_t exponential_integer = 0;
  if (positive_exponent || negative_exponent) {
    while ((c = peekValueLexer(l)) != EOF) {
      if (isdigit(c)) {
        exponential_integer = exponential_integer * 10 + (c - '0');
        nextValueLexer(l);
      } else {
        break;
      }
    }
  }

  if (fractional || negative_exponent) {
    // Decimalish
    double num = integer_value + fractional_component;
    if (positive_exponent) {
      for (int i = 0; i < exponential_integer; i++) {
        num *= 10;
      }
    }
    if (negative_exponent) {
      for (int i = 0; i < exponential_integer; i++) {
        num /= 10;
      }
    }
    je->number = num;
    je->kind = JE_number;
  } else {
    // Integer
    int64_t num = integer_value;
    if (positive_exponent) {
      for (int i = 0; i < exponential_integer; i++) {
        num *= 10;
      }
    }
    je->integer = num;
    je->kind = JE_integer;
  }

  return;
}

void certain_parseLiteralJson(JsonElem *je, Lexer *l, Vector *diagnostics) {
  LnCol start = l->position;
  Vector data;
  createVector(&data);
  int32_t c;
  while ((c = peekValueLexer(l)) != EOF) {
    if (isalpha(c)) {
      *VEC_PUSH(&data, char) = (char)c;
      nextValueLexer(l);
    } else {
      break;
    }
  }

  char *str_unterminated = VEC_GET(&data, 0, char);
  char str_len = VEC_LEN(&data, char);

  if (!strncmp("null", str_unterminated, str_len)) {
    je->kind = JE_null;
  } else if (!strncmp("true", str_unterminated, str_len)) {
    je->kind = JE_boolean;
    je->boolean = true;
  } else if (!strncmp("false", str_unterminated, str_len)) {
    je->kind = JE_boolean;
    je->boolean = false;
  } else {
    *VEC_PUSH(diagnostics, JsonParseDiagnostic) =
        JSONPARSEDIAGNOSTIC(JPDK_JsonMalformedLiteral, start);
    je->kind = JE_null;
  }
  return;
}

void certain_parseStringJson(JsonElem *je, Lexer *l, Vector *diagnostics) {
  LnCol start = l->position;
  int32_t c = nextValueLexer(l);
  assert(c == '"');

  typedef enum {
    StringParserText,
    StringParserBackslash,
    StringParserUnicode,
    StringParserFinished
  } StringParserState;

  Vector data;
  createVector(&data);

  StringParserState state = StringParserText;

  while (true) {
    switch (state) {
    case StringParserText: {
      c = nextValueLexer(l);
      switch (c) {
      case '\\': {
        state = StringParserBackslash;
        break;
      }
      case '\"': {
        state = StringParserFinished;
        break;
      }
      case EOF: {
        *VEC_PUSH(diagnostics, JsonParseDiagnostic) = JSONPARSEDIAGNOSTIC(
            JPDK_JsonStringExpectedDoubleQuote, l->position);
        state = StringParserFinished;
        break;
      }
      default: {
        *VEC_PUSH(&data, char) = (char)c;
        break;
      }
      }
      break;
    }
    case StringParserBackslash: {
      c = nextValueLexer(l);
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
        *VEC_PUSH(diagnostics, JsonParseDiagnostic) =
            JSONPARSEDIAGNOSTIC(JPDK_JsonStringInvalidControlChar, l->position);
        state = StringParserText;
        break;
      }
      }
      state = StringParserText;
      break;
    }
    case StringParserUnicode: {
      uint16_t code_point;
      for (int i = 0; i < 4; i++) {
        c = nextValueLexer(l);
        uint8_t value;
        if (c >= '0' && c <= '9') {
          value = c - '0';
        } else if (c >= 'a' && c <= 'f') {
          value = c - 'a';
        } else if (c >= 'A' && c <= 'F') {
          value = c - 'A';
        } else {
          *VEC_PUSH(diagnostics, JsonParseDiagnostic) = JSONPARSEDIAGNOSTIC(
              JPDK_JsonStringInvalidUnicodeSpecifier, l->position);
          value = 0;
        }
        code_point += code_point * 16 + value;
      }
      encodeUTFPoint(&data, code_point);
      state = StringParserText;
      break;
    }
    case StringParserFinished: {
      goto LOOPEND;
    }
    }
  }
LOOPEND:
  *VEC_PUSH(&data, char) = '\0';
  size_t len = VEC_LEN(&data, char) - 1;
  *je = strJson(manageMemArena(l->ar, releaseVector(&data)), len);
  return;
}

static void skipWhitespace(Lexer *l) {
  while (true) {
    switch (peekValueLexer(l)) {
    case ' ':
    case '\t':
    case '\r':
    case '\n': {
      // discard whitespace
      nextValueLexer(l);
      break;
    }
    default: {
      return;
    }
    }
  }
}

void parseJsonElem(JsonElem *je, Lexer *l, Vector *diagnostics);

static void certain_parseArrayJson(JsonElem *je, Lexer *l,
                                   Vector *diagnostics) {
  assert(nextValueLexer(l) == '[');

  // vector of elements
  Vector elems;
  createVector(&elems);

  typedef enum {
    ArrayParseExpectCommaOrEnd,
    ArrayParseExpectElem,
  } ArrayParseState;

  ArrayParseState state = ArrayParseExpectCommaOrEnd;

  while (true) {
    switch (state) {
    case ArrayParseExpectCommaOrEnd: {
      skipWhitespace(l);
      int32_t c = peekValueLexer(l);
      switch (c) {
      case ',': {
        nextValueLexer(l);
        state = ArrayParseExpectElem;
        break;
      }
      case ']': {
        nextValueLexer(l);
        goto CLEANUP;
      }
      case EOF: {
        *VEC_PUSH(diagnostics, JsonParseDiagnostic) = JSONPARSEDIAGNOSTIC(
            JPDK_JsonArrayExpectedRightBracket, l->position);
        goto CLEANUP;
      }
      default: {
        *VEC_PUSH(diagnostics, JsonParseDiagnostic) = JSONPARSEDIAGNOSTIC(
            JPDK_JsonArrayExpectedRightBracket, l->position);
        nextValueLexer(l);
        break;
      }
      }
      break;
    }
    case ArrayParseExpectElem: {
      skipWhitespace(l);
      JsonElem *elemptr = VEC_PUSH(&elems, JsonElem);
      parseJsonElem(elemptr, l, diagnostics);
      state = ArrayParseExpectCommaOrEnd;
      break;
    }
    }
  }
CLEANUP:;
  size_t len = VEC_LEN(&elems, JsonElem);
  *je = arrDefJson(manageMemArena(l->ar, releaseVector(&elems)), len);
}

static void parseKVJson(JsonKV *kv, Lexer *l, Vector *diagnostics) {
  JsonElem je;
  certain_parseStringJson(&je, l, diagnostics);
  if(je.kind != JE_string) {
    *VEC_PUSH(diagnostics, JsonParseDiagnostic) = JSONPARSEDIAGNOSTIC(JPDK_JsonKVExpectedQuoted, l->position);
  }
}

void parseJsonElem(JsonElem *je, Lexer *l, Vector *diagnostics) {
  skipWhitespace(l);

  int32_t c = peekValueLexer(l);
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
    certain_parseNumberJson(je, l, diagnostics);
    return;
  }
  case 't':
  case 'f':
  case 'n': {
    certain_parseLiteralJson(je, l, diagnostics);
    return;
  }
  case '\"': {
    certain_parseStringJson(je, l, diagnostics);
    return;
  }
  case '[': {
    certain_parseArrayJson(je, l, diagnostics);
    return;
  }
  case '{': {
    certain_parseObjectJson(je, l, diagnostics);
    return;
  }
  case EOF: {
    *VEC_PUSH(diagnostics, JsonParseDiagnostic) =
        JSONPARSEDIAGNOSTIC(JPDK_JsonElemEof, l->position);
    return;
  }
  default: {
    *VEC_PUSH(diagnostics, JsonParseDiagnostic) =
        JSONPARSEDIAGNOSTIC(JPDK_JsonElemUnknownCharacter, l->position);
    nextValueLexer(l);
    return;
  }
  }}
