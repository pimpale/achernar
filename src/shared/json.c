#include "json.h"

#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

#include "allocator.h"
#include "error.h"
#include "vector.h"

// GLORIOUS UTILS

#define ERROR(k, l) ((j_Error) { .kind=k, .loc=l})

// Accepts Vector<char>, pushes as many chars points as needed to encode the
// data
void encodeUTFPoint(Vector *data, uint32_t utf) {
  if (utf <= 0x7F) { // Plain ASCII
    char *out = vec_push(data, sizeof(char) * 1);
    out[0] = (char)utf;
  } else if (utf <= 0x07FF) {
    // 2-byte unicode
    char *out = vec_push(data, sizeof(char) * 2);
    out[0] = (char)(((utf >> 6) & 0x1F) | 0xC0);
    out[1] = (char)(((utf >> 0) & 0x3F) | 0x80);
  } else if (utf <= 0xFFFF) {
    // 3-byte unicode
    char *out = vec_push(data, sizeof(char) * 3);
    out[0] = (char)(((utf >> 12) & 0x0F) | 0xE0);
    out[1] = (char)(((utf >> 6) & 0x3F) | 0x80);
    out[2] = (char)(((utf >> 0) & 0x3F) | 0x80);
  } else if (utf <= 0x10FFFF) {
    // 4-byte unicode
    char *out = vec_push(data, sizeof(char) * 4);
    out[0] = (char)(((utf >> 18) & 0x07) | 0xF0);
    out[1] = (char)(((utf >> 12) & 0x3F) | 0x80);
    out[2] = (char)(((utf >> 6) & 0x3F) | 0x80);
    out[3] = (char)(((utf >> 0) & 0x3F) | 0x80);
  }
  // TODO gracefully handle error
}

// JSON TO STRING
static void j_unchecked_emitChar(Vector *vptr, char c) {
  *VEC_PUSH(vptr, char) = c;
}

static void j_unchecked_emitStr(Vector *vptr, char *str, size_t len) {
  // length of string in bytes
  memcpy(vec_push(vptr, len), str, len);
}

// Convert from int to string
static void j_emitInt(Vector *vptr, int64_t digit) {
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

static void j_emitNum(Vector *vptr, double number) {
  // up to 328 digits in a float
  char str[350];
  snprintf(str, 350, "%f", number);
  j_unchecked_emitStr(vptr, str, strlen(str));
}

static char toHex(uint8_t x) {
  if (x < 10) {
    return '0' + x;
  } else {
    return 'a' + x;
  }
}

// Checks for special characters
static void j_emitStr(Vector *vptr, j_Str str) {
  j_unchecked_emitChar(vptr, '\"');
  for (size_t i = 0; i < str.length; i++) {
    char c = str.string[i];
    switch (c) {
    case '\b': {
      j_unchecked_emitStr(vptr, "\\b", 2);
      break;
    }
    case '\f': {
      j_unchecked_emitStr(vptr, "\\f", 2);
      break;
    }
    case '\n': {
      j_unchecked_emitStr(vptr, "\\n", 2);
      break;
    }
    case '\r': {
      j_unchecked_emitStr(vptr, "\\r", 2);
      break;
    }
    case '\t': {
      j_unchecked_emitStr(vptr, "\\t", 2);
      break;
    }
    case '\"': {
      j_unchecked_emitStr(vptr, "\\\"", 2);
      break;
    }
    case '\\': {
      j_unchecked_emitStr(vptr, "\\\\", 2);
      break;
    }
    default: {
      if (c <= 0x001F) {
        char *ptr = vec_push(vptr, sizeof(char) * 6);
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
  j_unchecked_emitChar(vptr, '\"');
}

static void j_emitElem(Vector* data, j_Elem *j);

static void j_emitProp(Vector* vptr, j_Prop *prop) {
  j_emitStr(vptr, prop->key);
  j_unchecked_emitChar(vptr, ':');
  j_emitElem(vptr, &prop->value);
}

static void j_emitElem(Vector* data, j_Elem *j) {
  switch (j->kind) {
  case j_NullKind: {
    j_unchecked_emitStr(data, "null", 4);
    break;
  }
  case j_StrKind: {
    j_emitStr(data, j->string);
    break;
  }
  case j_BoolKind: {
    j_unchecked_emitStr(data, j->boolean ? "true" : "false",
                          j->boolean ? 4 : 5);
    break;
  }
  case j_IntKind: {
    // there are a max 20 digits in an integer
    j_emitInt(data, j->integer);
    break;
  }
  case j_NumKind: {
    j_emitNum(data, j->number);
    break;
  }
  case j_ArrayKind: {
    j_unchecked_emitChar(data, '[');
    for (size_t i = 0; i < j->array.length; i++) {
      j_emitElem(data, &j->array.values[i]);
      // if we are not on the ultimate element of the lsit
      if (i + 1 != j->array.length) {
        j_unchecked_emitChar(data, ',');
      }
    }
    j_unchecked_emitChar(data, ']');
    break;
  }
  case j_ObjectKind: {
    j_unchecked_emitChar(data, '{');
    for (size_t i = 0; i < j->object.length; i++) {
      j_emitProp(data, &j->object.props[i]);
      if (i + 1 != j->object.length) {
        j_unchecked_emitChar(data, ',');
      }
    }
    j_unchecked_emitChar(data, '}');
    break;
  }
  }
}

char *j_stringify(j_Elem *j, Allocator* a) {
  Vector data;
  vec_create(&data, a);
  j_emitElem(&data, j);
  // terminate string
  *VEC_PUSH(&data, char) = '\0';
  return vec_release(&data);
}

// Parsing

void certain_parseNumberJson(j_Elem *je, Lexer *l, Vector *diagnostics) {
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
      *VEC_PUSH(diagnostics, j_Error) =
          ERROR(j_NumExponentExpectedSign, l->position);
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
    je->kind = j_NumKind;
  } else {
    // Integer
    int64_t num = integer_value;
    if (positive_exponent) {
      for (int i = 0; i < exponential_integer; i++) {
        num *= 10;
      }
    }
    je->integer = num;
    je->kind = j_IntKind;
  }

  return;
}

void certain_parseLiteralJson(j_Elem *je, Lexer *l, Vector *diagnostics) {
  LnCol start = l->position;
  Vector data;

  // Fixed buffer size
  char buffer[6];
  size_t index = 0;
  while (true) {
    int32_t c = peekValueLexer(l);
    if (isalpha(c)) {
      // Fill up buffer
      if(index < 5) {
         buffer[index] = (char)c;
         index++;
      }
      // even if the buffer is finished we must continue on 
      nextValueLexer(l);
    } else {
      break;
    }
  }
  // Terminate with string length
  buffer[5] = '\0';

  if (!strcmp("null", buffer)) {
    je->kind = j_NullKind;
  } else if (!strcmp("true", buffer)) {
    je->kind = j_BoolKind;
    je->boolean = true;
  } else if (!strcmp("false", buffer)) {
    je->kind = j_BoolKind;
    je->boolean = false;
  } else {
    *VEC_PUSH(diagnostics, j_Error) = ERROR(j_MalformedLiteral, start);
    je->kind = j_NullKind;
  }
  return;
}

void certain_parseStringJson(j_Elem *je, Lexer *l, Vector *diagnostics) {
  LnCol start = l->position;
  int32_t c = nextValueLexer(l);
  assert(c == '\"');

  typedef enum {
    StringParserText,
    StringParserBackslash,
    StringParserUnicode,
    StringParserFinished,
  } StringParserState;

  Vector data;
  vec_create(&data, l->a);

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
        *VEC_PUSH(diagnostics, j_Error) = ERROR(
            j_StrExpectedDoubleQuote, l->position);
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
            ERROR(JPDK_JsonStringInvalidControlChar, l->position);
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
          *VEC_PUSH(diagnostics, JsonParseDiagnostic) = ERROR(
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

void parsej_Elem(j_Elem *je, Lexer *l, Vector *diagnostics);

static void certain_parseArrayJson(j_Elem *je, Lexer *l,
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
        *VEC_PUSH(diagnostics, JsonParseDiagnostic) = ERROR(
            JPDK_JsonArrayExpectedRightBracket, l->position);
        goto CLEANUP;
      }
      default: {
        *VEC_PUSH(diagnostics, JsonParseDiagnostic) = ERROR(
            JPDK_JsonArrayExpectedRightBracket, l->position);
        nextValueLexer(l);
        break;
      }
      }
      break;
    }
    case ArrayParseExpectElem: {
      skipWhitespace(l);
      j_Elem *elemptr = VEC_PUSH(&elems, j_Elem);
      parsej_Elem(elemptr, l, diagnostics);
      state = ArrayParseExpectCommaOrEnd;
      break;
    }
    }
  }
CLEANUP:;
  size_t len = VEC_LEN(&elems, j_Elem);
  *je = arrDefJson(manageMemArena(l->ar, releaseVector(&elems)), len);
}

static void parseKVJson(JsonKV *kv, Lexer *l, Vector *diagnostics) {
  j_Elem je;
  certain_parseStringJson(&je, l, diagnostics);
  if(je.kind != JE_string) {
    *VEC_PUSH(diagnostics, JsonParseDiagnostic) = ERROR(JPDK_JsonKVExpectedQuoted, l->position);
  }
}

void parsej_Elem(j_Elem *je, Lexer *l, Vector *diagnostics) {
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
        ERROR(j_ElemEof, l->position);
    return;
  }
  default: {
    *VEC_PUSH(diagnostics, JsonParseDiagnostic) =
        ERROR(j_ElemUnknownCharacter, l->position);
    nextValueLexer(l);
    return;
  }
  }}
