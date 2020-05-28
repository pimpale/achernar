#ifndef JSON_H_
#define JSON_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "lncol.h"
#include "lexer.h"

typedef enum {
  j_boolean,
  j_integer,
  j_number,
  j_string,
  j_array,
  j_object,
  j_null,
} j_JsonElemKind;

typedef struct j_JsonElem_s j_JsonElem;
typedef struct j_JsonKV_s JsonKV;

typedef struct JsonStr_s {
      char *string;
      size_t length;
} JsonStr;

typedef struct JsonElem_s {
  JsonElemKind kind;
  union {
    bool boolean;
    uint64_t integer;
    double number;
    JsonStr string;
    struct {
      JsonElem *values;
      size_t length;
    } array;
    struct {
      JsonKV *values;
      size_t length;
    } object;
  };
} JsonElem;

typedef struct JsonKV_s {
  char *key;
  JsonElem value;
} JsonKV;

// Utility functions
JsonElem j_jsonStrJson(char *x, size_t len);
JsonKV KVJson(char *key, JsonElem value);

JsonElem nullJson(void);
JsonElem boolJson(bool x);
JsonElem intJson(uint64_t x);
JsonElem numJson(double x);
JsonElem strJson(char *x, size_t len);
JsonElem arrDefJson(JsonElem *ptr, size_t len);
JsonElem objDefJson(JsonKV *ptr, size_t len);

char *toStringJsonElem(JsonElem *j);

// Parse JSON
// JSON Parsing errors
typedef enum JsonParseDiagnosticKind_e {
  JPDK_JsonElemEof,
  JPDK_JsonElemUnknownCharacter,
  JPDK_JsonStringExpectedDoubleQuote,
  JPDK_JsonStringInvalidControlChar,
  JPDK_JsonStringInvalidUnicodeSpecifier,
  JPDK_JsonMalformedLiteral,
  JPDK_JsonNumberExponentExpectedSign,
  JPDK_JsonArrayExpectedRightBracket,
  JPDK_JsonArrayExpectedJsonElem,
  JPDK_JsonObjExpectedRightBrace,
  JPDK_JsonObjExpectedJsonKV,
  JPDK_JsonKVExpectedQuoted,
  JPDK_JsonKVExpectedColon,
  JPDK_JsonKVExpectedValue,
} JsonParseDiagnosticKind ;

typedef struct JsonParseDiagnostic_s {
  JsonParseDiagnosticKind kind;
  LnCol loc;
} JsonParseDiagnostic;

#define JSONPARSEDIAGNOSTIC(k, l) ((JsonParseDiagnostic) { .kind=k, .loc=l})

void parseJsonElem(JsonElem *je, Lexer *l, Vector* diagnostics);

#endif
