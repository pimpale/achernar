#ifndef UTILS_H
#define UTILS_H

#include <stdint.h>
#include <ctype.h>
#include "error.h"

DiagnosticKind parseInteger(uint64_t *value, char *str, size_t len, 
uint64_t radix);

// Parses integer with radix
DiagnosticKind parseInteger(uint64_t *value, char *str, size_t len,
                                   uint64_t radix) {
  uint64_t ret = 0;
  for (size_t i = 0; i < len; i++) {
    // First we must determine the value of this digit
    char c = str[i];
    uint64_t digit_value = 0;
    if (c >= 'a' && c <= 'f') {
      digit_value = (uint64_t)(c - 'a') + 10;
    } else if (isdigit(c)) {
      digit_value = (uint64_t)(c - '0');
    } else {
      return DK_IntLiteralUnknownCharacter;
    }

    // If you put something higher than is requested
    if (digit_value >= radix) {
      return DK_IntLiteralDigitExceedsRadix;
    }

    uint64_t oldret = ret;
    ret = ret * radix + digit_value;
    if (oldret > ret) {
      return DK_IntLiteralOverflow;
    }
  }
  *value = ret;
  return DK_Ok;
}

#endif

