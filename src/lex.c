#include "lex.h"

#include <ctype.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "token.h"
#include "vector.h"

typedef struct {
  uint64_t val;
  ErrVal err;
} ResultU64;

// Parses integer with radix
static ResultU64 parseInteger(char* str, size_t len, uint64_t radix) {
  uint64_t ret = 0;
  for (size_t i = 0; i < len; i++) {
    // First we must determine the value of this digit
    char c = str[i];
    uint64_t digitValue = 0;
    if (c >= 'a' && c <= 'f') {
      digitValue = (uint64_t)(c - 'a') + 10;
    } else if (isdigit(c)) {
      digitValue = (uint64_t)(c - '0');
    } else {
      return (ResultU64){0, ErrBadargs};
    }

    // If you put something higher than is requested
    if (digitValue >= radix) {
      return (ResultU64){0, ErrBadargs};
    }

    uint64_t oldret = ret;
    ret = ret * radix + digitValue;
    if (oldret > ret) {
      return (ResultU64){0, ErrOverflow};
    }
  }
  return (ResultU64){ret, ErrOk};
}

// Call this function right before the first hash
// Returns control at the first noncomment area
// Lexes comments, annotations, and docstrings
static ResultTokenPtr lexComment(Parseable* stream) {
  if (nextValue(stream) != '#') {
    logError(ErrLevelError,
             "malformed comment found at line %" PRIu64 " and column %" PRIu64
             "\n",
             stream->lineNumber, stream->charNumber);
  }
  int32_t c = peekValue(stream);
  switch (c) {
    // This is a multi line comment
    case '*': {
      nextValue(stream);
      // comments are nested
      size_t stackDepth = 1;
      char lastChar = '\0';
      while ((c = nextValue(stream)) != EOF) {
        // If we see a *# to pair off the starting #*
        if (c == '#' && lastChar == '*') {
          stackDepth--;
          if (stackDepth == 0) {
            break;
          }
        } else if (c == '*' && lastChar == '#') {
          stackDepth++;
        }
        lastChar = (char)c;
      }
      return (ResultTokenPtr){newToken(SymComment, NULL), ErrOk};
    }
    // This is a docstring. It will continue until another quote hash is found.
    // These strings are preserved in the AST Like multiline comments, they are
    // also nestable
    // #" Yeet "#
    case '\"': {
      nextValue(stream);
      Vector* data = newVector();
      size_t stackDepth = 1;
      char lastChar = '\0';
      while ((c = nextValue(stream)) != EOF) {
        if (c == '#' && lastChar == '\"') {
          // If we see a "# to pair off the starting #"
          stackDepth--;
          if (stackDepth == 0) {
            break;
          }
        } else if (c == '\"' && lastChar == '#') {
          stackDepth++;
        }
        *VEC_PUSH(data, char) = (char)c;
        lastChar = (char)c;
      }
      // Push null byte
      *VEC_PUSH(data, char) = '\0';

      Token* t = newToken(SymDocumentation, malloc(lengthVector(data)));
      memcpy(t->payload, getVector(data, 0), lengthVector(data));
      deleteVector(data);
      return (ResultTokenPtr){t, ErrOk};
    }
    // This is an annotation. It will continue till a nonalphanumeric character
    // is found. They are not nestable
    // #@Annotation
    case '@': {
      nextValue(stream);
      Vector* data = newVector();
      while ((c = peekValue(stream)) != EOF) {
        if (isalnum(c)) {
          *VEC_PUSH(data, char) = (char)c;
          nextValue(stream);
        } else {
          break;
        }
      }
      // Push null byte
      *VEC_PUSH(data, char) = '\0';
      Token* t = newToken(SymAnnotation, malloc(lengthVector(data)));
      memcpy(t->payload, getVector(data, 0), lengthVector(data));
      deleteVector(data);
      return (ResultTokenPtr){t, ErrOk};
    }
    // If we don't recognize any of these characters, it's just a normal single
    // line comment # comment
    default: {
      while ((c = nextValue(stream)) != EOF) {
        if (c == '\n') {
          break;
        }
      }
      return (ResultTokenPtr){newToken(SymComment, NULL), ErrOk};
    }
  }
}

// Call this function right before the first quote of the string literal
// Returns control after the ending quote of this stream
// This function returns a Token containing the string or an error
static ResultTokenPtr lexStringLiteral(Parseable* stream) {
  if (nextValue(stream) != '\"') {
    logError(ErrLevelError, "malformed string: %" PRIu64 ", %" PRIu64 "\n",
             stream->lineNumber, stream->charNumber);
    return (ResultTokenPtr){NULL, ErrBadargs};
  }
  Vector* string = newVector();

  int32_t c;
  while ((c = nextValue(stream)) != EOF) {
    if (c == '\"') {
      break;
    } else {
      *VEC_PUSH(string, char) = (char)c;
    }
  }
  // Push null byte
  *VEC_PUSH(string, char) = (char)0;

  // Copy data to token, then delete the vector
  Token* t = newToken(SymStringLiteral, malloc(lengthVector(string)));
  memcpy(t->payload, getVector(string, 0), lengthVector(string));
  deleteVector(string);
  return (ResultTokenPtr){t, ErrOk};
}

// Call this function right before the first digit of the number literal
// Returns control right after the number is finished
// This function returns a Token or the error
static ResultTokenPtr lexNumberLiteral(Parseable* stream) {
  Vector* data = newVector();

  // Used to determine if float or not
  // decimalPointIndex will only be defined if hasDecimalPoint is true
  bool hasDecimalPoint = false;
  size_t decimalPointIndex = 0;

  size_t length = 0;
  int32_t c;
  while ((c = nextValue(stream)) != EOF) {
    if (isdigit(c)                 // Normal digit
        || c == '.'                // Decimal point
        || (c >= 'a' && c <= 'f')  // Hexadecimal Character
        || c == 'b' || c == 'd' || c == 'o' ||
        c == 'x'  // Character Interpretation
    ) {
      // If there's a decimal point we note the location
      // If this is the second time we've seen it, then the float is malformed
      if (c == '.') {
        if (hasDecimalPoint) {
          *VEC_PUSH(data, char) = '\0';
          logError(ErrLevelError,
                   "malformed float literal: excess decimal point: %" PRIu64
                   ", %" PRIu64 "\n",
                   stream->lineNumber, stream->charNumber);
          deleteVector(data);
          return (ResultTokenPtr){NULL, ErrBadargs};
        } else {
          hasDecimalPoint = true;
          decimalPointIndex = length;
        }
      }
      *VEC_PUSH(data, char) = (char)c;
      length++;
    } else if (c == '_') {
      // Do nothing
    } else {
      break;
    }
  }

  // If it's an integer
  if (!hasDecimalPoint) {
    uint64_t radix;
    // this is the null terminated string storing the text of the number
    char* intStr;
    size_t intStrLen;

    // Special Radix
    // More than 2 characters and first character is 0 and second character is
    // not digit
    if (length > 2 && *VEC_GET(data, 0, char) == '0' &&
        !isdigit(*VEC_GET(data, 1, char))) {
      // Set radix to what it has to be
      char secondCharacter = *VEC_GET(data, 1, char);
      switch (secondCharacter) {
        case 'b': {
          radix = 2;
          break;
        }
        case 'd': {
          radix = 10;
          break;
        }
        case 'o': {
          radix = 8;
          break;
        }
        case 'x': {
          radix = 16;
          break;
        }
        default: {
          *VEC_PUSH(data, char) = '\0';
          logError(ErrLevelError,
                   "malformed integer literal: unrecognized special radix "
                   "code: %s, %" PRIu64 ", %" PRIu64 "\n",
                   data->data, stream->lineNumber, stream->charNumber);
          deleteVector(data);
          return (ResultTokenPtr){NULL, ErrBadargs};
        }
      }
      intStr = VEC_GET(data, 2, char);
      intStrLen = VEC_LEN(data, char) - 2;
    } else {
      radix = 10;
      intStr = VEC_GET(data, 0, char);
      intStrLen = VEC_LEN(data, char);
    }
    ResultU64 ret = parseInteger(intStr, intStrLen, radix);
    deleteVector(data);
    if (ret.err != ErrOk) {
      *VEC_PUSH(data, char) = '\0';
      logError(ErrLevelError,
               "malformed integer literal: `%s`: %s: %" PRIu64 ", %" PRIu64
               "\n",
               data->data, strErrVal(ret.err), stream->lineNumber,
               stream->charNumber);
      return (ResultTokenPtr){NULL, ErrBadargs};
    } else {
      Token* t = newToken(SymIntLiteral, malloc(sizeof(ret.val)));
      memcpy(t->payload, &ret.val, sizeof(ret.val));
      return (ResultTokenPtr){t, ErrOk};
    }
  } else {
    // If it has a decimal point, it must be a float
    // We have already guaranteed that the string only has one decimal point,
    // at decimalPointIndex Floats are always in decimal notation

    // We parse the float as 2 integers, one above the decimal point, and one
    // below
    char* initialPortion = VEC_GET(data, 0, char);
    uint64_t initialPortionLen = decimalPointIndex;

    char* finalPortion = VEC_GET(data, decimalPointIndex + 1, char);
    uint64_t finalPortionLen = VEC_LEN(data, char) - decimalPointIndex - 1;

    double result = 0;

    // Parse the part ahead of the decimal point
    ResultU64 initialRet = parseInteger(initialPortion, initialPortionLen, 10);
    if (initialRet.err == ErrOk) {
      result += initialRet.val;
    } else {
      *VEC_PUSH(data, char) = '\0';
      logError(ErrLevelError,
               "malformed float literal: `%s': %s: %" PRIu64 ", %" PRIu64 "\n",
               data->data, strErrVal(initialRet.err), stream->lineNumber,
               stream->charNumber);
      deleteVector(data);
      return (ResultTokenPtr){NULL, ErrBadargs};
    }
    // If there's a bit after the inital part, then we must add it
    if (finalPortionLen > 0) {
      ResultU64 finalRet = parseInteger(finalPortion, finalPortionLen, 10);
      if (finalRet.err == ErrOk) {
        // don't want to include math.h, so we'll repeatedly divide by 10
        // this is probably dumb
        double decimalResult = finalRet.val;
        for (size_t i = 0; i < finalPortionLen; i++) {
          decimalResult /= 10;
        }
        result += decimalResult;
      } else {
        *VEC_PUSH(data, char) = '\0';
        logError(ErrLevelError,
                 "malformed float literal: `%s`: %s: %" PRIu64 ", %" PRIu64
                 "\n",
                 data->data, strErrVal(initialRet.err), stream->lineNumber,
                 stream->charNumber);
        deleteVector(data);
        return (ResultTokenPtr){NULL, ErrBadargs};
      }
    }
    Token* t = newToken(SymFloatLiteral, malloc(sizeof(result)));
    memcpy(t->payload, &result, sizeof(result));
    deleteVector(data);
    return (ResultTokenPtr){t, ErrOk};
  }
}

static ResultTokenPtr lexCharacterLiteral(Parseable* stream) {
  if (nextValue(stream) != '\'') {
    logError(ErrLevelError,
             "malformed character literal: %" PRIu64 ", %" PRIu64 "\n",
             stream->lineNumber, stream->charNumber);
    return (ResultTokenPtr){NULL, ErrBadargs};
  }

  Vector* chars = newVector();

  int32_t c;

  while ((c = nextValue(stream)) != EOF) {
    if (c == '\\') {
      c = nextValue(stream);
      switch (c) {
        case 't': {
          c = '\t';
          break;
        }
        case 'n': {
          c = '\n';
          break;
        }
        case '\\': {
          c = '\\';
          break;
        }
        case EOF: {
          logError(
              ErrLevelError,
              "malformed character literal: unexpected end of file: %" PRIu64
              ", %" PRIu64 "\n",
              stream->lineNumber, stream->charNumber);
          return (ResultTokenPtr){NULL, ErrEof};
        }
        default: {
          logError(ErrLevelError,
                   "malformed character literal: unexpected escape code: `%c` "
                   "%" PRIu64 ", %" PRIu64 "\n",
                   c, stream->lineNumber, stream->charNumber);
          return (ResultTokenPtr){NULL, ErrBadargs};
        }
      }
      *VEC_PUSH(chars, char) = (char)c;
    } else if (c == '\'') {
      break;
    } else {
      *VEC_PUSH(chars, char) = (char)c;
    }
  }

  switch (VEC_LEN(chars, char)) {
    case 0: {
      logError(ErrLevelError,
               "malformed character literal: too short %" PRIu64 ", %" PRIu64
               "\n",
               stream->lineNumber, stream->charNumber);
      return (ResultTokenPtr){NULL, ErrBadargs};
    }
    case 1: {
      // Copy data to token, then delete the vector
      Token* t = newToken(SymCharacterLiteral, malloc(sizeof(char)));
      *(char*)(t->payload) = (char)c;
      return (ResultTokenPtr){t, ErrOk};
    }
    default: {
      logError(ErrLevelError,
               "malformed character literal: too long %" PRIu64 ", %" PRIu64
               "\n",
               stream->lineNumber, stream->charNumber);
      return (ResultTokenPtr){NULL, ErrBadargs};
    }
  }
}

static ResultTokenPtr lexIdentifier(Parseable* stream) {
  Vector* data = newVector();
  int32_t c;
  while ((c = peekValue(stream)) != EOF) {
    if (isalnum(c)) {
      *VEC_PUSH(data, char) = (char)c;
      nextValue(stream);
    } else {
      break;
    }
  }
  // Push null byte
  *VEC_PUSH(data, char) = '\0';

  Token* t = NULL;

  if (!strcmp(VEC_GET(data, 0, char), "if")) {
    t = newToken(SymIf, NULL);
  } else if (!strcmp(VEC_GET(data, 0, char), "else")) {
    t = newToken(SymElse, NULL);
  } else if (!strcmp(VEC_GET(data, 0, char), "do")) {
    t = newToken(SymDoWhile, NULL);
  } else if (!strcmp(VEC_GET(data, 0, char), "while")) {
    t = newToken(SymWhile, NULL);
  } else if (!strcmp(VEC_GET(data, 0, char), "with")) {
    t = newToken(SymWith, NULL);
  } else if (!strcmp(VEC_GET(data, 0, char), "for")) {
    t = newToken(SymFor, NULL);
  } else if (!strcmp(VEC_GET(data, 0, char), "break")) {
    t = newToken(SymBreak, NULL);
  } else if (!strcmp(VEC_GET(data, 0, char), "continue")) {
    t = newToken(SymContinue, NULL);
  } else if (!strcmp(VEC_GET(data, 0, char), "return")) {
    t = newToken(SymReturn, NULL);
  } else {
    // It's a normal identifier, not a keyword
    Token* t = newToken(SymIdentifier, malloc(lengthVector(data)));
    memcpy(t->payload, getVector(data, 0), lengthVector(data));
    deleteVector(data);
    return (ResultTokenPtr){t, ErrOk};
  }
  // If it was a keyword;
  deleteVector(data);
  return (ResultTokenPtr){t, ErrOk};
}

ResultTokenPtr nextToken(Parseable* stream) {
  int32_t c;
  while ((c = peekValue(stream)) != EOF) {
    // We're dealing with an operator
    // Pop the current value
    switch (c) {
      case '&': {
        nextValue(stream);
        int32_t n = peekValue(stream);
        // && or &
        if (n == '&') {
          nextValue(stream);
          return (ResultTokenPtr){newToken(SymAnd, NULL), ErrOk};
        } else {
          return (ResultTokenPtr){newToken(SymBitAnd, NULL), ErrOk};
        }
      }
      case '|': {
        nextValue(stream);
        int32_t n = peekValue(stream);
        // || or |
        if (n == '|') {
          nextValue(stream);
          return (ResultTokenPtr){newToken(SymOr, NULL), ErrOk};
        }
        else if (n == '>') {
          nextValue(stream);
          return (ResultTokenPtr){newToken(SymPipe, NULL), ErrOk};
        } else {
          return (ResultTokenPtr){newToken(SymBitOr, NULL), ErrOk};
        }
      }
      case '!': {
        nextValue(stream);
        int32_t n = peekValue(stream);
        // ! or !=
        if (n == '=') {
          nextValue(stream);
          return (ResultTokenPtr){newToken(SymNotEqual, NULL), ErrOk};
        } else {
          return (ResultTokenPtr){newToken(SymNot, NULL), ErrOk};
        }
      }
      case '=': {
        nextValue(stream);
        int32_t n = peekValue(stream);
        // = or ==
        if (n == '=') {
          nextValue(stream);
          return (ResultTokenPtr){newToken(SymEqual, NULL), ErrOk};
        } else {
          return (ResultTokenPtr){newToken(SymAssign, NULL), ErrOk};
        }
      }
      case '<': {
        nextValue(stream);
        int32_t n = peekValue(stream);
        if (n == '<') {
          nextValue(stream);
          return (ResultTokenPtr){newToken(SymShiftLeft, NULL), ErrOk};
        } else if (n == '=') {
          nextValue(stream);
          return (ResultTokenPtr){newToken(SymCompLessEqual, NULL), ErrOk};
        } else {
          return (ResultTokenPtr){newToken(SymCompLess, NULL), ErrOk};
        }
      }
      case '>': {
        nextValue(stream);
        int32_t n = peekValue(stream);
        if (n == '>') {
          nextValue(stream);
          return (ResultTokenPtr){newToken(SymShiftRight, NULL), ErrOk};
        } else if (n == '=') {
          nextValue(stream);
          return (ResultTokenPtr){newToken(SymCompGreaterEqual, NULL), ErrOk};
        } else {
          return (ResultTokenPtr){newToken(SymCompGreater, NULL), ErrOk};
        }
      }
      case '+': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymAdd, NULL), ErrOk};
      }
      case '-': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymSub, NULL), ErrOk};
      }
      case '*': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymMul, NULL), ErrOk};
      }
      case '/': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymDiv, NULL), ErrOk};
      }
      case '%': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymMod, NULL), ErrOk};
      }
      case '$': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymRef, NULL), ErrOk};
      }
      case '@': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymDeref, NULL), ErrOk};
      }
      case '(': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymParenLeft, NULL), ErrOk};
      }
      case ')': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymParenRight, NULL), ErrOk};
      }
      case '[': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymBracketLeft, NULL), ErrOk};
      }
      case ']': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymBracketRight, NULL), ErrOk};
      }
      case '{': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymBraceLeft, NULL), ErrOk};
      }
      case '}': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymBraceRight, NULL), ErrOk};
      }
      case '.': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymDot, NULL), ErrOk};
      }
      case ',': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymComma, NULL), ErrOk};
      }
      case ':': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymColon, NULL), ErrOk};
      }
      case ';': {
        nextValue(stream);
        return (ResultTokenPtr){newToken(SymSemicolon, NULL), ErrOk};
      }
      default: {
        // Lex the weird literals, comments, annotation, identifiers, etc
        if (isblank(c) || c == '\n') {
          nextValue(stream);
        } else {
          ResultTokenPtr ret = (ResultTokenPtr){NULL, ErrUnknown};
          if (isdigit(c)) {
            ret = lexNumberLiteral(stream);
          } else if (c == '#') {
            ret = lexComment(stream);
          } else if (c == '\"') {
            ret = lexStringLiteral(stream);
          } else if (c == '\'') {
            ret = lexCharacterLiteral(stream);
          } else if (isalpha(c)) {
            ret = lexIdentifier(stream);
          } else {
            // If we simply don't recognize the character, we're going to give
            // an error and move on to the next value
            logError(ErrLevelError,
                     "unrecognized character: `%c`: %" PRIu64 ", %" PRIu64 "\n",
                     c, stream->lineNumber, stream->charNumber);
            nextValue(stream);
          }

          if (ret.err == ErrOk) {
            return ret;
          }
        }
      }
    }
  }
  // If we hit the end of the file just give a End of file error
  return (ResultTokenPtr){NULL, ErrEof};
}
