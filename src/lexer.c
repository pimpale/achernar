#include "lexer.h"

#include <ctype.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "vector.h"

Lexer *createLexerFile(Lexer *lexer, FILE *file) {
  lexer->backing = LexerBackingFile;
  lexer->file = file;
  lexer->lineNumber = 0;
  lexer->charNumber = 0;
  return lexer;
}

Lexer *createLexerMemory(Lexer *lexer, char *ptr, size_t len) {
  lexer->backing = LexerBackingMemory;
  lexer->lineNumber = 0;
  lexer->charNumber = 0;

  // Copy memory
  lexer->memory.len = len;
  lexer->memory.loc = 0;
  lexer->memory.ptr = malloc(len);
  memcpy(lexer->memory.ptr, ptr, len);
  return lexer;
}

void destroyLexer(Lexer *lexer) {
  switch (lexer->backing) {
  case LexerBackingMemory: {
    free(lexer->memory.ptr);
    break;
  }
  case LexerBackingFile: {
    break;
  }
  }
}

static int32_t nextValueLexer(Lexer *lexer) {
  int32_t nextValue;
  switch (lexer->backing) {
  case LexerBackingMemory: {
    if (lexer->memory.loc + 1 >= lexer->memory.len) {
      nextValue = EOF;
    } else {
      // Return the element at the location, and increment location
      nextValue = (lexer->memory.ptr[lexer->memory.loc]);
      lexer->memory.loc++;
      if (nextValue == '\n') {
        lexer->lineNumber += 1;
        lexer->charNumber = 0;
      } else {
        lexer->charNumber += 1;
      }
    }
    break;
  }
  case LexerBackingFile: {
    nextValue = getc(lexer->file);
    if (nextValue == '\n') {
      lexer->lineNumber += 1;
      lexer->charNumber = 0;
    } else {
      lexer->charNumber += 1;
    }
    break;
  }
  }
  return nextValue;
}

static int32_t peekValueLexer(Lexer *lexer) {
  switch (lexer->backing) {
  case LexerBackingMemory: {
    // If it's within the bounds, return the value ahead of us
    if (lexer->memory.loc + 2 < lexer->memory.len) {
      return (lexer->memory.ptr[lexer->memory.loc + 1]);
    } else {
      return EOF;
    }
  }
  case LexerBackingFile: {
    int32_t val = getc(lexer->file);
    ungetc(val, lexer->file);
    return val;
  }
  }
}

// Stuff to lex with
typedef struct {
  uint64_t val;
  ErrVal err;
} ResultU64;

// Parses integer with radix
static ResultU64 parseInteger(char *str, size_t len, uint64_t radix) {
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
// Returns control at the first noncomment (or nonannotate) area
// Lexes comments, annotations, and docstrings
static ResultToken lexCommentOrAnnotation(Lexer *lexer) {
  if (nextValueLexer(lexer) != '#') {
    logError(ErrLevelError,
             "malformed comment found at line %" PRIu64 " and column %" PRIu64
             "\n",
             lexer->lineNumber, lexer->charNumber);
  }
  int32_t c = peekValueLexer(lexer);

  // Now we determine the type of comment as well as gather the comment data
  switch (c) {
  case '*': {
    // This is a comment. It will continue until another quote asterix is found.
    // These strings are preserved in the AST. They are nestable
    // also nestable
    // #* Comment *#
    Vector *data = newVector();
    nextValueLexer(lexer);
    size_t stackDepth = 1;
    char lastChar = '\0';
    while ((c = nextValueLexer(lexer)) != EOF) {
      if (c == '#' && lastChar == '*') {
        // If we see a "# to pair off the starting #"
        stackDepth--;
        if (stackDepth == 0) {
          break;
        }
      } else if (c == '*' && lastChar == '#') {
        stackDepth++;
      }
      *VEC_PUSH(data, char) = (char)c;
      lastChar = (char)c;
    }
    // Push null byte
    *VEC_PUSH(data, char) = '\0';

    // Copy data over to the string
    char *string = malloc(lengthVector(data));
    memcpy(string, getVector(data, 0), lengthVector(data));
    deleteVector(data);

    // Return data
    // clang-format off
    return (ResultToken) {
      .val = (Token) {
        .type = TokenComment,
        .comment = string
      },
      .err = ErrOk
    };
    // clang-format on
  }
  case '@': {
    // This is an annotation. It will continue till a nonalphanumeric character
    // is found. They are not nestable
    // #@Annotation
    Vector *data = newVector();
    nextValueLexer(lexer);
    while ((c = peekValueLexer(lexer)) != EOF) {
      if (isalnum(c)) {
        *VEC_PUSH(data, char) = (char)c;
        nextValueLexer(lexer);
      } else {
        break;
      }
    }
    // Push null byte
    *VEC_PUSH(data, char) = '\0';

    // Copy data over to the string
    char *string = malloc(lengthVector(data));
    memcpy(string, getVector(data, 0), lengthVector(data));
    deleteVector(data);

    // Return data
    // clang-format off
    return (ResultToken) {
      .val = (Token) {
        .type = TokenAnnotation,
        .annotationLiteral = string
      },
      .err = ErrOk
    };
    // clang-format on
  }
  default: {
    // If we don't recognize any of these characters, it's just a normal single
    // line comment. These are not nestable, and continue till the end of line.
    // # comment
    Vector *data = newVector();
    while ((c = nextValueLexer(lexer)) != EOF) {
      if (c != '\n') {
        *VEC_PUSH(data, char) = (char)c;
        nextValueLexer(lexer);
      } else {
        break;
      }
    }
    *VEC_PUSH(data, char) = '\0';

    // Copy data over to the string
    char *string = malloc(lengthVector(data));
    memcpy(string, getVector(data, 0), lengthVector(data));
    deleteVector(data);

    // Return data
    // clang-format off
    return (ResultToken) {
      .val = (Token) {
        .type = TokenComment,
        .comment = string
      },
      .err = ErrOk
    };
    // clang-format on
  }
  }
}

// Call this function right before the first quote of the string literal
// Returns control after the ending quote of this lexer
// This function returns a Token containing the string or an error
static ResultToken lexStringLiteral(Lexer *lexer) {
  if (nextValueLexer(lexer) != '\"') {
    logError(ErrLevelError, "malformed string: %" PRIu64 ", %" PRIu64 "\n",
             lexer->lineNumber, lexer->charNumber);
    return (ResultToken){.err = ErrBadargs};
  }

  Vector *data = newVector();

  int32_t c;
  while ((c = nextValueLexer(lexer)) != EOF) {
    if (c == '\"') {
      break;
    } else {
      *VEC_PUSH(data, char) = (char)c;
    }
  }
  // Push null byte
  *VEC_PUSH(data, char) = '\0';

  // Copy data over to the string
  char *string = malloc(lengthVector(data));
  memcpy(string, getVector(data, 0), lengthVector(data));
  deleteVector(data);

  // Return data
  // clang-format off
  return (ResultToken) {
    .val = (Token) {
      .type = TokenStringLiteral,
      .stringLiteral = string
    },
    .err = ErrOk
  };
  // clang-format on
}

// Call this function right before the first digit of the number literal
// Returns control right after the number is finished
// This function returns a Token or the error
static ResultToken lexNumberLiteral(Lexer *lexer) {
  Vector *data = newVector();

  // Used to determine if float or not
  // decimalPointIndex will only be defined if hasDecimalPoint is true
  bool hasDecimalPoint = false;
  size_t decimalPointIndex = 0;

  size_t length = 0;
  int32_t c;
  while ((c = nextValueLexer(lexer)) != EOF) {
    if (isdigit(c)                // Normal digit
        || c == '.'               // Decimal point
        || (c >= 'a' && c <= 'f') // Hexadecimal Character
        || c == 'b' || c == 'd' || c == 'o' ||
        c == 'x' // Character Interpretation
    ) {
      // If there's a decimal point we note the location
      // If this is the second time we've seen it, then the float is malformed
      if (c == '.') {
        if (hasDecimalPoint) {
          *VEC_PUSH(data, char) = '\0';
          logError(ErrLevelError,
                   "malformed float literal: excess decimal point: %" PRIu64
                   ", %" PRIu64 "\n",
                   lexer->lineNumber, lexer->charNumber);
          deleteVector(data);
          return (ResultToken){.err = ErrBadargs};
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
    char *intStr;
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
                 data->data, lexer->lineNumber, lexer->charNumber);
        deleteVector(data);
        return (ResultToken){.err = ErrBadargs};
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
      logError(
          ErrLevelError,
          "malformed integer literal: `%s`: %s: %" PRIu64 ", %" PRIu64 "\n",
          data->data, strErrVal(ret.err), lexer->lineNumber, lexer->charNumber);

      deleteVector(data);
      return (ResultToken){.err = ErrBadargs};
    } else {
      // Return data
      // clang-format off
      return (ResultToken) {
        .val = (Token) {
          .type = TokenIntLiteral,
          .intLiteral = ret.val
        },
        .err = ErrOk
      };
      // clang-format on
    }
  } else {
    // If it has a decimal point, it must be a float
    // We have already guaranteed that the string only has one decimal point,
    // at decimalPointIndex Floats are always in decimal notation

    // We parse the float as an integer above decimal point, and one below

    // Represents the portion of the float literal prior to the decimal point
    char *initialPortion = VEC_GET(data, 0, char);
    uint64_t initialPortionLen = decimalPointIndex;

    // Represents the portion of the float literal after the decimal point
    char *finalPortion = VEC_GET(data, decimalPointIndex + 1, char);
    uint64_t finalPortionLen = VEC_LEN(data, char) - decimalPointIndex - 1;

    // the thing we'll return
    double result = 0;

    // Parse the part ahead of the decimal point
    ResultU64 initialRet = parseInteger(initialPortion, initialPortionLen, 10);
    if (initialRet.err == ErrOk) {
      result += initialRet.val;
    } else {
      *VEC_PUSH(data, char) = '\0';
      logError(ErrLevelError,
               "malformed float literal: `%s': %s: %" PRIu64 ", %" PRIu64 "\n",
               data->data, strErrVal(initialRet.err), lexer->lineNumber,
               lexer->charNumber);
      deleteVector(data);
      return (ResultToken){.err = ErrBadargs};
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
                 data->data, strErrVal(initialRet.err), lexer->lineNumber,
                 lexer->charNumber);
        deleteVector(data);
        return (ResultToken){.err = ErrBadargs};
      }
    }

    deleteVector(data);

    // Return data
    // clang-format off
    return (ResultToken) {
      .val = (Token) {
        .type = TokenFloatLiteral,
        .floatLiteral = result
      },
      .err = ErrOk
    };
    // clang-format on
  }
}

static ResultToken lexCharLiteral(Lexer *lexer) {
  if (nextValueLexer(lexer) != '\'') {
    logError(ErrLevelError,
             "malformed character literal: %" PRIu64 ", %" PRIu64 "\n",
             lexer->lineNumber, lexer->charNumber);
    return (ResultToken){.err = ErrBadargs};
  }

  // We basically read the whole thing into a string.
  Vector *chars = newVector();
  int32_t c;

  while ((c = nextValueLexer(lexer)) != EOF) {
    if (c == '\\') {
      c = nextValueLexer(lexer);
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
        break;
      }
      default: {
        // Clean up
        deleteVector(chars);
        logError(ErrLevelError,
                 "malformed character literal: unexpected escape code: `%c` "
                 "%" PRIu64 ", %" PRIu64 "\n",
                 c, lexer->lineNumber, lexer->charNumber);
        return (ResultToken){.err = ErrBadargs};
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
    // Clean up
    deleteVector(chars);
    logError(ErrLevelError,
             "malformed character literal: empty %" PRIu64 ", %" PRIu64 "\n",
             lexer->lineNumber, lexer->charNumber);
    return (ResultToken){.err = ErrBadargs};
  }
  case 1: {
    // We return the first character in the vector

    // clang-format off
    ResultToken result = (ResultToken) {
      .val = (Token) {
        .type = TokenCharLiteral,
        .charLiteral = *VEC_GET(chars, 0, char)
      },
      .err = ErrOk
    };
    // clang-format on

    // Clean up
    deleteVector(chars);
    return result;
  }
  default: {
    // Clean up
    deleteVector(chars);
    logError(ErrLevelError,
             "malformed character literal: too long %" PRIu64 ", %" PRIu64 "\n",
             lexer->lineNumber, lexer->charNumber);
    return (ResultToken){.err = ErrBadargs};
  }
  }
}

static ResultToken lexIdentifier(Lexer *lexer) {
  Vector *data = newVector();
  int32_t c;
  while ((c = peekValueLexer(lexer)) != EOF) {
    if (isalnum(c)) {
      *VEC_PUSH(data, char) = (char)c;
      nextValueLexer(lexer);
    } else {
      break;
    }
  }
  // Push null byte
  *VEC_PUSH(data, char) = '\0';

  Token t;

  if (!strcmp(VEC_GET(data, 0, char), "if")) {
    t = (Token){.type = TokenIf, .lineNumber = lexer->lineNumber};
  } else if (!strcmp(VEC_GET(data, 0, char), "else")) {
    t = (Token){.type = TokenElse, .lineNumber = lexer->lineNumber};
  } else if (!strcmp(VEC_GET(data, 0, char), "while")) {
    t = (Token){.type = TokenWhile, .lineNumber = lexer->lineNumber};
  } else if (!strcmp(VEC_GET(data, 0, char), "with")) {
    t = (Token){.type = TokenWith, .lineNumber = lexer->lineNumber};
  } else if (!strcmp(VEC_GET(data, 0, char), "for")) {
    t = (Token){.type = TokenFor, .lineNumber = lexer->lineNumber};
  } else if (!strcmp(VEC_GET(data, 0, char), "break")) {
    t = (Token){.type = TokenBreak, .lineNumber = lexer->lineNumber};
  } else if (!strcmp(VEC_GET(data, 0, char), "continue")) {
    t = (Token){.type = TokenContinue, .lineNumber = lexer->lineNumber};
  } else if (!strcmp(VEC_GET(data, 0, char), "return")) {
    t = (Token){.type = TokenReturn, .lineNumber = lexer->lineNumber};
  } else {
    // It is an identifier

    char *string = malloc(lengthVector(data));
    memcpy(string, getVector(data, 0), lengthVector(data));

    // clang-format off
    t = (Token) {
      .type = TokenIdentifier,
      .identifier = string,
      .lineNumber = lexer->lineNumber
    };
    // clang-format on
  }
  deleteVector(data);
  return (ResultToken){.val = t, .err = ErrOk};
}

/* clang-format off */
#define RETURN_RESULT_TOKEN(tokenType)                                         \
  return (ResultToken) {                                                       \
    .val = (Token){                                                            \
      .type = tokenType,                                                       \
      .lineNumber = lexer->lineNumber                                          \
    },                                                                         \
    .err = ErrOk                                                               \
  };                                                                           \

#define NEXT_AND_RETURN_RESULT_TOKEN(tokenType) \
  nextValueLexer(lexer); \
  RETURN_RESULT_TOKEN(tokenType)

/* clang-format on */
ResultToken nextToken(Lexer *lexer) {
  int32_t c;
  while ((c = peekValueLexer(lexer)) != EOF) {
    // We're dealing with an operator
    // Pop the current value
    switch (c) {
    case '&': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      // && or &
      if (n == '&') {
        NEXT_AND_RETURN_RESULT_TOKEN(TokenAnd)
      } else {
        RETURN_RESULT_TOKEN(TokenBitAnd)
      }
    }
    case '|': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      // || or |
      if (n == '|') {
        NEXT_AND_RETURN_RESULT_TOKEN(TokenOr)
      } else if (n == '>') {
        NEXT_AND_RETURN_RESULT_TOKEN(TokenPipe)
      } else {
        RETURN_RESULT_TOKEN(TokenBitOr)
      }
    }
    case '!': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      // ! or !=
      if (n == '=') {
        NEXT_AND_RETURN_RESULT_TOKEN(TokenNotEqual)
      } else {
        RETURN_RESULT_TOKEN(TokenNot)
      }
    }
    case '=': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      // = or ==
      if (n == '=') {
        NEXT_AND_RETURN_RESULT_TOKEN(TokenEqual)
      } else {
        RETURN_RESULT_TOKEN(TokenAssign)
      }
    }
    case '<': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      if (n == '<') {
        NEXT_AND_RETURN_RESULT_TOKEN(TokenShiftLeft)
      } else if (n == '=') {
        NEXT_AND_RETURN_RESULT_TOKEN(TokenCompLessEqual)
      } else {
        RETURN_RESULT_TOKEN(TokenCompLess)
      }
    }
    case '>': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      if (n == '>') {
        NEXT_AND_RETURN_RESULT_TOKEN(TokenShiftRight)
      } else if (n == '=') {
        NEXT_AND_RETURN_RESULT_TOKEN(TokenCompGreaterEqual)
      } else {
        RETURN_RESULT_TOKEN(TokenCompGreater)
      }
    }
    case '+': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenAdd)
    }
    case '-': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenSub)
    }
    case '*': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenMul)
    }
    case '/': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenDiv)
    }
    case '%': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenMod)
    }
    case '$': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenRef)
    }
    case '@': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenDeref)
    }
    case '(': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenParenLeft)
    }
    case ')': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenParenRight)
    }
    case '[': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenBracketLeft)
    }
    case ']': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenBracketRight)
    }
    case '{': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenBraceLeft)
    }
    case '}': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenBraceRight)
    }
    case '.': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenDot)
    }
    case ',': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenComma)
    }
    case ':': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenColon)
    }
    case ';': {
      NEXT_AND_RETURN_RESULT_TOKEN(TokenSemicolon)
    }
    default: {
      // Lex the weird literals, comments, annotation, identifiers, etc
      if (isdigit(c)) {
        return lexNumberLiteral(lexer);
      } else if (c == '#') {
        return lexCommentOrAnnotation(lexer);
      } else if (c == '\"') {
        return lexStringLiteral(lexer);
      } else if (c == '\'') {
        return lexCharLiteral(lexer);
      } else if (isalpha(c)) {
        return lexIdentifier(lexer);
      }
      if (isblank(c) || c == '\n') {
        nextValueLexer(lexer);
      } else {
        // If we simply don't recognize the character, we're going to give
        // an error and move on to the next value
        logError(ErrLevelError,
                 "unrecognized character: `%c`: %" PRIu64 ", %" PRIu64 "\n", c,
                 lexer->lineNumber, lexer->charNumber);
        nextValueLexer(lexer);
      }
    }
    }
  }
  // If we hit the end of the file just give a End of file error
  return (ResultToken){.err = ErrEof};
}
