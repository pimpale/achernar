#include "token.h"

#include <ctype.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "vector.h"

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
static ResultToken lexCommentOrAnnotation(Parseable *stream) {
  if (nextValue(stream) != '#') {
    logError(ErrLevelError,
             "malformed comment found at line %" PRIu64 " and column %" PRIu64
             "\n",
             stream->lineNumber, stream->charNumber);
  }
  int32_t c = peekValue(stream);

  // Now we determine the type of comment as well as gather the comment data
  switch (c) {
  case '*': {
    // This is a comment. It will continue until another quote asterix is found.
    // These strings are preserved in the AST. They are nestable
    // also nestable
    // #* Comment *#
    Vector *data = newVector();
    nextValue(stream);
    size_t stackDepth = 1;
    char lastChar = '\0';
    while ((c = nextValue(stream)) != EOF) {
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
    nextValue(stream);
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
    while ((c = nextValue(stream)) != EOF) {
      if (c != '\n') {
        *VEC_PUSH(data, char) = (char)c;
        nextValue(stream);
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
// Returns control after the ending quote of this stream
// This function returns a Token containing the string or an error
static ResultToken lexStringLiteral(Parseable *stream) {
  if (nextValue(stream) != '\"') {
    logError(ErrLevelError, "malformed string: %" PRIu64 ", %" PRIu64 "\n",
             stream->lineNumber, stream->charNumber);
    return (ResultToken){.err = ErrBadargs};
  }

  Vector *data = newVector();

  int32_t c;
  while ((c = nextValue(stream)) != EOF) {
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
static ResultToken lexNumberLiteral(Parseable *stream) {
  Vector *data = newVector();

  // Used to determine if float or not
  // decimalPointIndex will only be defined if hasDecimalPoint is true
  bool hasDecimalPoint = false;
  size_t decimalPointIndex = 0;

  size_t length = 0;
  int32_t c;
  while ((c = nextValue(stream)) != EOF) {
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
                   stream->lineNumber, stream->charNumber);
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
                 data->data, stream->lineNumber, stream->charNumber);
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
      logError(ErrLevelError,
               "malformed integer literal: `%s`: %s: %" PRIu64 ", %" PRIu64
               "\n",
               data->data, strErrVal(ret.err), stream->lineNumber,
               stream->charNumber);

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
               data->data, strErrVal(initialRet.err), stream->lineNumber,
               stream->charNumber);
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
                 data->data, strErrVal(initialRet.err), stream->lineNumber,
                 stream->charNumber);
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

static ResultToken lexCharLiteral(Parseable *stream) {
  if (nextValue(stream) != '\'') {
    logError(ErrLevelError,
             "malformed character literal: %" PRIu64 ", %" PRIu64 "\n",
             stream->lineNumber, stream->charNumber);
    return (ResultToken){.err = ErrBadargs};
  }

  // We basically read the whole thing into a string.
  Vector *chars = newVector();
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
        // Clean up
        deleteVector(chars);
        logError(ErrLevelError,
                 "malformed character literal: unexpected end of file: %" PRIu64
                 ", %" PRIu64 "\n",
                 stream->lineNumber, stream->charNumber);
        return (ResultToken){.err = ErrEof};
      }
      default: {
        // Clean up
        deleteVector(chars);
        logError(ErrLevelError,
                 "malformed character literal: unexpected escape code: `%c` "
                 "%" PRIu64 ", %" PRIu64 "\n",
                 c, stream->lineNumber, stream->charNumber);
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
             stream->lineNumber, stream->charNumber);
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
             stream->lineNumber, stream->charNumber);
    return (ResultToken){.err = ErrBadargs};
  }
  }
}

static ResultToken lexIdentifier(Parseable *stream) {
  Vector *data = newVector();
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

  Token t;

  if (!strcmp(VEC_GET(data, 0, char), "if")) {
    t = (Token){.type = TokenIf};
  } else if (!strcmp(VEC_GET(data, 0, char), "else")) {
    t = (Token){.type = TokenElse};
  } else if (!strcmp(VEC_GET(data, 0, char), "while")) {
    t = (Token){.type = TokenWhile};
  } else if (!strcmp(VEC_GET(data, 0, char), "with")) {
    t = (Token){.type = TokenWith};
  } else if (!strcmp(VEC_GET(data, 0, char), "for")) {
    t = (Token){.type = TokenFor};
  } else if (!strcmp(VEC_GET(data, 0, char), "break")) {
    t = (Token){.type = TokenBreak};
  } else if (!strcmp(VEC_GET(data, 0, char), "continue")) {
    t = (Token){.type = TokenContinue};
  } else if (!strcmp(VEC_GET(data, 0, char), "return")) {
    t = (Token){.type = TokenReturn};
  } else {
    // It is an identifier

    char *string = malloc(lengthVector(data));
    memcpy(string, getVector(data, 0), lengthVector(data));

    t = (Token){.type = TokenIdentifier, .identifier = string};
  }
  deleteVector(data);
  return (ResultToken){.val = t, .err = ErrOk};
}

static ResultToken resultTokenType(TokenType type) {
  // clang-format off
  return (ResultToken){
    .val = (Token) {
      .type = type
    },
    .err = ErrOk
  };
  // clang-format on
}

ResultToken nextToken(Parseable *stream) {
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
        return resultTokenType(TokenAnd);
      } else {
        return resultTokenType(TokenBitAnd);
      }
    }
    case '|': {
      nextValue(stream);
      int32_t n = peekValue(stream);
      // || or |
      if (n == '|') {
        nextValue(stream);
        return resultTokenType(TokenOr);
      } else if (n == '>') {
        nextValue(stream);
        return resultTokenType(TokenPipe);
      } else {
        return resultTokenType(TokenBitOr);
      }
    }
    case '!': {
      nextValue(stream);
      int32_t n = peekValue(stream);
      // ! or !=
      if (n == '=') {
        nextValue(stream);
        return resultTokenType(TokenNotEqual);
      } else {
        return resultTokenType(TokenNot);
      }
    }
    case '=': {
      nextValue(stream);
      int32_t n = peekValue(stream);
      // = or ==
      if (n == '=') {
        nextValue(stream);
        return resultTokenType(TokenEqual);
      } else {
        return resultTokenType(TokenAssign);
      }
    }
    case '<': {
      nextValue(stream);
      int32_t n = peekValue(stream);
      if (n == '<') {
        nextValue(stream);
        return resultTokenType(TokenShiftLeft);
      } else if (n == '=') {
        nextValue(stream);
        return resultTokenType(TokenCompLessEqual);
      } else {
        return resultTokenType(TokenCompLess);
      }
    }
    case '>': {
      nextValue(stream);
      int32_t n = peekValue(stream);
      if (n == '>') {
        nextValue(stream);
        return resultTokenType(TokenShiftRight);
      } else if (n == '=') {
        nextValue(stream);
        return resultTokenType(TokenCompGreaterEqual);
      } else {
        return resultTokenType(TokenCompGreater);
      }
    }
    case '+': {
      nextValue(stream);
      return resultTokenType(TokenAdd);
    }
    case '-': {
      nextValue(stream);
      return resultTokenType(TokenSub);
    }
    case '*': {
      nextValue(stream);
      return resultTokenType(TokenMul);
    }
    case '/': {
      nextValue(stream);
      return resultTokenType(TokenDiv);
    }
    case '%': {
      nextValue(stream);
      return resultTokenType(TokenMod);
    }
    case '$': {
      nextValue(stream);
      return resultTokenType(TokenRef);
    }
    case '@': {
      nextValue(stream);
      return resultTokenType(TokenDeref);
    }
    case '(': {
      nextValue(stream);
      return resultTokenType(TokenParenLeft);
    }
    case ')': {
      nextValue(stream);
      return resultTokenType(TokenParenRight);
    }
    case '[': {
      nextValue(stream);
      return resultTokenType(TokenBracketLeft);
    }
    case ']': {
      nextValue(stream);
      return resultTokenType(TokenBracketRight);
    }
    case '{': {
      nextValue(stream);
      return resultTokenType(TokenBraceLeft);
    }
    case '}': {
      nextValue(stream);
      return resultTokenType(TokenBraceRight);
    }
    case '.': {
      nextValue(stream);
      return resultTokenType(TokenDot);
    }
    case ',': {
      nextValue(stream);
      return resultTokenType(TokenComma);
    }
    case ':': {
      nextValue(stream);
      return resultTokenType(TokenColon);
    }
    case ';': {
      nextValue(stream);
      return resultTokenType(TokenSemicolon);
    }
    default: {
      // Lex the weird literals, comments, annotation, identifiers, etc
      if (isdigit(c)) {
        return lexNumberLiteral(stream);
      } else if (c == '#') {
        return lexCommentOrAnnotation(stream);
      } else if (c == '\"') {
        return lexStringLiteral(stream);
      } else if (c == '\'') {
        return lexCharLiteral(stream);
      } else if (isalpha(c)) {
        return lexIdentifier(stream);
      }
      if (isblank(c) || c == '\n') {
        nextValue(stream);
      } else {
        // If we simply don't recognize the character, we're going to give
        // an error and move on to the next value
        logError(ErrLevelError,
                 "unrecognized character: `%c`: %" PRIu64 ", %" PRIu64 "\n", c,
                 stream->lineNumber, stream->charNumber);
        nextValue(stream);
      }
    }
    }
  }
  // If we hit the end of the file just give a End of file error
  return (ResultToken){.err = ErrEof};
}

void destroyToken(Token* token) {
  switch (token->type) {
  case TokenIdentifier: {
    free(token->identifier);
    break;
  }
  case TokenComment: {
    free(token->comment);
    break;
  }
  case TokenStringLiteral: {
    free(token->stringLiteral);
    break;
  }
  case TokenAnnotation: {
    free(token->annotationLiteral);
    break;
  }
  default: {
    break;
  }
  }
}

void printToken(Token* token) {
  char* str;
  switch (token->type) {
    case TokenIdentifier: {
      str = "Identifier";
      break;
    }
    case TokenIf: {
      str = "If";
      break;
    }
    case TokenElse: {
      str = "Else";
      break;
    }
    case TokenWhile: {
      str = "While";
      break;
    }
    case TokenFor: {
      str = "For";
      break;
    }
    case TokenWith: {
      str = "With";
      break;
    }
    case TokenMatch: {
      str = "Match";
      break;
    }
    case TokenBreak: {
      str = "Break";
      break;
    }
    case TokenContinue: {
      str = "Continue";
      break;
    }
    case TokenReturn: {
      str = "Return";
      break;
    }
    case TokenStringLiteral: {
      str = "StringLiteral";
      break;
    }
    case TokenCharLiteral: {
      str = "CharacterLiteral";
      break;
    }
    case TokenFloatLiteral: {
      str = "FloatLiteral";
      break;
    }
    case TokenIntLiteral: {
      str = "IntLiteral";
      break;
    }
    case TokenAdd: {
      str = "Add";
      break;
    }
    case TokenSub: {
      str = "Sub";
      break;
    }
    case TokenMul: {
      str = "Mul";
      break;
    }
    case TokenDiv: {
      str = "Div";
      break;
    }
    case TokenMod: {
      str = "Mod";
      break;
    }
    case TokenAnd: {
      str = "And";
      break;
    }
    case TokenOr: {
      str = "Or";
      break;
    }
    case TokenNot: {
      str = "Not";
      break;
    }
    case TokenBitAnd: {
      str = "BitAnd";
      break;
    }
    case TokenBitOr: {
      str = "BitOr";
      break;
    }
    case TokenBitXor: {
      str = "BitXor";
      break;
    }
    case TokenBitNot: {
      str = "BitNot";
      break;
    }
    case TokenShiftLeft: {
      str = "ShiftLeft";
      break;
    }
    case TokenShiftRight: {
      str = "ShiftRight";
      break;
    }
    case TokenEqual: {
      str = "Equal";
      break;
    }
    case TokenNotEqual: {
      str = "NotEqual";
      break;
    }
    case TokenCompLess: {
      str = "CompLess";
      break;
    }
    case TokenCompLessEqual: {
      str = "CompLessEqual";
      break;
    }
    case TokenCompGreater: {
      str = "CompGreater";
      break;
    }
    case TokenCompGreaterEqual: {
      str = "CompGreaterEqual";
      break;
    }
    case TokenRef: {
      str = "Ref";
      break;
    }
    case TokenDeref: {
      str = "Deref";
      break;
    }
    case TokenAssign: {
      str = "Assign";
      break;
    }
    case TokenPipe: {
      str = "Pipe";
      break;
    }
    case TokenParenLeft: {
      str = "ParenLeft";
      break;
    }
    case TokenParenRight: {
      str = "ParenRight";
      break;
    }
    case TokenBracketLeft: {
      str = "BracketLeft";
      break;
    }
    case TokenBracketRight: {
      str = "BracketRight";
      break;
    }
    case TokenBraceLeft: {
      str = "BraceLeft";
      break;
    }
    case TokenBraceRight: {
      str = "BraceRight";
      break;
    }
    case TokenDot: {
      str = "Dot";
      break;
    }
    case TokenComma: {
      str = "Comma";
      break;
    }
    case TokenColon: {
      str = "Colon";
      break;
    }
    case TokenSemicolon: {
      str = "Semicolon";
      break;
    }
    case TokenComment: {
      str = "Comment";
      break;
    }
    case TokenAnnotation: {
      str = "Annotation";
      break;
    }
  }
  puts(str);
}
