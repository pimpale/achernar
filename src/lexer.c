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
  lexer->position = LNCOL(0, 0);

  // Files
  lexer->file = file;
  lexer->backing = LexerBackingFile;

  return lexer;
}

Lexer *createLexerMemory(Lexer *lexer, char *ptr, size_t len) {
  lexer->position = LNCOL(0, 0);

  // Copy memory
  lexer->backing = LexerBackingMemory;
  lexer->memory.len = len;
  lexer->memory.loc = 0;
  lexer->memory.ptr = malloc(len);
  memcpy(lexer->memory.ptr, ptr, len);

  return lexer;
}

Lexer *destroyLexer(Lexer *lexer) {
  switch (lexer->backing) {
  case LexerBackingMemory: {
    free(lexer->memory.ptr);
    break;
  }
  case LexerBackingFile: {
    break;
  }
  }
  return lexer;
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
        lexer->position.ln += 1;
        lexer->position.col = 0;
      } else {
        lexer->position.col += 1;
      }
    }
    break;
  }
  case LexerBackingFile: {
    nextValue = getc(lexer->file);
    if(nextValue != EOF) {
      if (nextValue == '\n') {
        lexer->position.ln += 1;
        lexer->position.col = 0;
      } else {
        lexer->position.col += 1;
      }
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

// Buffered Lexer

BufferedLexer* createBufferedLexer(BufferedLexer* bl, Lexer* l) {
  bl->hasNextToken = false;
  bl->l = l;
  return bl;
}

BufferedLexer * destroyBufferedLexer(BufferedLexer* bl) {
  // No memory to dealloc lmao
  return bl;
}

// If has an ungotten token, return that. Else return next in line, and cache it
void advanceToken(BufferedLexer* bl, Token* t) {
  if(bl->hasNextToken) {
    *t = bl->nextToken;
    bl->hasNextToken = false;
  } else {
    lexNextToken(bl->l, t);
  }
}

// If token exists in line
void setNextToken(BufferedLexer* bl, Token* t) {
  if(!bl->hasNextToken) {
    bl->hasNextToken = true;
    bl->nextToken = *t;
  } else {
    INTERNAL_ERROR("already set next token");
    PANIC();
  }
}

// Stuff to lex with
typedef struct {
  uint64_t val;
  DiagnosticType err;
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
      return (ResultU64){0, ErrorUnrecognizedCharacter};
    }

    // If you put something higher than is requested
    if (digitValue >= radix) {
      return (ResultU64){0, ErrorIntLiteralDigitExceedsRadix};
    }

    uint64_t oldret = ret;
    ret = ret * radix + digitValue;
    if (oldret > ret) {
      return (ResultU64){0, ErrorIntLiteralOverflow};
    }
  }
  return (ResultU64){ret, ErrorOk};
}

// Call this function right before the first hash
// Returns control at the first noncomment (or nonannotate) area
// Lexes comments, annotations, and docstrings
static void lexCommentOrAnnotation(Lexer *lexer, Token *token) {
  LnCol start = lexer->position;

  int32_t c = nextValueLexer(lexer);
  if(c != '#') {
    INTERNAL_ERROR("called comment lexer when there wasn't a comment");
    PANIC();
  }

  c = peekValueLexer(lexer);

  // Now we determine the type of comment as well as gather the comment data
  switch (c) {
  case '*': {
    // This is a comment. It will continue until another quote asterix is found.
    // These strings are preserved in the AST. They are nestable
    // also nestable
    // #* Comment *#
    Vector data;
    createVector(&data);
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
      *VEC_PUSH(&data, char) = (char)c;
      lastChar = (char)c;
    }
    // Push null byte
    *VEC_PUSH(&data, char) = '\0';

    // Return data
    // clang-format off
    *token = (Token) {
      .type = TokenComment,
      .comment = releaseVector(&data),
      .span = SPAN(start, lexer->position),
      .error = ErrorOk
    };
    return;
    // clang-format on
  }
  case '[': {
    // This is an attribute. It will continue till a closing squarebracket is found
    // is found. They are not nestable
    // #[annotation]
    Vector data;
    createVector(&data);

    nextValueLexer(lexer);
    while ((c = peekValueLexer(lexer)) != EOF) {
      if (c != ']') {
        *VEC_PUSH(&data, char) = (char)c;
        nextValueLexer(lexer);
      } else {
        break;
      }
    }

    // Push null byte
    *VEC_PUSH(&data, char) = '\0';

    // Return data
    // clang-format off
    *token = (Token) {
      .type = TokenAnnotation,
      .annotationLiteral = releaseVector(&data),
      .span = SPAN(start, lexer->position),
      .error = ErrorOk
    };
    return;
    // clang-format on
  }
  default: {
    // If we don't recognize any of these characters, it's just a normal single
    // line comment. These are not nestable, and continue till the end of line.
    // # comment
    Vector data;
    createVector(&data);
    while ((c = nextValueLexer(lexer)) != EOF) {
      if (c != '\n') {
        *VEC_PUSH(&data, char) = (char)c;
        nextValueLexer(lexer);
      } else {
        break;
      }
    }
    *VEC_PUSH(&data, char) = '\0';

    // Return data
    // clang-format off
    *token = (Token) {
      .type = TokenComment,
      .comment = releaseVector(&data),
      .span = SPAN(start, lexer->position),
      .error = ErrorOk
    };
    return;
    // clang-format on
  }
  }
}

// Call this function right before the first quote of the string literal
// Returns control after the ending quote of this lexer
// This function returns a Token containing the string or an error
static void lexStringLiteral(Lexer *lexer, Token *token) {
  LnCol start = lexer->position;
  // Skip first quote
  int32_t c = nextValueLexer(lexer);
  if(c != '\"') {
    INTERNAL_ERROR("called string lexer where there wasn't a string");
    PANIC();
  }

  Vector data;
  createVector(&data);

  while ((c = nextValueLexer(lexer)) != EOF) {
    if (c == '\"') {
      break;
    } else {
      *VEC_PUSH(&data, char) = (char)c;
    }
  }
  // Push null byte
  *VEC_PUSH(&data, char) = '\0';

  // Return data
  // clang-format off
  *token = (Token) {
      .type = TokenStringLiteral,
      .stringLiteral = releaseVector(&data),
      .span = SPAN(start, lexer->position),
      .error = ErrorOk
    };
  // clang-format on
  return;
}

// Call this function right before the first digit of the number literal
// Returns control right after the number is finished
// This function returns a Token or the error
static void lexNumberLiteral(Lexer *lexer, Token *token) {
  LnCol start = lexer->position;

  Vector data;
  createVector(&data);

  // Used to determine if float or not
  // decimalPointIndex will only be defined if hasDecimalPoint is true
  bool hasDecimalPoint = false;
  size_t decimalPointIndex = 0;

  int32_t c;
  while ((c = nextValueLexer(lexer)) != EOF) {
    if (isdigit(c)                // Normal digit
        || c == '.'               // Decimal point
        || (c >= 'a' && c <= 'f') // Hexadecimal Character
        || c == 'b' || c == 'd' || c == 'o' ||
        c == 'x' // Character Interpretation
    ) {
      // If there's a decimal point we note the location
      // If this is the second time we've seen it, then it's probably
      if (c == '.') {
        if (hasDecimalPoint) {
          break;
        } else {
          hasDecimalPoint = true;
          // Since we haven't appended the new dot yet, it's the previous value
          decimalPointIndex = VEC_LEN(&data, char);
        }
      }
      *VEC_PUSH(&data, char) = (char)c;
    } else if (c == '_') {
      // Do nothing
    } else {
      break;
    }
  }

  // The definition of string length doesn't include the \0
  size_t length = VEC_LEN(&data, char);
  *VEC_PUSH(&data, char) = '\0';
  char *string = releaseVector(&data);

  // If it's an integer
  if (!hasDecimalPoint) {
    uint64_t radix;
    // this is the null terminated string storing the text of the number
    char *intStr;
    size_t intStrLen;

    // Special Radix
    // More than 2 characters and first character is 0 and second character is
    // not digit
    if (length > 2 && string[0] == '0' && !isdigit(string[1])) {
      // Set radix to what it has to be
      // Switch on the second character aka x, or b or whatever comes after 0
      switch (string[1]) {
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
        free(string);
        // clang-format off
        *token = (Token) {
          .type = TokenNone,
          .span = SPAN(start, lexer->position),
          .error = ErrorIntLiteralUnrecognizedRadixCode

        };
        // clang-format on
        return;
      }
      }
      // Basically take the string after 0x or whatever
      intStr = string + 2;
      intStrLen = length - 2;
    } else {
      radix = 10;
      intStr = string;
      intStrLen = length;
    }

    // Now we get on to parsing the integer
    ResultU64 ret = parseInteger(intStr, intStrLen, radix);
    if (ret.err != ErrorOk) {
      free(string);
      // clang-format off
      *token = (Token) {
          .type = TokenNone,
          .span = SPAN(start, lexer->position),
          .error = ErrorIntLiteralUnrecognizedRadixCode

      };
      // clang-format on
      return;
    } else {
      // Return data
      free(string);
      // clang-format off
      *token  = (Token) {
        .type = TokenIntLiteral,
        .span = SPAN(start, lexer->position),
        .intLiteral = ret.val,
        .error = ErrorOk

      };
      // clang-format on
      return;
    }
  } else {
    // If it has a decimal point, it must be a float
    // We have already guaranteed that the string only has one decimal point,
    // at decimalPointIndex Floats are always in decimal notation

    // We parse the float as an integer above decimal point, and one below

    // Represents the portion of the float literal prior to the decimal point
    char *initialPortion = string;
    size_t initialPortionLen = decimalPointIndex;

    // Represents the portion of the float literal after the decimal point
    char *finalPortion = string + decimalPointIndex + 1;
    size_t finalPortionLen = length - decimalPointIndex - 1;

    // the thing we'll return
    double result = 0;

    // Parse the part ahead of the decimal point
    ResultU64 initialRet = parseInteger(initialPortion, initialPortionLen, 10);
    if (initialRet.err == ErrorOk) {
      result += initialRet.val;
    } else {
      free(string);
      // clang-format off
      *token = (Token) {
          .type = TokenNone,
          .span = SPAN(start, lexer->position),
          .error = initialRet.err

      };
      // clang-format on
      return;
    }
    // If there's a bit after the inital part, then we must add it
    if (finalPortionLen > 0) {
      ResultU64 finalRet = parseInteger(finalPortion, finalPortionLen, 10);
      if (finalRet.err == ErrorOk) {
        // don't want to include math.h, so we'll repeatedly divide by 10
        // this is probably dumb
        double decimalResult = finalRet.val;
        for (size_t i = 0; i < finalPortionLen; i++) {
          decimalResult /= 10;
        }
        result += decimalResult;
      } else {
        free(string);
        // clang-format off
        *token = (Token) {
            .type = TokenNone,
            .span = SPAN(start, lexer->position),
            .error = finalRet.err

        };
        // clang-format on
        return;
      }
    }

    // avoid leaking memory
    free(string);

    // Return data
    // clang-format off
    *token = (Token) {
      .type = TokenFloatLiteral,
      .floatLiteral = result,
      .span = SPAN(start, lexer->position),
      .error = ErrorOk
    };
    // clang-format on
    return;
  }
}

static void lexCharLiteral(Lexer *lexer, Token *token) {
  LnCol start = lexer->position;

  // Skip leading '
  if(nextValueLexer(lexer) != '\'') {
    INTERNAL_ERROR("called char lit lexer where there was no char literal");
    PANIC();
  }


  // We basically read the whole thing into a string.
  Vector data;
  createVector(&data);

  // Current character
  int32_t c;
  // last character
  int32_t pC = '\0';
  // last to last character
  int32_t ppC = '\0';
  while ((c = nextValueLexer(lexer)) != EOF) {
    // If we encounter a closing '
    if (c == '\'') {
      // If not (previous value is a backslash and the previous previous char is
      // not a backslash) Then we can break Ex: '\' -> keep reading Ex: '\\' ->
      // break Ex: '\a' -> break
      if (!(pC == '\\' && ppC != '\\')) {
        break;
      }
    }
    *VEC_PUSH(&data, char) = (char)c;
    ppC = pC;
    pC = c;
  }

  // get size and length + terminate string
  size_t length = VEC_LEN(&data, char);
  *VEC_PUSH(&data, char) = '\0';
  char *string = releaseVector(&data);

  switch (length) {
  case 0: {
    // Clean up
    free(string);

    // clang-format off
    *token = (Token) {
      .type = TokenNone,
      .span = SPAN(start, lexer->position),
      .error = ErrorCharLiteralEmpty
    };
    // clang-format on
    return;
  }
  case 1: {
    // We return the first character in the vector

    // clang-format off
    *token = (Token) {
      .type = TokenCharLiteral,
      .charLiteral = string[0],
      .span = SPAN(start, lexer->position),
      .error = ErrorOk
    };
    // clang-format on

    // Clean up
    free(string);
    return;
  }
  case 2: {
    if (string[0] != '\\') {
      free(string);
      // clang-format off
      *token = (Token) {
        .type = TokenNone,
        .span = SPAN(start, lexer->position),
        .error = ErrorCharLiteralTooLong
      };
      // clang-format on
      return;
    }

    char code;
    switch (string[1]) {
    case 'n': {
      code = '\n';
      break;
    }
    case 't': {
      code = '\t';
      break;
    }
    case '\\': {
      code = '\\';
      break;
    }
    case '\'': {
      code = '\'';
      break;
    }
    case '\"': {
      code = '\"';
      break;
    }
    case '\0': {
      code = '\0';
      break;
    }
    default: {
      free(string);
      // clang-format off
      *token = (Token) {
        .type = TokenNone,
        .span = SPAN(start, lexer->position),
        .error = ErrorCharLiteralUnrecognizedEscapeCode
      };
      // clang-format on
      return;
    }
    }

    // Clean up
    free(string);

    // clang-format off
    *token = (Token) {
      .type = TokenCharLiteral,
      .charLiteral = code,
      .span = SPAN(start, lexer->position),
      .error = ErrorOk
    };
    return;
    // clang-format on
  }
  default: {
    free(string);
    // clang-format off
    *token = (Token) {
      .type = TokenNone,
      .span = SPAN(start, lexer->position),
      .error = ErrorCharLiteralTooLong
    };
    // clang-format on
    return;
  }
  }
}

// Parses an identifer or macro
static void lexIdentifierOrMacro(Lexer *lexer, Token *token) {


  LnCol start = lexer->position;

  Vector data;
  createVector(&data);

  bool macro = false;

  int32_t c;
  while ((c = peekValueLexer(lexer)) != EOF) {
    if (isalnum(c)) {
      *VEC_PUSH(&data, char) = (char)c;
      nextValueLexer(lexer);
    } else if(c == '!') {
      macro = true;
      break;
    } else {
      break;
    }
  }

  token->span = SPAN(start, lexer->position);

  // Note that string length does not incude the trailing null byte
  size_t len = VEC_LEN(&data, char);
  // Push null byte
  *VEC_PUSH(&data, char) = '\0';
  char *string = releaseVector(&data);

  if(macro) {
    // It is an identifier, and we need to keep the string
    token->type = TokenMacro;
    token->macro = string;
    token->error = ErrorOk;
    return;
  }

  if (!strcmp(string, "if")) {
    token->type = TokenIf;
  } else if (!strcmp(string, "else")) {
    token->type = TokenElse;
  } else if (!strcmp(string, "while")) {
    token->type = TokenWhile;
  } else if (!strcmp(string, "with")) {
    token->type = TokenWith;
  } else if (!strcmp(string, "for")) {
    token->type = TokenFor;
  } else if (!strcmp(string, "break")) {
    token->type = TokenBreak;
  } else if (!strcmp(string, "continue")) {
    token->type = TokenContinue;
  } else if (!strcmp(string, "return")) {
    token->type = TokenReturn;
  } else if (!strcmp(string, "fn")) {
    token->type = TokenFunction;
  } else if (!strcmp(string, "let")) {
    token->type = TokenLet;
  } else if (!strcmp(string, "mut")) {
    token->type = TokenMut;
  } else if (!strcmp(string, "struct")) {
    token->type = TokenStruct;
  } else if (!strcmp(string, "alias")) {
    token->type = TokenAlias;
  } else {
    // It is an identifier, and we need to keep the string
    token->type = TokenIdentifier;
    token->identifier = string;
    token->error = ErrorOk;
    return;
  }

  // If it wasn't an identifier
  token->error = ErrorOk;
  free(string);
  return;
}

/* clang-format off */

#define RESULT_TOKEN(tokenType, errorType) \
  *token = (Token){                                                            \
      .type = tokenType,                                                       \
      .span = SPAN(start, lexer->position),                                    \
      .error = errorType                                                       \
  };                                                                           \

#define RETURN_RESULT_TOKEN(tokenType)                                         \
  RESULT_TOKEN(tokenType, errorType)                                           \
  return;

#define NEXT_AND_RETURN_RESULT_TOKEN(tokenType) \
  nextValueLexer(lexer); \
  RETURN_RESULT_TOKEN(tokenType)
/* clang-format on */

void lexNextToken(Lexer *lexer, Token *token) {
  int32_t c;

  // Set c to first nonblank character
  while ((c = peekValueLexer(lexer)) != EOF) {
    if (isblank(c) || c == '\n') {
      nextValueLexer(lexer);
    } else {
      break;
    }
  }

  LnCol start = lexer->position;

  if (isalpha(c)) {
    lexIdentifierOrMacro(lexer, token);
    return;
  } else if (isdigit(c)) {
    lexNumberLiteral(lexer, token);
    return;
  } else {
    switch (c) {
    case '\'': {
      lexCharLiteral(lexer, token);
      return;
    }
    case '\"': {
      lexStringLiteral(lexer, token);
      return;
    }
    case '#': {
      lexCommentOrAnnotation(lexer, token);
      return;
    }
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
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      if (n == '>') {
        NEXT_AND_RETURN_RESULT_TOKEN(TokenPipe)
      } else {
        RETURN_RESULT_TOKEN(TokenSub)
      }
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
    case EOF: {
      RESULT_TOKEN(TokenNone, ErrorEOF);
      return;
    }
    default: {
      RESULT_TOKEN(TokenNone, ErrorUnrecognizedCharacter);
      return;
    }
    }
  }
}
