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
    if (nextValue != EOF) {
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

BufferedLexer *createBufferedLexer(BufferedLexer *bl, Lexer *l) {
  bl->hasNextToken = false;
  bl->l = l;
  return bl;
}

BufferedLexer *destroyBufferedLexer(BufferedLexer *bl) {
  // No memory to dealloc lmao
  return bl;
}

// If has an ungotten token, return that. Else return next in line, and cache it
void advanceToken(BufferedLexer *bl, Token *t) {
  if (bl->hasNextToken) {
    *t = bl->nextToken;
    bl->hasNextToken = false;
  } else {
    lexNextToken(bl->l, t);
  }
}

// If token exists in line
void setNextToken(BufferedLexer *bl, Token *t) {
  if (!bl->hasNextToken) {
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
      return (ResultU64){0, E_UnrecognizedCharacter};
    }

    // If you put something higher than is requested
    if (digitValue >= radix) {
      return (ResultU64){0, E_IntLiteralDigitExceedsRadix};
    }

    uint64_t oldret = ret;
    ret = ret * radix + digitValue;
    if (oldret > ret) {
      return (ResultU64){0, E_IntLiteralOverflow};
    }
  }
  return (ResultU64){ret, E_Ok};
}

// Call this function right before the first hash
// Returns control at the first noncomment (or nonannotate) area
// Lexes comments, annotations, and docstrings
static void lexComment(Lexer *lexer, Token *token) {
  LnCol start = lexer->position;

  int32_t c = nextValueLexer(lexer);
  if (c != '#') {
    INTERNAL_ERROR("called comment lexer when there wasn't a comment");
    PANIC();
  }

  c = peekValueLexer(lexer);

  // Now we determine the type of comment as well as gather the comment data
  switch (c) {
  case '[': {
    // This is a comment. It will continue until another quote asterix is found.
    // These strings are preserved in the AST. They are nestable
    // also nestable
    // #[ Comment ]#
    Vector data;
    createVector(&data);
    nextValueLexer(lexer);
    size_t stackDepth = 1;
    char lastChar = '\0';
    while ((c = nextValueLexer(lexer)) != EOF) {
      if (c == '#' && lastChar == ']') {
        // If we see a "# to pair off the starting #"
        stackDepth--;
        if (stackDepth == 0) {
          break;
        }
      } else if (c == '[' && lastChar == '#') {
        stackDepth++;
      }
      *VEC_PUSH(&data, char) = (char)c;
      lastChar = (char)c;
    }
    // Pop last bracket
    VEC_POP(&data, NULL, char);
    // Push null byte
    *VEC_PUSH(&data, char) = '\0';

    // Return data
    // clang-format off
    *token = (Token) {
      .type = T_Comment,
      .comment = releaseVector(&data),
      .span = SPAN(start, lexer->position),
      .error = E_Ok
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
      } else {
        break;
      }
    }
    *VEC_PUSH(&data, char) = '\0';

    // Return data
    // clang-format off
    *token = (Token) {
      .type = T_Comment,
      .comment = releaseVector(&data),
      .span = SPAN(start, lexer->position),
      .error = E_Ok
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
  if (c != '\"') {
    INTERNAL_ERROR("called string lexer where there wasn't a string");
    PANIC();
  }

  Vector data;
  createVector(&data);

  while ((c = nextValueLexer(lexer)) != EOF) {
    if (c == '\\') {
      c = nextValueLexer(lexer);
      switch (c) {
      case '\\': {
        *VEC_PUSH(&data, char) = '\\';
        break;
      }
      case '\'': {
        *VEC_PUSH(&data, char) = '\'';
        break;
      }
      case '\"': {
        *VEC_PUSH(&data, char) = '\"';
        break;
      }
      case 'a': {
        *VEC_PUSH(&data, char) = '\a';
        break;
      }
      case 'b': {
        *VEC_PUSH(&data, char) = '\b';
        break;
      }
      case 'f': {
        *VEC_PUSH(&data, char) = '\f';
        break;
      }
      case 'n': {
        *VEC_PUSH(&data, char) = '\n';
        break;
      }
      case 'r': {
        *VEC_PUSH(&data, char) = '\r';
        break;
      }
      case 't': {
        *VEC_PUSH(&data, char) = '\t';
        break;
      }
      case 'v': {
        *VEC_PUSH(&data, char) = '\v';
        break;
      }
      default: {
        *token = (Token){.type = T_None,
                         .span = SPAN(start, lexer->position),
                         .error = E_StringLiteralUnrecognizedEscapeCode};
        destroyVector(&data);
        return;
      }
      }
    } else if (c == '\"') {
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
      .type = T_StringLiteral,
      .stringLiteral = releaseVector(&data),
      .span = SPAN(start, lexer->position),
      .error = E_Ok
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
    if (isalnum(c) || c == '.') {
      // If there's a decimal point we note the location
      // If this is the second time we've seen it, then it's probably
      if (c == '.') {
        if (hasDecimalPoint) {
          break;
        } else {
          hasDecimalPoint = true;
          // Since we haven't appended the new dot yet, it's the previous
          // value
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
    // More than 2 characters and first character is 0 and second character
    // is not digit
    if (length > 2 && string[0] == '0' && !isdigit(string[1])) {
      // Set radix to what it has to be
      // Switch on the second character aka x, or b or whatever comes after
      // 0
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
          .type = T_None,
          .span = SPAN(start, lexer->position),
          .error = E_IntLiteralUnrecognizedRadixCode

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
    if (ret.err != E_Ok) {
      free(string);
      // clang-format off
      *token = (Token) {
          .type = T_None,
          .span = SPAN(start, lexer->position),
          .error = E_IntLiteralUnrecognizedRadixCode

      };
      // clang-format on
      return;
    } else {
      // Return data
      free(string);
      // clang-format off
      *token  = (Token) {
        .type = T_IntLiteral,
        .span = SPAN(start, lexer->position),
        .intLiteral = ret.val,
        .error = E_Ok

      };
      // clang-format on
      return;
    }
  } else {
    // If it has a decimal point, it must be a float
    // We have already guaranteed that the string only has one decimal
    // point, at decimalPointIndex Floats are always in decimal notation

    // We parse the float as an integer above decimal point, and one below

    // Represents the portion of the float literal prior to the decimal
    // point
    char *initialPortion = string;
    size_t initialPortionLen = decimalPointIndex;

    // Represents the portion of the float literal after the decimal point
    char *finalPortion = string + decimalPointIndex + 1;
    size_t finalPortionLen = length - decimalPointIndex - 1;

    // the thing we'll return
    double result = 0;

    // Parse the part ahead of the decimal point
    ResultU64 initialRet = parseInteger(initialPortion, initialPortionLen, 10);
    if (initialRet.err == E_Ok) {
      result += initialRet.val;
    } else {
      free(string);
      // clang-format off
      *token = (Token) {
          .type = T_None,
          .span = SPAN(start, lexer->position),
          .error = initialRet.err

      };
      // clang-format on
      return;
    }
    // If there's a bit after the inital part, then we must add it
    if (finalPortionLen > 0) {
      ResultU64 finalRet = parseInteger(finalPortion, finalPortionLen, 10);
      if (finalRet.err == E_Ok) {
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
            .type = T_None,
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
      .type = T_FloatLiteral,
      .floatLiteral = result,
      .span = SPAN(start, lexer->position),
      .error = E_Ok
    };
    // clang-format on
    return;
  }
}

static void lexCharLiteral(Lexer *lexer, Token *token) {
  LnCol start = lexer->position;
  // Skip first quote
  int32_t c = nextValueLexer(lexer);
  if (c != '\'') {
    INTERNAL_ERROR("called char lexer where there wasn't a char");
    PANIC();
  }

  Vector data;
  createVector(&data);

  while ((c = nextValueLexer(lexer)) != EOF) {
    if (c == '\\') {
      c = nextValueLexer(lexer);
      switch (c) {
      case '\\': {
        *VEC_PUSH(&data, char) = '\\';
        break;
      }
      case '\'': {
        *VEC_PUSH(&data, char) = '\'';
        break;
      }
      case '\"': {
        *VEC_PUSH(&data, char) = '\"';
        break;
      }
      case 'a': {
        *VEC_PUSH(&data, char) = '\a';
        break;
      }
      case 'b': {
        *VEC_PUSH(&data, char) = '\b';
        break;
      }
      case 'f': {
        *VEC_PUSH(&data, char) = '\f';
        break;
      }
      case 'n': {
        *VEC_PUSH(&data, char) = '\n';
        break;
      }
      case 'r': {
        *VEC_PUSH(&data, char) = '\r';
        break;
      }
      case 't': {
        *VEC_PUSH(&data, char) = '\t';
        break;
      }
      case 'v': {
        *VEC_PUSH(&data, char) = '\v';
        break;
      }
      default: {
        *token = (Token){.type = T_None,
                         .span = SPAN(start, lexer->position),
                         .error = E_CharLiteralUnrecognizedEscapeCode};
        destroyVector(&data);
        return;
      }
      }
    } else if (c == '\'') {
      break;
    } else {
      *VEC_PUSH(&data, char) = (char)c;
    }
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
      .type = T_None,
      .span = SPAN(start, lexer->position),
      .error = E_CharLiteralEmpty
    };
    // clang-format on
    return;
  }
  case 1: {
    // We return the first character in the vector

    // clang-format off
    *token = (Token) {
      .type = T_CharLiteral,
      .charLiteral = string[0],
      .span = SPAN(start, lexer->position),
      .error = E_Ok
    };
    // clang-format on

    // Clean up
    free(string);
    return;
  }
  default: {
    free(string);
    // clang-format off
    *token = (Token) {
      .type = T_None,
      .span = SPAN(start, lexer->position),
      .error = E_CharLiteralTooLong
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
    } else if (c == '!') {
      nextValueLexer(lexer);
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

  if (macro) {
    // It is an identifier, and we need to keep the string
    token->type = T_Macro;
    token->macro = string;
    token->error = E_Ok;
    return;
  }

  if (!strcmp(string, "if")) {
    token->type = T_If;
  } else if (!strcmp(string, "else")) {
    token->type = T_Else;
  } else if (!strcmp(string, "while")) {
    token->type = T_While;
  } else if (!strcmp(string, "with")) {
    token->type = T_With;
  } else if (!strcmp(string, "for")) {
    token->type = T_For;
  } else if (!strcmp(string, "break")) {
    token->type = T_Break;
  } else if (!strcmp(string, "continue")) {
    token->type = T_Continue;
  } else if (!strcmp(string, "return")) {
    token->type = T_Return;
  } else if (!strcmp(string, "fn")) {
    token->type = T_Function;
  } else if (!strcmp(string, "let")) {
    token->type = T_Let;
  } else if (!strcmp(string, "struct")) {
    token->type = T_Struct;
  } else if (!strcmp(string, "alias")) {
    token->type = T_Alias;
  } else if (!strcmp(string, "sizeof")) {
    token->type = T_Sizeof;
  } else if (!strcmp(string, "typeof")) {
    token->type = T_Typeof;
  } else if (!strcmp(string, "alignof")) {
    token->type = T_Alignof;
  } else {
    // It is an identifier, and we need to keep the string
    token->type = T_Identifier;
    token->identifier = string;
    token->error = E_Ok;
    return;
  }

  // If it wasn't an identifier
  token->error = E_Ok;
  free(string);
  return;
}

/* clang-format off */

#define RESULT_TOKEN(tokenType, errorType)                                     \
  *token = (Token){                                                            \
      .type = tokenType,                                                       \
      .span = SPAN(start, lexer->position),                                    \
      .error = errorType                                                       \
  };                                                                           \

#define RETURN_RESULT_TOKEN(tokenType)                                         \
  RESULT_TOKEN(tokenType, E_Ok)                                                \
  return;

#define NEXT_AND_RETURN_RESULT_TOKEN(tokenType)                                \
  nextValueLexer(lexer);                                                       \
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
      lexComment(lexer, token);
      return;
    }
    case '&': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      // && or &
      if (n == '&') {
        NEXT_AND_RETURN_RESULT_TOKEN(T_And)
      } else {
        RETURN_RESULT_TOKEN(T_BitAnd)
      }
    }
    case '|': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      // || or |
      if (n == '|') {
        NEXT_AND_RETURN_RESULT_TOKEN(T_Or)
      } else {
        RETURN_RESULT_TOKEN(T_BitOr)
      }
    }
    case '!': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      // ! or !=
      if (n == '=') {
        NEXT_AND_RETURN_RESULT_TOKEN(T_NotEqual)
      } else {
        RETURN_RESULT_TOKEN(T_Not)
      }
    }
    case '=': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      // = or ==
      if (n == '=') {
        NEXT_AND_RETURN_RESULT_TOKEN(T_Equal)
      } else {
        RETURN_RESULT_TOKEN(T_Assign)
      }
    }
    case '<': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      if (n == '<') {
        NEXT_AND_RETURN_RESULT_TOKEN(T_ShiftLeft)
      } else if (n == '=') {
        NEXT_AND_RETURN_RESULT_TOKEN(T_CompLessEqual)
      } else {
        RETURN_RESULT_TOKEN(T_CompLess)
      }
    }
    case '>': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      if (n == '>') {
        NEXT_AND_RETURN_RESULT_TOKEN(T_ShiftRight)
      } else if (n == '=') {
        NEXT_AND_RETURN_RESULT_TOKEN(T_CompGreaterEqual)
      } else {
        RETURN_RESULT_TOKEN(T_CompGreater)
      }
    }
    case '+': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_Add)
    }
    case '-': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      if (n == '>') {
        NEXT_AND_RETURN_RESULT_TOKEN(T_Pipe)
      } else {
        RETURN_RESULT_TOKEN(T_Sub)
      }
    }
    case '[': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      if (n == '[') {
        NEXT_AND_RETURN_RESULT_TOKEN(T_AttrLeft)
      } else {
        RETURN_RESULT_TOKEN(T_BracketLeft)
      }
    }
    case ']': {
      nextValueLexer(lexer);
      int32_t n = peekValueLexer(lexer);
      if (n == ']') {
        NEXT_AND_RETURN_RESULT_TOKEN(T_AttrRight)
      } else {
        RETURN_RESULT_TOKEN(T_BracketRight)
      }
    }
    case '*': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_Mul)
    }
    case '/': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_Div)
    }
    case '%': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_Mod)
    }
    case '$': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_Ref)
    }
    case '@': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_Deref)
    }
    case '(': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_ParenLeft)
    }
    case ')': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_ParenRight)
    }
    case '{': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_BraceLeft)
    }
    case '}': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_BraceRight)
    }
    case '.': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_Dot)
    }
    case ',': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_Comma)
    }
    case ':': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_Colon)
    }
    case ';': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_Semicolon)
    }
    case '_': {
      NEXT_AND_RETURN_RESULT_TOKEN(T_Underscore)
    }
    case EOF: {
      RESULT_TOKEN(T_None, E_EOF)
      return;
    }
    default: {
      RESULT_TOKEN(T_None, E_UnrecognizedCharacter)
      nextValueLexer(lexer);
      return;
    }
    }
  }
}
