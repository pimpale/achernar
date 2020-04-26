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
  lexer->backing = LBK_LexerBackingFile;

  return lexer;
}

Lexer *createLexerMemory(Lexer *lexer, char *ptr, size_t len) {
  lexer->position = LNCOL(0, 0);

  // Copy memory
  lexer->backing = LBK_LexerBackingMemory;
  lexer->memory.len = len;
  lexer->memory.loc = 0;
  lexer->memory.ptr = malloc(len);
  memcpy(lexer->memory.ptr, ptr, len);

  return lexer;
}

Lexer *destroyLexer(Lexer *lexer) {
  switch (lexer->backing) {
  case LBK_LexerBackingMemory: {
    free(lexer->memory.ptr);
    break;
  }
  case LBK_LexerBackingFile: {
    break;
  }
  }
  return lexer;
}

static int32_t nextValueLexer(Lexer *lexer) {
  int32_t nextValue;
  switch (lexer->backing) {
  case LBK_LexerBackingMemory: {
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
  case LBK_LexerBackingFile: {
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
  case LBK_LexerBackingMemory: {
    // If it's within the bounds, return the value ahead of us
    if (lexer->memory.loc + 2 < lexer->memory.len) {
      return (lexer->memory.ptr[lexer->memory.loc + 1]);
    } else {
      return EOF;
    }
  }
  case LBK_LexerBackingFile: {
    int32_t val = getc(lexer->file);
    ungetc(val, lexer->file);
    return val;
  }
  }
}

// Buffered Lexer

BufferedLexer *createBufferedLexer(BufferedLexer *blp, Lexer *lp,
                                   Arena *arena) {
  blp->has_next = false;
  blp->lexer = lp;
  blp->arena = arena;
  return blp;
}

BufferedLexer *destroyBufferedLexer(BufferedLexer *bl) { return bl; }

// If has an ungotten token, return that. Else return next in line, and cache it
void advanceToken(BufferedLexer *blp, Token *t) {
  if (blp->has_next) {
    *t = blp->next;
    blp->has_next = false;
  } else {
    lexNextToken(blp->lexer, t, blp->arena);
  }
}

// If token exists in line
void setNextToken(BufferedLexer *bl, Token *t) {
  if (!bl->has_next) {
    bl->has_next = true;
    bl->next = *t;
  } else {
    INTERNAL_ERROR("already set next token");
    PANIC();
  }
}

// Parses integer with radix
static DiagnosticKind parseInteger(uint64_t *value, char *str, size_t len,
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
      return DK_UnrecognizedCharacter;
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

// Call this function right before the first hash
// Returns control at the first noncomment (or nonannotate) area
// Lexes comments, annotations, and docstrings
static void lexComment(Lexer *lexer, Token *token, Arena *arena) {
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
    // skip hash
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
    *token = (Token){.kind = TK_Comment,
                     .comment = manageMemArena(arena, releaseVector(&data)),
                     .span = SPAN(start, lexer->position),
                     .error = DK_Ok};
    return;
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

    char *string = manageMemArena(arena, releaseVector(&data));

    // Return data
    *token = (Token){.kind = TK_Comment,
                     .comment = string,
                     .span = SPAN(start, lexer->position),
                     .error = DK_Ok};
    free(string);
    return;
  }
  }
}

// Call this function right before the first quote of the string literal
// Returns control after the ending quote of this lexer
// This function returns a Token containing the string or an error
static void lexStringLiteral(Lexer *lexer, Token *token, Arena *arena) {
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
        *token = (Token){.kind = TK_None,
                         .span = SPAN(start, lexer->position),
                         .error = DK_StringLiteralUnrecognizedEscapeCode};

        // keep going till we hit an end double quote
        while ((c = nextValueLexer(lexer)) != EOF) {
          if (c == '\"') {
            break;
          }
        }
        // once we've hit the end, we release the data
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

  char *string = manageMemArena(arena, releaseVector(&data));

  // Return data
  // clang-format off
  *token = (Token) {
    .kind = TK_StringLiteral,
      .string_literal = string,
      .span = SPAN(start, lexer->position),
      .error = DK_Ok
  };
  // clang-format on
  return;
}

// Call this function right before the first digit of the number literal
// Returns control right after the number is finished
// This function returns a Token or the error
static void lexNumberLiteral(Lexer *lexer, Token *token, Arena *arena) {
  UNUSED(arena);
  LnCol start = lexer->position;

  Vector data;
  createVector(&data);

  // Used to determine if float or not
  // decimalPointIndex will only be defined if hasDecimalPoint is true
  bool hasDecimalPoint = false;
  size_t decimalPointIndex = 0;

  int32_t c;
  while ((c = peekValueLexer(lexer)) != EOF) {
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
      // consume digit
      nextValueLexer(lexer);
    } else if (c == '_') {
      // silently consume underscore
      nextValueLexer(lexer);
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
        // clang-format off
                   *token = (Token) {
                     .kind = TK_None,
                       .span = SPAN(start, lexer->position),
                       .error = DK_IntLiteralUnrecognizedRadixCode

                   };
        // clang-format on
        goto CLEANUP;
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
    uint64_t integer_value;
    DiagnosticKind ret = parseInteger(&integer_value, intStr, intStrLen, radix);
    if (ret != DK_Ok) {
      // clang-format off
      *token = (Token) {
        .kind = TK_None,
          .span = SPAN(start, lexer->position),
          .error = ret
      };
      // clang-format on
      goto CLEANUP;
    } else {
      // Return data
      // clang-format off
      *token  = (Token) {
        .kind = TK_IntLiteral,
          .span = SPAN(start, lexer->position),
          .int_literal = integer_value,
          .error = DK_Ok
      };
      // clang-format on
      goto CLEANUP;
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
    uint64_t initial_integer = 0;
    DiagnosticKind initial_err =
        parseInteger(&initial_integer, initialPortion, initialPortionLen, 10);
    if (initial_err == DK_Ok) {
      result += initial_integer;
    } else {
      // clang-format off
      *token = (Token) {
        .kind = TK_None,
          .span = SPAN(start, lexer->position),
          .error = initial_err
      };
      // clang-format on
      goto CLEANUP;
    }
    // If there's a bit after the inital part, then we must add it
    if (finalPortionLen > 0) {
      uint64_t final_integer = 0;
      DiagnosticKind final_err =
          parseInteger(&final_integer, finalPortion, finalPortionLen, 10);
      if (final_err == DK_Ok) {
        // don't want to include math.h, so we'll repeatedly divide by 10
        // this is probably dumb
        double decimalResult = final_integer;
        for (size_t i = 0; i < finalPortionLen; i++) {
          decimalResult /= 10;
        }
        result += decimalResult;
      } else {
        // clang-format off
        *token = (Token) {
          .kind = TK_None,
            .span = SPAN(start, lexer->position),
            .error = final_err
        };
        // clang-format on
        goto CLEANUP;
      }
    }

    // Return data
    // clang-format off
    *token = (Token) {
      .kind = TK_FloatLiteral,
        .float_literal = result,
        .span = SPAN(start, lexer->position),
        .error = DK_Ok
    };
    // clang-format on
    goto CLEANUP;
  }

CLEANUP:
  free(string);
  return;
}

static void lexCharLiteral(Lexer *lexer, Token *token, Arena *arena) {
  UNUSED(arena);
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
        *token = (Token){.kind = TK_None,
                         .span = SPAN(start, lexer->position),
                         .error = DK_CharLiteralUnrecognizedEscapeCode};

        // keep going till we hit an end single quote
        while ((c = nextValueLexer(lexer)) != EOF) {
          if (c == '\'') {
            break;
          }
        }
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
    // clang-format off
              *token = (Token) {
                .kind = TK_None,
                  .span = SPAN(start, lexer->position),
                  .error = DK_CharLiteralEmpty
              };
    // clang-format on
    goto CLEANUP;
  }
  case 1: {
    // We return the first character in the vector

    // clang-format off
              *token = (Token) {
                .kind = TK_CharLiteral,
                  .char_literal = string[0],
                  .span = SPAN(start, lexer->position),
                  .error = DK_Ok
              };
    // clang-format on
    goto CLEANUP;
  }
  default: {
    // clang-format off
               *token = (Token) {
                 .kind = TK_None,
                   .span = SPAN(start, lexer->position),
                   .error = DK_CharLiteralTooLong
               };
    // clang-format on
    goto CLEANUP;
  }
  }
CLEANUP:
  free(string);
  return;
}

// Parses an identifer or macro
static void lexIdentifierOrMacro(Lexer *lexer, Token *token, Arena *arena) {

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
  // Push null byte
  *VEC_PUSH(&data, char) = '\0';
  char *string = manageMemArena(arena, releaseVector(&data));

  if (macro) {
    // It is an identifier, and we need to keep the string
    token->kind = TK_Macro;
    token->macro = string;
    token->error = DK_Ok;
  }

  // boolean literals
  if (!strcmp(string, "true")) {
    token->kind = TK_BoolLiteral;
    token->bool_literal = true;
  } else if (!strcmp(string, "false")) {
    token->kind = TK_BoolLiteral;
    token->bool_literal = false;
    // keywords
  } else if (!strcmp(string, "if")) {
    token->kind = TK_If;
  } else if (!strcmp(string, "else")) {
    token->kind = TK_Else;
  } else if (!strcmp(string, "while")) {
    token->kind = TK_While;
  } else if (!strcmp(string, "with")) {
    token->kind = TK_With;
  } else if (!strcmp(string, "for")) {
    token->kind = TK_For;
  } else if (!strcmp(string, "break")) {
    token->kind = TK_Break;
  } else if (!strcmp(string, "continue")) {
    token->kind = TK_Continue;
  } else if (!strcmp(string, "return")) {
    token->kind = TK_Return;
  } else if (!strcmp(string, "fn")) {
    token->kind = TK_Function;
  } else if (!strcmp(string, "let")) {
    token->kind = TK_Let;
  } else if (!strcmp(string, "struct")) {
    token->kind = TK_Struct;
  } else if (!strcmp(string, "pack")) {
    token->kind = TK_Pack;
  } else if (!strcmp(string, "enum")) {
    token->kind = TK_Enum;
  } else if (!strcmp(string, "union")) {
    token->kind = TK_Union;
  } else if (!strcmp(string, "type")) {
    token->kind = TK_TypeAlias;
  } else if (!strcmp(string, "void")) {
    token->kind = TK_Void;
  } else if (!strcmp(string, "sizeof")) {
    token->kind = TK_Sizeof;
  } else if (!strcmp(string, "typeof")) {
    token->kind = TK_Typeof;
  } else if (!strcmp(string, "alignof")) {
    token->kind = TK_Alignof;
  } else {
    // It is an identifier, and we need to keep the string
    token->kind = TK_Identifier;
    token->identifier = string;
    token->error = DK_Ok;
  }
  // If it wasn't an identifier
  token->error = DK_Ok;

  return;
}

/* clang-format off */

#define RESULT_TOKEN( tokenType, errorType)                                    \
  *token = (Token){                                                            \
    .kind = tokenType,                                                         \
    .span = SPAN(start, lexer->position),                                      \
    .error = errorType                                                         \
  };                                                                           \

#define RETURN_RESULT_TOKEN( tokenType)                                        \
  RESULT_TOKEN(tokenType, DK_Ok)                                               \
  return;

#define NEXT_AND_RETURN_RESULT_TOKEN(tokenType)                                \
  nextValueLexer(lexer);                                                       \
  RETURN_RESULT_TOKEN(tokenType)
/* clang-format on */

void lexNextToken(Lexer *lexer, Token *token, Arena *arena) {
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
    lexIdentifierOrMacro(lexer, token, arena);
    return;
  } else if (isdigit(c)) {
    lexNumberLiteral(lexer, token, arena);
    return;
  } else {
    switch (c) {
    case '\'': {
      lexCharLiteral(lexer, token, arena);
      return;
    }
    case '\"': {
      lexStringLiteral(lexer, token, arena);
      return;
    }
    case '#': {
      lexComment(lexer, token, arena);
      return;
    }
    case '&': {
      nextValueLexer(lexer);
      // && or &
      switch (peekValueLexer(lexer)) {
      case '&': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_And)
      }
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_AssignBitAnd)
      }
      default: {
        RETURN_RESULT_TOKEN(TK_BitAnd)
      }
      }
    }
    case '|': {
      nextValueLexer(lexer);
      switch (peekValueLexer(lexer)) {
      case '|': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_Or)
      }
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_AssignBitOr)
      }
      default: {
        RETURN_RESULT_TOKEN(TK_BitOr)
      }
      }
    }
    case '!': {
      nextValueLexer(lexer);
      // ! or !=
      switch (peekValueLexer(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_NotEqual)
      }
      default: {
        RETURN_RESULT_TOKEN(TK_Not)
      }
      }
    }
    case '=': {
      nextValueLexer(lexer);
      // = or ==
      switch (peekValueLexer(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_Equal)
      }
      default: {
        RETURN_RESULT_TOKEN(TK_Assign)
      }
      }
    }
    case '<': {
      nextValueLexer(lexer);
      switch (peekValueLexer(lexer)) {
      case '<': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_ShiftLeft)
      }
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_CompLessEqual)
      }
      default: {
        RETURN_RESULT_TOKEN(TK_CompLess)
      }
      }
    }
    case '>': {
      nextValueLexer(lexer);
      switch (peekValueLexer(lexer)) {
      case '>': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_ShiftRight)
      }
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_CompGreaterEqual)
      }
      default: {
        RETURN_RESULT_TOKEN(TK_CompGreater)
      }
      }
    }
    case '+': {
      nextValueLexer(lexer);
      switch (peekValueLexer(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_AssignAdd)
      }
      default: {
        RETURN_RESULT_TOKEN(TK_Add)
      }
      }
    }
    case '-': {
      nextValueLexer(lexer);
      switch (peekValueLexer(lexer)) {
      case '>': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_Pipe)
      }
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_AssignSub)
      }
      default: {
        RETURN_RESULT_TOKEN(TK_Sub)
      }
      }
    }
    case '*': {
      nextValueLexer(lexer);
      switch (peekValueLexer(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_AssignMul)
      }
      default: {
        RETURN_RESULT_TOKEN(TK_Mul)
      }
      }
    }
    case '/': {
      nextValueLexer(lexer);
      switch (peekValueLexer(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_AssignDiv)
      }
      default: {
        RETURN_RESULT_TOKEN(TK_Div)
      }
      }
    }
    case '%': {
      nextValueLexer(lexer);
      switch (peekValueLexer(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_AssignMod)
      }
      default: {
        RETURN_RESULT_TOKEN(TK_Mod)
      }
      }
    }
    case ':': {
      nextValueLexer(lexer);
      switch (peekValueLexer(lexer)) {
      case ':': {
        NEXT_AND_RETURN_RESULT_TOKEN(TK_ScopeResolution)
      }
      default: {
        RETURN_RESULT_TOKEN(TK_Colon)
      }
      }
    }
    case '[': {
      NEXT_AND_RETURN_RESULT_TOKEN(TK_BracketLeft)
    }
    case ']': {
      NEXT_AND_RETURN_RESULT_TOKEN(TK_BracketRight)
    }
    case '$': {
      NEXT_AND_RETURN_RESULT_TOKEN(TK_Ref)
    }
    case '@': {
      NEXT_AND_RETURN_RESULT_TOKEN(TK_Deref)
    }
    case '(': {
      NEXT_AND_RETURN_RESULT_TOKEN(TK_ParenLeft)
    }
    case ')': {
      NEXT_AND_RETURN_RESULT_TOKEN(TK_ParenRight)
    }
    case '{': {
      NEXT_AND_RETURN_RESULT_TOKEN(TK_BraceLeft)
    }
    case '}': {
      NEXT_AND_RETURN_RESULT_TOKEN(TK_BraceRight)
    }
    case '.': {
      NEXT_AND_RETURN_RESULT_TOKEN(TK_FieldAccess)
    }
    case ',': {
      NEXT_AND_RETURN_RESULT_TOKEN(TK_Comma)
    }
    case ';': {
      NEXT_AND_RETURN_RESULT_TOKEN(TK_Semicolon)
    }
    case '_': {
      NEXT_AND_RETURN_RESULT_TOKEN(TK_Underscore)
    }
    case EOF: {
      RESULT_TOKEN(TK_None, DK_EOF)
      return;
    }
    default: {
      RESULT_TOKEN(TK_None, DK_UnrecognizedCharacter)
      nextValueLexer(lexer);
      return;
    }
    }
  }
}
