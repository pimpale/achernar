#include "lex.h"

#include <ctype.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"
#include "diagnostic.h"
#include "vector.h"


// Call this function right before the first hash
// Returns control at the first noncomment area
// Lexes comments
static Token lexComment(Lexer *lexer, Allocator *a) {
  LnCol start = lexer->position;

  int32_t c = lex_next(lexer);
  assert(c == '#');

  c = lex_peek(lexer);

  // Determine the scope of the
  char *scope = "";
  if (c == '@') {
    lex_next(lexer);

    Vector data;
    vec_create(&data, a);
    while ((c = lex_peek(lexer)) != EOF) {
      if (isalnum(c) || c == '/') {
        *VEC_PUSH(&data, char) = (char)c;
        lex_next(lexer);
      } else {
        break;
      }
    }
    *VEC_PUSH(&data, char) = '\0';
    scope = vec_release(&data);
  } else {
  }

  // Now we determine the type of comment as well as gather the comment data
  switch (c) {
  case '{': {
    // This is a comment. It will continue until another quote asterix is found.
    // These strings are preserved in the AST. They are nestable
    // also nestable
    // #{ Comment }#
    Vector data;
    vec_create(&data, a);
    size_t stackDepth = 1;
    char lastChar = '\0';

    // Drop initial {
    lex_next(lexer);
    while ((c = lex_next(lexer)) != EOF) {
      if (c == '#' && lastChar == '}') {
        // If we see a "# to pair off the starting #"
        stackDepth--;
        if (stackDepth == 0) {
          break;
        }
      } else if (c == '{' && lastChar == '#') {
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
    return (Token){
        .kind = tk_Comment,
        .comment =
            {
                .scope = scope,
                .comment = vec_release(&data),
            },
        .span = SPAN(start, lexer->position),
      };
  }
  default: {
    // If we don't recognize any of these characters, it's just a normal single
    // line comment. These are not nestable, and continue till the end of line.
    // # comment
    Vector data;
    vec_create(&data, a);
    while ((c = lex_next(lexer)) != EOF) {
      if (c != '\n') {
        *VEC_PUSH(&data, char) = (char)c;
      } else {
        break;
      }
    }
    *VEC_PUSH(&data, char) = '\0';

    // Return data
    return (Token){
        .kind = tk_Comment,
        .comment =
            {
                .scope = scope,
                .comment = vec_release(&data),
            },
        .span = SPAN(start, lexer->position),
        };
  }
  }
}

// Call this function right before the first quote of the string literal
// Returns control after the ending quote of this lexer
// This function returns a Token containing the string or an error
static void lexStringLiteral(Lexer *lexer, Allocator *a) {
  LnCol start = lexer->position;
  // Skip first quote
  int32_t c = lex_next(lexer);
  assert(c == '\"');

  Vector data;
  vec_create(&data, a);

  while ((c = lex_next(lexer)) != EOF) {
    if (c == '\\') {
      c = lex_next(lexer);
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
        // keep going till we hit an end double quote
        while ((c = lex_next(lexer)) != EOF) {
          if (c == '\"') {
            break;
          }
        }

        *token = (Token){.kind = tk_String,
                         .span = SPAN(start, lexer->position),
                         .error = DK_StringLiteralUnrecognizedEscapeCode,
                         .string_literal = vec_release(&data)};
        // once we've hit the end, we release the data
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
    .kind = tk_String,
      .string_literal = vec_release(&data),
      .span = SPAN(start, lexer->position),
      .error = DK_Ok
  };
  // clang-format on
  return;
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



// Call this function right before the first digit of the number literal
// Returns control right after the number is finished
// This function returns a Token or the error
static void lexNumberLiteral(Lexer *l, Allocator *a, Vector* diagnostics) {
  LnCol start = l->position;

  uint8_t radix;

  if(lex_peek(l) == '0') {
    lex_next(l);
    int32_t radixCode = lex_next(l);
    switch(radixCode) {
      case 'b': {
                  radix = 2;
                  break;
                }
      case 'o': {
                  radix = 8;
                  break;
                }
      case 'd': {
                  radix = 10;
                  break;
                }
      case 'x': {
                  radix =16;
                  break;
                }
                default: {

                         }
    }
  }
  

  int64_t integer_value = 0;
  int32_t c;
  while ((c = lex_peek(l)) != EOF) {
    if (isdigit(c)) {
      integer_value = integer_value * 10 + (c - '0');
      lex_next(l);
    } else {
      break;
    }
  }


  bool fractional = false;
  if (lex_peek(l) == '.') {
    fractional = true;
    lex_next(l);
  }

  double fractional_component = 0;
  if (fractional) {
    double place = 0.1;
    while ((c = lex_peek(l)) != EOF) {
      if (isdigit(c)) {
        fractional_component += place * (c - '0');
        place /= 10;
        lex_next(l);
      } else {
        break;
      }
    }
  }

  bool positive_exponent = false;
  bool negative_exponent = false;
  c = lex_peek(l);
  if (c == 'E' || c == 'e') {
    lex_next(l);
    switch (lex_next(l)) {
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
    while ((c = lex_peek(l)) != EOF) {
      if (isdigit(c)) {
        exponential_integer = exponential_integer * 10 + (c - '0');
        lex_next(l);
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
    return J_NUM_ELEM(num);
  } else {
    // Integer
    int64_t num = integer_value;
    if (positive_exponent) {
      for (int i = 0; i < exponential_integer; i++) {
        num *= 10;
      }
    }
    return J_INT_ELEM(num);
  }
}
}

static void lexCharLiteral(Lexer *lexer, Allocator *a) {
  // Skip first quote
  int32_t c = lex_next(lexer);

  if (c != '\'') {
    INTERNAL_ERROR(
        "called char or label lexer where there wasn't a char or label");
    PANIC();
  }

  DiagnosticKind dk = DK_Ok;

  Vector data;
  createVector(&data);

  // State of lexer
  typedef enum {
    LCS_Initial,     // the first character
    LCS_ExpectEnd,   // expects the closing '
    LCS_SpecialChar, // expects a character
    LCS_Label,       // parsing a label
  } LexCharState;

  LexCharState state = LCS_Initial;

  LnCol start = lexer->position;

  while ((c = lex_next(lexer)) != EOF) {
    switch (state) {
    case LCS_Initial: {
      if (c == '\\') {
        state = LCS_SpecialChar;
      } else {
        *VEC_PUSH(&data, char) = (char)c;
        state = LCS_ExpectEnd;
      }
      break;
    }
    case LCS_ExpectEnd: {
      if (c == '\'') {
        // break out of loop, successful
        goto EXIT_LOOP;
      } else {
        *VEC_PUSH(&data, char) = (char)c;
        state = LCS_Label;
      }
      break;
    }
    case LCS_SpecialChar: {
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
        *VEC_PUSH(&data, char) = (char)c;
        dk = DK_CharLiteralUnrecognizedEscapeCode;
        break;
      }
      }
      state = LCS_ExpectEnd;
      break;
    }
    case LCS_Label: {
      if (isalnum(c)) {
        *VEC_PUSH(&data, char) = (char)c;
      } else {
        goto EXIT_LOOP;
      }
      break;
    }
    }
  }

  // exit of loop
EXIT_LOOP:;

  switch (state) {
  case LCS_Initial:
  case LCS_SpecialChar: {
    *token = (Token){.kind = tk_None,
                     .span = SPAN(start, lexer->position),
                     .error = DK_EOF};
    destroyVector(&data);
    break;
  }
  case LCS_ExpectEnd: {
      // all paths through the expectEnd will end up with something pushed
    *token = (Token){.kind = tk_Char,
                     .char_literal = *VEC_GET(&data, 0, char),
                     .span = SPAN(start, lexer->position),
                     .error = dk};
    destroyVector(&data);
    return;
  }
  case LCS_Label: {
    size_t length = VEC_LEN(&data, char);
    for (size_t i = 0; i < length; i++) {
      if (!isalnum(VEC_GET(&data, i, char))) {
        dk = DK_LabelUnknownCharacter;
      }
    }

    *VEC_PUSH(&data, char) = '\0';
    *token = (Token){.kind = tk_Label,
                     .label = manageMemArena(ar, releaseVector(&data)),
                     .span = SPAN(start, lexer->position),
                     .error = dk};
    break;
  }
  }
}

// Parses an identifer or macro or builtin
static void lexWord(Lexer *lexer, Allocator *a) {

  LnCol start = lexer->position;

  Vector data;
  createVector(&data);

  bool macro = false;

  lex_peek(lexer);

  int32_t c;
  while ((c = lex_peek(lexer)) != EOF) {
    if (isalnum(c) || c == '_') {
      *VEC_PUSH(&data, char) = (char)c;
      lex_next(lexer);
    } else if (c == '!') {
      lex_next(lexer);
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
  char *string = releaseVector(&data);

  if (macro) {
    // It is an identifier, and we need to keep the string
    token->kind = tk_MacroCall;
    token->macro_call = manageMemArena(ar, string);
    token->error = DK_Ok;
    return;
  }

  if(!strcmp(string, "true")) {
     token->kind = tk_Bool;
     token->bool_literal = true;
  } else if(!strcmp(string, "false")) {
     token->kind = tk_Bool;
     token->bool_literal = false;
  } else if (!strcmp(string, "loop")) {
    token->kind = tk_Loop;
  } else if (!strcmp(string, "let")) {
    token->kind = tk_Let;
  } else if (!strcmp(string, "use")) {
    token->kind = tk_Use;
  } else if (!strcmp(string, "namespace")) {
    token->kind = tk_Namespace;
  } else if (!strcmp(string, "as")) {
    token->kind = tk_As;
  } else if (!strcmp(string, "match")) {
    token->kind = tk_Match;
  } else if (!strcmp(string, "defer")) {
    token->kind = tk_Defer;
  } else if (!strcmp(string, "break")) {
    token->kind = tk_Break;
  } else if (!strcmp(string, "continue")) {
    token->kind = tk_Continue;
  } else if (!strcmp(string, "return")) {
    token->kind = tk_Return;
  } else if (!strcmp(string, "fn")) {
    token->kind = tk_Fn;
  } else if (!strcmp(string, "pat")) {
    token->kind = tk_Pat;
  } else if (!strcmp(string, "void")) {
    token->kind = tk_Void;
  } else if (!strcmp(string, "struct")) {
    token->kind = tk_Struct;
  } else if (!strcmp(string, "enum")) {
    token->kind = tk_Enum;
  } else if (!strcmp(string, "type")) {
    token->kind = tk_Type;
  } else if (!strcmp(string, "macro")) {
    token->kind = tk_Macro;
  } else if (!strcmp(string, "unreachable")) {
    token->kind = tk_Unreachable;
  } else {
    // It is an identifier, and we need to keep the string
    token->kind = tk_Identifier;
    token->identifier = manageMemArena(ar, string);
    token->error = DK_Ok;
    return;
  }

  // If it wasn't an identifier or macro
  token->error = DK_Ok;
  free(string);
  return;
}

// Parses a builtin or an underscore token
static void lexBuiltinOrUnderscore(Lexer *lexer, Allocator *a) {
  LnCol start = lexer->position;
  // Skip first quote
  int32_t c = lex_next(lexer);
  assert(lex_next(lexer) == '_');

  int32_t c = lex_peek(lexer);
  if (!isalpha(c)) {
    *token = (Token){
        .kind = tk_Underscore,
        .span = SPAN(start, lexer->position),
        .error = DK_Ok,
    };
    return;
  }

  Vector data;
  createVector(&data);

  while ((c = lex_peek(lexer)) != EOF) {
    if (isalnum(c)) {
      *VEC_PUSH(&data, char) = (char)c;
      lex_next(lexer);
    } else {
      break;
    }
  }

  token->span = SPAN(start, lexer->position);

  // Note that string length does not incude the trailing null byte
  // Push null byte
  *VEC_PUSH(&data, char) = '\0';
  char *string = manageMemArena(ar, releaseVector(&data));

  // If it wasn't an identifier
  token->error = DK_Ok;
  token->builtin = string;
  token->kind = tk_Builtin;
  token->span = SPAN(start, lexer->position);
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
  lex_next(lexer);                                                       \
  RETURN_RESULT_TOKEN(tokenType)
/* clang-format on */

Token lexNextToken(Lexer *lexer, Allocator *a) {
  int32_t c;

  // Set c to first nonblank character
  while ((c = lex_peek(lexer)) != EOF) {
    if (isblank(c) || c == '\n') {
      lex_next(lexer);
    } else {
      break;
    }
  }

  LnCol start = lexer->position;

  if (isalpha(c)) {
    lexWord(lexer, a);
    return;
  } else if (isdigit(c)) {
    lexNumberLiteral(lexer, a);
    return;
  } else {
    switch (c) {
    case '\'': {
      lexCharLiteral(lexer, a);
      return;
    }
    case '\"': {
      lexStringLiteral(lexer, a);
      return;
    }
    case '_': {
      lexBuiltinOrUnderscore(lexer, a);
      return;
    }
    case '#': {
      lexComment(lexer, a);
      return;
    }
    case '&': {
      lex_next(lexer);
      // && or &
      switch (lex_peek(lexer)) {
      case '&': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_And)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Ref)
      }
      }
    }
    case '|': {
      lex_next(lexer);
      switch (lex_peek(lexer)) {
      case '|': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_Or)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Union)
      }
      }
    }
    case '!': {
      lex_next(lexer);
      // ! or !=
      switch (lex_peek(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_CompNotEqual)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Not)
      }
      }
    }
    case '=': {
      lex_next(lexer);
      // = or == or =>
      switch (lex_peek(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_CompEqual)
      }
      case '>': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_Arrow)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Assign)
      }
      }
    }
    case '<': {
      lex_next(lexer);
      switch (lex_peek(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_CompLessEqual)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_CompLess)
      }
      }
    }
    case '>': {
      lex_next(lexer);
      switch (lex_peek(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_CompGreaterEqual)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_CompGreater)
      }
      }
    }
    case '+': {
      lex_next(lexer);
      switch (lex_peek(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_AssignAdd)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Add)
      }
      }
    }
    case '-': {
      lex_next(lexer);
      switch (lex_peek(lexer)) {
      case '>': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_Pipe)
      }
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_AssignSub)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Sub)
      }
      }
    }
    case '*': {
      lex_next(lexer);
      switch (lex_peek(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_AssignMul)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Mul)
      }
      }
    }
    case '/': {
      lex_next(lexer);
      switch (lex_peek(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_AssignDiv)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Div)
      }
      }
    }
    case '%': {
      lex_next(lexer);
      switch (lex_peek(lexer)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_AssignMod)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Mod)
      }
      }
    }
    case ':': {
      lex_next(lexer);
      switch (lex_peek(lexer)) {
      case ':': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_ScopeResolution)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Colon)
      }
      }
    }
    case '.': {
      lex_next(lexer);
      switch (lex_peek(lexer)) {
      case '.': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_Rest)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_FieldAccess)
      }
      }
    }
    case '[': {
      NEXT_AND_RETURN_RESULT_TOKEN(tk_BracketLeft)
    }
    case ']': {
      NEXT_AND_RETURN_RESULT_TOKEN(tk_BracketRight)
    }
    case '@': {
      NEXT_AND_RETURN_RESULT_TOKEN(tk_Deref)
    }
    case '(': {
      NEXT_AND_RETURN_RESULT_TOKEN(tk_ParenLeft)
    }
    case ')': {
      NEXT_AND_RETURN_RESULT_TOKEN(tk_ParenRight)
    }
    case '{': {
      NEXT_AND_RETURN_RESULT_TOKEN(tk_BraceLeft)
    }
    case '}': {
      NEXT_AND_RETURN_RESULT_TOKEN(tk_BraceRight)
    }
    case ',': {
      NEXT_AND_RETURN_RESULT_TOKEN(tk_Tuple)
    }
    case ';': {
      NEXT_AND_RETURN_RESULT_TOKEN(tk_Semicolon)
    }
    case '$': {
      NEXT_AND_RETURN_RESULT_TOKEN(tk_Dollar)
    }
    case '`': {
      NEXT_AND_RETURN_RESULT_TOKEN(tk_Backtick)
    }
    case EOF: {
      RESULT_TOKEN(tk_None, DK_EOF)
      return;
    }
    default: {
      RESULT_TOKEN(tk_None, DK_UnrecognizedCharacter)
      lex_next(lexer);
      return;
    }
    }
  }
}
