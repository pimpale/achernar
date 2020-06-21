#include "code_to_tokens.h"

#include <ctype.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "constants.h"
#include "diagnostic.h"
#include "std_allocator.h"
#include "vector.h"

// Call this function right before the first hash
// Returns control at the first noncomment area
// Lexes comments
static Token lexComment(Lexer *lexer, UNUSED DiagnosticLogger* diagnostics,
                        Allocator *a) {

  LnCol start = lexer->position;

  int32_t c = lex_next(lexer);
  assert(c == '#');

  c = lex_peek(lexer);

  // Determine the scope of the
  char *scope = "";
  if (c == '@') {
    lex_next(lexer);

    Vector data = vec_create(a);
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
    Vector data = vec_create(a);
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
        .commentToken =
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
    Vector data = vec_create(a);
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
        .commentToken =
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
static Token lexStringLiteral(Lexer *lexer, DiagnosticLogger* diagnostics, Allocator *a) {
  LnCol start = lexer->position;
  // Skip first quote
  int32_t c = lex_next(lexer);
  assert(c == '\"');

  Vector data = vec_create(a);

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
        *dlogger_append(diagnostics) =
            diagnostic_standalone(SPAN(start, lexer->position), DSK_Error,
                                  "Unrecognized escape code in string literal");
        goto CLEANUP;
      }
      }
    } else if (c == '\"') {
      break;
    } else {
      *VEC_PUSH(&data, char) = (char)c;
    }
  }

CLEANUP:;
  size_t len = VEC_LEN(&data, char);
  char *ptr = vec_release(&data);
  // Return data
  return (Token){
      .kind = tk_String,
      .stringToken = {.data = ptr, .data_len = len},
      .span = SPAN(start, lexer->position),
  };
}

// Parses integer with radix
static uint64_t parseNumBaseComponent(Lexer *l, DiagnosticLogger* diagnostics,
                                      uint8_t radix) {
  uint64_t integer_value = 0;
  int32_t c;
  while ((c = lex_peek(l)) != EOF) {
    uint8_t digit_val;
    if (c >= '0' && c <= '9') {
      digit_val = (uint8_t)(c - '0');
    } else if (c >= 'A' && c <= 'F') {
      digit_val = 10 + (uint8_t)(c - 'A');
    } else if (c == '_') {
      // skip it
      lex_next(l);
      continue;
    } else if (!isalnum(c)) {
      // this component of the integer is finished processing
      break;
    } else {
      // means an alphabetical character out of this range
      *dlogger_append(diagnostics) = diagnostic_standalone(
          lex_peekSpan(l), DSK_Error, "num literal unknown character");
      digit_val = 0;
    }

    if (digit_val >= radix) {
      *dlogger_append(diagnostics) = diagnostic_standalone(
          lex_peekSpan(l), DSK_Error, "num literal char value exceeds radix");
      // correct the radix value
      digit_val = radix - 1;
    }

    // now we can
    uint64_t old_integer_value = integer_value;
    integer_value = integer_value * radix + digit_val;
    if (old_integer_value > integer_value) {
      *dlogger_append(diagnostics) = diagnostic_standalone(
          lex_peekSpan(l), DSK_Error, "num literal overflow");
    }

    // we can finally move past this char
    lex_next(l);
  }
  return integer_value;
}

static double parseNumFractionalComponent(Lexer *l, DiagnosticLogger* diagnostics,
                                          uint8_t radix) {
  // reused character variable
  int32_t c;

  // the reciprocal of the current decimal place
  // EX: at 0.1 will be 10 when parsing the 1
  // EX: at 0.005 will be 1000 when parsing the 5
  // This is because representing decimals is lossy
  double place = 1;

  // fractional component being computed
  double fractional_value = 0;

  while ((c = lex_peek(l)) != EOF) {
    uint8_t digit_val;
    if (c >= '0' && c <= '9') {
      digit_val = (uint8_t)(c - '0');
    } else if (c >= 'A' && c <= 'F') {
      digit_val = (uint8_t)(c - 'A');
    } else if (c == '_') {
      // skip it
      lex_next(l);
      continue;
    } else if (!isalnum(c)) {
      // this component of the number is finished processing
      break;
    } else {
      // means an alphabetical character out of this range
      *dlogger_append(diagnostics) = diagnostic_standalone(
          lex_peekSpan(l), DSK_Error, "num literal unknown character");
      digit_val = 0;
    }

    if (digit_val >= radix) {
      *dlogger_append(diagnostics) = diagnostic_standalone(
          lex_peekSpan(l), DSK_Error, "num literal char value exceeds radix");
      // correct the radix value
      digit_val = radix - 1;
    }

    place *= radix;
    fractional_value += digit_val / place;

    // we can finally move past this char
    lex_next(l);
  }
  return fractional_value;
}

// Call this function right before the first digit of the number literal
// Returns control right after the number is finished
// This function returns a Token or the error
static Token lexNumberLiteral(Lexer *l, DiagnosticLogger* diagnostics,
                              UNUSED Allocator *a) {

  LnCol start = l->position;

  uint8_t radix;

  if (lex_peek(l) == '0') {
    lex_next(l);
    int32_t radixCode = lex_next(l);
    switch (radixCode) {
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
      radix = 16;
      break;
    }
    default: {
      radix = 10;
      *dlogger_append(diagnostics) =
          diagnostic_standalone(SPAN(start, l->position), DSK_Error,
                                "num literal unrecognized radix code");
      break;
    }
    }
  } else {
    radix = 10;
  }

  uint64_t base_component = parseNumBaseComponent(l, diagnostics, radix);

  bool fractional = false;
  if (lex_peek(l) == '.') {
    fractional = true;
    lex_next(l);
  }

  if (fractional) {
    double fractional_component =
        parseNumFractionalComponent(l, diagnostics, radix);
    return (Token){
        .kind = tk_Float,
        .floatToken = {.data = (fractional_component + (double)base_component)},
        .span = SPAN(start, l->position)};
  } else {
    return (Token){.kind = tk_Int,
                   .intToken =
                       {
                           .data = base_component,
                       },
                   .span = SPAN(start, l->position)};
  }
}

static Token lexCharLiteralOrLabel(Lexer *lexer, DiagnosticLogger* diagnostics,
                                   Allocator *a) {
  LnCol start = lexer->position;
  // Skip first quote
  assert(lex_next(lexer) == '\'');

  // State of lexer
  typedef enum {
    LCS_Initial,     // the first character
    LCS_ExpectEnd,   // expects the closing '
    LCS_SpecialChar, // expects a character
    LCS_Label,       // parsing a label
  } LexCharState;

  // if true then it is a label
  bool label = false;
  // if it is a char literal, only this variable will be used
  char char_literal = 'a';
  // label data will be uninitialized unless it is a label
  Vector label_data;
  LexCharState state = LCS_Initial;

  // reused char variable
  while (true) {
    switch (state) {
    case LCS_Initial: {
      int32_t c = lex_next(lexer);
      if (c == '\\') {
        state = LCS_SpecialChar;
      } else {
        char_literal = (char)c;
        state = LCS_ExpectEnd;
      }
      break;
    }
    case LCS_SpecialChar: {
      int32_t c = lex_peek(lexer);
      switch (c) {
      case '\\': {
        char_literal = '\\';
        break;
      }
      case '\'': {
        char_literal = '\'';
        break;
      }
      case '\"': {
        char_literal = '\"';
        break;
      }
      case 'a': {
        char_literal = '\a';
        break;
      }
      case 'b': {
        char_literal = '\b';
        break;
      }
      case 'f': {
        char_literal = '\f';
        break;
      }
      case 'n': {
        char_literal = '\n';
        break;
      }
      case 'r': {
        char_literal = '\r';
        break;
      }
      case 't': {
        char_literal = '\t';
        break;
      }
      case 'v': {
        char_literal = '\v';
        break;
      }
      default: {
        char_literal = (char)c;
        *dlogger_append(diagnostics) =
            diagnostic_standalone(lex_peekSpan(lexer), DSK_Error,
                                  "char literal unrecognized escape code");
        break;
      }
      }
      // now go past this char
      lex_next(lexer);
      state = LCS_ExpectEnd;
      break;
    }
    case LCS_ExpectEnd: {
      int32_t c = lex_peek(lexer);
      if (c == '\'') {
        label = false;
        // skip this single quote
        lex_next(lexer);
        // break out of loop, successful
        goto EXIT_LOOP;
      } else {
        if (isalnum(char_literal) || char_literal == '_') {
          // we are dealing with a label
          label = true;
          // initialize label data vector
          label_data = vec_create(a);
          // push first char into vec
          *VEC_PUSH(&label_data, char) = char_literal;
          // set state
          state = LCS_Label;
        } else {
          // we are dealing with a bad char literal
          *dlogger_append(diagnostics) =
              diagnostic_standalone(lex_peekSpan(lexer), DSK_Error,
                                    "char literal expected close single quote");
          label = false;
          goto EXIT_LOOP;
        }
      }
      break;
    }
    case LCS_Label: {
      int32_t c = lex_peek(lexer);
      if (isalnum(c) || c == '_') {
        *VEC_PUSH(&label_data, char) = (char)lex_next(lexer);
      } else {
        goto EXIT_LOOP;
      }
      break;
    }
    }
  }

  // exit of loop
EXIT_LOOP:;

  if (label) {
    // ensure null byte
    *VEC_PUSH(&label_data, char) = '\0';
    return (Token){
        .kind = tk_Label,
        .labelToken = {vec_release(&label_data)},
        .span = SPAN(start, lexer->position),
    };
  } else {
    return (Token){
        .kind = tk_Char,
        .charToken = {char_literal},
        .span = SPAN(start, lexer->position),
    };
  }
}

// Parses an identifer or macro or builtin
static Token lexWord(Lexer *lexer, UNUSED DiagnosticLogger* diagnostics, Allocator *a) {

  LnCol start = lexer->position;

  Vector data = vec_create(a);

  bool macro = false;

  int32_t c;
  while ((c = lex_peek(lexer)) != EOF) {
    if (isalnum(c) || c == '_') {
      *VEC_PUSH(&data, char) = (char)c;
      lex_next(lexer);
    } else if (c == '`') {
      lex_next(lexer);
      macro = true;
      break;
    } else {
      break;
    }
  }

  Span span = SPAN(start, lexer->position);

  // Note that string length does not incude the trailing null byte
  // Push null byte
  *VEC_PUSH(&data, char) = '\0';
  char *string = vec_get(&data, 0);

  if (!strcmp(string, "_")) {
    vec_destroy(&data);
    return (Token){.kind = tk_Underscore, .span = span};
  }

  if (macro) {
    // It is an identifier, and we need to keep the string
    return (Token){
        .kind = tk_Macro, .macroToken = {vec_release(&data)}, .span = span};
  }

  if (!strcmp(string, "true")) {
    vec_destroy(&data);
    return (Token){.kind = tk_Bool, .boolToken = {true}, .span = span};
  } else if (!strcmp(string, "false")) {
    vec_destroy(&data);
    return (Token){.kind = tk_Bool, .boolToken = {false}, .span = span};
  }

  Token token;
  token.span = span;
  if (!strcmp(string, "loop")) {
    token.kind = tk_Loop;
  } else if (!strcmp(string, "val")) {
    token.kind = tk_Val;
  } else if (!strcmp(string, "use")) {
    token.kind = tk_Use;
  } else if (!strcmp(string, "namespace")) {
    token.kind = tk_Namespace;
  } else if (!strcmp(string, "as")) {
    token.kind = tk_As;
  } else if (!strcmp(string, "match")) {
    token.kind = tk_Match;
  } else if (!strcmp(string, "defer")) {
    token.kind = tk_Defer;
  } else if (!strcmp(string, "return")) {
    token.kind = tk_Return;
  } else if (!strcmp(string, "fn")) {
    token.kind = tk_Fn;
  } else if (!strcmp(string, "pat")) {
    token.kind = tk_Pat;
  } else if (!strcmp(string, "nil")) {
    token.kind = tk_Nil;
  } else if (!strcmp(string, "struct")) {
    token.kind = tk_Struct;
  } else if (!strcmp(string, "enum")) {
    token.kind = tk_Enum;
  } else if (!strcmp(string, "type")) {
    token.kind = tk_Type;
  } else if (!strcmp(string, "never")) {
    token.kind = tk_Never;
  } else {
    // It is an identifier, and we need to keep the string
    token.kind = tk_Identifier;
    token.identifierToken.data = vec_release(&data);
    return token;
  }

  // If it wasn't an identifier or macro
  vec_destroy(&data);
  return token;
}

#define RESULT_TOKEN(tokenType)                                                \
  (Token) { .kind = tokenType, .span = SPAN(start, lexer->position) }

#define RETURN_RESULT_TOKEN(tokenType) return RESULT_TOKEN(tokenType);

#define NEXT_AND_RETURN_RESULT_TOKEN(tokenType)                                \
  lex_next(lexer);                                                             \
  RETURN_RESULT_TOKEN(tokenType)

Token tk_next(Lexer *lexer, DiagnosticLogger* diagnostics, Allocator *a) {
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

  if (isalpha(c) || c == '_') {
    return lexWord(lexer, diagnostics, a);
  } else if (isdigit(c)) {
    return lexNumberLiteral(lexer, diagnostics, a);
  } else {
    switch (c) {
    case '\'': {
      return lexCharLiteralOrLabel(lexer, diagnostics, a);
    }
    case '\"': {
      return lexStringLiteral(lexer, diagnostics, a);
    }
    case '#': {
      return lexComment(lexer, diagnostics, a);
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
      case '+': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_Posit)
      }
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
      case '-': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_Negate)
      }
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
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_Define)
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
      RETURN_RESULT_TOKEN(tk_Eof)
    }
    default: {
      *dlogger_append(diagnostics) = diagnostic_standalone(
          lex_peekSpan(lexer), DSK_Error, "lexer unrecognized character");
      NEXT_AND_RETURN_RESULT_TOKEN(tk_None)
    }
    }
  }
}
