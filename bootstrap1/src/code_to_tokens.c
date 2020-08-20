#include "code_to_tokens.h"

#include "com_allocator.h"
#include "com_assert.h"
#include "com_bigint.h"
#include "com_biguint.h"
#include "com_format.h"
#include "com_scan.h"
#include "com_vec.h"
#include "com_writer_vec.h"
#include "constants.h"
#include "diagnostic.h"

// Call this function right before the first hash
// Returns control at the first noncomment area
// Lexes attributes
static Token lexAttributeOrComment(com_reader *r,
                                   attr_UNUSED DiagnosticLogger *diagnostics,
                                   com_allocator *a, bool attribute) {

  u8 character = attribute ? '$' : '#';

  com_loc_LnCol start = com_reader_position(r);

  com_reader_ReadU8Result c = com_reader_read_u8(r);
  com_assert_m(c.valid && c.value == character,
               attribute ? "expected $" : " expected #");

  com_vec data = com_vec_create(com_allocator_alloc(
      a, (com_allocator_HandleData){.len = 10,
                                    .flags = com_allocator_defaults(a) |
                                             com_allocator_REALLOCABLE |
                                             com_allocator_NOLEAK}));

  // Now we determine the type of comment as well as gather the comment data
  c = com_reader_peek_u8(r, 1);
  if (c.valid && c.value == '{') {
    // This is a multi line attribute. It will continue until another is found.
    // These strings are preserved in the AST. They are nestable
    // ${ Attribute }$

    usize stackDepth = 1;

    // Drop initial {
    com_reader_drop_u8(r);

    while (true) {
      com_reader_ReadU8Result cc = com_reader_peek_u8(r, 1);
      com_reader_ReadU8Result nc = com_reader_peek_u8(r, 2);
      if (cc.valid) {
        if (cc.value == '}' && nc.valid && nc.value == character) {
          stackDepth--;
          if (stackDepth == 0) {
            break;
          }
        } else if (cc.value == character && nc.valid && nc.value == '{') {
          stackDepth++;
        }
        *com_vec_push_m(&data, u8) = cc.value;
      } else {
        break;
      }
    }
  } else if (c.valid && c.value == character) {
    // it's a normal single line attribute.
    // These are not nestable, and continue till the end of line.
    // $$ attribute
    while (true) {
      com_reader_ReadU8Result cc = com_reader_peek_u8(r, 1);
      if (!cc.valid || cc.value == '\n') {
        break;
      } else {
        *com_vec_push_m(&data, u8) = (u8)cc.value;
      }
    }
  } else {
    // it's a single word attribute. continues until non alphanumeric
    // $attribute
    while (true) {
      com_reader_ReadU8Result cc = com_reader_peek_u8(r, 1);
      if (!cc.valid || !com_format_is_alphanumeric(cc.value)) {
        break;
      } else {
        *com_vec_push_m(&data, u8) = (u8)cc.value;
      }
    }
  }

  // Return data
  if (attribute) {
    return (Token){
        .kind = tk_Attribute,
        .attributeToken =
            {
                .content = com_str_demut(com_vec_to_str(&data)),
            },
        .span = com_loc_span_m(start, com_reader_position(r)),
    };
  } else {
    return (Token){
        .kind = tk_Attribute,
        .commentToken =
            {
                .comment = com_str_demut(com_vec_to_str(&data)),
            },
        .span = com_loc_span_m(start, com_reader_position(r)),
    };
  }
}

static Token lexAttribute(com_reader *r, DiagnosticLogger *diagnostics,
                          com_allocator *a) {
  return lexAttributeOrComment(r, diagnostics, a, true);
}

static Token lexComment(com_reader *r, DiagnosticLogger *diagnostics,
                        com_allocator *a) {
  return lexAttributeOrComment(r, diagnostics, a, false);
}

// Call this function right before the first quote of the string literal
// Returns control after the ending quote of this lexer
// This function returns a Token containing the string or an error
static Token lexStringLiteral(com_reader *r, DiagnosticLogger *diagnostics,
                              com_allocator *a) {
  com_loc_LnCol start = com_reader_position(r);

  // Skip first quote
  com_reader_ReadU8Result c = com_reader_read_u8(r);
  com_assert_m(c.valid && c.value == '\"', "expected quotation mark");

  // parse into vec writer until we get a success or a read error log errors
  // along the way

  // create vector writer
  com_vec vec = com_vec_create(com_allocator_alloc(
      a, (com_allocator_HandleData){
             .flags = com_allocator_defaults(a) | com_allocator_NOLEAK |
                      com_allocator_REALLOCABLE,
             // let's just go with 12 for now as initial capacity
             .len = 12}));
  com_writer writer = com_writer_vec_create(&vec);

  while (true) {
    com_scan_CheckedStrResult ret =
        com_scan_checked_str_until_quote(&writer, r);
    switch (ret.result) {
    case com_scan_CheckedStrSuccessful: {
      // We're finished, we can deallocate writer and release vec into a string
      // that we return as an element
      com_writer_destroy(&writer);

      // retrn success
      return (Token){
          .kind = tk_String,
          .span = com_loc_span_m(start, com_reader_position(r)),
          .stringToken = {.data = com_str_demut(com_vec_to_str(&vec))}};
    }
    case com_scan_CheckedStrReadFailed: {
      // deallocate resources
      com_writer_destroy(&writer);
      // give error
      *dlogger_append(diagnostics) =
          (Diagnostic){.span = ret.span,
                       .severity = DSK_Error,
                       .message = com_str_lit_m(
                           "unexpected EOF, expected closing double quote"),
                       .children_len = 0};
      // return invalid
      return (Token){
          .kind = tk_None,
          .span = com_loc_span_m(start, ret.span.end),
      };
    }
    case com_scan_CheckedStrInvalidControlChar: {
      *dlogger_append(diagnostics) = (Diagnostic){
          .span = ret.span,
          .severity = DSK_Error,
          .message = com_str_lit_m("invalid control char after backslash"),
          .children_len = 0};
      break;
    }
    case com_scan_CheckedStrInvalidUnicodeSpecifier: {
      *dlogger_append(diagnostics) =
          (Diagnostic){.span = ret.span,
                       .severity = DSK_Error,
                       .message = com_str_lit_m("invalid unicode point"),
                       .children_len = 0};
      break;
    }
    }
  }
}

// Parses integer with radix
// Radix must be between 2 and 16 inclusive
static u64 parseNumBaseComponent(com_reader *r, DiagnosticLogger *diagnostics,
                                 u8 radix) {
  u64 integer_value = 0;
  while (true) {
    com_loc_Span sp = com_reader_peek_span_u8(r, 1);
    com_reader_ReadU8Result ret = com_reader_peek_u8(r, 1);

    // exit if misread or not a hex number
    if (!ret.valid || com_format_is_hex(ret.value)) {
      break;
    }

    // ignore underscore
    if (ret.value == '_') {
      com_reader_drop_u8(r);
      continue;
    }

    u8 digit_val = com_format_from_hex(ret.value);

    if (digit_val >= radix) {
      *dlogger_append(diagnostics) = (Diagnostic){
          .span = sp,
          .severity = DSK_Error,
          .message = com_str_lit_m("num literal char value exceeds radix"),
          .children_len = 0};
      // put in dummy for the digit value
      digit_val = radix - 1;
    }

    // now we can create integer value
    u64 old_integer_value = integer_value;
    integer_value = integer_value * radix + digit_val;
    if (old_integer_value > integer_value) {
      *dlogger_append(diagnostics) =
          (Diagnostic){.span = sp,
                       .severity = DSK_Error,
                       .message = com_str_lit_m("num literal overflow"),
                       .children_len = 0};
    }

    // we can finally move past this char
    com_reader_drop_u8(r);
  }
  return integer_value;
}

static f64 parseNumFractionalComponent(com_reader *r,
                                       DiagnosticLogger *diagnostics,
                                       u8 radix) {
  // the reciprocal of the current decimal place
  // EX: at 0.1 will be 10 when parsing the 1
  // EX: at 0.005 will be 1000 when parsing the 5
  // This is because representing decimals is lossy
  f64 place = 1;

  // fractional component being computed
  f64 fractional_value = 0;

  while (true) {
    com_loc_Span sp = com_reader_peek_span_u8(r, 1);
    com_reader_ReadU8Result ret = com_reader_peek_u8(r, 1);

    // exit if misread or not a hex number
    if (!ret.valid || com_format_is_hex(ret.value)) {
      break;
    }

    // ignore underscore
    if (ret.value == '_') {
      com_reader_drop_u8(r);
      continue;
    }

    u8 digit_val = com_format_from_hex(ret.value);

    if (digit_val >= radix) {
      *dlogger_append(diagnostics) = (Diagnostic){
          .span = sp,
          .severity = DSK_Error,
          .message = com_str_lit_m("num literal char value exceeds radix"),
          .children_len = 0};
      // put in dummy for the digit value
      digit_val = radix - 1;
    }

    place *= radix;
    fractional_value += digit_val / place;

    // we can finally move past this char
    com_reader_drop_u8(r);
  }
  return fractional_value;
}

// Call this function right before the first digit of the number literal
// Returns control right after the number is finished
// This function returns a Token or the error
static Token lexNumberLiteral(com_reader *r, DiagnosticLogger *diagnostics,
                              attr_UNUSED com_allocator *a) {

  com_loc_LnCol start = com_reader_position(r);

  u8 radix = 10;
  {
    // char one ahead
    com_reader_ReadU8Result fdigit = com_reader_peek_u8(r, 1);
    // char two ahead
    com_reader_ReadU8Result code = com_reader_peek_u8(r, 2);

    if (fdigit.valid && fdigit.value == '0' && code.valid) {
      switch (code.value) {
      case 'b': {
        radix = 2;
        com_reader_drop_u8(r);
        com_reader_drop_u8(r);
        break;
      }
      case 'o': {
        radix = 8;
        com_reader_drop_u8(r);
        com_reader_drop_u8(r);
        break;
      }
      case 'd': {
        radix = 10;
        com_reader_drop_u8(r);
        com_reader_drop_u8(r);
        break;
      }
      case 'x': {
        radix = 16;
        com_reader_drop_u8(r);
        com_reader_drop_u8(r);
        break;
      }
      default: {
        radix = 10;
        if (!com_format_is_digit(code.value)) {
          // if it's not another digit, then we throw an error about an
          // unrecognized radix code + skip the first 2 chars
          com_reader_drop_u8(r);
          com_reader_drop_u8(r);

          *dlogger_append(diagnostics) = (Diagnostic){
              .span = com_loc_span_m(start, com_reader_position(r)),
              .severity = DSK_Error,
              .message = com_str_lit_m("num literal unrecognized radix code"),
              .children_len = 0};
        }
        // if it was a digit then it parses like a normal decimal number
        break;
      }
      }
    }
  }

  u64 base_component = parseNumBaseComponent(r, diagnostics, radix);

  bool fractional = false;
  {
    com_reader_ReadU8Result ret = com_reader_peek_u8(r, 1);
    if (ret.valid && ret.value == '.') {
      fractional = true;
    }
  }

  if (fractional) {
    f64 fractional_component =
        parseNumFractionalComponent(r, diagnostics, radix);
    return (Token){
        .kind = tk_Float,
        .floatToken = {.data = (fractional_component + (f64)base_component)},
        .span = com_loc_span_m(start, com_reader_position(r))};
  } else {
    return (Token){.kind = tk_Int,
                   .intToken =
                       {
                           .data = base_component,
                       },
                   .span = com_loc_span_m(start, com_reader_position(r))};
  }
}

static Token lexLabelLiteral(com_reader *r,
                             attr_UNUSED DiagnosticLogger *diagnostics,
                             com_allocator *a) {
  com_loc_LnCol start = com_reader_position(r);
  // Skip first quote
  {
    com_reader_ReadU8Result ret = com_reader_read_u8(r);
    com_assert_m(ret.valid && ret.value == '\'', "expected single quote");
  }
  com_vec label_data = com_vec_create(com_allocator_alloc(
      a, (com_allocator_HandleData){.len = 1,
                                    .flags = com_allocator_defaults(a) |
                                             com_allocator_REALLOCABLE |
                                             com_allocator_NOLEAK}));

  while (true) {
    com_reader_ReadU8Result ret = com_reader_peek_u8(r, 1);
    if (ret.valid &&
        (com_format_is_alphanumeric(ret.value) || ret.value == '_')) {
      *com_vec_push_m(&label_data, u8) = ret.value;
      com_reader_drop_u8(r);
    } else {
      break;
    }
  }

  return (Token){.span = com_loc_span_m(start, com_reader_position(r)),
                 .kind = tk_Label,
                 .labelToken = {
                     .data = com_str_demut(com_vec_to_str(&label_data)),
                 }};
}

static Token lexCharLiteral(com_reader *r, DiagnosticLogger *diagnostics,
                            attr_UNUSED com_allocator *a) {
  com_loc_LnCol start = com_reader_position(r);
  // Skip first quote
  {
    com_reader_ReadU8Result ret = com_reader_read_u8(r);
    com_assert_m(ret.valid && ret.value == '\'', "expected single quote");
  }

  // State of lexer
  typedef enum {
    LCS_Initial,     // the first character
    LCS_ExpectEnd,   // expects the closing '
    LCS_SpecialChar, // expects a character
  } LexCharState;

  // if it is a char literal, only this variable will be used
  u8 char_literal = 'a';
  // label data will be uninitialized unless it is a label
  LexCharState state = LCS_Initial;

  // reused char variable
  while (true) {
    switch (state) {
    case LCS_Initial: {
      com_reader_ReadU8Result ret = com_reader_read_u8(r);
      if (ret.valid) {
        if (ret.value == '\\') {
          state = LCS_SpecialChar;
        } else {
          char_literal = ret.value;
          state = LCS_ExpectEnd;
        }
      } else {
        com_loc_Span span = com_loc_span_m(start, com_reader_position(r));
        *dlogger_append(diagnostics) =
            (Diagnostic){.span = span,
                         .severity = DSK_Error,
                         .message = com_str_lit_m(
                             "unexpected EOF after opening single quote"),
                         .children_len = 0};
        return (Token){.kind = tk_None, .span = span};
      }
      break;
    }
    case LCS_SpecialChar: {
      com_reader_ReadU8Result ret = com_reader_read_u8(r);
      if (!ret.valid) {
        com_loc_Span span = com_loc_span_m(start, com_reader_position(r));
        *dlogger_append(diagnostics) =
            (Diagnostic){.span = span,
                         .severity = DSK_Error,
                         .message = com_str_lit_m(
                             "unexpected EOF after backslash in char literal"),
                         .children_len = 0};
        return (Token){.kind = tk_Char, .span = span

        };
      }
      switch (ret.value) {
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
        char_literal = ret.value;
        *dlogger_append(diagnostics) = (Diagnostic){
            .span = com_loc_span_m(start, com_reader_position(r)),
            .severity = DSK_Error,
            .message = com_str_lit_m("char literal unrecognized escape code"),
            .children_len = 0};
        break;
      }
      }
      state = LCS_ExpectEnd;
      break;
    }
    case LCS_ExpectEnd: {
      com_loc_Span sp = com_reader_peek_span_u8(r, 1);
      com_reader_ReadU8Result ret = com_reader_read_u8(r);
      if (!(ret.valid && ret.value == '\'')) {
        *dlogger_append(diagnostics) =
            (Diagnostic){.span = sp,
                         .severity = DSK_Error,
                         .message = com_str_lit_m(
                             "char literal expected closing single quote"),
                         .children_len = 0};
      }
      return (Token){
          .kind = tk_Char,
          .charToken = {.data = char_literal},
          .span = com_loc_span_m(start, com_reader_position(r)),
      };
    }
    }
  }
}

// Parses an identifer or macro or builtin
static Token lexWord(com_reader *r, attr_UNUSED DiagnosticLogger *diagnostics,
                     com_allocator *a) {

  com_loc_LnCol start = com_reader_position(r);

  com_vec data = com_vec_create(com_allocator_alloc(
      a, (com_allocator_HandleData){.len = 10,
                                    .flags = com_allocator_defaults(a) |
                                             com_allocator_REALLOCABLE |
                                             com_allocator_NOLEAK}));

  bool macro = false;

  while (true) {
    com_reader_ReadU8Result ret = com_reader_peek_u8(r, 1);
    if (ret.valid) {
      u8 c = ret.value;
      if (com_format_is_alphanumeric(c) || c == '_') {
        *com_vec_push_m(&data, u8) = c;
        com_reader_drop_u8(r);
      } else if (c == '!') {
        com_reader_drop_u8(r);
        macro = true;
        break;
      } else {
        // we encountered a nonword char
        break;
      }
    } else {
      // means we hit EOF
      break;
    }
  }

  com_loc_Span span = com_loc_span_m(start, com_reader_position(r));

  com_str str = com_str_demut(com_vec_to_str(&data));

  if (com_str_equal(str, com_str_lit_m("_"))) {
    return (Token){.kind = tk_Underscore, .span = span};
  }

  if (macro) {
    // It is an identifier, and we need to keep the string
    return (Token){.kind = tk_Macro, .macroToken = {str}, .span = span};
  }

  if (com_str_equal(str, com_str_lit_m("true"))) {
    return (Token){.kind = tk_Bool, .boolToken = {true}, .span = span};
  } else if (com_str_equal(str, com_str_lit_m("false"))) {
    return (Token){.kind = tk_Bool, .boolToken = {false}, .span = span};
  }

  Token token;
  token.span = span;

  if (com_str_equal(str, com_str_lit_m("loop"))) {
    token.kind = tk_Loop;
  } else if (com_str_equal(str, com_str_lit_m("match"))) {
    token.kind = tk_Match;
  } else if (com_str_equal(str, com_str_lit_m("pat"))) {
    token.kind = tk_Pat;
  } else if (com_str_equal(str, com_str_lit_m("val"))) {
    token.kind = tk_Val;
  } else if (com_str_equal(str, com_str_lit_m("template"))) {
    token.kind = tk_Template;
  } else if (com_str_equal(str, com_str_lit_m("ret"))) {
    token.kind = tk_Ret;
  } else if (com_str_equal(str, com_str_lit_m("defer"))) {
    token.kind = tk_Defer;
  } else if (com_str_equal(str, com_str_lit_m("fn"))) {
    token.kind = tk_Fn;
  } else if (com_str_equal(str, com_str_lit_m("pat"))) {
    token.kind = tk_Pat;
  } else if (com_str_equal(str, com_str_lit_m("as"))) {
    token.kind = tk_As;
  } else if (com_str_equal(str, com_str_lit_m("type"))) {
    token.kind = tk_Type;
  } else if (com_str_equal(str, com_str_lit_m("typefn"))) {
    token.kind = tk_Typefn;
  } else if (com_str_equal(str, com_str_lit_m("struct"))) {
    token.kind = tk_Struct;
  } else if (com_str_equal(str, com_str_lit_m("enum"))) {
    token.kind = tk_Enum;
  } else if (com_str_equal(str, com_str_lit_m("mod"))) {
    token.kind = tk_Mod;
  } else if (com_str_equal(str, com_str_lit_m("use"))) {
    token.kind = tk_Use;
  } else if (com_str_equal(str, com_str_lit_m("and"))) {
    token.kind = tk_And;
  } else if (com_str_equal(str, com_str_lit_m("or"))) {
    token.kind = tk_Or;
  } else if (com_str_equal(str, com_str_lit_m("not"))) {
    token.kind = tk_Not;
  } else if (com_str_equal(str, com_str_lit_m("nil"))) {
    token.kind = tk_Nil;
  } else if (com_str_equal(str, com_str_lit_m("never"))) {
    token.kind = tk_Never;
  } else {
    // It is an identifier, and we need to keep the string
    token.kind = tk_Identifier;
    token.identifierToken.data = str;
    return token;
  }

  return token;
}

#define RESULT_TOKEN(tokenType)                                                \
  (Token) {                                                                    \
    .kind = tokenType, .span = com_loc_span_m(start, com_reader_position(r))   \
  }

#define RETURN_RESULT_TOKEN(tokenType) return RESULT_TOKEN(tokenType);

#define NEXT_AND_RETURN_RESULT_TOKEN(tokenType)                                \
  com_reader_drop_u8(r);                                                       \
  RETURN_RESULT_TOKEN(tokenType)

// utility methods to handle the incredible amount of edge cases

typedef i16 inband_reader_result;
static inband_reader_result lex_peek(com_reader *r, usize n) {
  com_reader_ReadU8Result ret = com_reader_peek_u8(r, n);
  if (!ret.valid) {
    return -1;
  }
  return ret.value;
}

static bool is_alpha(inband_reader_result c) {
  return c != -1 && com_format_is_alpha((u8)c);
}

static bool is_alphanumeric(inband_reader_result c) {
  return c != -1 && com_format_is_alphanumeric((u8)c);
}

static bool is_digit(inband_reader_result c) {
  return c != -1 && com_format_is_digit((u8)c);
}

static bool is_whitespace(inband_reader_result c) {
  return c != -1 && com_format_is_whitespace((u8)c);
}

Token tk_next(com_reader *r, DiagnosticLogger *diagnostics, com_allocator *a) {
  // always defined after
  inband_reader_result c = -1;
  inband_reader_result c2 = -1;

  // Set c to first nonblank character
  while ((c = lex_peek(r, 1)) != -1) {
    // it's guaranteed that c is > 0 in body of while
    if (is_whitespace(c)) {
      com_reader_drop_u8(r);
    } else {
      c2 = lex_peek(r, 2);
      break;
    }
  }

  com_loc_LnCol start = com_reader_position(r);

  if (com_format_is_alpha((u8)c)) {
    return lexWord(r, diagnostics, a);
  } else if (c == '_' && is_alpha(c2)) {
    return lexWord(r, diagnostics, a);
  } else if (is_digit(c)) {
    return lexNumberLiteral(r, diagnostics, a);
  } else if ((c == '-' || c == '+') && is_digit(c2)) {
    return lexNumberLiteral(r, diagnostics, a);
  } else {
    switch (c) {
    case '\'': {
      if (lex_peek(r, 1) == '\\' || lex_peek(r, 2) == '\'') {
        return lexCharLiteral(r, diagnostics, a);
      } else {
        return lexLabelLiteral(r, diagnostics, a);
      }
    }
    case '\"': {
      return lexStringLiteral(r, diagnostics, a);
    }
    case '#': {
      return lexComment(r, diagnostics, a);
    }
    case '$': {
      return lexAttribute(r, diagnostics, a);
    }
    case '&': {
      NEXT_AND_RETURN_RESULT_TOKEN(tk_And)
    }
    case '|': {
      com_reader_drop_u8(r);
      switch (lex_peek(r, 1)) {
      case '|': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_Enum)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Intersection)
      }
      }
    }
    case '!': {
      com_reader_drop_u8(r);
      // ! or !=
      switch (lex_peek(r, 1)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_CompNotEqual)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Not)
      }
      }
    }
    case '=': {
      com_reader_drop_u8(r);
      // = or == or =>
      switch (lex_peek(r, 1)) {
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
      com_reader_drop_u8(r);
      switch (lex_peek(r, 1)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_CompLessEqual)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_CompLess)
      }
      }
    }
    case '>': {
      com_reader_drop_u8(r);
      switch (lex_peek(r, 1)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_CompGreaterEqual)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_CompGreater)
      }
      }
    }
    case '+': {
      com_reader_drop_u8(r);
      switch (lex_peek(r, 1)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_AssignAdd)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Add)
      }
      }
    }
    case '-': {
      com_reader_drop_u8(r);
      switch (lex_peek(r, 1)) {
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
      com_reader_drop_u8(r);
      switch (lex_peek(r, 1)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_AssignMul)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Mul)
      }
      }
    }
    case '/': {
      com_reader_drop_u8(r);
      switch (lex_peek(r, 1)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_AssignDiv)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Slash)
      }
      }
    }
    case '%': {
      com_reader_drop_u8(r);
      switch (lex_peek(r, 1)) {
      case '=': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_AssignRem)
      }
      default: {
        RETURN_RESULT_TOKEN(tk_Rem)
      }
      }
    }
    case ':': {
      com_reader_drop_u8(r);
      switch (lex_peek(r, 1)) {
      case ':': {
        NEXT_AND_RETURN_RESULT_TOKEN(tk_MemberResolution)
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
      com_reader_drop_u8(r);
      switch (lex_peek(r, 1)) {
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
    case '`': {
      NEXT_AND_RETURN_RESULT_TOKEN(tk_Backtick)
    }
    default: {
      com_reader_drop_u8(r);
      *dlogger_append(diagnostics) =
          (Diagnostic){.span = com_reader_peek_span_u8(r, 1),
                       .severity = DSK_Error,
                       .message = com_str_lit_m("lexer unrecognized character"),
                       .children_len = 0};
      NEXT_AND_RETURN_RESULT_TOKEN(tk_None)
    }
    }
  }
}
