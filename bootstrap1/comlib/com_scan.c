#include "com_scan.h"
#include "com_assert.h"
#include "com_imath.h"

com_scan_UntilResult com_scan_until(com_writer *destination, com_reader *source,
                                    u8 c) {

	bool source_limited = com_reader_flags(source) & com_reader_LIMITED ;

	bool destination_limited =  com_writer_flags(destination) & com_writer_LIMITED;

	bool no_max_read = !(source_limited || destination_limited);

  usize max_read = com_imath_usize_min(
      source_limited 
      	? com_reader_query(source) 
      	: usize_max_m, 
      destination_limited 
      	? com_writer_query(destination) 
      	: usize_max_m);

  // TODO for buffered readers request peeked large chunks and check
  // For now using naiive method

  usize i = 0;

  for (i = 0; no_max_read || i < max_read; i++) {
    com_reader_ReadResult read_ret = com_reader_read_u8(source);

    // fail on invalid read
    if (!read_ret.valid) {
      return (com_scan_UntilResult){
          .written = i, .read = i, .successful = false};
    }

    // if we encounter the terminal character, return early
    if (c == read_ret.value) {
      return (com_scan_UntilResult){
          .read = i + 1, .written = i, .successful = true};
    }

    // write to destination
    com_writer_WriteResult write_ret =
        com_writer_append_u8(destination, read_ret.value);

    // ensure write is valid
    if (!write_ret.valid) {
      return (com_scan_UntilResult){
          .read = i + 1, .written = i, .successful = false};
    }
  }

  // if we never found the answer
  return (com_scan_UntilResult){
      .read = i, .written = i, .successful = false};
}




com_scan_CheckedStrResult com_scan_checked_str_until_quote(com_writer *destination, com_reader *reader) {

    typedef enum {
      StringParserText,
      StringParserBackslash,
      StringParserUnicode,
    } StringParserState;

    StringParserState state = StringParserText;

    while (true) {
      switch (state) {
      case StringParserText: {
        com_reader_ReadResult ret =  com_reader_read_u8(reader);
        if(ret.valid) {
        switch (c) {
        case '\\': {
          state = StringParserBackslash;
          break;
        }
        case '\"': {
          goto LOOPEND;
        }
        default: {
          *com_vec_push_m(&data, char) = (char)c;
          break;
        }
        }
        break;
      }
      case StringParserBackslash: {
        c = lex_next(l);
        switch (c) {
        case '\"': {
          *com_vec_push_m(&data, char) = '\"';
          state = StringParserText;
          break;
        }
        case '\\': {
          *com_vec_push_m(&data, char) = '\\';
          state = StringParserText;
          break;
        }
        case '/': {
          *com_vec_push_m(&data, char) = '/';
          state = StringParserText;
          break;
        }
        case 'b': {
          *com_vec_push_m(&data, char) = '\b';
          state = StringParserText;
          break;
        }
        case 'f': {
          *com_vec_push_m(&data, char) = '\f';
          state = StringParserText;
          break;
        }
        case 'n': {
          *com_vec_push_m(&data, char) = '\n';
          state = StringParserText;
          break;
        }
        case 'r': {
          *com_vec_push_m(&data, char) = '\r';
          state = StringParserText;
          break;
        }
        case 't': {
          *com_vec_push_m(&data, char) = '\t';
          state = StringParserText;
          break;
        }
        case 'u': {
          state = StringParserUnicode;
          break;
        }
        default: {
          *com_vec_push_m(diagnostics, com_json_Error) =
              com_json_error_m(com_json_StrInvalidControlChar, l->position);
          state = StringParserText;
          break;
        }
        }
        break;
      }
      case StringParserUnicode: {
        uint32_t code_point = 0;
        for (int i = 0; i < 4; i++) {
          c = lex_next(l);
          if (c == EOF) {
            *VEC_PUSH(diagnostics, com_json_Error) =
                ERROR(com_json_StrExpectedDoubleQuote, l->position);
            goto LOOPEND;
          }
          int8_t value = fromHex((char)c);
          if (value < 0) {
            *VEC_PUSH(diagnostics, com_json_Error) =
                ERROR(com_json_StrInvalidUnicodeSpecifier, l->position);
            value = 0;
          }
          code_point += code_point * 16 + (uint8_t)value;
        }
        encodeUTFPoint(&data, code_point);
        state = StringParserText;
        break;
      }
      }
    }

  LOOPEND:;
    usize len = com_vec_len_m(&data, char);
    *vec_push_m(&data, char) = '\0';
    return J_STR(vec_release(&data), len);
  }
