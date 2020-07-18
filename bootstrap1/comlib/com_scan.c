#include "com_scan.h"
#include "com_assert.h"
#include "com_format.h"
#include "com_imath.h"

com_scan_UntilResult com_scan_until(com_writer *destination, com_reader *source,
                                    u8 c) {

  bool source_limited = com_reader_flags(source) & com_reader_LIMITED;

  bool destination_limited = com_writer_flags(destination) & com_writer_LIMITED;

  bool no_max_read = !(source_limited || destination_limited);

  usize max_read = com_imath_usize_min(
      source_limited ? com_reader_query(source) : usize_max_m,
      destination_limited ? com_writer_query(destination) : usize_max_m);

  // TODO for buffered readers request peeked large chunks and check
  // For now using naiive method

  for (usize i = 0; no_max_read || i < max_read; i++) {
    com_reader_ReadResult read_ret = com_reader_read_u8(source);

    // fail on invalid read
    if (!read_ret.valid) {
      return (com_scan_UntilResult){.successful = false};
    }

    // if we encounter the terminal character, return early
    if (c == read_ret.value) {
      return (com_scan_UntilResult){.successful = true};
    }

    // write to destination
    com_writer_WriteResult write_ret =
        com_writer_append_u8(destination, read_ret.value);

    // ensure write is valid
    if (!write_ret.valid) {
      return (com_scan_UntilResult){.successful = false};
    }
  }

  // if we never found the answer
  return (com_scan_UntilResult){.successful = false};
}

com_scan_CheckedStrResult
com_scan_checked_str_until_quote(com_writer *destination, com_reader *reader) {

  // Tagged union representing the state of the parser

  typedef enum {
    // parsing normal text
    StringParserText,
    // parsing text after backslash
    StringParserBackslash,
    // parsing unicode
    StringParserUnicode,
  } StringParserState;

  StringParserState state = StringParserText;

  while (true) {
    switch (state) {
    case StringParserText: {
      com_reader_ReadResult read_ret = com_reader_read_u8(reader);
      if (!read_ret.valid) {
        return (com_scan_CheckedStrResult){
            .result = com_scan_CheckedStrReadFailed,
            .location = com_reader_position(reader)};
      }
      u8 c = read_ret.value;

      switch (c) {
      case '\\': {
        state = StringParserBackslash;
        break;
      }
      case '\"': {
        return (com_scan_CheckedStrResult){
            .result = com_scan_CheckedStrSuccessful,
            .location = com_reader_position(reader)};
      }
      default: {
        com_writer_append_u8(destination, c);
        break;
      }
      }
      break;
    }
    case StringParserBackslash: {
      com_reader_ReadResult read_ret = com_reader_read_u8(reader);
      if (!read_ret.valid) {
        return (com_scan_CheckedStrResult){
            .result = com_scan_CheckedStrReadFailed,
            .location = com_reader_position(reader)};
      }
      u8 c = read_ret.value;

      switch (c) {
      case '\"': {
        com_writer_append_u8(destination, '\"');
        state = StringParserText;
        break;
      }
      case '/': {
        com_writer_append_u8(destination, '/');
        state = StringParserText;
        break;
      }
      case '\\': {
        com_writer_append_u8(destination, '\\');
        state = StringParserText;
        break;
      }
      case 'b': {
        com_writer_append_u8(destination, '\b');
        state = StringParserText;
        break;
      }
      case 'f': {
        com_writer_append_u8(destination, '\f');
        state = StringParserText;
        break;
      }
      case 'n': {
        com_writer_append_u8(destination, '\n');
        state = StringParserText;
        break;
      }
      case 'r': {
        com_writer_append_u8(destination, '\r');
        state = StringParserText;
        break;
      }
      case 't': {
        com_writer_append_u8(destination, '\t');
        state = StringParserText;
        break;
      }
      case 'u': {
        state = StringParserUnicode;
        break;
      }
      default: {
        return (com_scan_CheckedStrResult){
            .result= com_scan_CheckedStrInvalidControlChar,
            .location = com_reader_position(reader)};
      }
      }
      break;
    }
    case StringParserUnicode: {
      u32 code_point = 0;
      for (usize i = 0; i < 4; i++) {
        com_reader_ReadResult read_ret = com_reader_read_u8(reader);
        if (!read_ret.valid) {
          return (com_scan_CheckedStrResult){
              .result = com_scan_CheckedStrReadFailed,
              .location = com_reader_position(reader)};
        }

        u8 digit = read_ret.value;
        if (!com_format_is_hex(digit)) {
          return (com_scan_CheckedStrResult){
              .result = com_scan_CheckedStrInvalidUnicodeSpecifier,
              .location = com_reader_position(reader)};
        }

        u8 value = com_format_from_hex(digit);
        code_point += code_point * 16 + value;
      }
      com_format_append_utf_codepoint(destination, code_point);
      state = StringParserText;
      break;
    }
    }
  }
}

void com_scan_skip_whitespace(com_reader *reader) {
  com_assert_m(com_reader_flags(reader) & com_reader_BUFFERED, "reader must support peeking");
	while(true) {
		com_reader_ReadResult ret = com_reader_peek_u8(reader, 1);
		if(!ret.valid) {
    	// TODO do error or something
			return;
		}
		if(com_format_is_whitespace(ret.value)) {
    		com_reader_drop_u8(reader);
		} else {
    	// success
			return;
		}
	}
}
