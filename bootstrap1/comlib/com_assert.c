#include "com_assert.h"
#include "com_format.h"
#include "com_os_exit.h"
#include "com_os_io_out.h"

static void write_quoted(com_writer* writer, com_str str) {
  com_format_u8_char(writer, '\"');
  com_format_str_checked(writer, str);
  com_format_u8_char(writer, '\"');
}

bool attr_NORETURN com_assert_fail(com_str condition, com_str message, com_str file,
                                   u64 line, com_str function) {

  com_writer err = com_os_io_err_writer_create();

  com_format_u8_char(&err, '{');
  write_quoted(&err, com_str_lit_m("kind"));
  com_format_u8_char(&err, ':');
  write_quoted(&err, com_str_lit_m("assertion_failure"));
  com_format_u8_char(&err, ',');
  com_format_str_checked(&err, com_str_asciiz(condition));
  com_format_u8_char(&err, '{');
  com_format_u8_char(&err, '}');

  com_os_exit_panic(1);
}
