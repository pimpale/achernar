#ifndef COM_ASSERT_H
#define COM_ASSERT_H

#include "com_define.h"
#include "com_str.h"

// clang-format off
/**
 * Displays an error message to stderr and terminates the program with `com_os_exit_panic()`
 * REQUIRES: `condition` is a null terminated string containing the text of the expression evaluating to false
 * REQUIRES: `message` is a null terminated string containing an error message to be displayed
 * REQUIRES: `file` is a null terminated string containing the name of the file in which this expression was
 * REQUIRES: `function` is a null terminated string containing the name of the error in which this expression was
 * REQUIRES: `line` is the line that this expression is on
 * GUARANTEES: will attempt to terminate the program execution and print a human readable error message
 * GUARANTEES: returns false if the program execution was unable to terminate
 */
bool attr_NORETURN com_assert_fail(com_str condition, com_str message, com_str file, u32 line, com_str function);

/**
 * Displays an error message and terminates the program with `com_os_abort`
 * REQUIRES: `message` is a null terminated string containing an error message to be displayed
 * REQUIRES: `file` is a null terminated string containing the name of the file in which this statement was
 * REQUIRES: `function` is a null terminated string containing the name of the containing function
 * REQUIRES: `line` is the number of the line this expression was at
 * GUARANTEES: will attempt to output this data in a human readable format and then terminate the program using `com_os_exit_panic()` 
 */
void attr_NORETURN com_assert_unreachable(com_str message, com_str file, u32 line, com_str function);

/**
 * If `expr` evaluates to false, will invoke `com_assert_fail()` with local values
 * REQUIRES: `expr` is a valid C expression returning a boolean
 * REQUIRES: `failmsg` is a null terminated string
 * GUARANTEES: when `expr` is false, `com_assert_fail` will be evaluated
 */

// clang-format on

#define com_assert_m(expr, failmsg)                                            \
  ((expr) ? true                                                               \
          : com_assert_fail(com_str_lit_m(#expr), com_str_lit_m(failmsg),      \
                            com_str_lit_m(__FILE__), __LINE__,                 \
                            com_str_lit_m(__func__)))

/**
 * Will invoke `com_assert_unreachable()` with local values
 * REQUIRES: `failmsg` is a null terminated string
 */
#define com_assert_unreachable_m(failmsg)                                      \
  com_assert_unreachable(com_str_lit_m(failmsg), com_str_lit_m(__FILE__),      \
                         __LINE__, com_str_lit_m(__func__))

#endif
