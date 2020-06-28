#ifndef COM_ENSURE_H
#define COM_ENSURE_H

#include "com_define.h"

/*
 * Displays an error message to stderr and terminates the program with `abort()`
 * REQUIRES: `condition` is a null terminated string containing the text of the expression evaluating to false
 * REQUIRES: `message` is a null terminated string containing an error message to be displayed
 * REQUIRES: `file` is a null terminated string containing the name of the file in which this expression was
 * REQUIRES: `function` is a null terminated string containing the name of the error in which this expression was
 * REQUIRES: `line` is the line that this expression is on
 * GUARANTEES: will attempt to terminate the program execution and print a human readable error message
 * GUARANTEES: returns false if the program execution was unable to terminate
 */
bool attr_NORETURN com_ensure_fail(const char* condition, const char *message, const char* file, const u64 line, const char* function);

/* 
 * If `expr` evaluates to false, will invoke `com_ensure_fail`
 * REQUIRES: `expr` is a valid C expression returning a boolean
 * REQUIRES: `failmsg` is a null terminated string
 * GUARANTEES: when `expr` is false, `com_ensure_fail` will be evaluated
 */
#define com_ensure_m(expr, failmsg) ((expr) \
            ? true \
            : com_ensure_fail(#expr, (failmsg), __FILE__, __LINE__, __func__))


#endif

