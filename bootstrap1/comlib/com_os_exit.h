#ifndef COM_OS_EXIT_H
#define COM_OS_EXIT_H

// an implementation is purposely not provided for this file.
// you will have to implement it yourself depending on your OS

#include "com_define.h"

void attr_NORETURN com_os_exit_panic(void);
void attr_NORETURN com_os_exit(i32 code);

#endif

