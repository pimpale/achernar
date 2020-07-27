#ifndef COM_OS_TIME_H
#define COM_OS_TIME_H

// operating system based timers 

#include "com_define.h"

// GUARANTEES: returns time in seconds since the unix epoch (not necessarily monotonic)
u64 com_os_time_unix();

// GUARANTEES: is a monotonic clock/timer based function with precision greater than a second
u64 com_os_time_monotonic();

#endif
