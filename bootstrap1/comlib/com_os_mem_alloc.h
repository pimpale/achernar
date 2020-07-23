#ifndef COM_OS_MEM_ALLOC_H
#define COM_OS_MEM_ALLOC_H

#include "com_define.h"

void* com_os_mem_alloc(usize size);
void* com_os_mem_realloc(void* source, usize nsize);
void com_os_mem_dealloc(void*);

#endif
