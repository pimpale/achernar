#include "com_define.h"
#include "com_os_exit.h"
#include "com_os_iostream.h"
#include "com_os_mem_alloc.h"
#include "com_os_time.h"

// now include the c standard library functions
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>

// com_os_exit

void attr_NORETURN com_os_exit_panic() {
	abort();
}

void attr_NORETURN com_os_exit(i32 code) {
	exit(code);
}

// com_os_mem_alloc

void* com_os_mem_alloc(usize size) {
	return malloc(size);
}

void com_os_mem_dealloc(void* ptr) {
	free(ptr);
}

void *com_os_mem_realloc(void* src, usize nlen) {
	return realloc(src, nlen);
}

// com_os_iostream

usize com_os_iostream_write_err(const com_str str) {
	return fwrite(str.data, sizeof(u8), str.len, stderr);
}

usize com_os_iostream_write_out(const com_str str) {
	return fwrite(str.data, sizeof(u8), str.len, stdout);
}

com_str com_os_iostream_read_in(u8* buffer, usize buflen) {
	usize bytes_read = fread(buffer, sizeof(u8), buflen, stdin);
	return com_str_create(buffer, bytes_read);
}

// com_os_time
u64 com_os_time_monotonic() {
	return clock();
}

u64 com_os_time_unix() {
	return time(NULL);
}
