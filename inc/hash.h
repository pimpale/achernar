#ifndef HASH_H
#define HASH_H

#include <stdint.h>
#include <stdlib.h>

/* Non cryptographically secure hash */
uint64_t simpleHash(uint64_t seed, void *data, size_t datalen);

#endif /* HASH_H */
