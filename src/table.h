#ifndef TABLE_H
#define TABLE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct __attribute__((__packed__)) {
  void *key;
  size_t keylen;
  void *value;
  size_t valuelen;
  bool existent;
} Mapping;

// Do not manually modify
typedef struct {
  // The mappings in the table
  Mapping *mappings;
  // Number of mappings in mappings
  size_t mappingCount;
  // Total number of spots for mappings
  size_t mappingCapacity;
} Table;

/* Initialize the table */
void initTable(Table *table);
/* Initialize the table with a default capacity */
void initTableCapacity(Table *table, size_t capacity);
/* Delete the table */
void freeTable(Table *table);

/*
 * Puts a Mapping in the table, creating one if it doesnt exist. If value is
 * null, will set valuelen
 */
void putTable(Table *table, void *key, size_t keylen, void *value,
              size_t valuelen);

/* Returns the length of the value that is referenced by the first keylen bytes
 * of key */
size_t getValueLengthTable(Table *table, void *key, size_t keylen);

/*
 * Copies valuelen bytes of the value that is matched to the
 * first keylen bytes of key if the value pointed to the key exists
 *
 * In order to determine the size of the value, caller should call
 * getValueLengthTable
 */
void getTable(Table *table, void *key, size_t keylen, void *value,
              size_t valuelen);
/* Deletes the Mapping given by key */
void delTable(Table *table, void *key, size_t keylen);

/* Determines the current load of the table */
float currentLoadTable(Table *table);

#endif
