#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"
#include "object.h"

#define ALLOCATE(type, count) (type *)reallocate(NULL, sizeof(type) * (count))

#define FREE(type, pointer) reallocate(pointer, 0)

#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*2)

#define GROW_ARRAY(type, pointer, oldCount, newCount)                          \
  (type *)reallocate(pointer, sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) reallocate(pointer, 0)

// THE BOOK'S DEFINITION HAS AN AS YET UNUSED ARGUMENT OLD_SIZE!!!!1!!!!
void *reallocate(void *pointer, size_t newSize);
void freeObjects();

#endif
