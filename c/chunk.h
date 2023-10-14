#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
  OP_ADD,
  OP_CALL,
  OP_CLASS,
  OP_CLOSE_UPVALUE,
  OP_CLOSURE,
  OP_CONSTANT,
  OP_DEFINE_GLOBAL,
  OP_DIVIDE,
  OP_EQUAL,
  OP_FALSE,
  OP_GET_GLOBAL,
  OP_GET_LOCAL,
  OP_GET_PROPERTY,
  OP_GET_UPVALUE,
  OP_GREATER,
  OP_INHERIT,
  OP_INVOKE,
  OP_JUMP,
  OP_JUMP_IF_FALSE,
  OP_LESS,
  OP_LOOP,
  OP_METHOD,
  OP_MULTIPLY,
  OP_NEGATE,
  OP_NIL,
  OP_NOT,
  OP_POP,
  OP_PRINT,
  OP_RETURN,
  OP_SET_GLOBAL,
  OP_SET_LOCAL,
  OP_SET_PROPERTY,
  OP_SET_UPVALUE,
  OP_SUBTRACT,
  OP_TRUE,
} Opcode;

typedef struct {
  int count;
  int capacity;
  uint8_t *code;
  int *lines;
  ValueArray constants;
} Chunk;

void initChunk(Chunk *chunk);
void freeChunk(Chunk *chunk);
void writeChunk(Chunk *chunk, uint8_t byte, int line);
int addConstant(Chunk *chunk, Value value);

#endif
