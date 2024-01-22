#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include "printer.h"
#include <stdlib.h>

int64_t bird_main(int64_t * heap_cursor, int64_t * end_of_heap) asm("bird_main");

int main(int argc, char** argv) {
  int64_t* heap_cursor = malloc(sizeof(int64_t)*1048576);
  int64_t* end_of_heap = heap_cursor + 1048576;
  int64_t result = bird_main(heap_cursor, end_of_heap);
  printValue(result);
  return 0;
}
