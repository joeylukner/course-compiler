#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"

// These extern declarations allow us to refer to the variables you made global
// in your generated assembly file.

extern uint64_t *start_of_stack;
extern uint64_t *end_of_stack;
extern uint64_t *start_of_heap;
extern uint64_t *end_of_heap;
extern uint64_t *heap_cursor;
uint64_t closure_bit = 0x8000000000000000;
uint64_t *end_of_allocated_heap_memory;
/*
  The following macros allow you to use "debugf" in place of "printf" for
  formatted debugging.  For instance, you can write

      debugf("Pointer %p changed to pointer %p.\n", old, new);

  to print in the same way you would use printf.  The advantage is that,
  depending upon which of the two macros you are using, the debugf call either
  turns into a printf call or is erased entirely.  Use debugf to print-debug
  your garbage collector and then disable printing when you need to perform your
  unit tests.
*/

// This macro disables all debugf statements.  (They become no-ops.)
 //#define debugf(fmt, ...) ;

// This macro enables all debugf statements.  (They become printf statements.)
#define debugf(fmt, ...)      \
  printf(fmt, ##__VA_ARGS__); \
  fflush(stdout)

/**
 * This helper function will show the entire contents of your heap.  This output
 * can get very large if your heap is too big!
 */
void dump_heap()
{
  debugf("HEAP:\n");
  int c = 0;
  for (uint64_t *p = (uint64_t *)((uint64_t)start_of_heap & 0xFFFFFFFFFFFFFFFC);
       p < end_of_heap; p += 1)
  {
    if (c == 0)
    {
      debugf("%016" PRIx64 ":", p);
    }
    if (p >= start_of_heap)
    {
      debugf("    %016" PRIx64, *p);
    }
    else
    {
      debugf("            ");
    }
    c++;
    if (c == 4)
    {
      debugf("\n");
      c = 0;
    }
  }
  if (c != 0)
  {
    debugf("\n");
  }
}

int numargs(uint64_t *p, uint64_t *offset)
{
  uint64_t size = *p;
  if ((size & 0x8000000000000000) != 0)
  { // closure, take away tag bit
    *offset = 4;
    return ((*p) ^ closure_bit);
  }
  else
  {
    *offset = 2;
    return *p;
  }
}

bool if_heap_ptr(uint64_t current)
{
  return (((current & 0x3) == 0x1) && ((current > start_of_heap) && (current < end_of_heap)));
}

void mark_dfs(uint64_t *p)
{
  dump_heap();
  uint64_t *gcword = p + 1;
  uint64_t num_arguments;
  uint64_t offset;
  num_arguments = numargs(p, &offset);

  
  if (*gcword == 1)
  {
    return;
  }
  // THIS IS THE ACTUAL MARKING
  *gcword = 1; // label vertex as discovered

  for (uint64_t *i = p + offset; i < p + offset + num_arguments; i++)
  {
    if (if_heap_ptr(*i))
    { // another heap pointer
      mark_dfs(*i-1);
    }
    // else, simply increment
  }
}

void mark()
{
  uint64_t current;

  for (uint64_t *p = (uint64_t *)end_of_stack; p < start_of_stack; p++)
  {
    current = *p;
    if (if_heap_ptr(current))
    {
      // if the value is tagged as a heap pointer
      current -= 1;                  // remove last bit
      mark_dfs((uint64_t *)current); // dfs on current pointer
    }
  }
}

int calculate_size(uint64_t *next_heap_object)
{

  uint64_t size = *next_heap_object;
  if ((size & 0x8000000000000000) == 0)
  { // this is a tuple

    size += 2;
  }
  else
  { // this is a closure
    size = size ^ closure_bit;
    size += 4;
  }
  return size;
  // obj size has been calculated
}
uint64_t* forward()
{
  uint64_t *next_heap_object = start_of_heap;
  uint64_t *next_live_destination = start_of_heap;
  uint64_t *gcword;
  while (next_heap_object < heap_cursor)
  {
    gcword = next_heap_object + 1;
    // debugf("nextHEap_Forward:%p\n",next_heap_object);
    uint64_t size = calculate_size(next_heap_object);
    // if reachable
    if (*(next_heap_object + 1) == 0x1)
    {
      *gcword = next_live_destination;

      next_live_destination += size; // inc next_heap_obj by size of curr
    }

    next_heap_object += size;
  }
  return next_live_destination;
}

void change_ptr(uint64_t *i)
{
  uint64_t current = *i;
  if (if_heap_ptr(current))
  {                                        // if bird pointer
    current -= 1;                          // mask off tag bit
    current += 8;                          // advance by 8 bytes to get forward address
    uint64_t ptr = *((uint64_t *)current); // retrieve forward address
    ptr += 1;
    *i = ptr;
  }
}
void print_stack()
{
  for (uint64_t *p = (uint64_t *)end_of_stack; p < start_of_stack; p++)
  {
    uint64_t current = *p;

    if (if_heap_ptr(current))
    {
      debugf("stack pointer: %x\n", current);
    }
  }
}
void update()
{

  uint64_t num_arguments;
  uint64_t offset;
  uint64_t *next_heap_object = start_of_heap;
  for (uint64_t *p = (uint64_t *)end_of_stack; p < start_of_stack; p++)
  {
    change_ptr(p);
  }

  while (next_heap_object < heap_cursor)
  {
    num_arguments = numargs(next_heap_object, &offset);
    //  debugf("nextHEap update:%p\n",next_heap_object);

    uint64_t size = calculate_size(next_heap_object);

    for (uint64_t *i = next_heap_object + offset; i < next_heap_object + offset + num_arguments; i++)
    {
      uint64_t current = *i;
      if (if_heap_ptr(current))
      {                                        // if bird pointer
        current -= 1;                          // mask off tag bit
        current += 8;                          // advance by 8 bytes to get forward address
        uint64_t ptr = *((uint64_t *)current); // retrieve forward address
        ptr += 1;
        *i = ptr;
      }
    }
    next_heap_object += size;
  }
}

void compact()
{
  uint64_t *next_heap_object = start_of_heap;
  while (next_heap_object < heap_cursor)
  {
    //  debugf("next heap object compact: %p\n", next_heap_object);
    uint64_t size = calculate_size(next_heap_object);

    if (*(next_heap_object + 1) != 0x0)
    {
      // debugf("gc word: %d\n size: %d\n", *(next_heap_object+1), size);

      memmove(
          *(next_heap_object + 1), // destination is pointer located in gc word
          next_heap_object,        // src is start of heap object
          sizeof(uint64_t) * size);
    }
    next_heap_object += size;
  }
}

void unmark()
{
  dump_heap();
  uint64_t num_arguments;
  uint64_t offset;
  uint64_t *next_heap_object = start_of_heap;
  debugf("next heap at start: %p start of heap: %p end of heap : %p\n ", next_heap_object, start_of_heap, end_of_heap);
  for (uint64_t *p = (uint64_t *)end_of_stack; p < start_of_stack; p++)
  {
    uint64_t current = *p;

    if (if_heap_ptr(current))
    {
      current--;
      dump_heap();
      current += 8;
      *((uint64_t *)current) = 0x0000000000000000;
      debugf("current: %d \n", current);
      dump_heap();
      debugf("check the word!\n");
    }
  }

  while (next_heap_object < heap_cursor)
  {
    dump_heap();

    num_arguments = numargs(next_heap_object, &offset);
    uint64_t size = num_arguments + offset;

    debugf("nextHEap unmark:%p\n", next_heap_object);

    debugf("size %x\n", size);
    if (*(next_heap_object + 1) != 0)
    {
      *(next_heap_object + 1) = 0;
    }
    next_heap_object += size;
  }
}
void gc(int64_t desired_free)
{
  dump_heap();
  mark();
  dump_heap();
  uint64_t* next_heap_cursor = forward();

  dump_heap();
  print_stack();
  update();
  debugf("after update call\n");
  print_stack();
  dump_heap();

  compact();
  heap_cursor = next_heap_cursor;
  unmark();
  // dump_heap();
  //  tidying up
  uint64_t difference = end_of_heap - heap_cursor;
  difference *= 8;
  if (!(difference >= desired_free))
  {
    stopWithError(7);
  }
  else
  {
    for (uint64_t *tracker = heap_cursor; tracker < end_of_heap; tracker++)
    {
      *tracker = 0xbadbadffffbadbad;
    }
    dump_heap();
    debugf("heap cursor: %p\n", heap_cursor);
    return;
  }
}
