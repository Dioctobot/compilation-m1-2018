#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

void print_int(int64_t n) {
  /*<sujet>
    fprintf(stderr, "Students! This is your job!\n");
    </sujet>*/
  //<corrige>
  printf("%ld", n);
  //</corrige>
}

void observe_int(int64_t n) {
  print_int(n);
}

intptr_t* allocate_block (int64_t n) {
  return (intptr_t*)malloc (n * sizeof (int64_t));
}

intptr_t read_block (intptr_t* block, int64_t n) {
  return block[n];
}

int64_t write_block (intptr_t* block, int64_t n, intptr_t v) {
  block[n] = v;
  return 0;
}
