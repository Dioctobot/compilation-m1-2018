#include <stdio.h>
#include <stdlib.h>

typedef void* any_t;

#define as_int(x) (((int*)x)[0])

any_t any_int (int x) {
  any_t a = (int*) malloc (sizeof(int));
  as_int(a) = x;
  return a;
}

typedef struct closure closure_t;

typedef any_t (*function_ptr_t) (closure_t* closure, any_t args);

typedef struct closure {
  any_t* free_varaibles;
  function_ptr_t code;
} closure_t;


any_t g_code(closure_t* close, any_t x) {
  int vx = as_int(x);
  int vy = as_int(close->free_varaibles[0]);
  int vz = as_int(close->free_varaibles[1]);
  int v =  vx + vy + vz;
  printf("%d %d %d %s\n", vx, vy, vz, v);
  return any_int(v);
}

any_t f0_code(closure_t close, any_t z) {
  int y = ((int *) z)[0] * 2;
  closure_t* c = (closure_t*) malloc (sizeof (closure_t));
  c->code = g_code;
  c->free_varaibles = (any_t*) malloc (sizeof (any_t) * 2);
  c->free_varaibles[0] = any_int (y);
  c->free_varaibles[1] = z;
  return c;
}

int main() {
  closure_t* f0 = (closure_t*) malloc (sizeof (closure_t));
  f0->code = f0_code;

  closure_t* h = f0->code(f0, any_int(3));
  
  printf("%d\n", as_int(h->code(h, any_int(4))));
  exit(EXIT_SUCCESS);
}