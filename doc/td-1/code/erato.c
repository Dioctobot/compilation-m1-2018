#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define LIMIT (1ull << 28)

int main (int argc, char** argv) {
  uint64_t size = LIMIT / 64 + 1;
  uint64_t* sieve = (uint64_t*) malloc (size * sizeof(uint64_t));
  uint64_t i, j = 0;
  for (i = 0; i < size; i++) {
    sieve[i] = 0xffffffffffffffff;
  }
  for (i = 2; i <= LIMIT - 1; i++) {
    if ((sieve[i / 64] >> (i % 64)) & 1) {
      j = i;
      j += i;
      while (j <= LIMIT - 1) {
	sieve[j / 64] &= ~(1ull << (j % 64));
	j += i;
      }
    }
  }
  uint64_t x = 0;
  for (i = 2; i <= LIMIT - 1; i++) {
    if ((sieve[i / 64] >> (i % 64)) & 1) {
      x++;
    }
  }
  printf ("%d\n", x);
  exit (EXIT_SUCCESS);
}
