#include <stdio.h>

int print(int a, int b, int c, int d, int e, int f, int g, int h)
{
    
    return printf("a = %d, b = %d, c = %d, d = %d, e = %d, f = %d, g = %d, h = %d\n", a, b, c, d, e, f, g, h);
}

int main() {
   return (print (1, 2, 3, 4, 5, 6, 7, 8));
}