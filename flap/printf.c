#include <stdio.h>
#include <stdlib.h>

/*int print(int a, int b, int c, int d, int e, int f, int g, int h)
{
    
    return printf("a = %d, b = %d, c = %d, d = %d, e = %d, f = %d, g = %d, h = %d\n", a, b, c, d, e, f, g, h);
}
*/
int x = 0;

int print(int a, int b, int c, int d, int e, int f, int g, int h) { return a; }

int main() {
    /*print (x, 2, 3, 4, 5, 6, 7, 8)*/
    int y = 1, a = 2, b = 3, c = 4;
    exit (print(y, 2, 3, 4, 5, 6, 7, 8));
}