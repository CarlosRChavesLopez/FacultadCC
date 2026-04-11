#include <stdio.h>

int main(){
    // unsigned int ui = 71U;
    // signed long int sli = 9223372036854775807L;
    // unsigned long long int ui = 18446744073709551615ULL;

    // const int MAX = 100;
    // const float PI = 3.14159;
    // const int i = 1;
    // i = 2; error i es un const-qualified

    enum Color {RED, GREEN, BLUE};
    enum Color c = RED;
    printf("%d\n", c);
    printf("%d\n", GREEN);
    printf("%d\n", BLUE);

    enum Color2 {RED2 = 1, GREEN2 = 2, BLUE2 = 4};
    enum Color2 c2 = RED2;
    printf("%d\n", c2);
    printf("%d\n", GREEN2);
    printf("%d\n", BLUE2);


    
}
