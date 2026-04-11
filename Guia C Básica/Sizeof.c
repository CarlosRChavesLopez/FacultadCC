#include <stdio.h>

int main(){
    char c = 100;
    short s = -8712;
    int i = 123456;
    long l = 1234567890;

    printf("char(%lu): %d \n", sizeof(c), c);
    printf("short(%lu): %d \n", sizeof(s), s);
    printf("int(%lu): %d \n", sizeof(i), i);
    printf("int(%lu): %ld \n", sizeof(l), l);



    return 0;
}
