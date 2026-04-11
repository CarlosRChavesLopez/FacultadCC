#include <stdio.h>

int main(){
    float f = 3.4;
    double d = 5.2;

    printf("%f",f);
    printf("\n");

    printf("%f",d);
    printf("\n");

    int f_i = (int) f;
    int d_i = (int) d;

    printf("%d",f_i);
    printf("\n");

    printf("%d",d_i);
    printf("\n");

    // se pierde la información despues de la coma

}
