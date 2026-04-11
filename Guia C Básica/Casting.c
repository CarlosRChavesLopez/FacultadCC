#include <stdio.h>

int main(){
    int mensaje_secreto[] = {116, 104, 101, 32, 103, 105, 102, 116, 32, 111,
    102, 32, 119, 111, 114, 100, 115, 32, 105, 115, 32, 116, 104, 101, 32,
    103, 105, 102, 116, 32, 111, 102, 32, 100, 101, 99, 101, 112, 116, 105,
    111, 110, 32, 97, 110, 100, 32, 105, 108, 108, 117, 115, 105, 111, 110};

    size_t length = sizeof(mensaje_secreto) / sizeof(int);
    char decoded[length];

    int length2 = (int) length;

    for (int i = 0; i < length2; i++) {
    decoded[i] = (char) (mensaje_secreto[i]); // casting de int a char
    }

    for (int i = 0; i < length2; i++) {
    printf("%c", decoded[i]);
    }
    printf("\n");

    // length esta intentando saber la cantidad de elementos del array
    // a partir de sizeof se puede conocer el tamaño de todo el array y de un unico int
    // como el array esta lleno de elementos que son ints y cada uno pesa lo mismo
    // al dividir el peso del array por el peso de un int estas dividiendo el total del array por lo que pesa un elemento
    // obteniendo asi la cantidad de elementos
    // otra forma de verlo es haciendo sizeof(int) * nrodeelementos = sizeof(array) <=> nrodeelementos = sizeof(array) / sizeof(int)
}
