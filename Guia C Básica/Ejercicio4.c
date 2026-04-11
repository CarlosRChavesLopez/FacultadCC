#include <stdio.h>
#include <stdint.h>


int main(){

    int8_t  i8 = 127;
    int16_t i16 = 32767;
    int32_t i32 = 2147483647;
    int64_t i64 = 922337203799999999;
    
    
    uint8_t ui8 = 255 ;
    uint16_t ui16 = 65535 ;
    uint32_t ui32 = 4294967295 ;
    uint64_t ui64 = 1844674407999999999;

    printf("char(%lu): %d \n", sizeof(i8), i8);
    printf("short(%lu): %d \n", sizeof(i16), i16);
    printf("int(%lu): %d \n", sizeof(i32), i32);
    printf("int(%lu): %ld \n", sizeof(i64), i64);

    printf("char(%lu): %d \n", sizeof(ui8), ui8);
    printf("short(%lu): %d \n", sizeof(ui16), ui16);
    printf("int(%lu): %d \n", sizeof(ui32), ui32);
    printf("int(%lu): %ld \n", sizeof(ui64), ui64);


    return 0;
}
