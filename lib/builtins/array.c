#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* casting from __ type to int */
int array_len(int* arr){
    printf("sizeof =%d\n", sizeof(arr)/sizeof(arr[0]));
    printf("in array print name =%d, %d, %d, %d,%d\n", arr[0], arr[1], arr[2], arr[3], arr[4] );
    //name[3]= 55;
    return 99;
}
