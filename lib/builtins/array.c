#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* casting from __ type to int */
int array_len(int* arr){
    int i = 0;
    printf("sizeof =%d\n", sizeof(arr)/sizeof(arr[0]));
    printf("in array print =%d, %d, %d, %d,%d\n", arr[0], arr[1], arr[2], arr[3], arr[4] );
    while(arr[i++] != 0){
        printf("in array print arr = %d\n", arr[i - 1]);
    }
    //name[3]= 55;
    return i;
}
