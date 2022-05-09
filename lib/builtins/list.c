#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int list_len(int* arr){
    int i = 0;
    printf("sizeof =%d\n", sizeof(arr)/sizeof(arr[0]));
    printf("in array print =%d, %d, %d, %d,%d\n", arr[0], arr[1], arr[2], arr[3], arr[4] );
    while(arr[i++] != 0){
        printf("in array print arr = %d\n", arr[i - 1]);
    }
    //arr = (int*) malloc (22 * sizeof(int));
    arr[0] =55;
    arr[1] =55;
    arr[2] =55;
    arr[3]= 55;
    arr[4] =55;
    arr[5] =55;
    arr[6] =55;
    arr[20]= 55;
    return i;
}
