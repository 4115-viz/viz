#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int list_len(int* arr){
    int i = 0;
    while(arr[i] != NULL){
        i++;
    }
    //printf("arr[5] = %d\n", arr[5]);
    //arr = (int*) malloc (22 * sizeof(int));
    
    return i;
}
int pop(int* arr){
    int i = list_len(arr);
    int pop = arr[i-1];
    arr[i-1] = NULL;

    return pop;
}
