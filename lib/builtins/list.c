#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int list_len_int(int* arr){
    int i = 0;
    while(arr[i] != '\0'){
        i++;
    }
    //printf("arr[5] = %d\n", arr[5]);
    //arr = (int*) malloc (22 * sizeof(int));
    
    return i;
}
int list_len_str(char* arr){
    int i = 0;
    while(arr[i] != '\0'){
        i++;
    }
    //printf("arr[5] = %d\n", arr[5]);
    //arr = (int*) malloc (22 * sizeof(int));
    
    return i;
}
int pop(int* arr){
    int i = list_len_int(arr);
    int pop = arr[i-1];
    arr[i-1] = '\0';

    return pop;
}
void push(int* arr, int value){
    int i = list_len_int(arr);
    int pop = arr[i-1];
    arr[i-1] = '\0';
    printf("in push = %d\n", value);
    
    return;
}
