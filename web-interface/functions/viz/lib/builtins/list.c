#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int list_len_int(int* arr){
    int i = 0;

    while(arr[i] != '\0'){
        i++;
    }
    
    return i;
}
int list_len_str(char* arr){
    int i = 0;
    while(arr[i] != '\0'){
        i++;
    }
    
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
    arr[i] = value;
    arr[i+1] = '\0';
    
    return;
}
void print_list(int* arr){
    int i = list_len_int(arr);
    int j;
    printf("[");
    for( j = 0; j < i; j++){
      printf("%d", arr[j]);
      if(j+1 != i){
        printf(",");
      }
   }
    printf("]\n");
    
    return;
}
