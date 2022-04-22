#include <stdio.h>
#include <stdlib.h>

/* casting from __ type to int */
int double_to_int(double num){
  return (int) num;
}

int bool_to_int(int num){
  return (int) num;
}

int str_to_int(char *str) {
  return atoi(str);
}

double int_to_double(int num){
  return (double) num;
}

/*
int main() {
    printf("before cast\n");
    int new_val = double_to_int(5.0);
    printf("after cast: %d\n", new_val);

    printf("before cast\n");
    float new_val_2 = int_to_double(5);
    printf("after cast: %f\n", new_val_2);
}
*/