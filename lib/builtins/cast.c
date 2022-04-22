#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

/* casting from __ type to float */
double int_to_double(int num){
  return (double) num;
}

double str_to_double(char *str) {
  return atof(str);
}

/* casting from __ type to string */
char *bool_to_str(int num) {
  char *tr = "true";
  char *fls = "false";

  if (num) 
    return tr;
  else
    return fls;
}

char *int_to_str(int num) {
  char buffer[4096];
  sprintf(buffer, "%d", num);
  char *conversion = (char *) malloc(strlen(buffer));
  strcpy(conversion, buffer);
  return conversion;
}

char *double_to_str(double num) {
  char buffer[4096];
  sprintf(buffer, "%f", num);
  char *conversion = (char *) malloc(strlen(buffer));
  strcpy(conversion, buffer);
  return conversion;
}