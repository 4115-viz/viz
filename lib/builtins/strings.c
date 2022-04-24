#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *string_concat(char *str1, char *str2) {
    
    char buffer[4096];

    // write into buffer
    sprintf(buffer, "%s", str1);
    
    // concat string 2 to string 1
    strcat(buffer, str2);
    
    // malloc size of concatted string
    char *concat_string = (char *) malloc(strlen(buffer));
    
    // copy the buffer contents into the pointer
    strcpy(concat_string, buffer);

    // return the heap allocated pointer
    return concat_string;
}

int str_len(char *str) {
    return strlen(str);
}

char *to_upper(char *str) {
    
    char buffer[4096];
    int i = 0;
    for (int j = 0; j < strlen(str); j++)
    {
        char my_char = str[j];

        // lower case letter, need to make it upper
        if (my_char >= 97 && my_char <= 122) {
            my_char -= 32; // convert to upper case
            buffer[i++] = my_char;
        } else { // not a lower case char just add it
            buffer[i++] = my_char;
        }
    }
    buffer[i] = '\0';
    
    char *heap_string = (char *) malloc(strlen(buffer));

    // copy the buffer contents into the pointer
    strcpy(heap_string, buffer);
    
    return heap_string;
}

char *to_lower(char *str) {
    
    char buffer[4096];
    int i = 0;
    for (int j = 0; j < strlen(str); j++)
    {
        char my_char = str[j];

        // upper case letter, need to make it lower
        if (my_char >= 65 && my_char <= 90) {
            my_char += 32; // convert to lower case
            buffer[i++] = my_char;
        } else { // not a upper case char just add it
            buffer[i++] = my_char;
        }
    }
    buffer[i] = '\0';
    
    char *heap_string = (char *) malloc(strlen(buffer));

    // copy the buffer contents into the pointer
    strcpy(heap_string, buffer);
    
    return heap_string;
}

int string_equals(char *str1, char *str2) {
    if (strcmp(str1, str2) == 0)
        return 1;
    return 0;
}

int string_not_equals(char *str1, char *str2) {
    if (strcmp(str1, str2) == 0)
        return 0;
    return 1;
}

int string_lt(char *str1, char *str2) {
    if (strcmp(str1, str2) < 0)
        return 1;
    return 0;
}

int string_gt(char *str1, char *str2) {
    if (strcmp(str1, str2) > 0)
        return 1;
    return 0;
}

int string_gte(char *str1, char *str2) {
    if (string_equals(str1, str2) == 1)
        return 1;
    if (string_gt(str1, str2) == 1)
        return 1;
    return 0;
}

int string_lte(char *str1, char *str2) {
    if (string_equals(str1, str2) == 1)
        return 1;
    if (string_gt(str1, str2) == 0)
        return 1;
    return 0;
}

