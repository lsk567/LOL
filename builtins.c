#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#define MAXFLOATSIZE 50

typedef struct List_element {
    struct List_element *next;
    struct List_element *prev;
    void *data;
} list_element;

typedef struct List {
    int32_t length;
    list_element *head;
    list_element *tail;
} list;

/*
=====================================================
                   LIST FUNCTIONS
=====================================================
*/
//init list
list* list_init() {
    list* l = (list *) malloc(sizeof(list));
    l->length = 0;
    l->head = NULL;
    l->tail = NULL;
    return l;
}

//access element
void* list_get(list *l, int index) {
    if(index >= l->length) {
        return NULL;
    }
    list_element *curr = l->head;
    int count = 0;
    while(count < index) {
        curr = curr->next;
        count++;
    }

    return curr->data;
}

//set element
list_element* list_set(list *l, void *data, int index) {
    if(index >= l->length) {
        return NULL;
    }
    list_element *curr = l->head;
    int count = 0;
    while(count < index) {
        curr = curr->next;
        count++;
    }
    free(curr->data);
    curr->data = data;
    return curr;
}

//append element: can be any type
void list_append(list *l, void *data) {
    list_element *le = (list_element*) malloc(sizeof(list_element));
    le->data = data;
    le->next = NULL;
    le->prev = l->tail;
    if(l->tail) {
        l->tail->next = le;
    }
    else { //empty list
        l->head = le;
    }
    l->tail = le;
    l->length += 1;

    //return le;
}

//concatenate lists
list* list_concat(list *l1, list *l2) {
    list *new_list = list_init();
    list_element *curr = l1->head;
    while(curr) {
        list_append(new_list, curr->data);
        curr = curr->next;
    }
    curr = l2->head;
    while(curr) {
        list_append(new_list, curr->data);
        curr = curr->next;
    }
    return new_list;
}

/* assumes that the list contains the element */
void list_remove(list *l, list_element *e) {
    if(l->head == e) {
        l->head = e->next;
    }
    else {
        e->prev->next = e->next;
    }
    if(l->tail == e) {
        l->tail = e->prev;
    }
    else {
        e->next->prev = e->prev;
    }
    l->length--;
    free(e);
}

int32_t list_length(list *l) {
    return l->length;
}

// assumes the str that's based in is correctly null terminated
void println(char * str){
  printf("%s\n",str);
  return;
}

// assumes the str that's based in is correctly null terminated
void print(char * str){
  printf("%s",str);
  return;
}

char* str_of_int(int x) {
  int length = snprintf( NULL, 0, "%d", x );
  char* str = malloc( length + 1 );
  snprintf( str, length + 1, "%d", x );
  return str;
}

char* str_of_bool(int x) {
  int length;
  char* str;
  if (x) {
    str = malloc(sizeof("true") + 1);
    snprintf(str, sizeof("true") + 1, "%s", "true");
  } else {
    str = malloc(sizeof("false") + 1);
    snprintf(str, sizeof("false") + 1, "%s", "false");
  }
  return str;
}

char* string_concat(char * str1, char* str2) {
  int totalLength = strlen(str1) + strlen(str2) + 1;
  char* result = calloc( totalLength, 1 );
  strcat(result, str1);
  strcat(result, str2);
  return result;
}

int string_equals(char * str1, char* str2) {
  int res = strcmp(str1, str2);
  if (res == 0) {
    return 1;
  }
  return 0;
}

//BUT TECHNICALLY IT"S A C DOUBLE TYPE NOT C FLOAT TYPE
char* str_of_float(double x) {
  char* str;
  str = (char *) malloc( sizeof(char) * MAXFLOATSIZE);
  snprintf(str, MAXFLOATSIZE, "%g", x);
  return str;
}

// rounds float to nearest int
int int_of_float(double x){
  return (int) round(x);
}

double float_of_int(int x){
  return x * 1.0;
}

// returns -1 if the string is not a valid integer.
// expects a null terminated string -- if the string is not
// null terminated, undefined behavior will occur.
// If the integer is beyond INT_MAX as defined by C, that is, 2147483647,
// overflow may occur and the result is undefined.
int int_of_str(char *str){
  // reference: https://www.geeksforgeeks.org/write-your-own-atoi/
  int res = 0;  // Initialize result
  int sign = 1;  // Initialize sign as positive
  int i = 0;  // Initialize index of first digit
  // If number is negative, then update sign
  if (str[0] == '-'){
    sign = -1;
    i++;  // Also update index of first digit
  }

  // Iterate through all digits and update the result
  for (; str[i] != '\0'; ++i) {
    if(str[i]<48 || str[i]>57) {
       // not a digit
       return -1;
    }
    res = res*10 + str[i] - '0';
  }
  // Return result with sign
  return sign*res;
}
