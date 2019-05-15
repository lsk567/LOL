#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

// GSL header files
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>

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
  // int length;
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

/*
=====================================================
                      Matrices
=====================================================
*/
/*
The following is an attempt to create an interface between GSL and OCaml.
We will see if we can provide simplified version of GSL functions here for OCaml
to call and store the GSL objects.

To-do:
1. calling native GSL functions
*/

int printm(const gsl_matrix *m)
{
  int status, n = 0;

  for (size_t i = 0; i < m->size1; i++) {
    printf("||\t");
    for (size_t j = 0; j < m->size2; j++) {
      if ((status = printf("%g\t", gsl_matrix_get(m, i, j))) < 0)
        return -1;
      n += status;
    }
    printf("||");

    if ((status = printf("\n")) < 0)
      return -1;
    n += status;
  }
  printf("\n");

  return n;
}

// initializes an empty matrix with a certain number of rows and columns
gsl_matrix * minit(size_t m, size_t n) {
  // allocate matrix memory
  return gsl_matrix_calloc(m, n);
}

// get matrix element
// double gsl_matrix_get(const gsl_matrix * m, const size_t i, const size_t j)
double mget(const gsl_matrix * m, const size_t i, const size_t j) {
  return gsl_matrix_get(m, i, j);
}

// set matrix element
// void gsl_matrix_set(gsl_matrix * m, const size_t i, const size_t j, double x)
void mset(gsl_matrix * m, const size_t i, const size_t j, double x) {
  //printf("<%d,%d>\n",m->size1,m->size2);
  //printf("%d,%d\n",i,j);
  gsl_matrix_set(m, i, j, x);
  //print_matrix(m);
}

// Add. sub. mul. div
int madd(gsl_matrix * a, const gsl_matrix * b) {
  return gsl_matrix_add(a, b);
}

int msub(gsl_matrix * a, const gsl_matrix * b) {
  return gsl_matrix_sub(a, b);
}

int mmulc(gsl_matrix * a, const double x) {
  return gsl_matrix_scale(a, x);
}

int maddc(gsl_matrix * a, const double x) {
  return gsl_matrix_add_constant(a, x);
}

int mmule(gsl_matrix * a, const gsl_matrix * b) {
  return gsl_matrix_mul_elements(a, b);
}

int mdive(gsl_matrix * a, const gsl_matrix * b) {
  return gsl_matrix_div_elements(a, b);
}

// swap rows and columns, transpose, copy
int mswapr(gsl_matrix * m, size_t i, size_t j) {
  return gsl_matrix_swap_rows(m, i, j);
}

int mswapc(gsl_matrix * m, size_t i, size_t j) {
  return gsl_matrix_swap_columns(m, i, j);
}

gsl_matrix * mtrans(gsl_matrix * m) {
  gsl_matrix * newMatrix = gsl_matrix_calloc(m->size2, m->size1);
  gsl_matrix_transpose_memcpy(newMatrix, m);
  return newMatrix;
}

gsl_matrix * mcopy(gsl_matrix * m) {
  gsl_matrix * newMatrix = gsl_matrix_calloc(m->size1, m->size2);
  gsl_matrix_memcpy(newMatrix, m);
  return newMatrix;
}

// Matrix view
gsl_matrix * mgetr(gsl_matrix * m, size_t row) {
  gsl_matrix * newMatrix = gsl_matrix_calloc(1, m->size2);
  gsl_matrix_view v = gsl_matrix_submatrix(m, row, 0, 1, m->size2);
  gsl_matrix_memcpy(newMatrix, &v.matrix);
  return newMatrix;
}

gsl_matrix * mgetc(gsl_matrix * m, size_t col) {
  gsl_matrix * newMatrix = gsl_matrix_calloc(m->size1, 1);
  gsl_matrix_view v = gsl_matrix_submatrix(m, 0, col, m->size1, 1);
  gsl_matrix_memcpy(newMatrix, &v.matrix);
  return newMatrix;
}

gsl_matrix * mgetsub(gsl_matrix * m, size_t x1, size_t y1, size_t x2, size_t y2) {
  if (x1 > x2) {
    printf("Invalid x1 or x2! Expected x1 is less than or equal to x2.\n");
    return NULL;
  }
  if (y1 > y2) {
    printf("Invalid y1 or y2! Expected y1 is less than or equal to y2.\n");
    return NULL;
  }
  int n1 = x2 - x1 + 1;
  int n2 = y2 - y1 + 1;
  gsl_matrix * newMatrix = gsl_matrix_alloc(n1, n2);
  gsl_matrix_view v = gsl_matrix_submatrix(m, x1, y1, n1, n2);
  gsl_matrix_memcpy(newMatrix, &v.matrix);
  return newMatrix;
}

// BLAS
double mdot(gsl_matrix * m1, gsl_matrix * m2) {
  // Both matrices need to have same number of columns and one row
  double* result = malloc(sizeof(float));
  gsl_vector_view v1_view = gsl_matrix_row(m1, 0);
  gsl_vector_view v2_view = gsl_matrix_row(m2, 0);
  gsl_vector * v1 = gsl_vector_alloc(v1_view.vector.size);
  gsl_vector * v2 = gsl_vector_alloc(v2_view.vector.size);
  gsl_vector_memcpy(v1, &v1_view.vector);
  gsl_vector_memcpy(v2, &v2_view.vector);
  gsl_blas_ddot(v1, v2, result);
  double res = *result;
  free(result);
  free(v1);
  free(v2);
  return res;
}



// pipe the operator into SCall to a builtin
// M + N => SCall (concat, M, N) concat \in builtin
// keep Tensor as a class
// use bitcode to translate
