#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

// GSL header files
#include <gsl/gsl_sf_bessel.h>

#define MAXFLOATSIZE 50

// experimental function that only prints "hello world"
void printhw(char * str) {
  printf("%s Another hello world haha!\n", str);
  return;
}

void printnewln() {
  printf("\n");
  return;
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
The following is an attempt to create an interface between GSL and OCaml.

We will see if we can provide simplified version of GSL functions here for OCaml
to call and store the GSL objects. 

To-do:
1. calling native GSL functions
2. modularize builtins.c
*/

void gsl_test (char *str) {
	double x = 5.0;
	double y = gsl_sf_bessel_J0 (x); 
	printf ("J0(%g) = %.18e\n", x, y); 
	return;
}
