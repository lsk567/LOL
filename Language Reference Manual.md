# Language Reference Manual

## Table of Contents

- [1. Intro](#1-intro)
- [2. Comments and Whitespace](#2-comments-and-whitespace)
  * [2.1 Comments:](#21-comments-)
  * [Whitespace](#whitespace)
- [3. Types, Operators, and Expressions](#3-types--operators--and-expressions)
  * [Data Types](#data-types)
    + [Primitive Data Type](#primitive-data-type)
    + [The Void Keyword](#the-void-keyword)
  * [Operators](#operators)
    + [Precedence and Associativity](#precedence-and-associativity)
      - [Precedence](#precedence)
      - [Associativity](#associativity)
    + [Assignment Operator](#assignment-operator)
    + [Arithmetic Operators](#arithmetic-operators)
    + [Logical Operators](#logical-operators)
  * [Expressions](#expressions)
- [4. Variables](#4-variables)
  * [Variable Naming](#variable-naming)
  * [Scope of Variables](#scope-of-variables)
  * [Variable Declarations](#variable-declarations)
  * [Variable Assignment](#variable-assignment)
- [5. Statements](#5-statements)
  * [Simple Statements](#simple-statements)
  * [The For Loops](#the-for-loops)
  * [The While Loops](#the-while-loops)
  * [The If Statements](#the-if-statements)
- [6. Functions](#6-functions)
  * [Functions Overview](#functions-overview)
    + [Function Declaration](#function-declaration)
    + [Anonymous Functions](#anonymous-functions)
    + [Calling Functions](#calling-functions)
  * [Functional Programming in LOL](#functional-programming-in-lol)
    + [Pure Functions](#pure-functions)
    + [First-Class Functions](#first-class-functions)
    + [Higher-Order Functions](#higher-order-functions)
- [6. Structures](#6-structures)
  * [List](#list)
    + [Declaring Lists](#declaring-lists)
    + [Defining Lists](#defining-lists)
    + [Lists in Lists](#lists-in-lists)
    + [Accessing List Elements](#accessing-list-elements)
  * [Struct](#struct)
    + [Declaring Structs](#declaring-structs)
    + [Variables of Type Struct](#variables-of-type-struct)
    + [Dot Operator](#dot-operator)
  * [Tensor](#tensor)
    + [Declaring Tensors](#declaring-tensors)
    + [Defining Tensors](#defining-tensors)
    + [The Dimension of Tensor](#the-dimension-of-tensor)
    + [Accessing Tensor Elements](#accessing-tensor-elements)
- [Citation](#citation)

# 1. Intro

# 2. Comments and Whitespace

## 2.1 Comments:

LOL supports both single-line comments and block comments.
Single-line comments are denoted with two forward slashes:

```
// this is a line comment
```

Block comments are written in between /* and */. Block comments can be nested as long as the opening /* and closing */ are matched:

```
/* this is a block comment
*  this is a block comment
*  this is a block comment
*/

/* nested /* block */ comment */
```

## Whitespace

Whitespace, including newline character, tab, and space, is used only to separate tokens and is otherwise ignored by the LOL compiler.

# 3. Types, Operators, and Expressions

## Data Types

### Primitive Data Type

**Int**: an architecture-dependent signed 32-bit integer type that is made up of a sequence of digits representing a number in base 10. A `-` is used to denote negative numbers. It is a right-associative operator.

**float**: an architecture-dependent signed IEEE 64-bit floating-point type. A `-` is used to denote negative numbers. It is a right-associative operator.

**string**: a sequence of characters in ASCII enclosed in double-quotes and excludes the double-quote `“` character. There are no escape sequences. So, for example, you won’t be able to print out double-quote characters by escaping the double-quote, but you can print out two single-quotes if you would like something similar to double-quotes.

**bool**: an expression with a value of true or false.

### The Void Keyword

The type void has no associated value. It allows us to create functions that either do not require any parameters or do not return a value.

## Operators

### Precedence and Associativity

#### Precedence

LOL has well-defined rules for specifying the order in which the operators in an expression are evaluated when the expression has several operators. If there are two or more operators in an expression, then the operator of highest priority will be executed. For example, multiplication and division have a higher precedence than addition and subtraction.
In expression `1+2*5`, multiplication `*` operator will be processed first and then addition. It is because multiplication has higher priority of precedence than addition.

We can change the priority of a LOL operator by enclosing the lower order priority operator in parentheses but not the associativity. In the previous example, if we include parentheses in the expression `(1+2)*5`, then addition will be done first because parentheses has higher priority than multiplication operator.

#### Associativity

When an expression has two operators with the same precedence, the expression is evaluated according to its associativity. For example, `2*2*3` is treated as `(2*2)*3` since the `*` operator has right-to-left associativity.

### Assignment Operator

The equal sign `=` is used for assignment. It has right-to-left associativity.

### Arithmetic Operators
- `+` is used for addition in the traditional mathematical sense for int and float types:
  ```
  int a = 2;
  int b = 3;
  int sum = a + b; // sum: 5
  ```
  In addition, it can be used to concatenate two strings:
  ```
  string first = “hello ”;
  string second = “world”;
  string result = first + second; // result: “hello world”
  ```

- `-` is used for subtraction in the traditional mathematical sense for int and float types.

- `*` is used for multiplication in the traditional mathematical sense for int and float types.

- `/` is used for division in the traditional mathematical sense for int and float types. If the dividend should not be 0.

- `.` is used for inner product in the traditional mathematical sense for vectors:

  ```
  Tensor v1 = [1 2 3];
  Tensor v2 = [4 5 6];
  Tensor v3 = v1.v2; // v3=[32]
  ```

- `@` is used for outer product in the traditional mathematical sense for vectors

- `^` is used for division in the traditional mathematical sense for int and float types, and tensor object.

- `==` is used for checking equality. This boolean operator works for int, float and string types.

- `!=` is used for checking inequality. This boolean operator works for int, float and string types, and other defined structs.

- `<`, `>`, `>=`, `<=` work only for int and float types.

### Logical Operators

`!` is used for *NOT* in boolean expressions. `AND` and `OR` are reserved for keywords in boolean expressions.

## Expressions
An expression consists of at least one operand and zero or more operators. Operands are typed objects such as constants, variables, and function calls that return values. Below are some examples:

```
47
2 + 2
```

For parentheses group subexpressions, innermost expressions are evaluated first:

```
(2 * (3 + 5) )
```

# 4. Variables

## Variable Naming

All variable names must follow [a-zA-Z][a-zA-Z0-9]*. Reserved words are not allowed. Reserved words are as follows:
- int
- float
- string
- bool, true, false
- List,Tensor
- if, elif, else
- for, while
- func
- return
- void
- in
- struct
- AND, OR,
- var (optional)

## Scope of Variables

LoL is a statically scoped language which supports both local variables and global variables. Variables declared inside blocks (e.g. functions, loops, if statements) exist only inside the block. If there is a variable with the same name already declared, this new variable will override the previous one in the scope of its block.

Variables declared outside of blocks have global scope. The global variables can be accessed anywhere in the program following their declaration.

Multiple variables and functions of the same name, even of different types, can not be declared in the same scope.

## Variable Declarations

LOL is a strong typed language. Each variable has a type specified at the time of declaration. The type must be one of the primitive data types or struct specified above.

## Variable Assignment

The equal operator `=` is used for assignment. It sets the the value of the expression on the right-hand side of `=` to the variable on the left-hand side of `=`:

```
Tensor v1 = [1 2 3];
```

The type of the expression on the right-hand side must be consistent with the type of the variable on the left-hand side.

# 5. Statements

## Simple Statements
Statements are terminated with semicolons `;`. A semicolon cannot be by itself. It must be preceded by a statement. A statement can only be assignment, call, increment or decrement.

## The For Loops
For loops can be used to specify iteration and looping behavior.

A for loop statement has a header and a body. The header consists of three expressions separated by semicolons. Both parts (the header and the body) are mandatory. The header has an initialization statement, a testing condition, an increment/decrement expression.

The initialization statement is evaluated one time only. It is evaluated before you test the test condition for the first time. You can declare variables in this statement or have an expression here (or even any statement). This initialization statement is optional. If you choose to omit the initialization statement, you still must have one semicolon denoting where it would have ended.

The test condition is tested before you enter the loop the first time and then every time after the increment/decrement expression is evaluated until the test condition is false. This test statement is optional. If you choose to omit the test statement, you still must have one semicolon denoting where it would have ended.

Finally, the increment/decrement expression is evaluated after each loop iteration, before the test condition is tested again. This part is optional. If you choose to omit the increment/decrement expression, you still must have one semicolon denoting where it would have ended.

The loop body can contain any number of statements, including zero statements. The brackets are always required, even if the loop body is empty. A loop iteration consists of evaluating the statements in the loop body.

Example:

```
int sum = 0;
for (int i = 0; i < 10; i = i + 1) {
    sum = sum + i;
}
```

The initialization statement, test condition, and increment/decrement expression can all be omitted to get an infinite loop. Note, there is no break statement so it is often best to write potentially infinite loops inside of functions so you can use return statements to stop them.

```
for ( ; ; ) {
    // infinite loop
}
```

## The While Loops

While loops repeatedly execute a block of code as long as the test condition is true. A while loop statement has a header followed by curly braces that contain the body of the while loop, which is the block of code that is executed as long as the condition in the header is true. The header has the keyword while followed by parentheses enclosing the condition to test, which should evaluate to either true or false.

Example:

```
int i = 0;
while (i < 3) {
    i++;
}
```

Unlike the for loops, the test condition of while loop cannot be left empty.

## The If Statements

The if statement supports conditional execution. The if statement has a boolean expression in parenthesis followed by curly braces for the body. The body of the if statement can be zero or more statements. The if statement can then be followed by any number of optional elif statements, which must have a boolean expression specified inside its parenthesis, just like an if statement, and can contain zero or more statement in its body, which must be enclosed by curly braces as well. The elif statement (or the if statement if elif statements were omitted) can be followed by a single, optional else statement. The else statement does not have a boolean expression in parenthesis. The word "else" is simply followed by the curly braces for its body. If the boolean expression for the given statement evaluates to true, then the body associated with that expression is evaluated, and then any following elif/else statements are skipped and the next statement is evaluated. The parentheses and braces are always required.

Example:
```
if (x > 0) {
    println("x is positive"); // println is a built-in function.
} elif (x == 0) {
    println("x is zero");
} else {
    println("x is negative");
}
if (x == 0) {
    x = x + 5;
}
```

# 6. Functions

## Functions Overview

### Function Declaration

The keyword `func` is reserved for declaring function variables. To declare a variable of type `func`, you must include the keyword `func`, followed by the return type of the this function. Please note that if the function does not have any return value, please declare void as the return type.

After the return type is declared, you need to name your function. It is recommended to have function names in lowercase, as some uppercase letters are reserved for mathematical operations. After a function name is given, a set of parentheses needs to follow in which you can specify the parameters to be passed into this function. If a function does not take any parameter, simply leave the parentheses as they are.

In the function body, you can declare variables within the function scope, provide statements, and specify return value with the `return` keyword, if necessary.

Please note, a function declaration does not require a semicolon after the closing bracket.

A function definition has this form:
```
func return-type function-name (parameter declarations, if any) {
    declarations
    statements
    return statement, if any
}
```

Here are some examples:
```
// a function named “foo”, which takes an integer and returns a new integer
func int foo (int a) {
    return a + 1;
}

// a function named “bar”, which does not take any parameter and always returns the string “lol”
func string bar () {
    return “lol”;
}

// a function named “print_tensor”, which takes a tensor object and prints it out
func void print_tensor (Tensor t) {
    println(t);
}
```

### Anonymous Functions

It is very easy to declare anonymous functions in LOL. All one needs to do is to declare a normal function without a name. To declare an anonymous function, start with the `func` keyword, then specify the return type (`void` if nothing is returned) and parameters. The function body is declared the same way as a named function.

Anonymous function is useful for one-time-use functions, inline functions, as well as first-class functions, which will be covered in functional programming.

An anonymous function definition has this form:
```
func return-type (parameter declarations, if any) {
    declarations
    statements
    return statement, if any
}
```

Here are some examples.

```
// a simple hello world
func void () { println(“hello world!”) }

// assign a function to a function-type variable
func addOne = func int (int x) { return x + 1 }
```

### Calling Functions

To call a function, put the function name followed by parentheses. Inside the parentheses, put the arguments for the function. The arguments you provide need to match the type and number of arguments that your function expects. Finally, put a `;` at the end of this expression.

Here is an example.
```
// declare a function definition
func int addTwo (int a) {
    return a + 2;
}

println(addTwo(3));
// Output: 5
```

## Functional Programming in LOL

LOL supports many features for functional programming. Applying functional programming principles in LOL can lead to improved code quality,  faster debugging, strong logical support, and ease of testing. Below are functional programming elements supported and recommended by LOL. Functional programming is not strictly enforced in LOL. Programmers can choose between functional and imperative programming depending on their needs.

### Pure Functions

A pure function is a function which:
1. Given the same input, will always return the same output;
2. Produces no side effects.

Consider the code snippet below:
```
float PI = 3.14;

func float calculate_area (float r) {
    return r * r * PI;
}

println(calculate_area(10.0));
```

The above function is impure because it depends on a global variable, which is subject to mutation. When the value for global variable changes, the function calculate_area returns a different value given the same input, which violates the first rule of pure function.

Consider the modified code:

```
float PI = 3.14;

func float calculate_area (float r, float pi) {
    return r * r * pi;
}

println(calculate_area(10.0, PI));
```

The function calculate_area now becomes pure, since given the same inputs, it returns the same output.

### First-Class Functions

First class function is an essential element of functional programming. The idea of functions as first-class entities is that functions are also treated as values and used as data. Functions as first-class entities can:
1. refer to it from constants and variables
2. pass it as a parameter to other functions
3. return it as result from other functions

Suppose we have the following two functions:
```
func float divideNum (float a, float b) {
    return a / b;
}

func float multipleNum (float a, float b) {
    return a * b;
}
```

Since LOL supports first-class functions, meaning that functions can be treated and passed like value, we can combine the above functions into one:

```
func float ops(func f, float a, float b) {
    return func (a, b);
}

println(ops((/), 2.0, 3.0));
println(ops((*), 2.0, 3.0));
```

### Higher-Order Functions

A higher-order function is a function that either:
1. takes one or more functions as arguments, or
2. returns a function as its result

The ops function we implemented above is a higher-order function because it takes an operator function as an argument and uses it.

# 6. Structures
## List

A list is a container of primitive types, structs, tensors, functions, or other lists.

### Declaring Lists

The declaration and definition of lists can happen in one statement or two. See next section below about how to do declare and define lists in one statement.

The type of the elements in an list is specified at the time of definition. The size of the list is not fixed and will grow dynamically. Lists are declared using the keyword List, followed by arrow brackets, which contain the type of the elements in the list, `<type>`, the list name, and a semicolon. The semicolon can be omitted if the list is being declared and defined in one line.

```
/* Declare a variable of type List<int>.
It cannot be used until it is initialized. */
List<string> hello_list;
```

### Defining Lists

You never define the size of the list when you declare an list. Lists are not fixed in size. To define an list, you simply define the elements of the list explicitly. The elements are put in square brackets and are separated by commas. There can be zero or more elements in the brackets (i.e. the brackets can be empty). The last element in the list of elements is not followed by a comma. The whole statement is terminated by a semicolon.

```
/* Declare an list and specify the elements in the list explicitly during definition all in one statement. */
List<string> hello_list = [“Hello”, “World”, “!”];

// Defining the elements in the list explicitly after declaring the list.
List<string> hello_list;
hello_list = [“Hello”, “World”, “!”];
```

### Lists in Lists

To declare an list inside an list, you have two options. You can either define the values of all of the lists explicitly using the bracket syntax explained in 4.2 Defining lists. Example:

```
List<List<int>> foo = [
[1,2,3,4],
[11,12,13,14]
];
```

Or

```
List<string> hi_list = [“hi”];
List<string> hello_list = [“Hello”, “World”, “!”];
List<List<string>> hello_hi = [hi_list, hello_list ];
```

Additionally, you can have a struct with an list as a member so you can achieve nested lists by having lists of structs that have lists as members.

### Accessing List Elements

To access an element in an list, you use the variable name for that list followed by square brackets with an integer expression inside the square brackets as the index of the desired element. For nested lists, square brackets with indices in them are concatenated to get to the element at the desired layer of the nested list. Accessing elements beyond the size of the list causes undefined behavior. All indices start at 0.

```
List<int> a = [34, 235, 2534, 435];
// a[2] == 2534 is true
List<List<int>> foo = [
   [1,2,3,4],
   [11,12,13,14]
];
// foo[0][2] == 3 is true
```

## Struct
A struct is a collection of grouped variables, where the variable types can be any primitive type, lists, tensors, functions, or other structs.

### Declaring Structs
Structs are defined using the keyword struct, then the name, which must start with a capital letter and then can contain any combination of letters and numbers, all followed by `{ }` containing the body of the struct. The body of the struct can only contain variables declarations, which are possibly defined in the same statement, terminated by semicolons. The variables are optionally initialized to default values, which occur if the optional definition of the member fields are present. These default values cannot be accessed until a variable of the struct type is initialized. These default values are fresh every time a new struct of the type is created (i.e. if you have a function evaluated as a default value, it is called anew every time you create a struct of that type). The body of the struct can also be empty, but the `{ }` are still mandatory. Example:

```
struct BadStruct {
  int x;
  x = 4; // This is not allowed. Declaration and definition must happen in one line.
}

struct GoodStruct{ // Declare the struct.
  int a = 0; // Default values are optional.
  int b;
}

// Have a function as a default value in a struct.
func int myFunction(int y){
    return y+5;
}

struct GoodStruct2{
  /* This memeber is of type func that takes an int and returns an int and is
  initialized to myFunction. myFunction must be declared before this statement.*/
  Optional: func(int, int : int) field1 = myFunction;
  int field2;
}

struct GoodStruct3{} // Empty struct
```

### Variables of Type Struct

If the variable is of type struct, the struct type must be declared before the variable of that type is defined. The struct name, without the keyword struct, will be used to identify the type of the variable. Even though the variable is declared, its members cannot be accessed before it is defined, as explained in section 7.b.iii Defining Variables of Type Struct.

```
struct GoodStruct{ // Declare the struct.
  int a = 0; // Default values are optional.
  int b;
}
GoodStruct gs; // Declare a variable of type GoodStruct.
Defining Variables of Type Struct
A variable of type struct must be defined in the following way before it is accessed. Accessing a struct that has not been defined leads to undefined behavior.
Variables of type struct can be defined by writing the field name and then assigning it to a value followed by a comma. The last assignment is not followed by a comma. This definition must have a semicolon following its closing curly brace.

struct GoodStruct{ // Declare the struct.
  int a = 0;
  int b;
}
GoodStruct gs = {a = 0; b = 1}; // Define the variable by initializing its fields.
```

### Dot Operator

You can access the element of a struct using the dot operator `.`. You need to specify the name of the variable that is of the struct type and the fields within the struct that you would like to access or change.
The dot operator is left associative.

```
struct GoodStruct{ // Declare the struct.
  int a = 0;
  int b;
}
GoodStruct gs = {a = 0; b = 1};
// gs.a == 0 is true
```

## Tensor

A tensor is an object that is close to its mathematical equivalent.

### Declaring Tensors

The declaration and definition of lists can happen in one statement or two. See next section below about how to do declare and define lists in one statement.

The type of the elements in a tensor is float. The size of the tensor is fixed at the time of definition. Tensors are declared using the keyword Tensor, followed by the tensor name, and a semicolon. The semicolon can be omitted if the list is being declared and defined in one line.

```
/* Declare a variable of type Tensor. It cannot be used until it is initialized. */
Tensor t;
```

### Defining Tensors

You automatically define the size of the tensor when you define a tensor. Tensors are fixed in size. To define a tensor, you simply define the elements of the tensor explicitly. The elements are put in square brackets and are separated by space `( )`. There can be zero or more elements in the brackets (i.e. the brackets can be empty). The last element in the list of elements is not followed by a space. The whole statement is terminated by a semicolon.

```
/* Declare a tensor and specify the elements in the tensor explicitly during definition all in one statement. */
Tensor t = [0];
// Defining the elements in the tensor explicitly after declaring the tensor.
Tensor t;
t = [0];
```

### The Dimension of Tensor

A tensor can have a dimension of any non-negative supported integer. To declare a tensor with a dimension higher than 1, you can either define all values of the tensor explicitly using the bracket syntax explained in 7.c.ii Defining Tensors. Example:

```
Tensor t = [[1 2 3 4] [11,12,13,14]];
```

Or

```
Tensor t1 = [1];
Tensor t2 = [2];
Tensor t3 = [t1 t2];
```
Note in this case, the shape of t1 and t2 must be identical.

### Accessing Tensor Elements

To access an element in a tensor, you use the variable name for that tensor followed by square brackets with an integer expression inside the square brackets as the index of the desired element. For tensors with dimension higher than 1, square brackets with indices in them are concatenated to get to the element at the desired layer of the tensor. Accessing elements beyond the size of the tensor causes undefined behavior. All indices start at 0.

```
Tensor t = [5345 45 3245 435  345];
// a[3] == 435 is true
Tensor t = [[1 2 3 4] [11,12,13,14]];
// foo[0][2] == 3 is true
```


# citation

- [LRM of shoo-lang](https://github.com/sam-jay/shoo-lang/blob/master/Language_Reference_Manual.md)
