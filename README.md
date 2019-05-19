# LOL
The Linear Algebra Oriented Programming Language

# Requirements
- LLVM 7.0.0 or higher
- OCaml 4.05.0 or higher
- Clang
- OPAM
- GSL

# Usage

Build the compiler binary using make. Sometimes a clean is needed.

```
$ make clean
$ make
```

To compile, into LLVM:

`$ ./lol.native <filename>`

To compile and execute a program:

`$ ./run.sh <filename>`

To run tests:

`$ ./testall.sh`
