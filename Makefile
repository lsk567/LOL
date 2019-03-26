# "make test" Compiles everything and runs the regression tests

#.PHONY : test
#test : all testall.sh
#	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY : all
#all : lol.native printbig.o
all: lol.native

# "make lol.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

lol.native :
	opam config exec -- \
	ocamlbuild lol.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff
