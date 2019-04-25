# "make test" Compiles everything and runs the regression tests

#.PHONY : test
#test : all testall.sh
#	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY : all
all : lol.native builtins.o

.PHONY : lol.native
lol.native:
	rm -f *.o
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis,llvm.bitreader -cflags -w,+a-4 lol.native

builtins.o :
	# gcc -c builtins.c
	gcc -Wall -I/usr/local/include -c builtins.c
	clang -emit-llvm -o builtins.bc -c builtins.c -Wno-varargs


.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf lol.native scanner.ml parser.ml parser.mli
	rm -rf ./*.ll
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.s *.bc *.out *.exe

# More detailed: build using ocamlc/ocamlopt + ocamlfind to locate LLVM

OBJS = ast.cmx sast.cmx codegen.cmx parser.cmx scanner.cmx semant.cmx lol.cmx

lol : $(OBJS)
	ocamlfind ocamlopt -linkpkg -package llvm -package llvm.analysis $(OBJS) -o lol

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

%.cmx : %.ml
	ocamlfind ocamlopt -c -package llvm $<

### Generated by "ocamldep *.ml *.mli" after building scanner.ml and parser.ml
ast.cmo :
ast.cmx :
codegen.cmo : ast.cmo
codegen.cmx : ast.cmx
lol.cmo : semant.cmo scanner.cmo parser.cmi codegen.cmo ast.cmo
lol.cmx : semant.cmx scanner.cmx parser.cmx codegen.cmx ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
semant.cmo : ast.cmo
semant.cmx : ast.cmx
parser.cmi : ast.cmo
