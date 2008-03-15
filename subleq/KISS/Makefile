.SUFFIXES : .cmx .ml .cmi .cmo .mll .mly
runtestunit : testUnit.cmx bitreader.cmx runtestunit.cmx
	ocamlopt -o runtestunit testUnit.cmx bitreader.cmx runtestunit.cmx
testUnit.cmx : testUnit.ml
	ocamlopt -c testUnit.ml
.ml.cmx : testUnit.cmx
	ocamlopt -c $^
clean : 
	rm -f *.cmo *.cmi *.cmx *.o
	rm -f runtestunit
clean_exe :
	rm -f runtestunit
test : runtestunit
	./runtestunit