RM_FILES=bopway.cmx bopway.cmo bopway.cmi bopway.o

all: bopway

bopway: bopway.ml
	ocamlopt unix.cmxa $< -o $@

clean:
	rm -f $(RM_FILES)

cleanall: clean cleanexe

cleanexe:
	rm -f bopway
	