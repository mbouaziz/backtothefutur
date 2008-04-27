# On utilise la bplib fournie avec bebopframework

LIBDIR = ~/bebop1.0/boplibs/ 
LIB = bplib.cmxa	
OTHER = unix.cmxa graphics.cmxa

kiss : kiss.ml
	ocamlopt -o $@ $(OTHER) -I $(LIBDIR) $(LIB) $<

clean :
	rm -f *.cmx *.cmi *.o

clean_exe : clean
	rm -f kiss