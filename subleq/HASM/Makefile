# Makefile générique pour les fichiers BP
# Macro génération des executables de validation
# On produit _BPIO.ml à partir des fichiers bp

BPI = $(wildcard *.bpi)
BPO = $(wildcard *.bpo)

.SUFFIXES : .bpi .bpo .bpix .bpox

# Ou se trouve le dossier Bebop pour trouver les librairies

BEBOPDIR = ~/bebop1.0
LINK = $(BEBOPDIR)/boplibs

# biblioteques : selon les fonctions utilisees dans les bp
# standards du compilateur, ou biblioteque bebop
# les biblioteques bebop sont dans $(BEBOPDIR)/libs/

SLIBS = graphics.cmxa
BEBOPLIBS = bplib.cmxa

INCLUDES = $(foreach i, $(BEBOPLIBS), $(LINK)/$i)

# target et compilateur : selon l'option choisie dans les bp

TARGET = ml
PRODUITS = cmx cmi o
COMPILATEUR = ocamlopt
TMP = _tmp

# fichiers produits :

BPIX = $(patsubst %.bpi, %.bpix, $(BPI))
BPOX = $(patsubst %.bpo, %.bpox, $(BPO))

# Apres installation, bopin et bopipe sont dans accessibles dans le PATH

# regles pour generer les executables en passant par 2 compilations

.bpi.bpix :
	bopin $< > $(TMP).$(TARGET)
	$(COMPILATEUR) -o $@ $(SLIBS) -I $(LINK) $(BEBOPLIBS) $(TMP).$(TARGET)
	rm -f $(TMP).$(TARGET)
.bpo.bpox :
	bopipe $< > $(TMP).$(TARGET)
	$(COMPILATEUR) -o $@ $(SLIBS) -I $(LINK) $(BEBOPLIBS) $(TMP).$(TARGET)
	rm -f $(foreach i, $(PRODUITS) $(TARGET), $(TMP).$(i)) 

all : $(BPIX) $(BPOX)
	echo $(BPIX) $(BPOX)

clean :
	rm -f *.cmx *.cmo *.cmi $(TMP).*

clean_exe :
	rm -f $(BPIX) $(BPOX)
