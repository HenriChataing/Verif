GENERATED = parsing/parser.ml parsing/lexer.ml

MODULES = \
	parsing/positions \
  errors options \
  syntax \
  parsing/tokens \
  parsing/parser \
  parsing/lexer \
	front

EXECUTABLE = main

SRC_INCLUDES = -I parsing
INCLUDES = $(SRC_INCLUDES)
BFLAGS = -dtypes -g $(INCLUDES)
OFLAGS = -g $(INCLUDES)
BLIBS =
OLIBS =

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
MENHIR = menhir

.PHONY: all clean depend pprint

all: $(EXECUTABLE)

$(EXECUTABLE): $(MODULES:%=%.cmo)
	$(OCAMLC) $(BFLAGS) -o $(EXECUTABLE) $(BLIBS) $(MODULES:%=%.cmo)

$(EXECUTABLE).opt: $(MODULES:%=%.cmx)
	$(OCAMLOPT) $(OFLAGS) -o $(EXECUTABLE) $(OLIBS) $(MODULES:%=%.cmx)

%.cmo: %.ml
	$(OCAMLC) $(BFLAGS) -c $*.ml
%.cmi: %.mli
	$(OCAMLC) $(BFLAGS) -c $*.mli
%.cmx: %.ml
	$(OCAMLOPT) $(OFLAGS) -c $*.ml

%.ml %.mli: %.mly
	$(MENHIR) --explain --external-tokens Tokens $*.mly
	sed -e 's/4611686018427387903/max_int/' $*.ml > $*.tmp
	mv $*.tmp $*.ml

%.ml: %.mll
	$(OCAMLLEX) $*.mll

clean:
	rm -f $(EXECUTABLE) $(EXECUTABLE).opt parsing/parser.ml parsing/lexer.ml *.cm[iox] **/*.cm[iox] **/*.o *.annot **/*.annot

.depend: $(GENERATED) Makefile
	$(OCAMLDEP) $(SRC_INCLUDES) $(patsubst %, %.ml, $(MODULES)) \
		$(patsubst %, %.mli, $(MODULES)) $(GENERATED) > .depend

depend:
	rm -f .depend
	$(MAKE) .depend

include .depend

