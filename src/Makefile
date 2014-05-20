GENERATED = \
	parsing/parser.ml parsing/lexer.ml

MODULES = \
	common/logger common/positions \
	common/primitive common/types common/errors \
	syntax/vars syntax/smtlib syntax/expressions \
	parsing/tokens parsing/parser parsing/lexer \
	analysis/generics analysis/horn analysis/analysis \
	options front

MAIN = front
EXECUTABLE = verif

INCLUDES = -I parsing -I common -I analysis -I syntax
CFLAGS = -dtypes,-g,-I,/usr/lib/ocaml,-I,/usr/lib/ocaml/apron
LFLAGS = -g,-I,/usr/lib/ocaml,-I,/usr/lib/ocaml/apron
BFLAGS = -no-hygiene
BLIBS = bigarray,gmp,apron,boxMPFR,polkaMPQ

OCAMLBUILD = ocamlbuild
OCAMLLEX = ocamllex
MENHIR = menhir


.PHONY: all clean

all: $(EXECUTABLE)

$(EXECUTABLE): $(MODULES:%=%.ml) $(GENERATED)
	@echo "Building.."
	@$(OCAMLBUILD) $(BFLAGS) -libs $(BLIBS) -cflags $(CFLAGS) -lflags $(LFLAGS) $(INCLUDES) $(MAIN).native
	@mv $(MAIN).native $(EXECUTABLE)

parsing/parser.ml parsing/parser.mli: parsing/parser.mly
	@echo "Generating Parser module.."
	@$(MENHIR) --explain --external-tokens Tokens $<
	@sed -e 's/4611686018427387903/max_int/' $@ > $@.tmp
	@mv $@.tmp $@

%.ml: %.mll
	@echo "Generating Lexer module.."
	@$(OCAMLLEX) $*.mll

clean:
	rm -rf $(EXECUTABLE) _build **/*.conflicts
	rm -f $(GENERATED) $(GENERATED:%.ml=%.mli)

