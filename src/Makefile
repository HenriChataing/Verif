GENERATED = \
	smtlib/smtParser.ml smtlib/smtLexer.ml \
	parsing/parser.ml parsing/lexer.ml

MODULES = \
	common/positions \
	errors options \
	smtlib/smtSyntax \
	smtlib/smtTokens \
	smtlib/smtParser \
	smtlib/smtLexer \
	parsing/environment \
	parsing/tokens \
	parsing/parser \
	parsing/lexer \
	labels cfg \
	front

MAIN = front
EXECUTABLE = verif

INCLUDES = -I parsing -I common -I smtlib
CFLAGS = -dtypes,-g,-I,/usr/lib/ocaml,-I,/usr/lib/ocaml/apron
LFLAGS = -g,-I,/usr/lib/ocaml,-I,/usr/lib/ocaml/apron
BFLAGS = -no-hygiene
BLIBS = bigarray,apron,polkaMPQ

OCAMLBUILD = ocamlbuild
OCAMLLEX = ocamllex
MENHIR = menhir


.PHONY: all clean

all: $(EXECUTABLE)

$(EXECUTABLE): $(MODULES:%=%.ml) $(GENERATED)
	@echo "Building.."
	@$(OCAMLBUILD) $(BFLAGS) -libs $(BLIBS) -cflags $(CFLAGS) -lflags $(LFLAGS) $(INCLUDES) $(MAIN).native
	@mv $(MAIN).native $(EXECUTABLE)

smtlib/smtParser.ml smtlib/smtParser.mli: smtlib/smtParser.mly
	@echo "Generating SmtParser module.."
	@$(MENHIR) --explain --external-tokens SmtTokens $<
	@sed -e 's/4611686018427387903/max_int/' $@ > $@.tmp
	@mv $@.tmp $@

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

