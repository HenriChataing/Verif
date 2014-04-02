GENERATED = parsing/parser.ml parsing/lexer.ml

MODULES = \
	parsing/positions \
  errors options \
  syntax \
  parsing/tokens \
  parsing/parser \
  parsing/lexer \
	front

MAIN = front
EXECUTABLE = verif

INCLUDES = -I parsing
CFLAGS = -dtypes,-g
LFLAGS = -g
BFLAGS = -no-hygiene

OCAMLBUILD = ocamlbuild
OCAMLLEX = ocamllex
MENHIR = menhir


.PHONY: all clean

all: $(EXECUTABLE)

$(EXECUTABLE): $(MODULES:%=%.ml) $(GENERATED)
	@echo "Building.."
	@$(OCAMLBUILD) $(BFLAGS) -cflags $(CFLAGS) -lflags $(LFLAGS) $(INCLUDES) $(MAIN).native
	@mv $(MAIN).native $(EXECUTABLE)

%.ml %.mli: %.mly
	@echo "Generating Parser module.."
	@$(MENHIR) --explain --external-tokens Tokens $*.mly
	@sed -e 's/4611686018427387903/max_int/' $*.ml > $*.tmp
	@mv $*.tmp $*.ml

%.ml: %.mll
	@echo "Generating Lexer module.."
	@$(OCAMLLEX) $*.mll

clean:
	rm -rf $(EXECUTABLE) _build parsing/parser.ml parsing/lexer.ml

