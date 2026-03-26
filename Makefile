NAME := ft_turing
BYTE_NAME := $(NAME).byte
SRC_DIR := src
DEPS_STAMP := .deps_checked

SOURCES := $(wildcard $(SRC_DIR)/*.ml)
SORTED_SOURCES := $(shell ocamlfind ocamldep -sort -I $(SRC_DIR) $(SOURCES))

OCAMLC := ocamlfind ocamlc
OCAMLOPT := ocamlfind ocamlopt
OCAMLFLAGS := -package yojson -linkpkg -I $(SRC_DIR)

.DEFAULT_GOAL := all

all: $(DEPS_STAMP) $(NAME)


$(DEPS_STAMP):
	@command -v ocamlfind >/dev/null 2>&1 || (echo "Installing findlib via opam..." && opam install -y ocamlfind)
	@ocamlfind query yojson >/dev/null 2>&1 || (echo "Installing yojson via opam..." && opam install -y yojson)
	@touch $(DEPS_STAMP)

check-deps: $(DEPS_STAMP)

$(NAME): $(SOURCES)
	$(OCAMLOPT) $(OCAMLFLAGS) -o $(NAME) $(SORTED_SOURCES)

byte: $(BYTE_NAME)

$(BYTE_NAME): $(SOURCES)
	$(OCAMLC) $(OCAMLFLAGS) -o $(BYTE_NAME) $(SORTED_SOURCES)

native: $(NAME)

clean:
	rm -f $(SRC_DIR)/*.cmi $(SRC_DIR)/*.cmo $(SRC_DIR)/*.cmx $(SRC_DIR)/*.o

fclean: clean
	rm -f $(NAME) $(BYTE_NAME) $(DEPS_STAMP)

re: fclean all

.PHONY: check-deps byte native clean fclean re
