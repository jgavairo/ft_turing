NAME := ft_turing
SRC_DIR := src

SOURCES := $(wildcard $(SRC_DIR)/*.ml)
SORTED_SOURCES := $(shell ocamlfind ocamldep -sort -I $(SRC_DIR) $(SOURCES))

OCAMLC := ocamlfind ocamlc
OCAMLFLAGS := -package yojson -linkpkg -I $(SRC_DIR)

all: $(NAME)

$(NAME): $(SOURCES)
	$(OCAMLC) $(OCAMLFLAGS) -o $(NAME) $(SORTED_SOURCES)

clean:
	rm -f $(SRC_DIR)/*.cmi $(SRC_DIR)/*.cmo $(SRC_DIR)/*.cmx $(SRC_DIR)/*.o

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
