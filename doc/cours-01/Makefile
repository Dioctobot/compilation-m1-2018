.PHONY: clean

all:
	ocamlopt -o marthe unix.cmxa marthe.ml
	ocamlopt -o sum sum.ml

clean:
	rm -f marthe sum *.cmx *.cmi *.cmo *.o *~
