FROM ocaml/opam
MAINTAINER Yann Regis-Gianas

USER root
RUN apt-get install -y bc sshpass

USER opam
RUN opam update
RUN opam switch 4.05.0
RUN echo 'Building dependencies'
RUN opam install ocamlfind menhir sexplib ppx_sexp_conv pprint ppx_deriving --use-internal-solver
RUN eval `opam config env`






