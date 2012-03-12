#(********************************************************************)
#(* Antelatex - Makefile                                             *)
#(********************************************************************)
#(* $Time-stamp: <Malo - 2012>$ *)

SRC=common.ml io.ml preproc.ml grammar.ml trans.ml antelatex.ml
BC=common.cmo io.cmo preproc.cmo grammar.cmo trans.cmo antelatex.cmo 

default: antelatex

all: clean antelatex

antelatex: $(BC)
	ocamlc -o antelatex $(BC)

%.cmi: %.mli
	ocamlc $<

%.cmo: %.ml
	ocamlc -c $<

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmi *.cmo *.cmx *~
	rm -f antelatex


