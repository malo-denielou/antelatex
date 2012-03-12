#(********************************************************************)
#(* Antelatex - Makefile                                             *)
#(********************************************************************)
#(* $Time-stamp: <Pierre-Malo Denielou - 2012>$ *)


BINDIR := /usr/local/bin

VERSION := `cat VERSION`


all: byte native

src/version.ml: VERSION
	echo 'let version ="' | cat - VERSION > src/version.ml
	echo '"' >> src/version.ml

native: src/version.ml
	ocamlbuild src/antelatex.native

byte: src/version.ml
	ocamlbuild src/antelatex.byte

clean: 
	ocamlbuild -clean
	rm -f src/version.ml
	rm -f *~
	rm -f src/*~
	rm -f ex/test.tex

install: install-native

install-native:
	cp _build/src/antelatex.native $(BINDIR)/antelatex

install-byte:
	cp _build/src/antelatex.native $(BINDIR)/antelatex

test: test-native

test-native:
	./antelatex.native -g ex/grammar.gra ex/test.ant

test-byte:
	./antelatex.byte -g ex/grammar.gra ex/test.ant


dist: clean
	mkdir tmp
	cp -f AUTHORS CHANGELOG LICENSE  Makefile  NEWS  README TODO VERSION tmp/
	cp -r src/ tmp/
	cp -r ex/ tmp/
	mv tmp antelatex-$(VERSION)
	tar -cf antelatex-$(VERSION).tar antelatex-$(VERSION)
	gzip antelatex-$(VERSION).tar
	rm -rf antelatex-$(VERSION)