# arch-tag: Primary makefile
# Copyright (c) 2004 John Goerzen
#
PACKAGES := -package shell -package missinglib -package pcre
all:	buildcd

buildcd: cashutil.cmx buildcd.cmx
	ocamlfind ocamlopt $(PACKAGES) -linkpkg \
		camlp4/camlp4.cmxa cash.cmxa -o $@ $^

buildcd.cmx: cashutil.cmx

%.cmx: %.ml
	ocamlfind ocamlopt $(PACKAGES) -c -o $@ $<

