# arch-tag: Primary makefile
# Copyright (c) 2004 John Goerzen
#
PACKAGES := -package shell -package missinglib
all:	buildcd

buildcd: buildcd.cmx
	ocamlfind ocamlopt $(PACKAGES) -linkpkg -o $@ str.cmxa $^

%.cmx: %.ml
	ocamlfind ocamlopt $(PACKAGES) -c -o $@ $<

