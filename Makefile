# arch-tag: Primary makefile
# Copyright (c) 2004 John Goerzen
#
PACKAGES := -package shell -package missinglib -package extlib
all:	buildcd lib/linuxrc

buildcd: dfsutils.cmx cashutil.cmx buildcd.cmx
	ocamlfind ocamlopt $(PACKAGES) -package pcre -linkpkg \
		camlp4/camlp4.cmxa cash.cmxa -o $@ $^

lib/linuxrc.cmx: dfsutils.cmx shellutil.cmx

lib/linuxrc: dfsutils.cmx shellutil.cmx lib/linuxrc.cmx
	ocamlfind ocamlopt $(PACKAGES) -linkpkg \
		-o $@ $^

buildcd.cmx: cashutil.cmx

%.cmx: %.ml
	ocamlfind ocamlopt $(PACKAGES) -c -o $@ $<

