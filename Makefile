# arch-tag: Primary makefile
# Copyright (c) 2004 John Goerzen
#
PACKAGES := -package shell -package missinglib
all:	buildcd lib/linuxrc lib/startup

clean:
	-rm -f buildcd lib/linuxrc lib/startup
	-rm -f `find . -name "*.cm*"`
	-rm -f `find . -name "*~"`


buildcd: dfsutils.cmx cashutil.cmx mirror.cmx buildcd.cmx
	ocamlfind ocamlopt $(PACKAGES) -package pcre -linkpkg \
		camlp4/camlp4.cmxa cash.cmxa -o $@ $^

test: cashutil.cmx mirror.cmx test.cmx
	ocamlfind ocamlopt $(PACKAGES) -package pcre -linkpkg \
		camlp4/camlp4.cmxa cash.cmxa -o $@ $^

lib/linuxrc.cmx: dfsutils.cmx shellutil.cmx
lib/startup.cmx: dfsutils.cmx shellutil.cmx

mirror.cmx: dfsutils.cmx shellutil.cmx
mirror.cmo: dfsutils.cmo shellutil.cmo

lib/linuxrc: dfsutils.cmx shellutil.cmx lib/linuxrc.cmx
	ocamlfind ocamlopt -cclib -static $(PACKAGES) -linkpkg \
		-o $@ $^
	strip -s $@

lib/startup: dfsutils.cmx shellutil.cmx lib/startup.cmx
	ocamlfind ocamlopt -cclib -static $(PACKAGES) -linkpkg -o $@ $^
	strip -s $@

buildcd.cmx: cashutil.cmx

%.cmx: %.ml
	ocamlfind ocamlopt $(PACKAGES) -c -o $@ $<

%.cmo: %.ml
	ocamlfind ocamlc $(PACKAGES) -c -o $@ $<

