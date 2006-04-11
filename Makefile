# arch-tag: Primary makefile
# Copyright (c) 2004-2006 John Goerzen
#

all: setup			# GHC build
	./setup configure
	./setup build
	cd libsrc && ../setup configure
	cd libsrc && ../setup build

hugsbuild: setup
	./setup configure --hugs
	./setup build

setup: Setup.lhs dfsbuild.cabal
	ghc -package Cabal Setup.lhs -o setup

clean:
	-./setup clean
	-rm -rf dist *.ho *.hi *.o *.a setup *~
	-cd doc && scons -c && scons -c html pdf text ps
	-rm -rf dfsbuild lib doc/.sconsign .depend test dfsbuild.bc
	-rm -f `find . -name "*.cm*"` doc/manpage* doc/*.1
	-rm -f `find . -name "*~"` `find . -name "*.o"`

test: utils/dfsutils.cmx utils/shellutil.cmx  test.cmx
	ocamlfind ocamlopt -compact $(PACKAGES) -linkpkg \
		-o $@ $^


lib/dfshelp: libsrc/dfshelp
	cp $^  $@

lib/dfshints: libsrc/dfshints
	cp $^ $@

lib/home.html: libsrc/home.html
	cp $^ $@

lib/dfsbuildinfo: libsrc/dfsbuildinfo
	cp $^ $@

lib/linuxrc: utils/dfsutils.cmx utils/shellutil.cmx \
	libsrc/linuxrc.cmx
	ocamlfind ocamlopt -compact -cclib -static -cclib --unresolved-symbols=ignore-all \
	$(PACKAGES) -linkpkg \
		-o $@ $^
	strip -s $@

lib/startup: utils/dfsutils.cmx utils/shellutil.cmx \
	libsrc/startup.cmx
	ocamlfind ocamlopt -compact -cclib -static -cclib --unresolved-symbols=ignore-all \
	$(PACKAGES) -linkpkg -o $@ $^
	strip -s $@

%.cmx: %.ml
	ocamlfind ocamlopt -compact $(PACKAGES) -warn-error A -c -o $@ $<

%.cmo: %.ml
	ocamlfind ocamlc -g $(PACKAGES) -warn-error A -c -o $@ $<

lib/dfs.html/index.html: doc/dfs.sgml lib
	-rm -r lib/dfs.html
	cd doc && scons html
	cp -r doc/html lib/dfs.html
	for FILE in lib/dfs.html/*.html; do \
		lynx -dump -nolist $$FILE > $$FILE.txt; done

lib/dfs.pdf: doc/dfs.sgml lib
	cd doc && scons pdf
	cp doc/dfs.pdf lib

lib/dfs.ps: doc/dfs.sgml lib
	cd doc && scons ps
	cp doc/dfs.ps lib

lib/dfs.txt: doc/dfs.sgml lib
	cd doc && scons text
	cp doc/dfs.txt lib

