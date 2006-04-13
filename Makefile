# arch-tag: Primary makefile
# Copyright (c) 2004-2006 John Goerzen
#

all: setup			# GHC build
	./setup configure
	./setup build

doc: lib/dfs.html/index.html lib/dfs.pdf lib/dfs.ps lib/dfs.txt

hugsbuild: setup
	./setup configure --hugs
	./setup build

setup: Setup.lhs dfsbuild.cabal
	ghc -package Cabal Setup.lhs -o setup

clean:
	-./setup clean
	-cd libsrc && ../setup clean
	-rm -rf dist libsrc/dist *.ho *.hi *.o *.a setup *~
	-cd doc && scons -c && scons -c html pdf text ps
	-rm -rf dfsbuild lib/dfs.html lib/dfs.pdf lib/dfs.ps lib/dfs.txt doc/.sconsign .depend test dfsbuild.bc
	-rm -f `find . -name "*.cm*"` doc/manpage* doc/*.1
	-rm -f `find . -name "*~"` `find . -name "*.o"`

test: utils/dfsutils.cmx utils/shellutil.cmx  test.cmx
	ocamlfind ocamlopt -compact $(PACKAGES) -linkpkg \
		-o $@ $^


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

