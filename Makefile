# arch-tag: Primary makefile
# Copyright (c) 2004 John Goerzen
#
PACKAGES := -package shell -package missinglib -I . -I bootloaders
all:	.depend dfsbuild lib lib/linuxrc lib/startup lib/dfs.html/index.html \
	lib/dfs.pdf lib/dfs.ps lib/dfs.txt lib/dfshelp lib/dfshints \
	lib/home.html lib/dfsbuildinfo

lib:
	if [ ! -d lib ] ; then mkdir lib; fi

clean:
	-cd doc && scons -c && scons -c html pdf text ps
	-rm -rf dfsbuild lib doc/.sconsign .depend test
	-rm -f `find . -name "*.cm*"` doc/manpage* doc/*.1
	-rm -f `find . -name "*~"` `find . -name "*.o"`


dfsbuild: dfsutils.cmx unixutil.cmx shellutil.cmx archsupport.cmx mirror.cmx \
	configfiles.cmx bootloaders/grub.cmx bootloaders/bootloader.cmx \
	dfsbuild.cmx
	ocamlfind ocamlopt $(PACKAGES) -linkpkg \
		-o $@ $^

test: dfsutils.cmx unixutil.cmx shellutil.cmx  test.cmx
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

lib/linuxrc: dfsutils.cmx unixutil.cmx shellutil.cmx libsrc/linuxrc.cmx
	ocamlfind ocamlopt -compact -cclib -static -cclib --unresolved-symbols=ignore-all \
	$(PACKAGES) -linkpkg \
		-o $@ $^
	strip -s $@

lib/startup: dfsutils.cmx unixutil.cmx shellutil.cmx libsrc/startup.cmx
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

.depend:
	ocamldep -I . -I bootloaders `find . -name "*.ml"` > .depend

-include .depend
