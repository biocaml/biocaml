############################################################
#
# Compiling code and documentation
#
all: lib doc

lib:
	cd src; ocamlbuild bioCaml.cma bioCaml.cmxa

doc/html/%:
	cd src; ocamlbuild $*.docdir/index.html; rm -f $*.docdir
	mkdir -p doc/html/$*
	cp -fR src/_build/$*.docdir/* doc/html/$*

doc: clean-doc doc/html/base doc/html/bioCaml


############################################################
#
# Installing and uninstalling
#
install: all uninstall
	cd src/_build; ocamlfind install biocaml ../META bioCaml.cmi bioCaml.a bioCaml.cma bioCaml.cmxa

uninstall:
	ocamlfind remove biocaml


############################################################
#
# Clean up
#

# delete compiled code
clean:
	cd src; ocamlbuild -clean

# delete compiled documentation
clean-doc:
	rm -rf doc/html/*

# delete all automatically generated files
clean-all: clean-doc clean

# clean everything and uninstall
fresh: clean-all uninstall

.PHONY: all lib doc install uninstall clean clean-doc clean-all fresh
