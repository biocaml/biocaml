lib=biocaml
findlib-pkg=$(lib)

############################################################
#
# Compiling library, documentation, and applications
#
all: $(lib) apps

$(lib):
	cd src; ocamlbuild $(lib).cma $(lib).cmxa $(lib).cmxs

doc/html/%:
	cd src; ocamlbuild $*.docdir/index.html; rm -f $*.docdir
	mkdir -p doc/html/$*
	cp -fR src/_build/$*.docdir/* doc/html/$*

doc: clean-doc doc/html/base doc/html/$(lib)

apps:
	cd src; ocamlbuild $(patsubst src/%.ml,%.native,$(wildcard src/app/*.ml)); rm -f *.native


############################################################
#
# Installing and uninstalling
#
install: $(lib) uninstall
	cd src/_build; ocamlfind install $(findlib-pkg) ../META $(patsubst %,$(lib).%,cmi a cma cmxa cmxs)

uninstall:
	ocamlfind remove $(findlib-pkg)


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

.PHONY: all $(lib) doc apps install uninstall clean clean-doc clean-all fresh
