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
# Compiling dependency graph
#
dot: doc/dot/dependency_graph.ps

doc/dot/dependency_graph.out: $(wildcard src/biocaml/*.ml)
	ocamldoc -o $@ -I src/_build -I src/_build/biocaml -I src/_build/ext/sesame -I src/_build/ext/xmlm-1.0.2/src -dot $^

doc/dot/dependency_graph.ps: doc/dot/dependency_graph.out
	dot -Tps $^ >$@


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

# delete compiled dot files
clean-dot:
	rm -f doc/dot/*

# delete all automatically generated files
clean-all: clean-doc clean

# clean everything and uninstall
fresh: clean-all uninstall

.PHONY: all $(lib) doc dot apps install uninstall clean clean-doc clean-dot clean-all fresh
