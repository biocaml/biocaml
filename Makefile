help:
	@echo Usage:
	@echo "  make BINDIR=path target"
	@echo
	@echo "where 'path' is directory to install executables. This can be"
	@echo "omitted when 'target' does not require it."

lib=biocaml
findlib-pkg=$(lib)

############################################################
#
# Do everything
#
world: install-lib install-apps doc


############################################################
#
# Compiling library, documentation, and applications
#
lib:
	cd src; ocamlbuild $(lib).cma $(lib).cmxa $(lib).cmxs

doc/html/%:
	cd src; ocamlbuild $*.docdir/index.html; rm -f $*.docdir
	mkdir -p doc/html/$*
	cp -fR src/_build/$*.docdir/* doc/html/$*

doc: clean-doc doc/html/$(lib)

APPS=$(patsubst src/app/%.ml,%,$(wildcard src/app/*.ml))
apps:
	cd src; ocamlbuild $(patsubst %,app/%.native,$(APPS))
	rm -f $(patsubst %,src/%.native,$(APPS))


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
# Installing/uninstalling library and applications
#
install-lib: lib uninstall-lib
	cd src/_build/; ocamlfind install $(findlib-pkg) \
            ../META $(patsubst %,biocaml/$(lib).%,a cma cmxa cmxs) \
            biocaml/*.cmi biocaml/*.cmo biocaml/*.cmx

uninstall-lib:
	ocamlfind remove $(findlib-pkg)

install-apps: apps
	mkdir $(BINDIR); bin/install_apps.ml src/_build/app $(BINDIR)

uninstall-apps:
	rm -f $(patsubst %,$(BINDIR)/%,$(APPS))

############################################################
#
# Clean up
#

# delete compiled library and applications
clean:
	cd src; ocamlbuild -clean

# delete compiled documentation
clean-doc:
	rm -rf doc/html/*

# delete compiled dot files
clean-dot:
	rm -f doc/dot/*

# delete all automatically generated files
clean-all: clean clean-doc clean-dot

# clean everything and uninstall
fresh: clean-all uninstall-lib uninstall-apps

.PHONY: lib doc apps dot install-lib uninstall-lib install-apps uninstall-apps clean clean-doc clean-dot clean-all fresh
