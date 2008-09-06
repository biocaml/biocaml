LIB_PKGS=extlib #findlib package name
LIB_NAMES=extLib
LIBS=$(patsubst %,%.cma,$(LIB_NAMES))
DIRS=tylesBase

# Given a library package name, return the directory in which the library exists
lib_inc = -I $(shell ocamlfind query -format "%d" $(1))

# Given a directory, return list of cmo files in order in which they should be loaded
get_deps = $(patsubst $(1)/%,%,$(shell ocamldsort -I $(1) -byte $(1)/*.ml))

INCLUDE=$(foreach lib,$(LIB_PKGS),$(call lib_inc,$(lib))) $(patsubst %,-I _build/%,$(DIRS)) -I _build

.PHONY: tylesBase all doc clean

all: tylesBase app/mirror_tylesBase.native

tylesBase:
	cd src; ocamlbuild tylesBase.cma
	cd src; ocamlbuild tylesBase.cmxa

app/%.native:
	cd src; ocamlbuild $@
	cp -f src/_build/$@ src/app

doc/html/%: src/%/*.mli
	rm -rf $@
	mkdir -p $@
	ocamldoc $(INCLUDE) -d $@ -no-stop -html $^

doc: doc/html/tylesBase

clean:
	cd src; ocamlbuild -clean
	rm -rf doc/html
	rm -rf src/app/*.native src/app/*.byte
