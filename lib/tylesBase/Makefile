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

install: all
	cp -f src/_build/app/*.native bin/; chmod 744 bin/*.native
	cp -f src/_build/tylesBase.{a,o,cm*} lib/ # this is a hack, install using findlib

all: tylesBase app/mirror_tylesBase.native

tylesBase:
	cd src; ocamlbuild tylesBase.cma
	cd src; ocamlbuild tylesBase.cmxa

app/%.native:
	cd src; ocamlbuild $@

doc/html/tylesBase: src/tylesBase/*.mli src/tylesBase/stringMap.ml src/tylesBase/intMap.ml src/tylesBase/stringSet.ml src/tylesBase/intSet.ml 
	rm -rf $@
	mkdir -p $@
	ocamldoc $(INCLUDE) -d $@ -no-stop -html $^

doc: doc/html/tylesBase

clean:
	cd src; ocamlbuild -clean
	rm -rf doc/html
	rm -rf bin/*.native bin/*.byte
	rm -f lib/*.{a,o,so,cm*}
