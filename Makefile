######################################################################
## System directories and programs.
LIB_PKGS=num unix str extlib pcre netsys netstring #findlib package name
LIB_NAMES=nums unix str extLib pcre netsys netstring
LIBS=$(patsubst %,%.cma,$(LIB_NAMES))

# Source code directories to include in ocamlbuild's search path
DIRS=base bioCaml

# Given a library package name, return the directory in which the library exists
lib_inc = $(shell ocamlfind query -format "%d" $(1))

# Given a directory, return list of cmo files in order in which they should be loaded
get_deps = $(patsubst $(1)/%,%,$(shell ocamldsort -I $(1) -byte $(1)/*.ml))

INCLUDE=$(foreach lib,$(LIB_PKGS),-I $(call lib_inc,$(lib))) $(patsubst %,-I src/_build/%,$(DIRS)) -I src/_build

.PHONY: all doc clean install

install: all
	cd src/_build; cp -f bioCaml.a bioCaml.o bioCaml.cm* ../../lib/ # this is a hack, install using findlib

all:
	make -C lib/tylesBase all
	cd lib/tylesBase/src/_build; cp -f tylesBase.a tylesBase.o tylesBase.cm* ../../../
	cd src; ocamlbuild bioCaml.cma
	cd src; ocamlbuild bioCaml.cmxa

doc/html/%:
	cd src; ocamlbuild $*.docdir/index.html; rm -f $*.docdir
	rm -rf doc/html/$*; mkdir -p doc/html/$*
	cp -fR src/_build/$*.docdir/* doc/html/$*

doc: doc/html/base doc/html/bioCaml

AUTO_TEX_SUFFIXES=bbl log dvi blg pdf aux
clean:
	make -C lib/tylesBase clean; echo ""
	rm -f lib/*.{a,o,so,cm*}
	cd src; ocamlbuild -clean
	rm -rf doc/html/*
	rm -f $(wildcard $(patsubst %,notes/*.%,$(AUTO_TEX_SUFFIXES)))
	rm -f $(wildcard $(patsubst %,notes/*/*.%,$(AUTO_TEX_SUFFIXES)))
