######################################################################
## System directories and programs.
LIB_PKGS=num unix str extlib pcre netsys netstring #findlib package name
LIB_NAMES=nums unix str extLib pcre netsys netstring
LIBS=$(patsubst %,%.cma,$(LIB_NAMES))

# Source code directories to include in ocamlbuild's search path
DIRS=base bioCaml
DOC_DIRS=$(patsubst %,doc/html/%,$(DIRS))

# Given a library package name, return the directory in which the library exists
lib_inc = $(shell ocamlfind query -format "%d" $(1))

# Given a directory, return list of cmo files in order in which they should be loaded
get_deps = $(patsubst $(1)/%,%,$(shell ocamldsort -I $(1) -byte $(1)/*.ml))

INCLUDE=$(foreach lib,$(LIB_PKGS),-I $(call lib_inc,$(lib))) $(patsubst %,-I src/_build/%,$(DIRS)) -I src/_build

.PHONY: all install doc

install: all
#	cd src/_build; cp -f bioCaml.a bioCaml.o bioCaml.cm* ../../lib/ # this is a hack, install using findlib
	cd src/_build; cp ../META .; ocamlfind install biocaml META bioCaml.cmi bioCaml.cma bioCaml.cmxa; rm -f META

all:
#	make -C lib/tylesBase all
#	cd lib/tylesBase/src/_build; cp -f tylesBase.a tylesBase.o tylesBase.cm* ../../../
	cd src; ocamlbuild bioCaml.cma
	cd src; ocamlbuild bioCaml.cmxa

doc/html/%:
	cd src; ocamlbuild $*.docdir/index.html; rm -f $*.docdir
	rm -rf doc/html/$*; mkdir -p doc/html/$*
	cp -fR src/_build/$*.docdir/* doc/html/$*

doc: $(DOC_DIRS)

doc/module_dependencies.eps: doc
	cd src/_build; ocamlfind ocamldoc -load base/color.odoc -load base/rSet.odoc -load base/romanNum.odoc -load base/browserLinesHelper.odoc -load base/trackLineHelper.odoc -load base/tracksHelper.odoc -load base/wigHelper.odoc -load base/wigLexer.odoc -load base/wigParser.odoc  -load bioCaml/about.odoc -load bioCaml/bar.odoc -load bioCaml/bed.odoc -load bioCaml/bpmap.odoc -load bioCaml/cel.odoc -load bioCaml/chrName.odoc -load bioCaml/commentLines.odoc -load bioCaml/fasta.odoc -load bioCaml/gff.odoc -load bioCaml/histogram.odoc -load bioCaml/math.odoc -load bioCaml/range.odoc -load bioCaml/seq.odoc -load bioCaml/sgr.odoc -load bioCaml/strandName.odoc -load bioCaml/wig.odoc -dot-types -dot-include-all
	cd src/_build; dot -O -Teps ocamldoc.out; mv ocamldoc.out.eps ../../doc/module_dependencies.eps

############################################################
# Commands to clean up
.PHONY: clean-doc clean clean-all clean-notes uninstall

# delete compiled documentation
clean-doc:
	rm -rf doc/html/*

# delete compiled code
clean:
	make -C lib/tylesBase clean; echo ""
	cd lib; rm -f *.a *.o *.so *.cm*
	cd src; ocamlbuild -clean

# delete compiled notes files
AUTO_TEX_SUFFIXES=bbl log dvi blg pdf aux
clean-notes:
	rm -f $(wildcard $(patsubst %,notes/*.%,$(AUTO_TEX_SUFFIXES)))
	rm -f $(wildcard $(patsubst %,notes/*/*.%,$(AUTO_TEX_SUFFIXES)))

# delete installed files
uninstall:
	ocamlfind remove biocaml

# delete all automatically generated files
clean-all: clean-doc clean clean-notes uninstall
