
all: build

build byte native: configure
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml
	ocaml $< -configure

setup.ml: _oasis
	oasis.dev setup

doc install: all
	ocaml setup.ml -$@
uninstall:
# OASIS uninstall does not seem to work properly
	ocaml setup.ml -$@
	ocamlfind remove biocamlxmlm
	ocamlfind remove biocaml

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

clean:
	ocaml setup.ml -clean

# clean everything and uninstall
fresh: clean uninstall

.PHONY: all byte native configure doc install uninstall reinstall upload-doc

