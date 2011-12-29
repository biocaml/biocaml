
all: build

setup.log build byte native: setup.data
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml
	ocaml $< -configure

setup.ml: _oasis
	oasis.dev setup

doc install uninstall reinstall: setup.log
	ocaml setup.ml -$@

clean:
	ocaml setup.ml -clean

# clean everything and uninstall
fresh: clean uninstall

.PHONY: all byte native configure doc install uninstall reinstall upload-doc

