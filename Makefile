all: build

build:
	ocaml setup.ml -build

install:
	ocaml setup.ml -reinstall

uninstall:
	ocaml setup.ml -uninstall

doc:
	ocaml setup.ml -doc

clean:
	ocaml setup.ml -clean

# clean everything and uninstall
fresh: clean uninstall

.PHONY: doc install uninstall clean fresh
