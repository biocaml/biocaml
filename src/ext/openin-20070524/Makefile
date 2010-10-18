pa_openin.cmo: pa_openin.ml
	ocamlc -c -pp camlp4of -I +camlp4 pa_openin.ml

clean:
	rm -rf *~ *.cm*


VER=openin-`date +%Y%m%d`

package:
	rm -Rf $(VER)
	mkdir $(VER)
	cp Makefile README pa_openin.ml $(VER)
	tar czf $(VER).tar.gz $(VER)
	rm -Rf $(VER)

copy:
	scp $(VER).tar.gz frisch.fr:www/info