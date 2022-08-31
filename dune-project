(lang dune 1.10)

(generate_opam_files true)

(name biocaml)

(source
 (github biocaml/biocaml))

(homepage "https://github.com/biocaml/biocaml/")

(bug_reports "https://github.com/biocaml/biocaml/issues")

(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")

(authors
 "Sebastien Mondet"
 "Ashish Agarwal"
 "Philippe Veber"
 "Christophe Troestler"
 "Sergei Lebedev"
 "David Koppstein"
 "Alexander Gryzlov"
 "Francois Berenger")

(maintainers "philippe.veber@gmail.com")

(package
 (name biocaml)
 (tags (bioinformatics))
 (synopsis "The OCaml Bioinformatics Library")
 (description
  "\nBiocaml aims to be a high-performance user-friendly library for\nBioinformatics. See the [website](http://biocaml.org) for detailed API\ndocumentation. We welcome contributors and feedback from users. Please\ncontact us with any comments and suggestions for features you would\nlike added.\n")
 (depends
  base64
  (dune
   (>= 1.10))
  (core
   (>= v0.15.0))
  (core_unix
   (>= v0.15.0))
  sexplib
  (camlzip
   (>= 1.05))
  xmlm
  cfstream
  ppx_compare
  ppx_sexp_conv
  re
  rresult
  uri)
 (depopts async lwt))
