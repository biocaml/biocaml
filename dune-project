(lang dune 1.10)
(generate_opam_files true)

(name biocaml)
(source (github biocaml/biocaml))
(homepage "https://github.com/biocaml/biocaml/")
(bug_reports "https://github.com/biocaml/biocaml/issues")
(license "LGPL + linking exception")
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
  (description "
Biocaml aims to be a high-performance user-friendly library for
Bioinformatics. See the [website](http://biocaml.org) for detailed API
documentation. We welcome contributors and feedback from users. Please
contact us with any comments and suggestions for features you would
like added.
")
  (depends
    base64
    (dune (>= 1.10))
    (core_kernel (>= v0.14.0))
    sexplib
    (camlzip (>= 1.05))
    xmlm
    cfstream
    ppx_compare
    ppx_sexp_conv
    re
    rresult
    uri)
  (depopts
    async
    core
    lwt))
