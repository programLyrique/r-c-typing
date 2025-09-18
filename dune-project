(lang dune 3.20)

(name r-c-typing)

(generate_opam_files true)

(source
 (github programLyrique/r-c-typing))

(authors "Pierre Donat-Bouillud <pierre.donat.bouillud@fit.cvut.cz>")

(maintainers "Pierre Donat-Bouillud <pierre.donat.bouillud@fit.cvut.cz>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name r-c-typing)
 (synopsis "Analyze and type C code used in R packages")
 (description "A longer description")
 (depends (ocaml (>= 5.3.0)) dune mlsem sstt)
 (tags
  ("R" "types" "C")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
