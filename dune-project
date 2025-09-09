(lang dune 3.20)

(name r-c-typing)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Pierre Donat-Bouillud <pierre.donat.bouillud@cvut.cz>")

(maintainers "Pierre Donat-Bouillud <pierre.donat.bouillud@cvut.cz>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name r-c-typing)
 (synopsis "Analyze and type C code used in R packages")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("R" "types" "C")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
