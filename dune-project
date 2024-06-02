(lang dune 3.14)

(name ninety_nine_problems)

(generate_opam_files true)

(source
 (github avg_cs_student/99_ocaml_problems))

(authors "Justin Cromer")

(maintainers "Justin Cromer")

(license MIT)

(package
 (name ninety_nine_problems)
 (synopsis "Learning OCaml with small puzzles.")
 (description "https://ocaml.org/exercises")
 (depends ocaml dune)
 (tags
  (topics "learn ocaml" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
