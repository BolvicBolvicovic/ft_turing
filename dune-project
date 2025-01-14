(lang dune 3.14)

(name ft_turing)

(generate_opam_files true)

(source
 (github BolvicBolvicovic/ft_turing))

(authors "Victor Cornille")

(maintainers "Victor Cornille")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ft_turing)
 (synopsis "A Turing machine Turing complete")
 (description "The 42 project ft_turing. The ft_turing project is functionnal implementation of a single infinite tape Turing machine.")
 (depends ocaml dune)
 (tags
  (topics "turing" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
