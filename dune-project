(lang dune 3.6)

(name gcc_debug_tree_visualizer)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name gcc_debug_tree_visualizer)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune base stdio ppx_jane)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
