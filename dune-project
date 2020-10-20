(lang dune 2.7)

(version 1.0.0)

(generate_opam_files true)

(source
 (github mirage/ocaml-cohttp))

(license ISC)

(authors "Anil Madhavapeddy" "Rudi Grinberg")

(maintainers "team@mirage.org")

(package
 (name kisa)
 (synopsis "An OCaml library for HTTP clients and servers")
 (description "A longer description")
 (depends
  (alcotest :with-test)
  (dune
   (> 2.7))
  (foo
   (and
    :dev
    (> 1.5)
    (< 2.0)))
  (uri
   (>= 1.9.0))
  (uri
   (< 2.0.0))
  (fieldslib
   (> v0.12))
  (fieldslib
   (< v0.13))))
