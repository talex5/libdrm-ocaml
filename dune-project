(lang dune 3.18)
(name libdrm)
(using ctypes 0.3)
(formatting disabled)
(generate_opam_files true)

(source (github talex5/libdrm-ocaml))
(license ISC)
(authors "Thomas Leonard")
(maintainers "Thomas Leonard")
(documentation "https://talex5.github.io/libdrm-ocaml/")

(package
 (name libdrm)
 (synopsis "OCaml bindings for libdrm")
 (description "Allows configuring displays and using GPU devices")
 (depends
  (ocaml (>= 5.3.0))
  (ctypes (>= 0.23.0))
  (ctypes-foreign (>= 0.23.0))
  (fmt (>= 0.8.9))))
