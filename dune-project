(lang dune 3.20)
(name pml)

(generate_opam_files true)

; (source
;  (github username/reponame))

(authors "Thorsten Ohl <ohl@physik.uni-wuerzburg.de>")

(maintainers "Thorsten Ohl <ohl@physik.uni-wuerzburg.de>")

(license "GPL 3.0")

; (documentation https://url/to/documentation)

(package
 (name pml)
 (synopsis "Physical Media Library")
 (description "Cataloging and ripping physical discs with interface to MusicBrainz")
 (depends
   (ocaml (>= 5.04.0))
   (cmdliner (>= 2.1.0))
   (re (>= 1.14.0))   ; preferred regular expression matching
   (pcre2 (>= 8.0.4)) ; convenient for sed-like editing
   (ubase (>= 0.20))
   (curl (>= 0.10.0))
   (jsont (>= 0.2.0))
   (bytesrw (>= 0.3.0))
   (ppx_inline_test (>= 0.17.1)))
 (tags ("music" "media")))

(package
 (name clibdiscid)
 (synopsis "Wrapper for MusicBrainz Discid functions")
 (description "Generated with Ctypes")
 (depends ocaml)
 (tags ("media")))

(using ctypes 0.3)
