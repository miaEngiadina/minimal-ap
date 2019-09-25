(use-modules (guix packages)
             (guix gexp)
             (guix download)
             (guix git-download)
             (gnu packages autotools)
             (gnu packages pkg-config)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (guix build-system gnu)
             (guix licenses))

(define %source-dir (dirname (current-filename)))

(package
  (name "minimal-ap")
  (version "0.1.0")
  (source (local-file %source-dir
                      #:recursive? #t
                      #:select? (git-predicate %source-dir)))
  (build-system gnu-build-system)
  (native-inputs
   `(("pkg-config" ,pkg-config)
     ("autoconf" ,autoconf)
     ("automake" ,automake)))
  (inputs
   `(("guile" ,guile-2.2)
     ("guile-json" ,guile-json)))
  (synopsis "An experimental ActivityPub server")
  (description "An experimental ActivityPub server written in Guile.")
  (home-page "https://gitlab.com/pukkamustard/minimal-ap")
  (license agpl3+))
