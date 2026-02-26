(define-module (tinymist)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define %version "0.14.10")

(define-public tinymist
  (package
    (name "tinymist")
    (version %version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Myriad-Dreamin/tinymist/releases/download/v"
             version "/tinymist-"
             (if (string=? (%current-system) "aarch64-linux")
                 "aarch64-unknown-linux-gnu"
                 "x86_64-unknown-linux-gnu")
             ".tar.gz"))
       (sha256
        (base32
         (if (string=? (%current-system) "aarch64-linux")
             "0p2msj0ih39yvsf9628i1ls6yyi4m255hq8sx05a8p9j72jvk4b2"
             "0vr1q5qx8aacyhj550k66ia74ywd1w88g0g2pks6xl6d7qxzfl6y")))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("tinymist" "bin/tinymist"))
       #:validate-runpath? #f
       #:strip-binaries? #f
       #:phases (modify-phases %standard-phases
                  (delete 'install-license-files))))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://github.com/Myriad-Dreamin/tinymist")
    (synopsis "Language server for Typst")
    (description "Tinymist is an integrated language service for Typst.  It
provides features such as autocompletion, diagnostics, jump to definition,
formatting, and more via the Language Server Protocol (LSP).")
    (license license:asl2.0)))
