(define-module (uv)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define %version "0.10.4")

(define-public uv
  (package
    (name "uv")
    (version %version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/astral-sh/uv/releases/download/"
             version "/uv-"
             (if (string=? (%current-system) "aarch64-linux")
                 "aarch64-unknown-linux-gnu"
                 "x86_64-unknown-linux-gnu")
             ".tar.gz"))
       (sha256
        (base32
         (if (string=? (%current-system) "aarch64-linux")
             "0bi2iiqd9fc2q2sz3salm5cca2sxkbrfby2y5xpalp3i0mj6wjn8"
             "1xv9qb0wdm9f6sjg4cdfb5x792dln93byy1j2xg1rsnyb1rs8lkb")))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("uv" "bin/uv")
                        ("uvx" "bin/uvx"))
       #:validate-runpath? #f
       #:strip-binaries? #f
       #:phases (modify-phases %standard-phases
                  (delete 'install-license-files))))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://docs.astral.sh/uv/")
    (synopsis "Fast Python package and project manager")
    (description "uv is a fast Python package and project manager, written in
Rust.  It provides a drop-in replacement for pip, pip-tools, pipx, poetry,
pyenv, twine, virtualenv, and more.")
    (license license:expat)))
