(define-module (zellij)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public zellij
  (package
    (name "zellij")
    (version "0.43.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/zellij-org/zellij/releases/download/v"
             version "/zellij-aarch64-unknown-linux-musl.tar.gz"))
       (sha256
        (base32 "0rdi60lz1zmhg76xg874hipnc8y8bpwisb8nav8n4b0wyvailcij"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("zellij" "bin/zellij"))
       #:validate-runpath? #f
       #:strip-binaries? #f
       #:phases (modify-phases %standard-phases
                  (delete 'install-license-files))))
    (supported-systems '("aarch64-linux"))
    (home-page "https://zellij.dev")
    (synopsis "Terminal workspace and multiplexer")
    (description "Zellij is a terminal workspace and multiplexer with
batteries included.")
    (license license:expat)))
