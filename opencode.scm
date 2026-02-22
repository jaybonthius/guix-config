(define-module (opencode)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages rust-apps))

(define-public opencode
  (package
    (name "opencode")
    (version "1.2.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/anomalyco/opencode/releases/download/v"
             version "/opencode-linux-arm64.tar.gz"))
       (sha256
        (base32 "0xbpgc5ny787qi03kzaapxr5lxck0pl4d20fihjldlhypkqd9afr"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("opencode" "bin/opencode"))
       #:validate-runpath? #f
       #:strip-binaries? #f
       #:phases (modify-phases %standard-phases
                  (delete 'install-license-files))))
    (propagated-inputs (list ripgrep))
    (supported-systems '("aarch64-linux"))
    (home-page "https://opencode.ai")
    (synopsis "AI coding assistant for the terminal")
    (description "OpenCode is an interactive CLI tool that helps with
software engineering tasks using AI models.")
    (license license:expat)))
