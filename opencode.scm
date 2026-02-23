(define-module (opencode)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages rust-apps))

(define %version "1.1.12")

(define-public opencode
  (package
    (name "opencode")
    (version %version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/anomalyco/opencode/releases/download/v"
             version "/opencode-linux-"
             (if (string=? (%current-system) "aarch64-linux")
                 "arm64"
                 "x64")
             ".tar.gz"))
       (sha256
        (base32
         (if (string=? (%current-system) "aarch64-linux")
             "05dskc1n749yzx0lvb72269j9azz8g97zajlrfx8pyfjsgacprbc"
             "008fzni0yhxmphsqxsrzy451sq6gnq6pxihsa3svq6pmnh2nw8bs")))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("opencode" "bin/opencode"))
       #:validate-runpath? #f
       #:strip-binaries? #f
       #:phases (modify-phases %standard-phases
                  (delete 'install-license-files))))
    (propagated-inputs (list ripgrep))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://opencode.ai")
    (synopsis "AI coding assistant for the terminal")
    (description "OpenCode is an interactive CLI tool that helps with
software engineering tasks using AI models.")
    (license license:expat)))
