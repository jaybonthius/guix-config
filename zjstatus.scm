(define-module (zjstatus)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public zjstatus
  (package
    (name "zjstatus")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/dj95/zjstatus/releases/download/v"
             version "/zjstatus.wasm"))
       (file-name (string-append "zjstatus-" version ".wasm"))
       (sha256
        (base32
         "0lyxah0pzgw57wbrvfz2y0bjrna9bgmsw9z9f898dgqw1g92dr2d"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("zjstatus.wasm" "share/zjstatus/zjstatus.wasm"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (copy-file source "zjstatus.wasm")))
         (delete 'install-license-files))))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://github.com/dj95/zjstatus")
    (synopsis "Configurable statusbar plugin for zellij")
    (description "zjstatus is a configurable and themable statusbar plugin
for the zellij terminal multiplexer.")
    (license license:expat)))
