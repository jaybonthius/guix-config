(define-module (minimal-emacs)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public minimal-emacs.d
  (package
    (name "minimal-emacs.d")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jamescherti/minimal-emacs.d")
             (commit "00ac669d9c58630e6957941104f744881cad1f21")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i8jfd18w5wfwjnrzqc25lrf9wzzyd72vyzzrb6p02q19q0vanyx"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~(list (list "init.el" "share/minimal-emacs.d/init.el")
              (list "early-init.el" "share/minimal-emacs.d/early-init.el"))))
    (home-page "https://github.com/jamescherti/minimal-emacs.d")
    (synopsis "Optimized Emacs base configuration")
    (description "A lightweight and optimized Emacs base (init.el and
early-init.el) that provides better defaults, faster startup, and a clean
foundation for building your own vanilla Emacs setup.")
    (license license:gpl3+)))
