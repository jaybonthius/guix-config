;;; pre-early-init.el --- Pre-early initialization -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

;; Enable debug on error during development.
(setq debug-on-error t)

;; Disable package.el initialization since Elpaca replaces it.
(setq minimal-emacs-package-initialize-and-refresh nil)

;; Fix native compilation on macOS with Homebrew gcc.
;; libgccjit needs LIBRARY_PATH to find libemutls_w.a.
(when (and (eq system-type 'darwin)
           (native-comp-available-p))
  (let ((gcc-lib-dir (car (file-expand-wildcards
                           "/opt/homebrew/lib/gcc/current/gcc/*/*/"))))
    (when (and gcc-lib-dir (file-directory-p gcc-lib-dir))
      (setenv "LIBRARY_PATH"
              (string-join
               (seq-uniq (cons gcc-lib-dir
                               (split-string (or (getenv "LIBRARY_PATH") "") ":" t)))
               ":")))))

;; Remove title bar and window decorations (including stoplight buttons).
(add-to-list 'default-frame-alist '(undecorated . t))

;; Reduce clutter in ~/.config/emacs by redirecting files to var/.
(setq user-emacs-directory (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;;; pre-early-init.el ends here
