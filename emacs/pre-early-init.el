;;; pre-early-init.el --- Pre-early initialization -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

;; Enable debug on error during development.
(setq debug-on-error t)

;; Reduce clutter in ~/.config/emacs by redirecting files to var/.
(setq user-emacs-directory (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;;; pre-early-init.el ends here
