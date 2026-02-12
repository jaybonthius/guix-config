;;; post-init.el --- Post initialization -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

;; Auto-revert: automatically update buffers to reflect changes on disk.
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

;; Recentf: maintain a list of recently accessed files.
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  :config
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;; Savehist: preserve minibuffer history between sessions.
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600))

;; Save-place: remember last cursor position in files.
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;;; post-init.el ends here
