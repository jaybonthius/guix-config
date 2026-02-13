;;; post-init.el --- Post initialization -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

;; compile-angel: ensure all packages are byte-compiled and native-compiled.
;; Must be loaded before all other packages.
(use-package compile-angel
  :demand t
  :ensure t
  :custom
  (compile-angel-verbose t)
  :config
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)
  (compile-angel-on-load-mode 1))

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

;;; Autosave

;; Enable auto-save-mode to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)
(setq auto-save-interval 300)
(setq auto-save-timeout 30)

;; auto-save-visited-mode: save file-visiting buffers after idle time.
(setq auto-save-visited-interval 5)
(auto-save-visited-mode 1)

;;; Load custom.el

(load custom-file 'noerror 'no-message)

;;; post-init.el ends here
