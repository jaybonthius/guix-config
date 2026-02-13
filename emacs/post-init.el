;;; post-init.el --- Post initialization -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

;;; ============================================================================
;;; Native compilation
;;; ============================================================================

;; compile-angel: ensure all packages are byte-compiled and native-compiled.
;; Must be loaded before all other packages.
(use-package compile-angel
  :ensure t
  :demand t
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

;;; ============================================================================
;;; System integration
;;; ============================================================================

;; Inherit environment variables (especially PATH) from the shell on macOS.
;; Without this, GUI Emacs won't find tools like rg, git, node, etc.
(use-package exec-path-from-shell
  :ensure t
  :defer t
  :hook (elpaca-after-init . exec-path-from-shell-initialize))

;; macOS key modifiers: Command=Control, Option=Meta, Control=Super, Fn=Hyper.
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'control
        ns-option-modifier 'meta
        ns-control-modifier 'super
        ns-function-modifier 'hyper))

;; Terminal mouse support.
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; Ask before quitting Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Start the Emacs server for emacsclient usage.
(use-package server
  :ensure nil
  :defer t
  :commands server-start
  :hook (after-init . server-start))

;;; ============================================================================
;;; Built-in modes
;;; ============================================================================

;; Auto-revert: automatically update buffers to reflect changes on disk.
(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
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
  :commands (recentf-mode recentf-cleanup)
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))
  :config
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;; Savehist: preserve minibuffer history between sessions.
(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring
     register-alist
     mark-ring global-mark-ring
     search-ring regexp-search-ring)))

;; Save-place: remember last cursor position in files.
(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook (after-init . save-place-mode)
  :custom
  (save-place-limit 400))

;; which-key: display available keybindings in a popup.
(use-package which-key
  :ensure nil ; builtin since Emacs 30
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

;;; Autosave

;; Enable auto-save-mode to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)
(setq auto-save-interval 300)
(setq auto-save-timeout 30)

;; auto-save-visited-mode: save file-visiting buffers after idle time.
(setq auto-save-visited-interval 5)
(auto-save-visited-mode 1)

;;; ============================================================================
;;; UI & appearance
;;; ============================================================================

;; Tab bar: hide by default, use tab-bar-history for window undo/redo.
(use-package tab-bar
  :ensure nil
  :bind (("C-c <left>" . tab-bar-history-back)
         ("C-c <right>" . tab-bar-history-forward))
  :custom
  (tab-bar-show nil)
  :config
  (tab-bar-history-mode 1))

;; Default font size: 14pt (140 = 14.0pt in Emacs units).
(set-face-attribute 'default nil :height 140)

;; twilight-bright-theme: a clean light theme (bogdan's fork).
(use-package twilight-bright-theme
  :ensure (:host github :repo "Bogdanp/twilight-bright-theme.el")
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'twilight-bright t))

;; twilight-anti-bright-theme: the dark counterpart (bogdan's fork).
(use-package twilight-anti-bright-theme
  :ensure (:host github :repo "Bogdanp/twilight-anti-bright-theme"))

(defun toggle-twilight-theme ()
  "Toggle between twilight-bright and twilight-anti-bright themes."
  (interactive)
  (let ((current (car custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (if (eq current 'twilight-bright)
        (load-theme 'twilight-anti-bright t)
      (load-theme 'twilight-bright t))))

;;; ============================================================================
;;; Completion (minibuffer): Vertico + Orderless + Marginalia + Consult + Embark
;;; ============================================================================

;; Vertico: vertical completion interface for the minibuffer.
(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;; Orderless: flexible matching for completion candidates.
;; Allows space-separated patterns matched in any order.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia: rich annotations in the minibuffer.
(use-package marginalia
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :hook (elpaca-after-init . marginalia-mode))

;; Consult: enhanced search, navigation, and buffer commands.
(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

;; Embark: context-sensitive actions on completion candidates.
(use-package embark
  :ensure t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark-consult: integration between Embark and Consult.
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; ============================================================================
;;; Completion (in-buffer): Corfu + Cape
;;; ============================================================================

;; Corfu: compact in-buffer completion popup.
(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  :config
  (global-corfu-mode))

;; Cape: Completion At Point Extensions.
;; Provides additional completion backends for corfu.
(use-package cape
  :ensure t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;; ============================================================================
;;; Help & discovery
;;; ============================================================================

;; Helpful: better *help* buffers with more contextual information.
(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

;;; ============================================================================
;;; Undo
;;; ============================================================================

;; undo-fu: lightweight undo/redo wrapper.
(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :bind (("C-z" . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo)))

;; undo-fu-session: persist undo history across Emacs sessions.
(use-package undo-fu-session
  :ensure t
  :commands undo-fu-session-global-mode
  :hook (elpaca-after-init . undo-fu-session-global-mode))

;;; ============================================================================
;;; Navigation
;;; ============================================================================

;; Avy: jump to visible text with minimal keystrokes.
(use-package avy
  :ensure t
  :commands (avy-goto-char
             avy-goto-char-2
             avy-next)
  :bind ("C-'" . avy-goto-char-2))

;;; ============================================================================
;;; Git
;;; ============================================================================

;; Transient: required by magit. The built-in version is too old,
;; so we install the current version from MELPA via Elpaca.
(use-package transient
  :ensure t)

;; Magit: full-featured git porcelain inside Emacs.
(use-package magit
  :ensure t
  :after transient
  :commands (magit-status
             magit-dispatch
             magit-file-dispatch)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)))

;; diff-hl: highlight uncommitted changes in the gutter.
(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode
             global-diff-hl-mode)
  :hook (prog-mode . diff-hl-mode)
  :init
  (setq diff-hl-flydiff-delay 0.4)
  (setq diff-hl-show-staged-changes nil)
  (setq diff-hl-update-async t)
  (setq diff-hl-global-modes '(not pdf-view-mode image-mode)))

;;; ============================================================================
;;; Snippets
;;; ============================================================================

;; yasnippet-snippets: official collection of snippets.
;; Declared before yasnippet so it's available when yas-global-mode loads.
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; YASnippet: template expansion system.
(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode
             yas-global-mode)
  :hook (elpaca-after-init . yas-global-mode)
  :custom
  (yas-also-auto-indent-first-line t)
  (yas-also-indent-empty-lines t)
  (yas-snippet-revival nil)
  (yas-wrap-around-region nil)
  :init
  (setq yas-verbosity 0))

;;; ============================================================================
;;; Tree-sitter
;;; ============================================================================

;; treesit-auto: automatically install and use tree-sitter grammars.
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; ============================================================================
;;; Code navigation
;;; ============================================================================

;; dumb-jump: jump-to-definition using ripgrep/grep heuristics.
;; Falls back gracefully when no LSP is available.
(use-package dumb-jump
  :ensure t
  :commands dumb-jump-xref-activate
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 90)
  (setq dumb-jump-aggressive nil)
  (setq dumb-jump-max-find-time 3)
  (setq dumb-jump-selector 'completing-read)
  (when (executable-find "rg")
    (setq dumb-jump-force-searcher 'rg)
    (setq dumb-jump-prefer-searcher 'rg)))

;;; ============================================================================
;;; Load custom.el
;;; ============================================================================

;; Load custom.el after Elpaca has activated all packages.
(add-hook 'elpaca-after-init-hook
          (lambda () (load custom-file 'noerror 'no-message)))

;;; post-init.el ends here
