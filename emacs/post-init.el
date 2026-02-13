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

;;; Font & cursor

(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 140
                    :weight 'regular)

(setq-default cursor-type 'bar)

;;; Tab bar: hidden, with history-based window undo/redo.

(use-package tab-bar
  :ensure nil
  :bind (("C-c <left>" . tab-bar-history-back)
         ("C-c <right>" . tab-bar-history-forward))
  :custom
  (tab-bar-show nil)
  :config
  (tab-bar-history-mode 1))

;;; Themes

;; twilight-bright-theme: a clean light theme (bogdan's fork).
(use-package twilight-bright-theme
  :ensure (:host github :repo "Bogdanp/twilight-bright-theme.el")
  :demand t)

;; twilight-anti-bright-theme: the dark counterpart (bogdan's fork).
(use-package twilight-anti-bright-theme
  :ensure (:host github :repo "Bogdanp/twilight-anti-bright-theme")
  :demand t)

(defun jb-system-dark-mode-p ()
  "Return non-nil if macOS is in dark mode."
  (when (eq system-type 'darwin)
    (string-match-p
     "Dark"
     (shell-command-to-string
      "defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light"))))

(defun jb-detect-and-apply-system-theme ()
  "Detect macOS appearance and load the matching twilight theme."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (if (jb-system-dark-mode-p)
      (load-theme 'twilight-anti-bright t)
    (load-theme 'twilight-bright t)))

(defun toggle-twilight-theme ()
  "Toggle between twilight-bright and twilight-anti-bright themes."
  (interactive)
  (let ((current (car custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (if (eq current 'twilight-bright)
        (load-theme 'twilight-anti-bright t)
      (load-theme 'twilight-bright t))))

;; Apply system theme after Elpaca has installed and activated theme packages.
(add-hook 'elpaca-after-init-hook #'jb-detect-and-apply-system-theme)

;;; Ligatures

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures t '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "\\\\" "://"))
  (global-ligature-mode t))

;;; Centering

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 100))

(use-package auto-olivetti
  :ensure (:host sourcehut :repo "ashton314/auto-olivetti")
  :custom
  (auto-olivetti-enabled-modes '(prog-mode text-mode fundamental-mode))
  :config
  (auto-olivetti-mode))

;;; hl-todo: highlight TODO/FIXME/etc keywords.

(use-package hl-todo
  :ensure t
  :custom
  (hl-todo-keyword-faces
   '(("TODO"   . (:inherit error :weight bold))
     ("FIXME"  . (:inherit error :weight bold))
     ("DEBUG"  . (:inherit warning :weight bold))
     ("GOTCHA" . (:inherit warning :weight bold))
     ("STUB"   . (:inherit font-lock-keyword-face :weight bold))
     ("HACK"   . (:inherit warning :weight bold))
     ("NOTE"   . (:inherit success :weight bold))))
  :bind (:map hl-todo-mode-map
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :config
  (global-hl-todo-mode 1))

;;; Page break lines: visual indicators for ^L characters.

(use-package page-break-lines
  :ensure t
  :commands (page-break-lines-mode global-page-break-lines-mode)
  :hook (emacs-lisp-mode . page-break-lines-mode))

;;; Persist text scale across sessions.

(use-package persist-text-scale
  :ensure t
  :commands (persist-text-scale-mode persist-text-scale-restore)
  :hook (after-init . persist-text-scale-mode))

;;; Modeline

(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))
(add-hook 'after-init-hook #'display-time-mode)

;;; Line numbers: disabled in buffers.

(setq display-line-numbers-type nil)

;;; Tree-sitter: maximum syntax highlighting.

(setq treesit-font-lock-level 4)

;;; Pixel-precise scrolling (not needed on emacs-mac which handles it natively).

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  (setq pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode 1))

;;; Built-in minor modes enabled after init.

(add-hook 'after-init-hook #'show-paren-mode)
(add-hook 'after-init-hook #'delete-selection-mode)
(add-hook 'after-init-hook #'window-divider-mode)
(add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)

;;; Uniquify: disambiguate buffer names with path components.

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

;;; Prevent accidental zoom via mouse wheel.

(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

;;; Terminal: map F12 as a Control modifier prefix.

(unless (display-graphic-p)
  (define-key key-translation-map (kbd "<f12>") 'event-apply-control-modifier))

;;; ============================================================================
;;; Modal editing: Symex + Meow
;;; ============================================================================

;;; Symex: structural s-expression editing via a Lithium modal interface.
;;; Uses IJKL directional keys to match the Meow layout below.

(use-package symex-core
  :ensure (:host github
           :repo "drym-org/symex.el"
           :files ("symex-core/symex*.el")))

(use-package symex
  :after symex-core
  :ensure (:host github
           :repo "drym-org/symex.el"
           :files ("symex/symex*.el"
                   "symex/doc/*.texi"
                   "symex/doc/figures"))
  :bind ("s-;" . symex-mode-interface)
  :custom
  (symex-orientation 'inverted)
  :config
  (symex-mode 1)
  (lithium-define-keys symex-editing-mode
    (;; IJKL movement (matches Meow normal state)
     ("h" symex-insert-at-beginning :exit)
     ("i" symex-go-down)
     ("j" symex-go-backward)
     ("k" symex-go-up)
     ("l" symex-go-forward)
     ;; Branch navigation
     ("C-i" symex-descend-branch)
     ("C-k" symex-climb-branch)
     ("M-i" symex-goto-lowest)
     ("M-k" symex-goto-highest)
     ;; Insertion (all exit symex modal UI)
     ("H" symex-insert-before :exit)
     ("A" symex-append-after :exit)
     ("a" symex-append-at-end :exit))))

(use-package symex-ide
  :after symex
  :ensure (:host github
           :repo "drym-org/symex.el"
           :files ("symex-ide/symex*.el"))
  :config
  (symex-ide-mode 1))

;;; Meow: modal editing with a custom IJKL directional layout.
;;; i=up, j=left, k=down, l=right.  h=insert (replaces the traditional i).

(use-package meow
  :ensure (:host github :repo "meow-edit/meow")
  :config
  (defun meow-setup ()
    "Configure Meow keybindings with an IJKL directional layout."
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

    ;; Motion state (read-only buffers: Dired, Help, etc.)
    (meow-motion-define-key
     '("i" . meow-prev)
     '("k" . meow-next)
     '("<escape>" . ignore))

    ;; Leader (SPC prefix)
    (meow-leader-define-key
     ;; Digit arguments
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     ;; Code navigation (xref)
     '("." . xref-find-definitions)
     '("," . xref-go-back)
     '("r" . xref-find-references)
     ;; Search & replace
     '("l" . consult-line)
     '("s" . query-replace)
     '("S" . query-replace-regexp))

    ;; Normal state (main editing)
    (meow-normal-define-key
     ;; Expansion
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     ;; Movement (IJKL)
     '("i" . meow-prev)
     '("I" . meow-prev-expand)
     '("j" . meow-left)
     '("J" . meow-left-expand)
     '("k" . meow-next)
     '("K" . meow-next-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     ;; Editing
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-insert)
     '("H" . meow-open-above)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))

  (meow-setup)
  (meow-global-mode 1))

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
         ("C-x M-g" . magit-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

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
;;; Session & buffer management
;;; ============================================================================

;; easysession: persist and restore file buffers, indirect buffers/clones,
;; Dired buffers, windows/splits, tab-bar state, and frames across sessions.
(use-package easysession
  :ensure t
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)

  :custom
  (easysession-mode-line-misc-info t)    ; Display session name in modeline
  (easysession-save-interval (* 10 60))  ; Save every 10 minutes

  :init
  ;; Key mappings (C-c e prefix)
  (global-set-key (kbd "C-c e s") #'easysession-save)
  (global-set-key (kbd "C-c e l") #'easysession-switch-to)
  (global-set-key (kbd "C-c e L") #'easysession-switch-to-and-restore-geometry)
  (global-set-key (kbd "C-c e r") #'easysession-rename)
  (global-set-key (kbd "C-c e R") #'easysession-reset)
  (global-set-key (kbd "C-c e d") #'easysession-delete)

  (if (fboundp 'easysession-setup)
      ;; Modern: easysession-setup adds hooks for automatic session
      ;; loading (startup or daemon), periodic saving, and save-on-exit.
      (easysession-setup)
    ;; Legacy fallback: depth 102/103 ensures session loads after
    ;; minimal-emacs.d restores file-name-handler-alist at depth 101.
    (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
    (add-hook 'emacs-startup-hook #'easysession-save-mode 103)))

;; easysession-scratch: persist *scratch* buffer contents across sessions.
(use-package easysession-scratch
  :ensure nil
  :after easysession
  :config
  (easysession-scratch-mode 1))

;; buffer-terminator: automatically kill inactive buffers to keep the
;; buffer list clean and reduce resource usage.
(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)
  (buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes
  (buffer-terminator-interval (* 10 60))           ; 10 minutes
  :config
  (buffer-terminator-mode 1))

;;; ============================================================================
;;; Load custom.el
;;; ============================================================================

;; Load custom.el after Elpaca has activated all packages.
(add-hook 'elpaca-after-init-hook
          (lambda () (load custom-file 'noerror 'no-message)))

;;; post-init.el ends here
