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
  (compile-angel-verbose nil)
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

;; macOS key modifiers: Control=Control, Option=Meta, Command=Super, Fn=Hyper.
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'super
        ns-option-modifier 'meta
        ns-control-modifier 'control
        ns-function-modifier 'hyper))

;; kkp: Kitty Keyboard Protocol support for terminal Emacs.
;; The default query timeout (0.1s) is too short for SSH sessions and
;; can cause KKP negotiation to silently fail.  When that happens,
;; modifyOtherKeys CSI-u sequences arrive but nothing decodes them,
;; producing "u is undefined" errors on Ctrl+Shift combos.
(use-package kkp
  :ensure t
  :custom
  (kkp-terminal-query-timeout 0.5)
  (kkp-super-modifier 'super)
  (kkp-control-modifier 'control)
  :config
  (global-kkp-mode 1))

;; clipetty: send kills to the system clipboard over SSH/tmux via OSC 52.
(use-package clipetty
  :ensure t
  :bind ("M-w" . clipetty-kill-ring-save))

;; Terminal mouse support.
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; Guix Home: reconfigure dotfiles from within Emacs.
(defun jb-guix-home-reconfigure ()
  "Run `guix home reconfigure' to apply dotfiles changes.
Uses `compile' so output is displayed in a *compilation* buffer
with clickable error locations."
  (interactive)
  (compile "guix home reconfigure -L ~/dotfiles ~/dotfiles/home.scm"))

(global-set-key (kbd "C-c g r") #'jb-guix-home-reconfigure)

;; Ask before quitting Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Start the Emacs server for emacsclient usage.
;; Use EMACS_SESSION_NAME (set by zellij layout) as the server name so each
;; session gets its own socket and avoids conflicts.
(use-package server
  :ensure nil
  :defer t
  :commands server-start
  :hook (after-init . server-start)
  :init
  (when-let* ((name (getenv "EMACS_SESSION_NAME"))
              (_ (not (string-empty-p name))))
    (setq server-name name)))

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
  (auto-revert-avoid-polling t)
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

(auto-save-visited-mode 1)

;;; ============================================================================
;;; UI & appearance
;;; ============================================================================

;;; Font & cursor

(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 140
                    :weight 'regular)

(set-face-attribute 'fixed-pitch nil :family "Fira Code")

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

;; modus-themes: highly accessible themes (WCAG AAA contrast).
;; modus-operandi (light) / modus-vivendi (dark).
(use-package modus-themes
  :ensure t
  :demand t
  :bind (("<f5>" . modus-themes-toggle))
  :custom
  (modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-headings
   '((1 . (1.3))
     (2 . (1.2))
     (3 . (1.1))
     (t . (1.0))))
  (modus-themes-completions '((matches . (bold))
                              (selection . (semibold))))
  (modus-themes-prompts '(bold)))

;; Remember theme: persist the last-used theme across sessions.
;; On quit, the current theme name is saved to ~/.emacs-theme.
;; On startup, that theme is restored (defaulting to twilight-anti-bright).

(defvar jb-theme-file (expand-file-name "~/.emacs-theme")
  "File used to remember the last active theme between sessions.")

(defun jb-save-theme ()
  "Save the current theme name to `jb-theme-file'."
  (when-let* ((theme (car custom-enabled-themes)))
    (with-temp-file jb-theme-file
      (insert (symbol-name theme) "\n"))))

(defun jb-load-remembered-theme ()
  "Load the theme saved in `jb-theme-file', or twilight-anti-bright."
  (mapc #'disable-theme custom-enabled-themes)
  (let ((theme (when (file-exists-p jb-theme-file)
                 (with-temp-buffer
                   (insert-file-contents jb-theme-file)
                   (let ((name (string-trim (buffer-string))))
                     (unless (string-empty-p name)
                       (intern name)))))))
    (load-theme (or theme 'twilight-anti-bright) t)))

(defun toggle-twilight-theme ()
  "Toggle between twilight-bright and twilight-anti-bright themes."
  (interactive)
  (let ((current (car custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (if (eq current 'twilight-bright)
        (load-theme 'twilight-anti-bright t)
      (load-theme 'twilight-bright t)))
  (jb-save-theme))

;; Restore remembered theme after Elpaca has installed and activated theme packages.
(add-hook 'elpaca-after-init-hook #'jb-load-remembered-theme)
;; Save theme on quit.
(add-hook 'kill-emacs-hook #'jb-save-theme)
;; Persist modus theme switches through the remember-theme system.
(advice-add 'modus-themes-toggle :after (lambda (&rest _) (jb-save-theme)))

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
     '(";" . flash-jump)
     '("t" . uflash-treesitter)
     '("s" . query-replace)
     '("S" . query-replace-regexp)
     '("r" . consult-ripgrep)
     '("p" . easysession-switch-to-previous))

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
     '("y" . clipetty-kill-ring-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))

  (meow-setup)
  (setq meow-cursor-type-default '(bar . 2))
  (meow-global-mode 1)

  ;; s-m: toggle meow on/off in the current buffer.
  ;; When toggled on, forces normal state (full keymap) so the underlying
  ;; mode's bindings are completely suppressed.
  (defun meow-toggle ()
    "Toggle meow-mode in the current buffer.
When enabling, always enter normal state so that meow's full keymap
takes priority and the underlying mode's keys are suppressed."
    (interactive)
    (if meow-mode
        (progn
          (meow-mode -1)
          (meow--set-cursor-type '(bar . 2)))
      (meow-mode 1)
      (meow--switch-state 'normal)))
  (global-set-key (kbd "s-m") #'meow-toggle))

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

;; consult-project-extra: unified consult interface for project.el
;; (project buffers, project files, and known projects via narrowing).
(use-package consult-project-extra
  :ensure t
  :bind (("C-x p f" . consult-project-extra-find)
         ("C-x p F" . consult-project-extra-find-other-window)))

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
  :commands (avy-gotochar
             avy-goto-char-2
             avy-next))

(use-package casual-avy
  :ensure t
  :after avy
  :bind ("s-k" . casual-avy-tmenu))

;; Flash: incremental search-based navigation with jump labels.
;; Unlike avy's fixed-length input, flash lets you type an arbitrary
;; search pattern and shows labels alongside results as you type.
(use-package flash
  :ensure (:host github :repo "Prgebish/flash")
  :commands (flash-jump flash-jump-continue flash-treesitter)
  :custom
  (flash-multi-window t)
  (flash-backdrop t)
  (flash-rainbow t)
  (flash-nohlsearch t)
  (flash-search-history t)
  (flash-label-position 'overlay)
  :config
  (require 'flash-isearch)
  (flash-isearch-mode 1))

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
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :config
  ;; Disable meow by default in magit buffers so all magit keys work
  ;; natively.  Press s-m to toggle meow on for IJKL movement.
  (add-hook 'magit-mode-hook (lambda () (meow-mode -1))))

;; diff-hl: highlight uncommitted changes in the gutter.
(defun jb-diff-hl-highlight-inline (ovl type _shape)
  "Highlight OVL with a colored indicator at the right edge of the left margin.
TYPE is one of `insert', `delete', `change', etc.  The indicator
is placed in the left margin via a `display' spec, but padded
with spaces so it sits at the rightmost column — immediately
adjacent to the buffer text.  This keeps it visually attached to
olivetti-centered text instead of appearing at column 0."
  (let* ((face (intern (format "diff-hl-%s" type)))
         (margin-width (or (car (window-margins)) 0)))
    (when (< margin-width 2)
      (setq margin-width 2))
    (let* ((padding (propertize (make-string (max 0 (- margin-width 2)) ?\s) 'face 'default))
           (gap (propertize " " 'face 'default))
           (indicator (concat padding (propertize "▎" 'face face) gap)))
      (overlay-put ovl 'before-string
                   (propertize " " 'display
                               `((margin left-margin) ,indicator))))))

(defun jb-diff-hl-ensure-margin ()
  "Ensure a minimum 2-column left margin exists for diff-hl indicators.
Olivetti provides large margins when active; this covers the case
when it is not."
  (when (and diff-hl-mode (< (or left-margin-width 0) 2))
    (setq-local left-margin-width 2)
    ;; Force the window to pick up the new margin width.
    (dolist (win (get-buffer-window-list nil nil t))
      (set-window-buffer win (current-buffer)))))

(defun jb-diff-hl-update-on-window-change ()
  "Re-apply diff-hl margins and refresh overlays after a window change.
Ensures the margin indicator padding stays correct when olivetti
recalculates margins for a new window geometry."
  (when (bound-and-true-p diff-hl-mode)
    (jb-diff-hl-ensure-margin)
    (diff-hl-update)))

(defun jb-diff-hl-on-enable ()
  "Register or deregister the window-change hook for diff-hl."
  (if diff-hl-mode
      (add-hook 'window-configuration-change-hook
                #'jb-diff-hl-update-on-window-change nil t)
    (remove-hook 'window-configuration-change-hook
                 #'jb-diff-hl-update-on-window-change t)))

(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode
             global-diff-hl-mode
             diff-hl-flydiff-mode)
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (diff-hl-mode . jb-diff-hl-ensure-margin)
         (diff-hl-mode . jb-diff-hl-on-enable))
  :init
  (setq diff-hl-flydiff-delay 0.4)
  (setq diff-hl-show-staged-changes nil)
  (setq diff-hl-update-async t)
  (setq diff-hl-global-modes '(not pdf-view-mode image-mode))
  (setq diff-hl-highlight-function #'jb-diff-hl-highlight-inline)
  :config
  (diff-hl-flydiff-mode 1))

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

;; treesit-auto: keep the package for grammar installation only.
;; `global-treesit-auto-mode' is intentionally disabled because
;; `treesit-auto--ready-p' runs on every `find-file' and takes ~1.8s
;; per call (known issue: treesit-auto#135).  Instead, remap modes
;; manually via `major-mode-remap-alist'.
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt))

(setq major-mode-remap-alist
      '((python-mode    . python-ts-mode)
        (json-mode      . json-ts-mode)
        (sh-mode        . bash-ts-mode)
        (css-mode       . css-ts-mode)
        (yaml-mode      . yaml-ts-mode)
        (toml-mode      . toml-ts-mode)))

;; kdl-mode: major mode for KDL document language files (.kdl).
;; Uses tree-sitter for highlighting (auto-installs grammar), derives from
;; prog-mode so auto-olivetti picks it up.
(use-package kdl-mode
  :ensure t)

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
             easysession-save-mode
             easysession-load-including-geometry)

  :custom
  (easysession-mode-line-misc-info t)    ; Display session name in modeline
  (easysession-save-interval (* 10 60))  ; Save every 10 minutes

  :init
  ;; Track previous session for quick toggling
  (defvar easysession-previous-session nil
    "Name of the previously visited easysession.")

  (defun easysession-switch-to-previous ()
    "Switch to the previous easysession."
    (interactive)
    (if easysession-previous-session
        (easysession-switch-to easysession-previous-session)
      (user-error "No previous session")))

  (define-advice easysession-switch-to (:before (&rest _) track-previous)
    "Stash current session name before switching."
    (when (bound-and-true-p easysession--current-session-name)
      (setq easysession-previous-session easysession--current-session-name)))

  ;; Key mappings (C-c e prefix)
  (global-set-key (kbd "C-c e s") #'easysession-save)
  (global-set-key (kbd "C-c e l") #'easysession-switch-to)
  (global-set-key (kbd "C-c e L") #'easysession-switch-to-and-restore-geometry)
  (global-set-key (kbd "C-c e p") #'easysession-switch-to-previous)
  (global-set-key (kbd "C-c e r") #'easysession-rename)
  (global-set-key (kbd "C-c e R") #'easysession-reset)
  (global-set-key (kbd "C-c e d") #'easysession-delete)

  ;; Load session after elpaca has installed easysession.
  ;; If EMACS_SESSION_NAME is set, switch to that session (creating it
  ;; if needed); otherwise load the last/default session.
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              (let ((env-session (getenv "EMACS_SESSION_NAME")))
                (if (and env-session (not (string-empty-p env-session)))
                    (progn
                      (defvar easysession-confirm-new-session)
                      (let ((easysession-confirm-new-session nil))
                        (easysession-switch-to env-session)))
                  (easysession-load-including-geometry)))
              (easysession-save-mode 1))
            95))

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
;;; Languages
;;; ============================================================================

;;; Eglot workspace configuration (global, all languages).
;;; gopls: enable staticcheck linting and auto-complete unimported packages.
;;; basedpyright: default settings (works well out of the box).

(with-eval-after-load 'eglot
  ;; Register basedpyright as the Python LSP server.
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("basedpyright-langserver" "--stdio")))

  ;; Register terraform-ls as the LSP server for terraform-mode.
  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("terraform-ls" "serve")))

  ;; Register tinymist as the Typst LSP server.
  (add-to-list 'eglot-server-programs
               '(typst-ts-mode . ("tinymist" "lsp")))

  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

  ;; Disable the JSONRPC events buffer to avoid logging overhead.
  (setq eglot-events-buffer-size 0)

  (setq-default eglot-workspace-configuration
                '(:gopls (:staticcheck t
                          :completeUnimported t)
                  :basedpyright.analysis (:typeCheckingMode "basic"))))

;;; Go

;; Teach project.el to find go.mod as project root so Eglot scopes
;; the workspace correctly for Go modules.
(with-eval-after-load 'project
  (defun jb-project-find-go-module (dir)
    "Find the nearest parent directory containing go.mod."
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'jb-project-find-go-module))

;; go-ts-mode: tree-sitter based Go major mode (built-in).
;; Hooks: eglot for LSP, format + organize imports on save.
(use-package go-ts-mode
  :ensure nil
  :defer t
  :mode "\\.go\\'"
  :hook ((go-ts-mode . eglot-ensure))
  :config
  (setq go-ts-mode-indent-offset tab-width)
  (defun jb-go-before-save-hooks ()
    "Set up format-on-save and organize-imports-on-save for Go."
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    (add-hook 'before-save-hook
              (lambda ()
                (when (eglot-managed-p)
                  (call-interactively #'eglot-code-action-organize-imports)))
              nil t))
  (add-hook 'go-ts-mode-hook #'jb-go-before-save-hooks))

;;; Python

;; python-ts-mode: tree-sitter based Python major mode (built-in).
;; Hooks: eglot for LSP (basedpyright), compile-command for M-x compile.
;; No format-on-save — format manually with M-x eglot-format-buffer.
(use-package python
  :ensure nil
  :defer t
  :hook ((python-ts-mode . eglot-ensure))
  :config
  (defun jb-python-compile-command ()
    "Set compile-command to run the current Python file."
    (setq-local compile-command
                (concat "python3 "
                        (when buffer-file-name
                          (shell-quote-argument
                           (file-name-nondirectory buffer-file-name))))))
  (add-hook 'python-ts-mode-hook #'jb-python-compile-command))

;;; JSON

;; json-ts-mode: tree-sitter based JSON major mode (built-in).
;; treesit-auto handles auto-mode-alist and grammar installation.
;; Eglot has built-in server registration for vscode-json-language-server.
(use-package json-ts-mode
  :ensure nil
  :defer t
  :hook ((json-ts-mode . eglot-ensure))
  :custom
  (json-ts-mode-indent-offset 2))

;;; Terraform

;; terraform-mode: major mode for Terraform/HCL configuration files.
;; Provides syntax highlighting, indentation, and imenu.
;; hcl-mode is pulled in automatically as a dependency.
(use-package terraform-mode
  :ensure t
  :defer t
  :hook ((terraform-mode . eglot-ensure))
  :custom
  (terraform-indent-level 2)
  (terraform-format-on-save nil)
  :config
  (defun jb-terraform-before-save-hooks ()
    "Set up format-on-save for Terraform via eglot (terraform-ls)."
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
  (add-hook 'terraform-mode-hook #'jb-terraform-before-save-hooks))

;;; Markdown

;; markdown-mode: syntax highlighting, editing commands, and preview for
;; Markdown documents.  Also provides faces used by punct highlighting.
(use-package markdown-mode
  :ensure t
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

;;; Racket

;; Helper functions defined before use-package forms so they are
;; available immediately at init time, avoiding Elpaca's deferred
;; evaluation race condition where hooks fire before the use-package
;; body has been evaluated.

(defun jb-insert-lisp-section (section)
  "Insert a Lisp section header comment for SECTION at point.
Produces a line like: ;; Foo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  (interactive "sSection: ")
  (let ((suffix (make-string (max 0 (- 72 (length section) 4)) ?\;)))
    (insert (format ";; %s %s\n" section suffix))))

(defun jb-racket-mode-hook ()
  "Set up Flymake with the raco-review backend for Racket buffers."
  (add-hook 'flymake-diagnostic-functions #'jb-flymake-racket-review nil t)
  (flymake-mode 1))

;; Register hooks immediately — these run at init time, before Elpaca
;; processes its queue, so they're in place when racket-mode activates.
(add-hook 'racket-mode-hook #'jb-racket-mode-hook)
(add-hook 'racket-mode-hook #'racket-xp-mode)
(add-hook 'racket-hash-lang-mode-hook #'racket-xp-mode)
(add-hook 'racket-mode-hook #'rainbow-delimiters-mode)
(add-hook 'racket-hash-lang-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;; Punct (.punct) markdown highlighting for racket-hash-lang-mode.
;;
;; Punct files are CommonMark markdown with inline Racket via #lang punct.
;; racket-hash-lang-mode provides Racket back-end integration but its
;; color-lexer treats prose as undifferentiated text.  We layer markdown
;; font-lock keywords on top so headings, bold, italic, links, etc. are
;; visually highlighted while keeping full Racket tooling.
;;
;; This works because racket-hash-lang-mode explicitly supports
;; font-lock-add-keywords: it applies back-end token faces first (setting
;; syntax-table properties for comments/strings), then runs
;; font-lock-fontify-keywords-region for any added keyword rules.

(defvar jb-punct-markdown-keywords
  '(;; YAML front matter: --- delimiters and key: value pairs
    ("^---$" 0 'markdown-markup-face prepend)
    ("^\\([[:alpha:]][[:alnum:]_-]*\\):\\s-+\\(.*\\)$"
     (1 'markdown-metadata-key-face prepend)
     (2 'markdown-metadata-value-face prepend))

    ;; ATX headings: # through ######
    ;; Ordered long-to-short so ###### matches before #.
    ("^\\(######\\)\\s-+\\(.*\\)$"
     (1 'markdown-header-delimiter-face prepend)
     (2 'markdown-header-face-6 prepend))
    ("^\\(#####\\)\\s-+\\(.*\\)$"
     (1 'markdown-header-delimiter-face prepend)
     (2 'markdown-header-face-5 prepend))
    ("^\\(####\\)\\s-+\\(.*\\)$"
     (1 'markdown-header-delimiter-face prepend)
     (2 'markdown-header-face-4 prepend))
    ("^\\(###\\)\\s-+\\(.*\\)$"
     (1 'markdown-header-delimiter-face prepend)
     (2 'markdown-header-face-3 prepend))
    ("^\\(##\\)\\s-+\\(.*\\)$"
     (1 'markdown-header-delimiter-face prepend)
     (2 'markdown-header-face-2 prepend))
    ("^\\(#\\)\\s-+\\(.*\\)$"
     (1 'markdown-header-delimiter-face prepend)
     (2 'markdown-header-face-1 prepend))

    ;; Horizontal rules: three or more -, *, or _ (possibly spaced)
    ("^\\s-*\\([-*_]\\s-*\\)\\{3,\\}$" 0 'markdown-hr-face prepend)

    ;; Blockquotes: > at start of line
    ("^\\s-*\\(>\\)\\s-?\\(.*\\)$"
     (1 'markdown-markup-face prepend)
     (2 'markdown-blockquote-face prepend))

    ;; Unordered list markers: -, *, + at start of line
    ("^\\s-*\\([-*+]\\)\\s-+" 1 'markdown-list-face prepend)
    ;; Ordered list markers: 1. 2. etc.
    ("^\\s-*\\([0-9]+\\.\\)\\s-+" 1 'markdown-list-face prepend)

    ;; Fenced code block delimiters: ``` or ~~~
    ;; Captures optional language identifier separately.
    ("^\\s-*\\(```\\|~~~\\)\\([[:alpha:]][[:alnum:]_+-]*\\)?\\s-*$"
     (1 'markdown-markup-face prepend)
     (2 'markdown-language-keyword-face prepend t))

    ;; Inline code: `code`
    ("\\(`\\)\\([^`\n]+?\\)\\(`\\)"
     (1 'markdown-markup-face prepend)
     (2 'markdown-inline-code-face prepend)
     (3 'markdown-markup-face prepend))

    ;; Bold: **text** or __text__
    ("\\(\\*\\*\\)\\([^*\n]+?\\)\\(\\*\\*\\)"
     (1 'markdown-markup-face prepend)
     (2 'markdown-bold-face prepend)
     (3 'markdown-markup-face prepend))
    ("\\(__\\)\\([^_\n]+?\\)\\(__\\)"
     (1 'markdown-markup-face prepend)
     (2 'markdown-bold-face prepend)
     (3 'markdown-markup-face prepend))

    ;; Italic: *text* or _text_ (but not ** or __)
    ("\\(?:^\\|[^*]\\)\\(\\*\\)\\([^*\n]+?\\)\\(\\*\\)\\(?:[^*]\\|$\\)"
     (1 'markdown-markup-face prepend)
     (2 'markdown-italic-face prepend)
     (3 'markdown-markup-face prepend))
    ("\\(?:^\\|[^_]\\)\\(_\\)\\([^_\n]+?\\)\\(_\\)\\(?:[^_]\\|$\\)"
     (1 'markdown-markup-face prepend)
     (2 'markdown-italic-face prepend)
     (3 'markdown-markup-face prepend))

    ;; Strikethrough: ~~text~~
    ("\\(~~\\)\\([^~\n]+?\\)\\(~~\\)"
     (1 'markdown-markup-face prepend)
     (2 'markdown-strike-through-face prepend)
     (3 'markdown-markup-face prepend))

    ;; Links: [text](url) with optional "title"
    ("\\(\\[\\)\\([^]\n]*\\)\\(\\]\\)(\\([^)\" \t\n]*\\)\\(?:\\s-+\"\\([^\"]*\\)\"\\)?)"
     (1 'markdown-markup-face prepend)
     (2 'markdown-link-face prepend)
     (3 'markdown-markup-face prepend)
     (4 'markdown-url-face prepend)
     (5 'markdown-link-title-face prepend t))

    ;; Images: ![alt](url) with optional "title"
    ("\\(!\\[\\)\\([^]\n]*\\)\\(\\]\\)(\\([^)\" \t\n]*\\)\\(?:\\s-+\"\\([^\"]*\\)\"\\)?)"
     (1 'markdown-markup-face prepend)
     (2 'markdown-link-face prepend)
     (3 'markdown-markup-face prepend)
     (4 'markdown-url-face prepend)
     (5 'markdown-link-title-face prepend t))

    ;; Reference links: [text][ref]
    ("\\(\\[\\)\\([^]\n]*\\)\\(\\]\\)\\(\\[\\)\\([^]\n]*\\)\\(\\]\\)"
     (1 'markdown-markup-face prepend)
     (2 'markdown-link-face prepend)
     (3 'markdown-markup-face prepend)
     (4 'markdown-markup-face prepend)
     (5 'markdown-reference-face prepend)
     (6 'markdown-markup-face prepend))

    ;; Reference definitions: [ref]: url "title"
    ("^\\s-*\\(\\[\\)\\([^]\n]+\\)\\(\\]\\):\\s-+\\(\\S-+\\)\\(?:\\s-+\"\\([^\"]*\\)\"\\)?$"
     (1 'markdown-markup-face prepend)
     (2 'markdown-reference-face prepend)
     (3 'markdown-markup-face prepend)
     (4 'markdown-url-face prepend)
     (5 'markdown-link-title-face prepend t))

    ;; Footnote markers: [^label]
    ("\\(\\[\\^\\)\\([^]\n]+\\)\\(\\]\\)"
     (1 'markdown-markup-face prepend)
     (2 'markdown-footnote-marker-face prepend)
     (3 'markdown-markup-face prepend))

    ;; GFM tables: lines with | delimiters
    ("^\\s-*|.*|\\s-*$" 0 'markdown-table-face prepend)

    ;; HTML comments: <!-- ... -->
    ("\\(<!--\\)\\(\\(?:.\\|\n\\)*?\\)\\(-->\\)"
     (1 'markdown-markup-face prepend)
     (2 'markdown-comment-face prepend)
     (3 'markdown-markup-face prepend))

    ;; Angle-bracket URLs: <http://...>
    ("\\(<\\)\\(https?://[^>\n]+\\)\\(>\\)"
     (1 'markdown-markup-face prepend)
     (2 'markdown-plain-url-face prepend)
     (3 'markdown-markup-face prepend))

    ;; Backslash escapes: \* \[ \] etc.
    ("\\(\\\\\\)[\\\\`*_{}\\[\\]()#+.!~|>-]"
     1 'markdown-markup-face prepend)

    ;; Task list checkboxes: - [ ] or - [x] or - [X]
    ("^\\s-*[-*+]\\s-+\\(\\[[ xX]\\]\\)"
     1 'markdown-gfm-checkbox-face prepend)

    ;; Punct inline Racket: •(...) — highlight the bullet marker
    ("\\(•\\)" 1 'markdown-markup-face prepend))
  "Font-lock keywords for CommonMark constructs in Punct files.
Uses `prepend' so markdown faces override the back-end's `text' token
face while respecting syntax-table properties that prevent matches
inside comments and strings.")

(defun jb-punct-hash-lang-hook (module-language)
  "Add or remove markdown font-lock keywords based on MODULE-LANGUAGE.
When the hash-lang is punct, layer CommonMark highlighting on top of
racket-hash-lang-mode's token-based fontification."
  (if (or (and (stringp module-language)
               (string-match-p "punct" module-language))
          ;; Fallback: detect via buffer file extension when the lang
          ;; doesn't supply module-language info.
          (and (null module-language)
               buffer-file-name
               (string-match-p "\\.punct\\'" buffer-file-name)))
      (progn
        (require 'markdown-mode)
        (font-lock-add-keywords nil jb-punct-markdown-keywords 'append)
        (font-lock-flush))
    ;; Non-punct lang: remove markdown keywords if present.
    (font-lock-remove-keywords nil jb-punct-markdown-keywords)
    (font-lock-flush)))

(add-hook 'racket-hash-lang-module-language-hook #'jb-punct-hash-lang-hook)

;; rainbow-delimiters: colorize nested delimiters for visual depth cues.
(use-package rainbow-delimiters
  :ensure t
  :defer t)

;; Flymake backend for raco review: lint Racket source files.
(defvar-local jb-flymake-racket-review--proc nil
  "The running raco-review process for the current buffer, if any.")

(defun jb-flymake-racket-review (report-fn &rest _args)
  "Flymake backend that runs `raco review' on the current file.
REPORT-FN is the Flymake callback for reporting diagnostics."
  (unless (executable-find "raco")
    (error "Cannot find `raco' on exec-path"))
  (let ((source (current-buffer))
        (filename (buffer-file-name)))
    (when filename
      ;; Kill any previous running process.
      (when (process-live-p jb-flymake-racket-review--proc)
        (kill-process jb-flymake-racket-review--proc))
      (setq jb-flymake-racket-review--proc
            (make-process
             :name "flymake-racket-review"
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer " *flymake-racket-review*")
             :command (list "raco" "review" filename)
             :sentinel
             (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (unwind-protect
                     (if (with-current-buffer source
                           (eq proc jb-flymake-racket-review--proc))
                         (with-current-buffer (process-buffer proc)
                           (goto-char (point-min))
                           (let ((diags nil))
                             (while (search-forward-regexp
                                     "^.+?:\\([0-9]+\\):\\([0-9]+\\):\\(error\\|warning\\):\\(.*\\)$"
                                     nil t)
                               (let* ((line (string-to-number (match-string 1)))
                                      (col (string-to-number (match-string 2)))
                                      (severity (match-string 3))
                                      (msg (string-trim (match-string 4)))
                                      (type (if (string= severity "error") :error :warning))
                                      (region (flymake-diag-region source line col)))
                                 (when region
                                   (push (flymake-make-diagnostic
                                          source (car region) (cdr region) type msg)
                                         diags))))
                             ;; Non-zero exit with no diagnostics: tool error.
                             (when (and (not diags)
                                        (not (zerop (process-exit-status proc)))
                                        (not (eq (process-status proc) 'signal)))
                               (goto-char (point-min))
                               (funcall report-fn :panic
                                        :explanation
                                        (buffer-substring
                                         (point-min)
                                         (min (point-max)
                                              (line-end-position)))))
                             (unless (eq (process-status proc) 'signal)
                               (funcall report-fn diags))))
                       (flymake-log :warning "Canceling obsolete raco review check %s" proc))
                   (kill-buffer (process-buffer proc))))))))))

;; racket-hash-lang: major mode for Racket hash-lang files (.rhm, .scrbl, .punct).
(use-package racket-hash-lang
  :ensure (racket-mode :host github :repo "greghendershott/racket-mode")
  :defer t
  :mode (("\\.rhm\\'" . racket-hash-lang-mode)
         ("\\.scrbl\\'" . racket-hash-lang-mode)
         ("\\.punct\\'" . racket-hash-lang-mode))
  :bind (:map racket-hash-lang-mode-map
              ("C-c C-d" . racket-xp-describe)
              ("C-c C-r" . racket-xp-rename)
              ("C-c ."   . xref-find-definitions)
              ("C-c ,"   . xref-go-back)))

;; racket-mode: major mode for Racket source files.
(use-package racket-mode
  :ensure nil
  :defer t
  :mode (("\\.rkt\\'" . racket-mode))
  :config
  (setq racket-repl-buffer-name-function #'racket-repl-buffer-name-project
        racket-show-functions '(racket-show-echo-area))
  :bind (:map racket-mode-map
              ("C-c C-d" . racket-xp-describe)
              ("C-c C-r" . racket-xp-rename)
              ("C-c C-s" . jb-insert-lisp-section)
              ("C-c r t" . racket-tidy-requires)
              ("C-c r i" . racket-add-require-for-identifier)
              ("C-c ."   . xref-find-definitions)
              ("C-c ,"   . xref-go-back)))

;; racket-xp-mode: cross-reference and analysis annotations.
;; Hooks registered above via standalone add-hook calls.
(use-package racket-xp
  :ensure nil
  :defer t)

;;; Typst

;; typst-ts-mode: tree-sitter based major mode for Typst documents.
;; Provides syntax highlighting, indentation, imenu, and raw block
;; highlighting.  LSP provided by tinymist via eglot.
(use-package typst-ts-mode
  :ensure (:host codeberg :repo "meow_king/typst-ts-mode")
  :defer t
  :mode "\\.typ\\'"
  :hook ((typst-ts-mode . eglot-ensure))
  :custom
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (add-to-list 'treesit-language-source-alist
               '(typst "https://github.com/uben0/tree-sitter-typst"
                       "master" "src"))
  (unless (treesit-language-available-p 'typst)
    (treesit-install-language-grammar 'typst)))

;; typst-preview: live preview of Typst documents via tinymist.
;; Fixed ports allow SSH port forwarding so the preview can be viewed
;; in a local browser while editing remotely in terminal Emacs.
;; Forward the data-plane port: ssh -L 23625:127.0.0.1:23625 remote-host
;; Then open http://localhost:23625 in your local browser.
(use-package typst-preview
  :ensure t
  :custom
  (typst-preview-open-browser-automatically nil)
  :init
  ;; The upstream package hardcodes random ports (127.0.0.1:0) for the
  ;; --host, --data-plane-host, and --control-plane-host flags, and
  ;; tinymist rejects duplicate flags.  This advice rewrites the
  ;; hardcoded values to fixed ports so we can set up SSH port
  ;; forwarding in advance.  Placed in :init so the advice is active
  ;; before typst-preview-start is ever called.
  (defvar jb-typst-preview-data-plane-host "127.0.0.1:23625"
    "Fixed data-plane host:port for typst-preview SSH forwarding.")
  (defvar jb-typst-preview-control-plane-host "127.0.0.1:23626"
    "Fixed control-plane host:port for typst-preview SSH forwarding.")

  (define-advice start-process (:filter-args (args) jb-typst-preview-fixed-ports)
    "Rewrite typst-preview process args to use fixed ports.
Also remove the deprecated --host flag so tinymist does not start a
redundant static file server on a random port."
    (when (string= (car args) "typst-preview-proc")
      (let ((result nil)
            (rest (cddr args)))
        ;; Walk the CLI args, dropping --host and its value, rewriting
        ;; --data-plane-host and --control-plane-host values.
        (while rest
          (cond
           ((string= (car rest) "--host")
            (setq rest (cddr rest)))           ; skip flag + value
           ((string= (car rest) "--data-plane-host")
            (push (pop rest) result)           ; keep flag
            (push jb-typst-preview-data-plane-host result) ; replace value
            (pop rest))                        ; skip original value
           ((string= (car rest) "--control-plane-host")
            (push (pop rest) result)           ; keep flag
            (push jb-typst-preview-control-plane-host result)
            (pop rest))
           (t
            (push (pop rest) result))))
        (setq args (append (list (car args) (cadr args)) (nreverse result)))))
    args))

;;; ============================================================================
;;; Load custom.el
;;; ============================================================================

;; Load custom.el after Elpaca has activated all packages.
(add-hook 'elpaca-after-init-hook
          (lambda () (load custom-file 'noerror 'no-message)))

;;; post-init.el ends here
