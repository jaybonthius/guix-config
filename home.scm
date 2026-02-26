(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu home services shepherd)
              (gnu packages emacs)
              (gnu packages fonts)
               (gnu packages commencement)
               (gnu packages golang)
               (gnu packages golang-apps)
               (gnu packages linux)
               (gnu packages python)
              (gnu packages racket)
              (gnu packages rust-apps)
             (gnu packages shells)
             (gnu packages shellutils)
             (gnu packages terminals)
             (gnu services)
             (gnu services shepherd)
             (guix gexp)
             (minimal-emacs)
               (opencode)
               (tinymist)
                (uv)
               (zellij)
              (zjstatus))

(home-environment
 (packages
     (list emacs-no-x
           earlyoom
           eza
          fish
          font-fira-code
          fzf
          gcc-toolchain
          go
          gopls
          opencode
          python
          racket
          ripgrep
           starship
           tinymist
           typst
           uv
           zellij
           zoxide))
 (services
  (list
   ;; Fish shell configuration
   (service home-fish-service-type
            (home-fish-configuration
             (config
              (list (local-file "fish/config.fish")))))

   ;; Fish plugins: symlink fisher and fish_plugins
   ;; into ~/.config/fish/
   (simple-service
    'fish-plugins
    home-xdg-configuration-files-service-type
    (list (list "fish/functions/fisher.fish"
                (local-file "fish/functions/fisher.fish"))
          (list "fish/functions/zlast.fish"
                (local-file "fish/functions/zlast.fish"))
          (list "fish/fish_plugins"
                (local-file "fish/fish_plugins"))))

   ;; Emacs configuration: symlink minimal-emacs.d init files and
   ;; personal config files into ~/.config/emacs/
   (simple-service
    'emacs-config
    home-xdg-configuration-files-service-type
    (list (list "emacs/init.el"
                (file-append minimal-emacs.d
                             "/share/minimal-emacs.d/init.el"))
          (list "emacs/early-init.el"
                (file-append minimal-emacs.d
                             "/share/minimal-emacs.d/early-init.el"))
          (list "emacs/pre-early-init.el"
                (local-file "emacs/pre-early-init.el"))
          (list "emacs/pre-init.el"
                (local-file "emacs/pre-init.el"))
           (list "emacs/post-init.el"
                 (local-file "emacs/post-init.el"))))

   ;; Git configuration
   (simple-service
    'git-config
    home-xdg-configuration-files-service-type
    (list (list "git/config"
                (local-file "git/config"))))

    ;; Zellij configuration: symlink config and layout files
    ;; into ~/.config/zellij/
    (simple-service
     'zellij-config
     home-xdg-configuration-files-service-type
     (list (list "zellij/config.kdl"
                 (local-file "zellij/config.kdl"))
           (list "zellij/layouts/project.kdl"
                 (local-file "zellij/layouts/project.kdl"))
            (list "zellij/layouts/copilot.kdl"
                  (local-file "zellij/layouts/copilot.kdl"))
            (list "zellij/themes/modus_vivendi.kdl"
                  (local-file "zellij/themes/modus_vivendi.kdl"))
            (list "zellij/themes/modus_vivendi_deuteranopia.kdl"
                  (local-file "zellij/themes/modus_vivendi_deuteranopia.kdl"))
            (list "zellij/themes/modus_vivendi_tinted.kdl"
                  (local-file "zellij/themes/modus_vivendi_tinted.kdl"))
            (list "zellij/themes/modus_vivendi_tritanopia.kdl"
                  (local-file "zellij/themes/modus_vivendi_tritanopia.kdl"))
            (list "zellij/themes/modus_operandi.kdl"
                  (local-file "zellij/themes/modus_operandi.kdl"))
            (list "zellij/themes/modus_operandi_deuteranopia.kdl"
                  (local-file "zellij/themes/modus_operandi_deuteranopia.kdl"))
            (list "zellij/themes/modus_operandi_tinted.kdl"
                  (local-file "zellij/themes/modus_operandi_tinted.kdl"))
            (list "zellij/themes/modus_operandi_tritanopia.kdl"
                  (local-file "zellij/themes/modus_operandi_tritanopia.kdl"))
            (list "zellij/plugins/zjstatus.wasm"
                  (file-append zjstatus
                               "/share/zjstatus/zjstatus.wasm"))
            (list "zellij/plugins/zellij-emacs-session.wasm"
                  (local-file
                   "zellij-emacs-session/zellij-emacs-session.wasm"))))

    ;; Emacs daemon: single long-running instance managed by Shepherd.
    ;; Connect with: emacsclient -t
    (simple-service
     'emacs-daemon
     home-shepherd-service-type
     (list (shepherd-service
            (provision '(emacs-daemon))
            (documentation "Run Emacs as a daemon.")
            (start #~(make-forkexec-constructor
                      (list #$(file-append emacs-no-x "/bin/emacs")
                            "--fg-daemon")
                      #:log-file
                      (string-append (or (getenv "XDG_STATE_HOME")
                                         (string-append (getenv "HOME")
                                                        "/.local/state"))
                                     "/emacs-daemon.log")))
             (stop #~(make-kill-destructor)))))

     ;; earlyoom: kill processes before kernel OOM to prevent hard freezes.
     ;; Runs with defaults: triggers at 10% free memory or swap.
     (simple-service
      'earlyoom
      home-shepherd-service-type
      (list (shepherd-service
             (provision '(earlyoom))
             (documentation "Run earlyoom OOM killer daemon.")
             (start #~(make-forkexec-constructor
                       (list #$(file-append earlyoom "/bin/earlyoom"))))
             (stop #~(make-kill-destructor))))))))


