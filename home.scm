(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu packages emacs)
             (gnu packages fonts)
             (gnu packages racket)
             (gnu packages rust-apps)
             (gnu packages shells)
             (gnu packages shellutils)
             (gnu packages terminals)
             (gnu services)
             (guix gexp)
             (minimal-emacs)
             (opencode)
             (zellij))

(home-environment
 (packages
   (list emacs-no-x
         eza
         fish
         font-fira-code
         fzf
         opencode
         racket
         starship
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
                 (local-file "zellij/layouts/project.kdl")))))))
