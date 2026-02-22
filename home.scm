(use-modules (gnu home)
             (gnu home services shells)
             (gnu packages emacs)
             (gnu packages shells)
             (gnu services)
             (guix gexp)
             (opencode))

(home-environment
 (packages
  (list emacs-no-x fish opencode))
 (services
  (list
   (service home-fish-service-type
            (home-fish-configuration
             (config
              (list (local-file "config.fish"))))))))
