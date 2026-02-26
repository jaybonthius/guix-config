((auto-olivetti :source "elpaca-menu-lock-file" :recipe
                (:source "elpaca-menu-lock-file" :protocol https :inherit t
                         :depth treeless :host sourcehut :repo
                         "ashton314/auto-olivetti" :package "auto-olivetti" :ref
                         "406b2fca6b320f323d6d2f96240bc4c8551c12a9"))
 (autothemer :source "elpaca-menu-lock-file" :recipe
             (:package "autothemer" :fetcher github :repo "jasonm23/autothemer"
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https :inherit
                       t :depth treeless :ref
                       "e62bf83414abd8b1cefafb7480612faa30ed7878"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :protocol https :inherit t
                :depth treeless :ref "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (buffer-terminator :source "elpaca-menu-lock-file" :recipe
                    (:package "buffer-terminator" :fetcher github :repo
                              "jamescherti/buffer-terminator.el" :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el"
                                         "*-test.el" "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file" :protocol https
                              :inherit t :depth treeless :ref
                              "eeaefca1860f62c5b7ff11f7a0550d7b11892151"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https :inherit t
                 :depth treeless :ref "2b2a5c5bef16eddcce507d9b5804e5a0cc9481ae"))
 (casual :source "elpaca-menu-lock-file" :recipe
         (:package "casual" :fetcher github :repo "kickingvegas/casual"
                   :old-names
                   (casual-agenda casual-bookmarks casual-calc casual-dired
                                  casual-editkit casual-ibuffer casual-info
                                  casual-isearch cc-isearch-menu casual-lib
                                  casual-re-builder)
                   :files (:defaults "docs/images") :source
                   "elpaca-menu-lock-file" :protocol https :inherit t :depth
                   treeless :ref "ec52aca21dc6402ef6dce6a46b5714337d578329"))
 (casual-avy :source "elpaca-menu-lock-file" :recipe
             (:package "casual-avy" :fetcher github :repo
                       "kickingvegas/casual-avy" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https :inherit
                       t :depth treeless :ref
                       "c5bc8e9d57a843f75e6125f097550414af3d5ec7"))
 (clipetty :source "elpaca-menu-lock-file" :recipe
           (:package "clipetty" :repo "spudlyo/clipetty" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https :inherit t
                     :depth treeless :ref
                     "01b39044b9b65fa4ea7d3166f8b1ffab6f740362"))
 (compile-angel :source "elpaca-menu-lock-file" :recipe
                (:package "compile-angel" :fetcher github :repo
                          "jamescherti/compile-angel.el" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol https
                          :inherit t :depth treeless :ref
                          "18ad7b2b03f6caaacb7c11798dd9bbcaf69c5f04"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
             treeless :ref "8bf87d45e169ebc091103b2aae325aece3aa804d"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :ref
                    "d1d39d52151a10f7ca29aa291886e99534cc94db"))
 (consult-project-extra :source "elpaca-menu-lock-file" :recipe
                        (:package "consult-project-extra" :fetcher github :repo
                                  "Qkessler/consult-project-extra" :files
                                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                   "*.texinfo" "doc/dir" "doc/*.info"
                                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                   "docs/dir" "docs/*.info" "docs/*.texi"
                                   "docs/*.texinfo"
                                   (:exclude ".dir-locals.el" "test.el"
                                             "tests.el" "*-test.el" "*-tests.el"
                                             "LICENSE" "README*" "*-pkg.el"))
                                  :source "elpaca-menu-lock-file" :protocol
                                  https :inherit t :depth treeless :ref
                                  "2b3fa36fd3a14deacf594f4acd54d220d6890c55"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files
                  (:defaults "extensions/corfu-*.el") :fetcher github :source
                  "elpaca-menu-lock-file" :protocol https :inherit t :depth
                  treeless :ref "abfe0003d71b61ffdcf23fc6e546643486daeb69"))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
           (:package "csv-mode" :repo
                     ("https://github.com/emacsmirror/gnu_elpa" . "csv-mode")
                     :branch "externals/csv-mode" :files ("*" (:exclude ".git"))
                     :source "elpaca-menu-lock-file" :protocol https :inherit t
                     :depth treeless :ref
                     "ba5dc934b9dbdc2b57ab1917a669cdfd7d1838d3"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "elpaca-menu-lock-file"
                 :protocol https :inherit t :depth treeless :ref
                 "d3a84021dbe48dba63b52ef7665651e0cf02e915"))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :ref
                    "3eefe68941933c8549049502007411ed2bf70387"))
 (dumb-jump :source "elpaca-menu-lock-file" :recipe
            (:package "dumb-jump" :repo "jacktasia/dumb-jump" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t
                      :depth treeless :ref
                      "04b9ad6866bc316078f044d9f3fe32682b1ead77"))
 (easysession :source "elpaca-menu-lock-file" :recipe
              (:package "easysession" :fetcher github :repo
                        "jamescherti/easysession.el" :files
                        (:defaults "extensions/easysession*.el") :source
                        "elpaca-menu-lock-file" :protocol https :inherit t
                        :depth treeless :ref
                        "6198a7b212284600d24a72ff09b911b4f67217da"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github
                       :files (:defaults (:exclude "elisp-refs-bench.el"))
                       :source "elpaca-menu-lock-file" :protocol https :inherit
                       t :depth treeless :ref
                       "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :protocol https :inherit ignore :depth 1 :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "a7fee1f17e5f9a6aac6ff6abc5e834fab12e3488" :files
            (:defaults "elpaca-test.el" (:exclude "extensions")) :build
            (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el") :main
                               "extensions/elpaca-use-package.el" :build
                               (:not elpaca--compile-info) :source
                               "Elpaca extensions" :protocol https :inherit t
                               :depth treeless :ref
                               "a7fee1f17e5f9a6aac6ff6abc5e834fab12e3488"))
 (embark :source "elpaca-menu-lock-file" :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github :files
                   ("embark.el" "embark-org.el" "embark.texi") :source
                   "elpaca-menu-lock-file" :protocol https :inherit t :depth
                   treeless :ref "cf0f0f0c29e7f30df23693b6380072a3b6733841"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
                 (:package "embark-consult" :repo "oantolin/embark" :fetcher
                           github :files ("embark-consult.el") :source
                           "elpaca-menu-lock-file" :protocol https :inherit t
                           :depth treeless :ref
                           "cf0f0f0c29e7f30df23693b6380072a3b6733841"))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                  "*.texinfo" "doc/dir" "doc/*.info"
                                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                  "docs/dir" "docs/*.info" "docs/*.texi"
                                  "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el" "*-tests.el"
                                            "LICENSE" "README*" "*-pkg.el"))
                                 :source "elpaca-menu-lock-file" :protocol https
                                 :inherit t :depth treeless :ref
                                 "7552abf032a383ff761e7d90e6b5cbb4658a728a"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
              treeless :ref "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flash :source "elpaca-menu-lock-file" :recipe
        (:package "flash" :fetcher github :repo "Prgebish/flash" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :protocol https :inherit t
                  :depth treeless :host github :ref
                  "f93bc5baf60f23c96239d8bfe154d2c6727c8c66"))
 (gruvbox-theme :source "elpaca-menu-lock-file" :recipe
                (:package "gruvbox-theme" :fetcher github :repo
                          "greduan/emacs-theme-gruvbox" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol https
                          :inherit t :depth treeless :ref
                          "6cbf80b6cde3c2390502dc94a911ab7378495249"))
 (hcl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "hcl-mode" :repo "hcl-emacs/hcl-mode" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https :inherit t
                     :depth treeless :ref
                     "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed"))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :ref
                    "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :ref
                    "9540fc414014822dde00f0188b74e17ac99e916d"))
 (kdl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "kdl-mode" :fetcher github :repo
                     "taquangtrung/emacs-kdl-mode" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https :inherit t
                     :depth treeless :ref
                     "2d849e298199f490e4894c01764a8a83decd704a"))
 (kkp :source "elpaca-menu-lock-file" :recipe
      (:package "kkp" :fetcher github :repo "benotn/kkp" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :protocol https :inherit t
                :depth treeless :ref "1a7b4f395aa4e1e04afc45fe2dbd6a045871803b"))
 (ligature :source "elpaca-menu-lock-file" :recipe
           (:package "ligature" :fetcher github :repo "mickeynp/ligature.el"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https :inherit t
                     :depth treeless :ref
                     "6ac1634612dbd42f7eb81ecaf022bd239aabb954"))
 (lithium :source "elpaca-menu-lock-file" :recipe
          (:package "lithium" :fetcher github :repo "countvajhula/lithium"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :ref
                    "d8a8e1287df0d42e0357f8dda450d2c2c0294a75"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source "elpaca-menu-lock-file"
                  :protocol https :inherit t :depth treeless :ref
                  "de61773fc378d40f478f8daf67543a51889ecded"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
                   ("git-hooks" "git-hooks/*")
                   (:exclude "lisp/magit-section.el"))
                  :source "elpaca-menu-lock-file" :protocol https :inherit t
                  :depth treeless :ref
                  "0376b01d7d2036666dc3e0bc318a235fd88a2686"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit"
                          :files
                          ("lisp/magit-section.el" "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "elpaca-menu-lock-file" :protocol https
                          :inherit t :depth treeless :ref
                          "0376b01d7d2036666dc3e0bc318a235fd88a2686"))
 (mantra :source "elpaca-menu-lock-file" :recipe
         (:package "mantra" :fetcher github :repo "countvajhula/mantra" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https :inherit t
                   :depth treeless :ref
                   "49f885b8947662bc837297f5c8a225d65dafcacd"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https :inherit
                       t :depth treeless :ref
                       "142e4da1bd76dc5bdbbfd15532571b8a271e680e"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol https
                          :inherit t :depth treeless :ref
                          "9de2df5a9f2f864c82ec112d3369154767a2bb49"))
 (meow :source "elpaca-menu-lock-file" :recipe
       (:package "meow" :repo "meow-edit/meow" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https :inherit t
                 :depth treeless :host github :ref
                 "2246f593552c6208bac533de9af04b085fafa6fa"))
 (modus-themes :source "elpaca-menu-lock-file" :recipe
               (:package "modus-themes" :fetcher github :repo
                         "protesilaos/modus-themes" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol https
                         :inherit t :depth treeless :ref
                         "b45b0d8a7c1e8b9679eea7ec366207280e4f89ed"))
 (olivetti :source "elpaca-menu-lock-file" :recipe
           (:package "olivetti" :fetcher github :repo "rnkn/olivetti" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https :inherit t
                     :depth treeless :ref
                     "845eb7a95a3ca3325f1120c654d761b91683f598"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t
                      :depth treeless :ref
                      "3a2a32181f7a5bd7b633e40d89de771a5dd88cc7"))
 (page-break-lines :source "elpaca-menu-lock-file" :recipe
                   (:package "page-break-lines" :fetcher github :repo
                             "purcell/page-break-lines" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                              "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el"
                                        "*-test.el" "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol https
                             :inherit t :depth treeless :ref
                             "f54aa2b96f6ed249e103346cdb872c97c3c98054"))
 (paredit :source "elpaca-menu-lock-file" :recipe
          (:package "paredit" :fetcher git :url
                    "https://paredit.org/paredit.git" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :ref
                    "af075775af91f2dbc63b915d762b4aec092946c4"))
 (persist-text-scale :source "elpaca-menu-lock-file" :recipe
                     (:package "persist-text-scale" :fetcher github :repo
                               "jamescherti/persist-text-scale.el" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el"
                                          "*-test.el" "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :protocol https
                               :inherit t :depth treeless :ref
                               "1d2983986a32da95c878ce05929f43ecc370c82c"))
 (popup :source "elpaca-menu-lock-file" :recipe
        (:package "popup" :fetcher github :repo "auto-complete/popup-el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :protocol https :inherit t
                  :depth treeless :ref
                  "45a0b759076ce4139aba36dde0a2904136282e73"))
 (pubsub :source "elpaca-menu-lock-file" :recipe
         (:package "pubsub" :fetcher github :repo "countvajhula/pubsub" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https :inherit t
                   :depth treeless :ref
                   "f1e2b72d773b5221d577c64c868716ef0df2de53"))
 (racket-mode :source "elpaca-menu-lock-file" :recipe
              (:package "racket-mode" :fetcher github :repo
                        "greghendershott/racket-mode" :files
                        (:defaults "*.rkt" ("racket" "racket/*")
                                   (:exclude "racket/example/*" "racket/test/*"))
                        :source "elpaca-menu-lock-file" :protocol https :inherit
                        t :depth treeless :host github :ref
                        "71f27c643dadf70847e447e773760df6df48fe5a"))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
                     (:package "rainbow-delimiters" :fetcher github :repo
                               "Fanael/rainbow-delimiters" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el"
                                          "*-test.el" "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :protocol https
                               :inherit t :depth treeless :ref
                               "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (repeat-ring :source "elpaca-menu-lock-file" :recipe
              (:package "repeat-ring" :fetcher github :repo
                        "countvajhula/repeat-ring" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol https :inherit
                        t :depth treeless :ref
                        "824bda59af6bb9f15b6616bf2f45b667ae0fddf6"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
              treeless :ref "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (switchy-window :source "elpaca-menu-lock-file" :recipe
                 (:package "switchy-window" :repo
                           ("https://git.sr.ht/~tsdh/switchy-window"
                            . "switchy-window")
                           :files ("*" (:exclude ".git")) :source "GNU ELPA"
                           :protocol https :inherit t :depth treeless :ref
                           "3ac4e522231a052e9beaa2d25478090d1215c7ae"))
 (symex :source "elpaca-menu-lock-file" :recipe
        (:source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                 treeless :host github :repo "drym-org/symex.el" :files
                 ("symex/symex*.el" "symex/doc/*.texi" "symex/doc/figures")
                 :package "symex" :ref
                 "64c863d84355f1d4618c5f77f14996d37f7bf2d4"))
 (symex-core :source "elpaca-menu-lock-file" :recipe
             (:source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                      treeless :host github :repo "drym-org/symex.el" :files
                      ("symex-core/symex*.el") :package "symex-core" :ref
                      "64c863d84355f1d4618c5f77f14996d37f7bf2d4"))
 (symex-ide :source "elpaca-menu-lock-file" :recipe
            (:source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                     treeless :host github :repo "drym-org/symex.el" :files
                     ("symex-ide/symex*.el") :package "symex-ide" :ref
                     "64c863d84355f1d4618c5f77f14996d37f7bf2d4"))
 (terraform-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "terraform-mode" :repo "hcl-emacs/terraform-mode"
                           :fetcher github :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                            "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                            "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el" "tests.el"
                                      "*-test.el" "*-tests.el" "LICENSE"
                                      "README*" "*-pkg.el"))
                           :source "elpaca-menu-lock-file" :protocol https
                           :inherit t :depth treeless :ref
                           "01635df3625c0cec2bb4613a6f920b8569d41009"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t
                      :depth treeless :ref
                      "bda7c2e0772deaee8e36a217d15c14784e8c6800"))
 (treesit-auto :source "elpaca-menu-lock-file" :recipe
               (:package "treesit-auto" :fetcher github :repo
                         "renzmann/treesit-auto" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol https
                         :inherit t :depth treeless :ref
                         "31466e4ccfd4f896ce3145c95c4c1f8b59d4bfdf"))
 (twilight-anti-bright-theme :source "elpaca-menu-lock-file" :recipe
                             (:package "twilight-anti-bright-theme" :repo
                                       "Bogdanp/twilight-anti-bright-theme"
                                       :fetcher github :files
                                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                        "*.texinfo" "doc/dir" "doc/*.info"
                                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                        "docs/dir" "docs/*.info" "docs/*.texi"
                                        "docs/*.texinfo"
                                        (:exclude ".dir-locals.el" "test.el"
                                                  "tests.el" "*-test.el"
                                                  "*-tests.el" "LICENSE"
                                                  "README*" "*-pkg.el"))
                                       :source "elpaca-menu-lock-file" :protocol
                                       https :inherit t :depth treeless :host
                                       github :ref
                                       "147a73d6b72dd4a700004a0c916917b51cc0cae0"))
 (twilight-bright-theme :source "elpaca-menu-lock-file" :recipe
                        (:package "twilight-bright-theme" :repo
                                  "Bogdanp/twilight-bright-theme.el" :fetcher
                                  github :files
                                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                   "*.texinfo" "doc/dir" "doc/*.info"
                                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                   "docs/dir" "docs/*.info" "docs/*.texi"
                                   "docs/*.texinfo"
                                   (:exclude ".dir-locals.el" "test.el"
                                             "tests.el" "*-test.el" "*-tests.el"
                                             "LICENSE" "README*" "*-pkg.el"))
                                  :source "elpaca-menu-lock-file" :protocol
                                  https :inherit t :depth treeless :host github
                                  :ref
                                  "ffca0f5ae10ccfc3a2efcd50c7fe2f084eb34be6"))
 (typst-preview :source "elpaca-menu-lock-file" :recipe
                (:package "typst-preview" :fetcher github :repo
                          "havarddj/typst-preview.el" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol https
                          :inherit t :depth treeless :ref
                          "7e89cf105e4fef5e79977a4a790d5b3b18d305f6"))
 (typst-ts-mode :source "elpaca-menu-lock-file" :recipe
                (:package "typst-ts-mode" :repo "meow_king/typst-ts-mode" :files
                          ("*" (:exclude ".git")) :source
                          "elpaca-menu-lock-file" :protocol https :inherit t
                          :depth treeless :host codeberg :ref
                          "1e8cc8072a22ac1dc07f1db356ca8f71c524d0c0"))
 (undo-fu :source "elpaca-menu-lock-file" :recipe
          (:package "undo-fu" :fetcher codeberg :repo "ideasman42/emacs-undo-fu"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :ref
                    "b4ce5ed20c1cf0591e497e6998a7634a172726fa"))
 (undo-fu-session :source "elpaca-menu-lock-file" :recipe
                  (:package "undo-fu-session" :fetcher codeberg :repo
                            "ideasman42/emacs-undo-fu-session" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                             "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el"
                                       "*-test.el" "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :protocol https
                            :inherit t :depth treeless :ref
                            "92d733a5b162a70c572fac17b9f9e872426df547"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/vertico-*.el") :fetcher github
                    :source "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :ref
                    "93f15873d7d6244d72202c5dd7724a030a2d5b9a"))
 (virtual-ring :source "elpaca-menu-lock-file" :recipe
               (:package "virtual-ring" :fetcher github :repo
                         "countvajhula/virtual-ring" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol https
                         :inherit t :depth treeless :ref
                         "2d51ae8104f847c508abc3fe32663411449548f6"))
 (websocket :source "elpaca-menu-lock-file" :recipe
            (:package "websocket" :repo "ahyatt/emacs-websocket" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t
                      :depth treeless :ref
                      "03d1cca4bd910a8df73e4ec637836c6ac25213a2"))
 (with-editor :source "elpaca-menu-lock-file" :recipe
              (:package "with-editor" :fetcher github :repo "magit/with-editor"
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol https :inherit
                        t :depth treeless :ref
                        "902b4d572af2c2f36060da01e3c33d194cdec32b"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github
                      :files ("yasnippet.el" "snippets") :source
                      "elpaca-menu-lock-file" :protocol https :inherit t :depth
                      treeless :ref "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (yasnippet-snippets :source "elpaca-menu-lock-file" :recipe
                     (:package "yasnippet-snippets" :repo
                               "AndreaCrotti/yasnippet-snippets" :fetcher github
                               :files ("*.el" "snippets" ".nosearch") :source
                               "elpaca-menu-lock-file" :protocol https :inherit
                               t :depth treeless :ref
                               "606ee926df6839243098de6d71332a697518cb86")))
