((ace-link :source "elpaca-menu-lock-file" :recipe
   (:package "ace-link" :repo "abo-abo/ace-link" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id ace-link :type git :protocol https
    :inherit t :depth treeless :ref "d9bd4a25a02bdfde4ea56247daf3a9ff15632ea4"))
 (ace-window :source "elpaca-menu-lock-file" :recipe
   (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id ace-window :type git :protocol https
    :inherit t :depth treeless :ref "77115afc1b0b9f633084cf7479c767988106c196"))
 (aggressive-indent :source "elpaca-menu-lock-file" :recipe
   (:package "aggressive-indent" :repo "Malabarba/aggressive-indent-mode"
    :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id aggressive-indent :type git :protocol
    https :inherit t :depth treeless :ref
    "a437a45868f94b77362c6b913c5ee8e67b273c42"))
 (all-the-icons :source "elpaca-menu-lock-file" :recipe
   (:package "all-the-icons" :repo "domtronn/all-the-icons.el" :fetcher github
    :files (:defaults "data") :source "elpaca-menu-lock-file" :id all-the-icons
    :type git :protocol https :inherit t :depth treeless :ref
    "4778632b29c8c8d2b7cd9ce69535d0be01d846f9"))
 (amx :source "elpaca-menu-lock-file" :recipe
   (:package "amx" :repo "DarwinAwardWinner/amx" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id amx :type git :protocol https :inherit t
    :depth treeless :ref "5b3aa1aae84f4a225cb8d26ab79a32f97693f023"))
 (avy :source "elpaca-menu-lock-file" :recipe
   (:package "avy" :repo "abo-abo/avy" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id avy :type git :protocol https :inherit t
    :depth treeless :ref "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (benchmark-init :source "elpaca-menu-lock-file" :recipe
   (:package "benchmark-init" :fetcher github :repo "dholm/benchmark-init-el"
    :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id benchmark-init :type git :protocol https
    :inherit t :depth treeless :ref "54b9703389f25012e4cc20fe4a0d4ea253ce4820"))
 (closql :source "elpaca-menu-lock-file" :recipe
   (:package "closql" :fetcher github :repo "magit/closql" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id closql :type git :protocol https
    :inherit t :depth treeless :ref "d382e7427f5d375ffc872851b049e9f9c4a43dfc"))
 (company :source "elpaca-menu-lock-file" :recipe
   (:package "company" :fetcher github :repo "company-mode/company-mode" :files
    (:defaults "icons" ("images/small" "doc/images/small/*.png")) :source
    "elpaca-menu-lock-file" :id company :type git :protocol https :inherit t
    :depth treeless :ref "a8c75c5d3fd7eb50b57a5b6aecc9aca58a3e9fcf"))
 (compat :source "elpaca-menu-lock-file" :recipe
   (:package "compat" :repo
    ("https://github.com/emacs-compat/compat" . "compat") :tar "31.0.0.1" :host
    gnu :files ("*" (:exclude ".git")) :source "elpaca-menu-lock-file" :id
    compat :type git :protocol https :inherit t :depth treeless :ref
    "dd66b81feed6fc3f250d3b979fb56d9117014f8c"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id cond-let :type git :protocol https
    :inherit t :depth treeless :ref "c48600dfab6372670225f046cace263700c78eab"))
 (consult :source "elpaca-menu-lock-file" :recipe
   (:package "consult" :repo "minad/consult" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id consult :type git :protocol https
    :inherit t :depth treeless :ref "5849e82baeaff378b5e2d88c5d81cf7d314d43aa"))
 (dash :source "elpaca-menu-lock-file" :recipe
   (:package "dash" :fetcher github :repo "magnars/dash.el" :files
    ("dash.el" "dash.texi") :source "elpaca-menu-lock-file" :id dash :type git
    :protocol https :inherit t :depth treeless :ref
    "d3a84021dbe48dba63b52ef7665651e0cf02e915"))
 (devdocs :source "elpaca-menu-lock-file" :recipe
   (:package "devdocs" :fetcher github :repo "astoff/devdocs.el" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id devdocs :type git :protocol https
    :inherit t :depth treeless :ref "25c746024ddf73570195bf42b841f761a2fee10c"))
 (dired-rsync :source "elpaca-menu-lock-file" :recipe
   (:package "dired-rsync" :repo "stsquad/dired-rsync" :fetcher github :files
    ("dired-rsync.el") :source "elpaca-menu-lock-file" :id dired-rsync :type git
    :protocol https :inherit t :depth treeless :ref
    "24ceb60b168c591d7e2d9440a7f1895880681f48"))
 (drag-stuff :source "elpaca-menu-lock-file" :recipe
   (:package "drag-stuff" :repo "rejeep/drag-stuff.el" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id drag-stuff :type git :protocol https
    :inherit t :depth treeless :ref "6d06d846cd37c052d79acd0f372c13006aa7e7c8"))
 (edit-indirect :source "elpaca-menu-lock-file" :recipe
   (:package "edit-indirect" :fetcher github :repo "Fanael/edit-indirect" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id edit-indirect :type git :protocol https
    :inherit t :depth treeless :ref "82a28d8a85277cfe453af464603ea330eae41c05"))
 (eldoc-box :source "elpaca-menu-lock-file" :recipe
   (:package "eldoc-box" :repo "casouri/eldoc-box" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id eldoc-box :type git :protocol https
    :inherit t :depth treeless :ref "e458cefba4013785ef6099c6245463689a50dd7d"))
 (elfeed :source "elpaca-menu-lock-file" :recipe
   (:package "elfeed" :fetcher github :repo "emacs-elfeed/elfeed" :files
    (:defaults "README.md") :source "elpaca-menu-lock-file" :id elfeed :type git
    :protocol https :inherit t :depth treeless :ref
    "149bb4ef375c3461f7ff37620759c33050bfb132"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
   (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
    (:defaults (:exclude "elisp-refs-bench.el")) :source "elpaca-menu-lock-file"
    :id elisp-refs :type git :protocol https :inherit t :depth treeless :ref
    "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo
    "https://github.com/progfolio/elpaca.git" :ref
    "74848674bfca8590e9286309d11e9645c8425400" :depth 1 :inherit ignore :files
    (:defaults "elpaca-test.el" (:exclude "extensions")) :build
    (:not elpaca-activate) :type git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
   (:package "elpaca-use-package" :wait t :repo
    "https://github.com/progfolio/elpaca.git" :files
    ("extensions/elpaca-use-package.el") :main
    "extensions/elpaca-use-package.el" :build
    (:not elpaca-source elpaca-build-docs) :source "elpaca-menu-lock-file" :id
    elpaca-use-package :type git :protocol https :inherit t :depth treeless :ref
    "74848674bfca8590e9286309d11e9645c8425400"))
 (emacsql :source "elpaca-menu-lock-file" :recipe
   (:package "emacsql" :fetcher github :repo "magit/emacsql" :files
    (:defaults "README.md" "sqlite") :source "elpaca-menu-lock-file" :id emacsql
    :type git :protocol https :inherit t :depth treeless :ref
    "d811bbefcb5e27841af55cae53aa939ba720de77"))
 (embark :source "elpaca-menu-lock-file" :recipe
   (:package "embark" :repo "oantolin/embark" :fetcher github :files
    ("embark.el" "embark-org.el" "embark.texi") :source "elpaca-menu-lock-file"
    :id embark :type git :protocol https :inherit t :depth treeless :ref
    "350ca86924c5027e80875943fba7b912a71e5791"))
 (emmet-mode :source "elpaca-menu-lock-file" :recipe
   (:package "emmet-mode" :fetcher github :repo "smihica/emmet-mode" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id emmet-mode :type git :protocol https
    :inherit t :depth treeless :ref "322d3bb112fced57d63b44863357f7a0b7eee1e3"))
 (empv :source "elpaca-menu-lock-file" :recipe
   (:package "empv" :fetcher github :repo "isamert/empv.el" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id empv :type git :protocol https :inherit
    t :depth treeless :ref "7f8af0b41a83c36acf7fe826839c02ecbffa33fc"))
 (eshell-syntax-highlighting :source "elpaca-menu-lock-file" :recipe
   (:package "eshell-syntax-highlighting" :fetcher github :repo
    "akreisher/eshell-syntax-highlighting" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id eshell-syntax-highlighting :type git
    :protocol https :inherit t :depth treeless :ref
    "62418fd8b2380114a3f6dad699c1ba45329db1d2"))
 (evil :source "elpaca-menu-lock-file" :recipe
   (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
    (:defaults "doc/build/texinfo/evil.texi" (:exclude "evil-test-helpers.el"))
    :source "elpaca-menu-lock-file" :id evil :type git :protocol https :inherit
    t :depth treeless :ref "3b678a221ee99cc6a95b01d7a3129ce5efc4c3da"))
 (evil-collection :source "elpaca-menu-lock-file" :recipe
   (:package "evil-collection" :fetcher github :repo
    "emacs-evil/evil-collection" :files (:defaults "modes") :source
    "elpaca-menu-lock-file" :id evil-collection :type git :protocol https
    :inherit t :depth treeless :ref "162183159dde328336bf2c92cef66b4151df26cf"))
 (evil-commentary :source "elpaca-menu-lock-file" :recipe
   (:package "evil-commentary" :repo "linktohack/evil-commentary" :fetcher
    github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id evil-commentary :type git :protocol
    https :inherit t :depth treeless :ref
    "c5945f28ce47644c828aac1f5f6ec335478d17fb"))
 (evil-ghostel :source "elpaca-menu-lock-file" :recipe
   (:package "evil-ghostel" :fetcher github :repo "dakra/ghostel" :files
    ("extensions/evil-ghostel/evil-ghostel.el") :source "elpaca-menu-lock-file"
    :id evil-ghostel :type git :protocol https :inherit t :depth treeless :ref
    "eb806d158df4ff302aee68e91caf257f11d66320"))
 (evil-goggles :source "elpaca-menu-lock-file" :recipe
   (:package "evil-goggles" :repo "edkolev/evil-goggles" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id evil-goggles :type git :protocol https
    :inherit t :depth treeless :ref "34ca276a85f615d2b45e714c9f8b5875bcb676f3"))
 (evil-multiedit :source "elpaca-menu-lock-file" :recipe
   (:package "evil-multiedit" :repo "hlissner/evil-multiedit" :fetcher github
    :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id evil-multiedit :type git :protocol https
    :inherit t :depth treeless :ref "23b53bc8743fb82a8854ba907b1d277374c93a79"))
 (evil-numbers :source "elpaca-menu-lock-file" :recipe
   (:package "evil-numbers" :repo "juliapath/evil-numbers" :fetcher github
    :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id evil-numbers :type git :protocol https
    :inherit t :depth treeless :ref "616aff9e5cee012954756ed2715209fa90308cdf"))
 (evil-org :source "elpaca-menu-lock-file" :recipe
   (:package "evil-org" :fetcher github :repo "Somelauw/evil-org-mode" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id evil-org :type git :protocol https
    :inherit t :depth treeless :ref "b1f309726b1326e1a103742524ec331789f2bf94"))
 (evil-surround :source "elpaca-menu-lock-file" :recipe
   (:package "evil-surround" :repo "emacs-evil/evil-surround" :fetcher github
    :old-names (surround) :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id evil-surround :type git :protocol https
    :inherit t :depth treeless :ref "e6548372e8359ee55e67d73ca418314086011f1a"))
 (f :source "elpaca-menu-lock-file" :recipe
   (:package "f" :fetcher github :repo "rejeep/f.el" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id f :type git :protocol https :inherit t
    :depth treeless :ref "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
   (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id flycheck :type git :protocol https
    :inherit t :depth treeless :ref "96f1852c7e352c969393e6e66176178177e933be"))
 (forge :source "elpaca-menu-lock-file" :recipe
   (:package "forge" :fetcher github :repo "magit/forge" :files
    ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source "elpaca-menu-lock-file"
    :id forge :type git :protocol https :inherit t :depth treeless :ref
    "9628f76740aec9270e9fb31457ff4cb38d9f3f16"))
 (geiser :source "elpaca-menu-lock-file" :recipe
   (:package "geiser" :fetcher codeberg :repo "geiser/geiser" :files
    ("elisp/*.el" "doc/dir" "doc/geiser.texi") :source "elpaca-menu-lock-file"
    :id geiser :type git :protocol https :inherit t :depth treeless :ref
    "84c25e9683a18d00387b6c16b0cee66269536c3c"))
 (geiser-guile :source "elpaca-menu-lock-file" :recipe
   (:package "geiser-guile" :fetcher codeberg :repo "geiser/guile" :files
    (:defaults ("src" "src/*")) :source "elpaca-menu-lock-file" :id geiser-guile
    :type git :protocol https :inherit t :depth treeless :ref
    "cbab81bd2dcb4c787bcda4ae18062db3087e6887"))
 (general :source "elpaca-menu-lock-file" :recipe
   (:package "general" :fetcher github :repo "noctuid/general.el" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id general :wait t :type git :protocol
    https :inherit t :depth treeless :ref
    "a48768f85a655fe77b5f45c2880b420da1b1b9c3"))
 (ghostel :source "elpaca-menu-lock-file" :recipe
   (:package "ghostel" :fetcher github :repo "dakra/ghostel" :files
    (:defaults "etc" "src" "vendor" "build.zig" "build.zig.zon" "symbols.map")
    :source "elpaca-menu-lock-file" :id ghostel :type git :protocol https
    :inherit t :depth treeless :ref "eb806d158df4ff302aee68e91caf257f11d66320"))
 (ghub :source "elpaca-menu-lock-file" :recipe
   (:package "ghub" :fetcher github :repo "magit/ghub" :files
    ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source "elpaca-menu-lock-file"
    :id ghub :type git :protocol https :inherit t :depth treeless :ref
    "59d0b9b33e780d6cff5131886904ff26033dd2e6"))
 (gif-screencast :source "elpaca-menu-lock-file" :recipe
   (:package "gif-screencast" :repo "Ambrevar/emacs-gif-screencast" :fetcher
    gitlab :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id gif-screencast :type git :protocol https
    :inherit t :depth treeless :ref "6798656d3d3107d16e30cc26bc3928b00e50c1ca"))
 (goto-chg :source "elpaca-menu-lock-file" :recipe
   (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id goto-chg :type git :protocol https
    :inherit t :depth treeless :ref "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (gptel :source "elpaca-menu-lock-file" :recipe
   (:package "gptel" :repo "oreodave/gptel" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "MELPA" :id gptel :host github :type git :protocol https :inherit t
    :depth treeless :ref "b82afcc733f9bf2f73d447532f234701c982e423"))
 (haskell-mode :source "elpaca-menu-lock-file" :recipe
   (:package "haskell-mode" :repo "haskell/haskell-mode" :fetcher github :files
    (:defaults "NEWS" "logo.svg") :source "elpaca-menu-lock-file" :id
    haskell-mode :type git :protocol https :inherit t :depth treeless :ref
    "781e4669a0e0917fa8c532371cbfb1eb5b03b645"))
 (helpful :source "elpaca-menu-lock-file" :recipe
   (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id helpful :type git :protocol https
    :inherit t :depth treeless :ref "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
   (:package "hl-todo" :fetcher github :repo "tarsius/hl-todo" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id hl-todo :type git :protocol https
    :inherit t :depth treeless :ref "527d545b8c2f36243194cbe4a8d0e6ac9d50e6a7"))
 (ht :source "elpaca-menu-lock-file" :recipe
   (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id ht :type git :protocol https :inherit t
    :depth treeless :ref "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (htmlize :source "elpaca-menu-lock-file" :recipe
   (:package "htmlize" :fetcher github :repo "emacsorphanage/htmlize" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id htmlize :type git :protocol https
    :inherit t :depth treeless :ref "fa644880699adea3770504f913e6dddbec90c076"))
 (hydra :source "elpaca-menu-lock-file" :recipe
   (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
    (:defaults (:exclude "lv.el")) :source "elpaca-menu-lock-file" :id hydra
    :wait t :type git :protocol https :inherit t :depth treeless :ref
    "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (iedit :source "elpaca-menu-lock-file" :recipe
   (:package "iedit" :repo "victorhge/iedit" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id iedit :type git :protocol https :inherit
    t :depth treeless :ref "14161daa295332a49dda92b97c00d62efd38acfe"))
 (jagger :source "elpaca-menu-lock-file" :recipe
   (:source "elpaca-menu-lock-file" :package "jagger" :id jagger :host github
    :repo "twlz0ne/jagger" :type git :protocol https :inherit t :depth treeless
    :ref "268ce96cb4dd7c15bbd4c2b70ee145bec2b51d47"))
 (jeison :source "elpaca-menu-lock-file" :recipe
   (:package "jeison" :repo "SavchenkoValeriy/jeison" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id jeison :type git :protocol https
    :inherit t :depth treeless :ref "19a51770f24eaa7b538c7be6a8a5c25d154b641f"))
 (keycast :source "elpaca-menu-lock-file" :recipe
   (:package "keycast" :fetcher github :repo "tarsius/keycast" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id keycast :type git :protocol https
    :inherit t :depth treeless :ref "a6518e1b48b08ba883e9b1a2db0872d5bf3d85f4"))
 (llama :source "elpaca-menu-lock-file" :recipe
   (:package "llama" :fetcher github :repo "tarsius/llama" :files
    ("llama.el" ".dir-locals.el") :source "elpaca-menu-lock-file" :id llama
    :type git :protocol https :inherit t :depth treeless :ref
    "4d4024048053b898a01521046e0f063ee47615b0"))
 (llm-tool-collection :source "elpaca-menu-lock-file" :recipe
   (:source "elpaca-menu-lock-file" :package "llm-tool-collection" :id
    llm-tool-collection :host github :repo "skissue/llm-tool-collection" :type
    git :protocol https :inherit t :depth treeless :ref
    "b9fd45bedf3e0fb07d289730991199ae18785157"))
 (lorem-ipsum :source "elpaca-menu-lock-file" :recipe
   (:package "lorem-ipsum" :fetcher github :repo "jschaf/emacs-lorem-ipsum"
    :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id lorem-ipsum :type git :protocol https
    :inherit t :depth treeless :ref "4e87a899868e908a7a9e1812831d76c8d072f885"))
 (lv :source "elpaca-menu-lock-file" :recipe
   (:package "lv" :repo "abo-abo/hydra" :fetcher github :files ("lv.el") :source
    "elpaca-menu-lock-file" :id lv :type git :protocol https :inherit t :depth
    treeless :ref "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (magit :source "elpaca-menu-lock-file" :recipe
   (:package "magit" :fetcher github :repo "magit/magit" :files
    ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md"
     "LICENSE" ".dir-locals.el" ("githooks" "githooks/*")
     ("git-hooks" "git-hooks/*") (:exclude "lisp/magit-section.el"))
    :source "elpaca-menu-lock-file" :id magit :type git :protocol https :inherit
    t :depth treeless :ref "b6c512597fd66abe69883a058a2d13bcea76bf33"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
   (:package "magit-section" :fetcher github :repo "magit/magit" :files
    ("lisp/magit-section.el" "docs/magit-section.texi" "magit-section-pkg.el")
    :source "elpaca-menu-lock-file" :id magit-section :type git :protocol https
    :inherit t :depth treeless :ref "b6c512597fd66abe69883a058a2d13bcea76bf33"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
   (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode"
    :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id markdown-mode :type git :protocol https
    :inherit t :depth treeless :ref "1f72cefa6a4b759f90e335e4908725a721b17ad9"))
 (minuet :source "elpaca-menu-lock-file" :recipe
   (:package "minuet" :fetcher github :repo "milanglacier/minuet-ai.el" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id minuet :host github :type git :protocol
    https :inherit t :depth treeless :ref
    "13fb314a795951b9190c53c59ef281abf7a2cb4f"))
 (nasm-mode :source "elpaca-menu-lock-file" :recipe
   (:package "nasm-mode" :repo "skeeto/nasm-mode" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id nasm-mode :type git :protocol https
    :inherit t :depth treeless :ref "fab76d8e092419c341b6240fcc7123975db177e1"))
 (nhexl-mode :source "elpaca-menu-lock-file" :recipe
   (:package "nhexl-mode" :repo
    ("https://github.com/emacsmirror/gnu_elpa" . "nhexl-mode") :tar "1.5" :host
    gnu :branch "externals/nhexl-mode" :files ("*" (:exclude ".git")) :source
    "elpaca-menu-lock-file" :id nhexl-mode :type git :protocol https :inherit t
    :depth treeless :ref "70d3c545857f59e892fba9dbefdca4fa25b9af9a"))
 (no-littering :source "elpaca-menu-lock-file" :recipe
   (:package "no-littering" :fetcher github :repo "emacscollective/no-littering"
    :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id no-littering :wait t :type git :protocol
    https :inherit t :depth treeless :ref
    "719c2a3773419ebc92a06e61b0fb26f6d64e750e"))
 (notmuch :source "elpaca-menu-lock-file" :recipe
   (:package "notmuch" :url "https://git.notmuchmail.org/git/notmuch" :fetcher
    git :files ("emacs/*.el" "emacs/*.svg") :source "elpaca-menu-lock-file" :id
    notmuch :type git :protocol https :inherit t :depth treeless :ref
    "039533fd5bb6cb6af7f38ea235b8408099bbecf3"))
 (olivetti :source "elpaca-menu-lock-file" :recipe
   (:package "olivetti" :fetcher github :repo "rnkn/olivetti" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id olivetti :type git :protocol https
    :inherit t :depth treeless :ref "d2ccae56b442d9c5b06dd2481057abbd7eb82551"))
 (orderless :source "elpaca-menu-lock-file" :recipe
   (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id orderless :type git :protocol https
    :inherit t :depth treeless :ref "0ffd9d6903714c1f6d8fcbb6a20941fb33dd7ae5"))
 (org :source "elpaca-menu-lock-file" :recipe
   (:package "org" :host github :repo "emacsmirror/org" :autoloads
    "org-loaddefs.el" :depth treeless :build
    ((:not elpaca-build-autoloads)
     (:before elpaca-build-link elpaca-menu-org--build))
    :files (:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi")) :source
    "elpaca-menu-lock-file" :id org :wait t :type git :protocol https :inherit t
    :ref "beb10b13b95bfc6b86c599565012a87a0a09379a"))
 (org-msg :source "elpaca-menu-lock-file" :recipe
   (:package "org-msg" :repo "jeremy-compostella/org-msg" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id org-msg :type git :protocol https
    :inherit t :depth treeless :ref "7b45df759340f3e388e84f497052b7cf3a41698c"))
 (org-super-agenda :source "elpaca-menu-lock-file" :recipe
   (:package "org-super-agenda" :fetcher github :repo
    "alphapapa/org-super-agenda" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id org-super-agenda :type git :protocol
    https :inherit t :depth treeless :ref
    "fb20ad9c8a9705aa05d40751682beae2d094e0fe"))
 (plz :source "elpaca-menu-lock-file"
   :recipe
   (:package "plz" :repo ("https://github.com/alphapapa/plz.el.git" . "plz")
    :tar "0.9.1" :host gnu :files ("*" (:exclude ".git" "LICENSE")) :source
    "elpaca-menu-lock-file" :id plz :type git :protocol https :inherit t :depth
    treeless :ref "e2d07838e3b64ee5ebe59d4c3c9011adefb7b58e"))
 (posframe :source "elpaca-menu-lock-file" :recipe
   (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id posframe :type git :protocol https
    :inherit t :depth treeless :ref "74c8c56131ed866db47ae4191364b72dd4852456"))
 (powerthesaurus :source "elpaca-menu-lock-file" :recipe
   (:package "powerthesaurus" :repo "SavchenkoValeriy/emacs-powerthesaurus"
    :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id powerthesaurus :type git :protocol https
    :inherit t :depth treeless :ref "4b97797cf789aaba411c61a85fe23474ebc5bedc"))
 (pulsar :source "elpaca-menu-lock-file" :recipe
   (:package "pulsar" :repo ("https://github.com/protesilaos/pulsar" . "pulsar")
    :tar "1.3.4" :host gnu :files
    ("*" (:exclude ".git" "COPYING" "doclicense.texi")) :source
    "elpaca-menu-lock-file" :id pulsar :type git :protocol https :inherit t
    :depth treeless :ref "2155112b174a08d6ebb5ed828507b40e90d9eadd"))
 (queue :source "elpaca-menu-lock-file" :recipe
   (:package "queue" :repo ("https://github.com/emacsmirror/gnu_elpa" . "queue")
    :tar "0.2" :host gnu :branch "externals/queue" :files
    ("*" (:exclude ".git")) :source "elpaca-menu-lock-file" :id queue :type git
    :protocol https :inherit t :depth treeless :ref
    "f986fb68e75bdae951efb9e11a3012ab6bd408ee"))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
   (:package "rainbow-delimiters" :fetcher github :repo
    "Fanael/rainbow-delimiters" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id rainbow-delimiters :type git :protocol
    https :inherit t :depth treeless :ref
    "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (rg :source "elpaca-menu-lock-file" :recipe
   (:package "rg" :fetcher github :repo "dajva/rg.el" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id rg :type git :protocol https :inherit t
    :depth treeless :ref "e46a16b8bdba111c9c0036d0e209490dd7a3690f"))
 (rmsbolt :source "elpaca-menu-lock-file" :recipe
   (:package "rmsbolt" :files (:defaults "starters") :fetcher gitlab :repo
    "jgkamat/rmsbolt" :source "elpaca-menu-lock-file" :id rmsbolt :type git
    :protocol https :inherit t :depth treeless :ref
    "05c4795226f859009bc570940139473b6b6f7555"))
 (rust-mode :source "elpaca-menu-lock-file" :recipe
   (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id rust-mode :type git :protocol https
    :inherit t :depth treeless :ref "93778358b6af8ad9e123a41bc2c8c91877a9ffd5"))
 (s :source "elpaca-menu-lock-file" :recipe
   (:package "s" :fetcher github :repo "magnars/s.el" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id s :type git :protocol https :inherit t
    :depth treeless :ref "7393fa6fa305403e628058c0ec78c35d610fab05"))
 (screenshot :source "elpaca-menu-lock-file" :recipe
   (:source "elpaca-menu-lock-file" :package "screenshot" :id screenshot :type
    git :host github :repo "tecosaur/screenshot" :protocol https :inherit t
    :depth treeless :ref "2770c0cfefe1cc09d55585f4f2f336a1b26e610e"))
 (separedit :source "elpaca-menu-lock-file" :recipe
   (:package "separedit" :fetcher github :repo "twlz0ne/separedit.el" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id separedit :type git :protocol https
    :inherit t :depth treeless :ref "5cb46a65fc6e12b753dce8f581fbfa144d011a80"))
 (shift-number :source "elpaca-menu-lock-file" :recipe
   (:package "shift-number" :fetcher codeberg :repo
    "ideasman42/emacs-shift-number" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id shift-number :type git :protocol https
    :inherit t :depth treeless :ref "52f4d32080cca50da0f88b2141d597827c7341cf"))
 (sly :source "elpaca-menu-lock-file" :recipe
   (:package "sly" :repo "joaotavora/sly" :fetcher github :files
    (:defaults "lib" "slynk" "contrib" "doc/images"
     (:exclude "sly-autoloads.el"))
    :version-regexp "%v" :source "elpaca-menu-lock-file" :id sly :type git
    :protocol https :inherit t :depth treeless :ref
    "759c0ff8741ced8793257f2b7ed95a23e13e1407"))
 (smartparens :source "elpaca-menu-lock-file" :recipe
   (:package "smartparens" :fetcher github :repo "Fuco1/smartparens" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id smartparens :type git :protocol https
    :inherit t :depth treeless :ref "82d2cf084a19b0c2c3812e0550721f8a61996056"))
 (transient :source "elpaca-menu-lock-file" :recipe
   (:package "transient" :fetcher github :repo "magit/transient" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id transient :type git :protocol https
    :inherit t :depth treeless :ref "3d20a780605f0a33d6360dc0a2ce9174c69a9a92"))
 (treepy :source "elpaca-menu-lock-file" :recipe
   (:package "treepy" :repo "volrath/treepy.el" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id treepy :type git :protocol https
    :inherit t :depth treeless :ref "806c000bd40153d17dfa5709c6d19546d507a416"))
 (ts :source "elpaca-menu-lock-file" :recipe
   (:package "ts" :fetcher github :repo "alphapapa/ts.el" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id ts :type git :protocol https :inherit t
    :depth treeless :ref "552936017cfdec89f7fc20c254ae6b37c3f22c5b"))
 (typescript-mode :source "elpaca-menu-lock-file" :recipe
   (:package "typescript-mode" :fetcher github :repo
    "emacs-typescript/typescript.el" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id typescript-mode :type git :protocol
    https :inherit t :depth treeless :ref
    "2535780bdb318d86761b9bd21b0347ca6a89628f"))
 (undo-tree :source "elpaca-menu-lock-file" :recipe
   (:package "undo-tree" :repo
    ("https://gitlab.com/tsc25/undo-tree" . "undo-tree") :tar "0.8.2" :host gnu
    :files ("*" (:exclude ".git")) :source "elpaca-menu-lock-file" :id undo-tree
    :type git :protocol https :inherit t :depth treeless :ref
    "2bf5e230f1d11df7bbd9d8c722749e34482bc458"))
 (use-package :source "elpaca-menu-lock-file"
   :recipe
   (:package "use-package" :repo
    ("https://github.com/emacs-mirror/emacs" . "use-package") :tar "2.4.6" :host
    gnu :branch "master" :files
    ("lisp/use-package/*" "doc/emacs/doclicense.texi" "doc/emacs/docstyle.texi"
     "doc/misc/use-package.texi" (:exclude ".git"))
    :source "elpaca-menu-lock-file" :id use-package :type git :protocol https
    :inherit t :depth treeless :ref "040b5a18fc327ab3939b668947bd89dd5086af15"))
 (use-package-hydra :source "elpaca-menu-lock-file" :recipe
   (:package "use-package-hydra" :repo "to1ne/use-package-hydra" :fetcher gitlab
    :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id use-package-hydra :wait t :type git
    :protocol https :inherit t :depth treeless :ref
    "8cd55a1128fbdf6327bb38a199d206225896d146"))
 (vertico :source "elpaca-menu-lock-file" :recipe
   (:package "vertico" :repo "minad/vertico" :files
    (:defaults "extensions/vertico-*.el") :fetcher github :source
    "elpaca-menu-lock-file" :id vertico :type git :protocol https :inherit t
    :depth treeless :ref "97a781560ff7cb77ed6e7cf09c24e0e1f2e2d95e"))
 (web-mode :source "elpaca-menu-lock-file" :recipe
   (:package "web-mode" :repo "fxbois/web-mode" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id web-mode :type git :protocol https
    :inherit t :depth treeless :ref "aeee2d4c82a791ff69657c1413873bf9265544df"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
   (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep" :files
    ("wgrep.el") :source "elpaca-menu-lock-file" :id wgrep :type git :protocol
    https :inherit t :depth treeless :ref
    "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (with-editor :source "elpaca-menu-lock-file" :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id with-editor :type git :protocol https
    :inherit t :depth treeless :ref "45bfc6084f03e3aa7f4f8db20836d559186c5957"))
 (yaml :source "elpaca-menu-lock-file" :recipe
   (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id yaml :type git :protocol https :inherit
    t :depth treeless :ref "5546f36bde24a9a8c1934e0f6ce205cd41d72537"))
 (yaml-mode :source "elpaca-menu-lock-file" :recipe
   (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher github :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
     "docs/*.texi" "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
      "LICENSE" "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :id yaml-mode :type git :protocol https
    :inherit t :depth treeless :ref "96ef0201101a7cd591febd5886633154dae8834c"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
   (:package "yasnippet" :fetcher github :repo "joaotavora/yasnippet" :files
    (:defaults ("doc" "doc/*.org")) :source "elpaca-menu-lock-file" :id
    yasnippet :type git :protocol https :inherit t :depth treeless :ref
    "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37")))
