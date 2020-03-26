;;; private/ocaml/config.el -*- lexical-binding: t; -*-

(use-package! utop
  :config
  (map!
   :localleader
   :map tuareg-mode-map
   :desc "Repl" "c" #'utop
   (:prefix ("e" . "eval")
     :desc "Buffer" "b" #'utop-eval-buffer
     :desc "Region" "r" #'utop-eval-region)))


(when (featurep! +lsp)
  (after! lsp
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "ocamllsp")
                      :major-modes '(tuareg-mode)
                      :server-id 'ocaml-lsp))
    (add-hook 'tuareg-mode-hook #'(lambda () (lsp-mode)))))
