;;; private/completion/config.el -*- lexical-binding: t; -*-

(map!
 :map icomplete-minibuffer-map
 ;; unbind anything I want to use for useful stuff
 "C-j" nil
 "C-k" nil
 "C-b" nil
 "TAB" nil

 "C-j" #'icomplete-forward-completions
 "C-k" #'icomplete-backward-completions
 "C-n" #'icomplete-forward-completions
 "C-p" #'icomplete-backward-completions
 "TAB" #'icomplete-force-complete
 "C-b" #'completions)

(setq icomplete-separator "\t|\t")
(setq icomplete-in-buffer t)

(define-key!
  [remap compile]           #'compile
  [remap find-file]         #'find-file
  [remap describe-function] #'counsel-describe-function
  [remap describe-variable] #'counsel-describe-variable)

(add-hook 'doom-first-input-hook #'icomplete-mode)
