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

(after! projectile
  (setq projectile-completion-system 'default))

(add-hook 'doom-first-input-hook #'icomplete-mode)
