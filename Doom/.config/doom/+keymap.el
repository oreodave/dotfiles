;;; ~/Dotfiles/Doom/.config/doom/elisp/+keymap.el -*- lexical-binding: t; -*-

;;; Leader
(map!
 :leader
 ;; Single binds
 "SPC" #'execute-extended-command
 "!"   #'async-shell-command
 "T"   #'eshell
 "C"   #'calc
 "-"   #'dired-jump
 "_"   #'dired-jump-other-window
 "w"   #'ace-window

 ;; Personal
 (:prefix ("m" . "personal")
  :desc   "Open books"         "b"  #'(lambda () (interactive) (dired (concat org-directory "/Books"))); I like my books
  :desc   "Convert auto-fill"  "f"  #'dx:org/fill-to-long-lines
  :desc   "Change theme"       "t"  #'dx:themes/set-new-theme ; From my own collection
  :desc   "Generate template"  "g"  #'+gentemplate/generate-template) ; From my own collection

 ;; Projectile
 (:after projectile
  (:prefix "f"
   "g" #'projectile-regenerate-tags
   "p" #'(lambda () (interactive) (doom-project-find-file "~/Dotfiles")))
  ">"  #'projectile-switch-to-buffer)

 ;; Search
 (:prefix "s"
  (:after counsel-etags
   "t"  #'counsel-etags-find-tag)
  (:after counsel
   "s"  #'counsel-grep-or-swiper
   "r"  #'counsel-rg))

 ;; Code
 (:prefix "c"
  "m"  #'+make/run))

;;; Non leader
(map!
 (:after evil
  "TAB" #'evil-jump-item)
 "M-v" #'dx:newline
 "M-V" #'(lambda () (interactive) (dx:newline 1)))

;;; Remaps
(define-key!
  [remap compile]           #'compile)
