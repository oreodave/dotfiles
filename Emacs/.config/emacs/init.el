(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

(package-initialize)
(package-install 'use-package)

(setq ring-bell-function 'ignore)
(load-theme 'tango-dark)

(use-package evil
  :ensure t
  :config
  (evil-mode +1))

(use-package evil-surround
  :ensure t
  :after (evil)
  :config
  (evil-surround-mode +1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package general
  :ensure t
  :config
  (general-create-definer leader
    :prefix "SPC"
    :states 'normal
    :keymaps 'override)
  (general-create-definer localleader
    :prefix ","
    :states 'normal
    :keymaps 'override)

  (leader
    "!" #'async-shell-command
    "f" '(:ignore t :wk "File")
    "s" '(:ignore t :wk "Search")
    "b" '(:ignore t :wk "Buffer"))

  (general-def
    :states 'visual
    "gr" 'eval-region)

  (leader
    :infix "b"
    "n" #'next-buffer
    "p" #'prev-buffer
    "b" #')

  (leader
    :infix "f"
    "s" #'save-buffer
    "f" #'find-file
    "p" #'((lambda () (interactive) (find-file "~/.config/emacs/init.el"))
	   :wk "Open init.el")))

(use-package counsel
  :ensure t
  :general
  (leader
    "SPC" #'counsel-M-x)
  (leader
    :infix "s"
    "s" #'(swiper :wk "Search buffer")
    "r" #'(counsel-rg :wk "Ripgrep"))
  :config
  (counsel-mode +1))

(use-package org
  :ensure t)

(use-package evil-magit
  :ensure t
  :defer t)

(use-package magit
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-surround which-key counsel swiper general use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
