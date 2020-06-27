;;; init.el -- My custom emacs config
;;; Commentary:
;;; Simple Emacs config that uses org-babel to execute org files which contain a literate config
;;; Code:

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)
(unless (fboundp 'use-package)
  (package-install 'use-package))

(setq user-emacs-directory (expand-file-name "~/.config/emacs/")
      ring-bell-function 'ignore
      inhibit-startup-screen t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode -1)
(load-theme 'tango-dark)
(add-hook 'prog-mode-hook #'hs-minor-mode)

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

  (general-def
    "M-s" nil
    "M-s" #'occur)

  (leader
   "!" #'async-shell-command
   "f" '(:ignore t :wk "File")
   "s" '(:ignore t :wk "Search")
   "b" '(:ignore t :wk "Buffer"))

  (general-def
    :states 'visual
    "gr" #'eval-region)

  (leader
   :infix "b"
   "n" #'next-buffer
   "p" #'previous-buffer
   "b" #'counsel-switch-buffer
   "i" #'ibuffer)

  (leader
   :infix "o"
   "-" #'dired)

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
  (general-def
    :keymaps 'ivy-minibuffer-map
    "C-j" #'ivy-next-line-or-history
    "C-k" #'ivy-previous-line-or-history)
  :init
  (counsel-mode +1))

(use-package org :ensure t)

(use-package magit :ensure t)

(use-package evil-magit :ensure t)

(use-package telephone-line
  :ensure t
  :hook (after-init . telephone-line-mode)
  :init
                                        ; Faces
  (defface my-accent-dark    '((t (:foreground "Black" :background "Cadet Blue"))) "")
  (defface my-evil-dark      '((t (:foreground "White" :background "Black"))) "")
  (defface my-accent-light   '((t (:foreground "black" :background "Light Slate Grey"))) "")
  (defface my-evil-light     '((t (:foreground "black" :background "Sky Blue"))) "")

  ;; Set telephone line faces
  (setq telephone-line-faces
        '((evil . (my-evil-dark . my-evil-dark))
          (modal . telephone-line-modal-face)
          (ryo . telephone-line-ryo-modal-face)
          (accent . (my-accent-dark . telephone-line-accent-inactive))
          (nil mode-line . mode-line-inactive)))

                                        ; Seperators
  (setq telephone-line-primary-left-separator    'telephone-line-abs-left
        telephone-line-secondary-left-separator  'telephone-line-identity-hollow-left
        telephone-line-primary-right-separator   'telephone-line-abs-right
        telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)

                                        ; LSP segment
  (telephone-line-defsegment +oreoline-lsp-segment ()
    (if (bound-and-true-p lsp-mode)
        (propertize "")
      (propertize "")))

                                        ; LHS
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment
                     telephone-line-buffer-modified-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-filesize-segment
                     telephone-line-buffer-name-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . ())))

                                        ; RHS
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment
                     +oreoline-lsp-segment
                     telephone-line-flycheck-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (telephone-line-mode +1)
  :config
  (size-indication-mode +1))
