(setq user-emacs-directory (expand-file-name "~/.config/emacs/"))
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq ring-bell-function 'ignore)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode -1)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(load-theme 'tango-dark)

(use-package evil
  :config
  (evil-mode +1))

(use-package evil-surround
  :after (evil)
  :config
  (evil-surround-mode +1))

(use-package which-key
  :config
  (which-key-mode +1))

(use-package general
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
    "gr" 'eval-region)

  (leader
    :infix "b"
    "n" #'next-buffer
    "p" #'previous-buffer
    "b" #'counsel-switch-buffer
    "i" #'ibuffer)


  (leader
    :infix "f"
    "s" #'save-buffer
    "f" #'find-file
    "p" #'((lambda () (interactive) (find-file "~/.config/emacs/init.el"))
	   :wk "Open init.el")))

(use-package counsel
  :general
  (leader
    "SPC" #'counsel-M-x)
  (leader
    :infix "s"
    "s" #'(swiper :wk "Search buffer")
    "r" #'(counsel-rg :wk "Ripgrep"))
  (general-def
    :keymaps 'ivy-mode-map
    "C-j" #'ivy-next-line-or-history
    "C-k" #'ivy-previous-line-or-history)
  :init
  (counsel-mode +1))

(use-package org)

(use-package magit)

(use-package evil-magit)

(use-package telephone-line
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
