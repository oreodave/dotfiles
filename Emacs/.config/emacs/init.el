;;; init.el -- My custom emacs config
;;; Commentary:
;;; Simple Emacs config that uses org-babel to execute org files which contain a literate config
;;; Code:

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)
(unless (fboundp 'use-package)
  (package-install 'use-package))

(setq package-quickstart t)
(use-package org
  :ensure t
  :config
  (org-babel-load-file "~/.config/emacs/config.org" t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" default))
 '(package-selected-packages
   '(evil-org doom-themes general evil-surround evil evil-mode ace-window magit counsel which-key yasnippet use-package))
 '(safe-local-variable-values
   '((org-babel-default-header-args:elisp
      (:results . "none"))))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
