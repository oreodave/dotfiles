;; Straight
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

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;;; Load literate
;; setup autoload for org-tangle
(autoload 'org-babel-tangle-file "ob-tangle")

;; Setup directories and constants
(setq user-emacs-directory "~/.config/emacs/")
(defconst +literate/org-files (list (concat user-emacs-directory "config.org")))
(defconst +literate/output-files
  (mapcar #'(lambda (x) (replace-regexp-in-string ".org" ".el" x)) +literate/org-files))

(defun +literate/load-config ()
  (mapc #'(lambda (x) (load-file (concat user-emacs-directory x))) +literate/output-files))

(defun +literate/compile-config ()
  (require 'ob-tangle)
  (mapc #'org-babel-tangle-file +literate/files))

(defun +literate/files-exist ()
  "Checks if output files exist, for compilation purposes. Don't use if only one file."
  (require 'cl-lib)
  (cl-reduce #'(lambda (x y) (and x y)) (mapc #'file-exists-p +literate/output-files)
             :initial-value t))

(add-hook 'kill-emacs-hook #'+literate/compile-config)

(unless (file-exists-p "config.el") ; only one file
  (+literate/compile-config))

(+literate/load-config)

(when (daemonp)
  (require 'org)
  (require 'notmuch)
  (require 'eglot)
  (require 'ivy))

;; Programming
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
