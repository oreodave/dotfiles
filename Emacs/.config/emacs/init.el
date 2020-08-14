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
  "Load all files in +literate/output-files."
  (mapc #'(lambda (x) (load-file x)) +literate/output-files))

(defun +literate/org-files-exist ()
  "Checks if output files exist, for compilation purposes."
  (require 'cl-lib)
  (if (< 1 (length +literate/output-files))
      (cl-reduce #'(lambda (x y) (and x y)) (mapc #'file-exists-p +literate/output-files)
                 :initial-value t)
    (file-exists-p (car +literate/output-files))))

(defun +literate/compile-config ()
  "Compile all files in +literate/org-files via org-babel-tangle."
  (mapc #'org-babel-tangle-file +literate/org-files))

;; Killing Emacs hook
(add-hook
 'kill-emacs-hook
 #'+literate/compile-config)

;; When no output files exist, compile
(unless (+literate/org-files-exist)
  (+literate/compile-config))

(+literate/load-config)

(when (daemonp)
  (require 'org)
  (require 'notmuch)
  (require 'eglot))

(setq gc-cons-threshold 8000)

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
