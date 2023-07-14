;;; literate.el --- My literate configuration setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Aryadev Chavali

;; Author: Aryadev Chavali <aryadev@aryadevchavali.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun +literate/filter (predicate list)
  (if (null list)
      nil
    (let ((first (car list))
	        (rest (cdr list)))
      (if (funcall predicate first)
	        (cons first (+literate/filter predicate rest))
	      (+literate/filter predicate rest)))))

(defconst +literate/org-files (list (concat user-emacs-directory "config.org")))

(defconst +literate/output-files
  (mapcar #'(lambda (x) (replace-regexp-in-string ".org" ".el" x)) +literate/org-files))

(defconst +literate/elisp-files
  `(,(concat user-emacs-directory "early-init.el")
    ,(concat user-emacs-directory "init.el")
    ,@(mapcar
       #'(lambda (name) (concat user-emacs-directory "elisp/" name))
       ;; Only take .el files
       (+literate/filter
	      (lambda (name) (string= "el" (file-name-extension name)))
        (cddr (directory-files (concat user-emacs-directory "elisp/")))))))

(defconst +literate/elisp-byte-compiled
  `(,@(mapcar #'(lambda (x) (replace-regexp-in-string ".el" ".elc" x)) +literate/output-files)
    ,@(mapcar #'(lambda (x) (replace-regexp-in-string ".el" ".elc" x)) +literate/elisp-files)))

;; Setup predicates and loading

(defun +literate/--reduce-bool (bools init)
  (if (= (length bools) 0)
      init
    (+literate/--reduce-bool (cdr bools) (and (car bools) init))))

(defun +literate/output-files-exist ()
  "Checks if output files exist, for compilation purposes."
  (if (< 1 (length +literate/output-files))
      (+literate/--reduce-bool (mapc #'file-exists-p +literate/output-files) t)
    (file-exists-p (car +literate/output-files))))

(defun +literate/load-config ()
  "Load all files in +literate/output-files."
  (interactive)
  (mapc #'(lambda (x) (load-file x)) +literate/output-files))

(autoload #'org-babel-tangle-file "ob-tangle")

(defun +literate/compile-config ()
  "Compile all files in +literate/org-files via org-babel-tangle."
  (interactive)
  (message "Compiling files...")
  (mapcar #'org-babel-tangle-file +literate/org-files)
  (message "Files compiled")

  (message "Byte-compiling literate files...")
  (mapcar #'(lambda (file) (byte-compile-file file)) +literate/output-files)
  (message "Literate files byte-compiled")
  (message "Byte compiling init.el, early-init.el, elisp/*")
  (mapcar #'(lambda (file) (byte-compile-file file)) +literate/elisp-files)
  (message "Finished byte-compiling"))

(provide 'literate)
;;; literate.el ends here
