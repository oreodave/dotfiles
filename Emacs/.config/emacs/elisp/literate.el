;;; literate.el --- My literate configuration setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Aryadev Chavali

;; Author: Aryadev Chavali <aryadev@aryadevchavali.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License version
;; 2 as published by the Free Software Foundation.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; Predicates
(defun +literate/org-to-el (name)
  (string-replace ".org" ".el" name))

(defun +literate/org-to-elc (name)
  (string-replace ".org" ".elc" name))

(defun +literate/el-to-elc (name)
  (string-replace ".el" ".elc" name))

(defun +literate/filter (predicate list)
  (if (null list)
      nil
    (let ((first (car list))
	        (rest (cdr list)))
      (if (funcall predicate first)
	        (cons first (+literate/filter predicate rest))
	      (+literate/filter predicate rest)))))

(defun +literate/org-p (filename)
  (string= "org" (file-name-extension filename)))

(defun +literate/el-p (filename)
  (string= "el" (file-name-extension filename)))

(defun +literate/--reduce-bool (bools init)
  (if (= (length bools) 0)
      init
    (+literate/--reduce-bool (cdr bools) (and (car bools) init))))

;; Files
(defconst +literate/org-files
  (+literate/filter
   #'+literate/org-p
   (mapcar #'(lambda (file) (concat user-emacs-directory file))
      (cddr (directory-files user-emacs-directory)))))

(defconst +literate/el-init-files
  `(,(concat user-emacs-directory "early-init.el")
    ,(concat user-emacs-directory "init.el")))

(defconst +literate/el-lib-files
  (mapcar
   #'(lambda (name) (concat user-emacs-directory "elisp/" name))
   ;; Only take .el files
   (+literate/filter
	  #'+literate/el-p
    (cddr (directory-files (concat user-emacs-directory "elisp/"))))))

(defconst +literate/el-org-files
  (mapcar #'+literate/org-to-el +literate/org-files))

(defconst +literate/el-files
  (cl-concatenate
   'list
   +literate/el-init-files
   +literate/el-lib-files
   +literate/el-org-files))

(defconst +literate/elc-init-files
  (mapcar #'+literate/el-to-elc +literate/el-init-files))

(defconst +literate/elc-lib-files
  (mapcar #'+literate/el-to-elc +literate/el-lib-files))

(defconst +literate/elc-org-files
  (mapcar #'+literate/org-to-elc +literate/org-files))

(defvar +literate/bytecompile? t
  "Bytecompile all files?")

;; Basic compilation and loading files
(autoload #'org-babel-tangle-file "ob-tangle")

(defun +literate/tangle-if-old (org-file)
  (let ((output-file (+literate/org-to-el org-file)))
    (when (file-newer-than-file-p org-file output-file)
      (message "[Literate]:\tTangle(%s)->%s" org-file output-file)
      (org-babel-tangle-file org-file))))

(defun +literate/byte-compile-if-old (el-file)
  (let ((output-file (+literate/el-to-elc el-file)))
    (when (file-newer-than-file-p el-file output-file)
      (message "[Literate]:\tByteCompile(%s)->%s" el-file output-file)
      (byte-compile-file el-file))))

(defun +literate/load-org-file (org-file)
  (+literate/tangle-if-old org-file)
  (load-file (+literate/org-to-el org-file)))

(defun +literate/load-config ()
  "Load the config.el."
  (interactive)
  (mapcar #'+literate/tangle-if-old +literate/org-files)
  (load-file (concat user-emacs-directory "config.el")))

;; Compiling all files
(defun +literate/compile-init-files ()
  (when +literate/bytecompile?
    (message "[Literate/init]: Byte compiling init files...")
    (mapc #'+literate/byte-compile-if-old +literate/el-init-files))
  (message "[Literate/init]: Init files compiled!"))

(defun +literate/compile-lib-files ()
  (when +literate/bytecompile?
    (message "[Literate/lib]: Byte compiling lib files...")
    (mapc #'+literate/byte-compile-if-old +literate/el-lib-files))
  (message "[Literate/lib]: Lib files compiled!"))

(defun +literate/compile-org-files ()
  (message "[Literate/org]: Tangling org files...")
  (mapc #'+literate/tangle-if-old +literate/org-files)
  (message "[Literate/org]: Tangled org files!")
  (when +literate/bytecompile?
    (message "[Literate/org]: Byte compiling org files...")
    (mapc #'+literate/byte-compile-if-old +literate/el-org-files)
    (message "[Literate/org]: Byte compiled org files!")))

(defun +literate/compile-config ()
  "Compile all files in +literate/org-files via org-babel-tangle."
  (interactive)
  (message "[Literate]: Starting compilation...")
  (+literate/compile-init-files)
  (+literate/compile-lib-files)
  (+literate/compile-org-files)
  (message "[Literate]: Finished compilation!"))

;; Cleaning config
(defun +literate/clean-config ()
  "Removes all .el files generated by literate org files and .elc
files by byte compilation"
  (interactive)
  (message "[Literate]: Cleaning configuration...")
  (mapcar #'delete-file
     (cl-concatenate
      'list
      +literate/el-org-files
      +literate/elc-init-files
      +literate/elc-lib-files
      +literate/elc-org-files))
  (message "[Literate]: Cleaned configuration!"))

(provide 'literate)
;;; literate.el ends here
