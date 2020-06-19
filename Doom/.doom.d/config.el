;;; ~/Dotfiles/doom.d/config.el -*- lexical-binding: t; -*-

(defconst +bootstrap/org-files '("config.org" "personal.org") "Org files to load relative to =doom-private-dir=/org/*")
(defconst +bootstrap/byte-compile t "Byte compile org files")

(defun +bootstrap/apply-files (babel-func)
  "Apply babel-func to a transformed list of org-files"
  (cl-map nil #'(lambda (file) (funcall babel-func (expand-file-name (concat doom-private-dir "org/" file)))) +bootstrap/org-files))

(defun +bootstrap/load-files ()
  "Load org files into current Emacs session via =org-babel-load-file="
  (interactive)
  (+bootstrap/apply-files #'org-babel-load-file))

(defun +bootstrap/compile-files ()
  "Compile org files via =org-babel-tangle-file="
  (interactive)
  (+bootstrap/apply-files #'org-babel-tangle-file))

(+bootstrap/load-files)
