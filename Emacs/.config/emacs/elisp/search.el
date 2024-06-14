;;; search.el --- Search a list of git directories at once!  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Aryadev Chavali

;; Author: Aryadev Chavali <aryadev@aryadevchavali.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License Version
;; 2 as published by the Free Software Foundation.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Given a list of git source directories, provide a completing-read
;; interface which will narrow and give you a file.

;;; Code:

(autoload #'swiper "swiper")

(defvar +search/directories
  '("~/Dotfiles/" "~/Text/" "~/.local/src/dwm/" "~/.local/src/dwmblocks/" "~/.local/src/st/" "~/Website/")
  "List of directories to get candidates from.")

(defun +search/get-candidates (directory)
  "Get files from DIRECTORY using counsel-git-cands.
Returns a list of files with the directory preprended to them."
  (let* ((default-directory directory)
         (names (split-string
                 (shell-command-to-string "git ls-files -z --full-name --")
                 "\0")))
    (mapcar #'(lambda (name)
           (concat directory name))
       names)))

(defun +search/get-all-candidates ()
  (string-join
   (mapcar #'(lambda (directory)
          (+search/get-candidates (expand-file-name directory)))
      +search/directories)))

(defun +search/find-file (&optional arg)
  (interactive "P")
  (let ((file-name (completing-read "Find file: " (+search/get-all-candidates) nil t)))
    (with-current-buffer (find-file file-name)
      (if arg
          (swiper)))))

(defun +search/-format-grep-candidates ()
  (string-join
   (mapcar
    #'(lambda (x) (concat "\"" x "\""))
    (cl-remove-if #'directory-name-p (+search/get-all-candidates)))
   " "))

(defun +search/search-all ()
  (interactive)
  (let ((format-str "grep --color=auto -nIH --null -e \"%s\" -- %s")
        (term (read-string "Search for: "))
        (candidates (+search/-format-grep-candidates)))
    (grep (format format-str term candidates))))

(provide 'search)
;;; search.el ends here
