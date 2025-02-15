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

(autoload #'grep "grep")
(autoload #'thread-last "subr-x")

(defvar +search/directories
  '("~/Dotfiles/" "~/Text/" "~/.local/src/dwm/" "~/.local/src/dwmblocks/"
    "~/.local/src/st/" "~/.local/src/dmenu/" "~/Website/")
  "List of directories to get candidates from.")

(defun +search/get-candidates (directory)
  "Get files from DIRECTORY using `git ls-files`.
Returns a list of files with the directory preprended to them."
  (let* ((default-directory directory)
         (git-files (shell-command-to-string "git ls-files -z --full-name --"))
         (names (split-string git-files "\0")))
    (mapcar #'(lambda (name) (concat directory name)) names)))

(defun +search/get-all-candidates ()
  (thread-last (mapcar #'(lambda (directory) (expand-file-name directory)) +search/directories)
               (mapcar #'(lambda (directory) (+search/get-candidates directory)))
               (cl-reduce #'(lambda (x y) (append x y)))))

(defun +search/find-file ()
  (interactive)
  (find-file (completing-read "Find file: " (+search/get-all-candidates) nil t)))

(defun +search/-format-grep-candidates ()
  (thread-last (+search/get-all-candidates)
               (cl-remove-if #'directory-name-p)
               (mapcar #'(lambda (x) (concat "\"" x "\" ")))
               (string-join)))

(defun +search/search-all ()
  (interactive)
  (let ((term (read-string "Search for: " (thing-at-point 'symbol)))
        (candidates (+search/-format-grep-candidates)))
    (thread-last candidates
                 (format "grep --color=auto -nIHZe \"%s\" -- %s" term)
                 (grep))
    (next-error)))

(provide 'search)
;;; search.el ends here
